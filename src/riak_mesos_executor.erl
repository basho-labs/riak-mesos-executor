-module(riak_mesos_executor).

-behaviour(erl_mesos_executor).
-export([
         init/1,
         registered/3,
         disconnected/2,
         reregister/2,
         reregistered/2,
         launch_task/3,
         kill_task/3,
         acknowledged/3,
         framework_message/3,
         error/3,
         shutdown/2,
         handle_info/3,
         terminate/3
        ]).
-export([start_link/0]).

-include_lib("erl_mesos/include/executor_protobuf.hrl").

-include_lib("erl_mesos/include/executor_info.hrl").

-record(state, {riak_node,
                framework_info,
                agent_info,
                task_status,
                rnp_state,
                ack_dict}).

-type state() :: #state{}.

-spec start_link() -> {ok, pid()}.
start_link() ->
    erl_mesos_executor:start_link(?MODULE, ?MODULE, [], []).

-spec init(state()) -> {ok, erl_mesos_executor:'Call.Subscribe'(), state()}.
init(_Options) ->
    {ok, #'Call.Subscribe'{}, #state{ack_dict = dict:new()}}.

-spec registered(erl_mesos_executor:executor_info(),
                 erl_mesos_executor:'Event.Subscribed'(), state()) ->
    {ok, state()}.
registered(ExecutorInfo, EventSubscribed, State) ->
    lager:info("registered ~p~n", [ExecutorInfo]),
    #'Event.Subscribed'{framework_info = FrameworkInfo,
                        agent_info = AgentInfo} = EventSubscribed,
    {ok, State#state{framework_info = FrameworkInfo,
                     agent_info = AgentInfo}}.

-spec reregister(erl_mesos_executor:executor_info(), state()) ->
    {ok, erl_mesos_executor:'Call.Subscribe'(), state()}.
reregister(_ExecutorInfo, #state{ack_dict = AckDict} = State) ->
    UnackUpdates = [Value || {_Key, Value} <- dict:to_list(AckDict)],
    {ok, #'Call.Subscribe'{unacknowledged_updates = UnackUpdates},
     State#state{ack_dict = dict:new()}}.

-spec reregistered(erl_mesos_executor:executor_info(), state()) ->
    {ok, state()}.
reregistered(ExecutorInfo, State) ->
    % TODO We need to separate e.g. lager:info/2 call in registered/2 from this
    % so that we can differentiate between info logging and stuff for CLI
    lager:info("Re-registered Executor on slave ~p~n",
               [ExecutorInfo#executor_info.agent_host]),
    {ok, State}.

-spec disconnected(erl_mesos_executor:executor_info(), state()) ->
    {ok, state()}.
disconnected(ExecutorInfo, State) ->
    lager:info("Executor disconnected ~p~n", [ExecutorInfo]),
    {ok, State}.

-spec launch_task(erl_mesos_executor:executor_info(),
                  erl_mesos_executor:'Event.Launch'(), state()) ->
    {ok, state()}.
launch_task(ExecutorInfo, #'Event.Launch'{task = TaskInfo}, State) ->
    #'TaskInfo'{task_id = TaskId,
                agent_id = AgentId} = TaskInfo,
    lager:debug("Launching task: ~p~n", [TaskId]),
    {ok, State1} = create_task_status(ExecutorInfo, TaskId, AgentId, State),
    {ok, RNPSetup} = rme_rnp:setup(TaskInfo),
    case rme_rnp:start(RNPSetup) of
        {ok, RNPStarted, Bytes} ->
            State2 = State1#state{rnp_state = RNPStarted},
            update_task_status(ExecutorInfo, 'TASK_RUNNING', Bytes, State2);
        {error,_} = Err ->
            lager:warning("Failed to start: ~p", [Err]),
            %% TODO Surely we need to return some other state here...
            update_task_status(ExecutorInfo, 'TASK_FAILED', State1)
    end.

-spec kill_task(erl_mesos_executor:executor_info(),
                erl_mesos_executor:'Event.Kill'(), state()) ->
    {ok, state()}.
kill_task(ExecutorInfo, EventKill, #state{rnp_state = RNPSt0} = State) ->
    #'Event.Kill'{task_id = TaskId} = EventKill,
    lager:debug("Killing task: ~p~n", [TaskId]),
    ok = rme_rnp:stop(RNPSt0),
    update_task_status(ExecutorInfo, 'TASK_KILLED', State).

-spec acknowledged(erl_mesos_executor:executor_info(),
                   erl_mesos_executor:'Event.Acknowledged'(), state()) ->
    {ok, state()}.
acknowledged(_ExecutorInfo, EventAcknowledged, #state{ack_dict = AckDict} = State) ->
    lager:debug("Acknowledged: ~p~n", [EventAcknowledged]),
    #'Event.Acknowledged'{uuid = Uuid} = EventAcknowledged,
    case dict:find(Uuid, AckDict) of
        {ok, TaskStatus} ->
            AckDict1 = dict:erase(Uuid, AckDict),
            lager:debug("TaskStatus is ~p, current state ~p~n",
                        [TaskStatus#'TaskStatus'.state,
                         State#state.task_status#'TaskStatus'.state]),
            {ok, State#state{ack_dict = AckDict1}};
        _ ->
            lager:debug("TaskStatus is not found"),
            {ok, State}
    end.

-spec framework_message(erl_mesos_executor:executor_info(),
                        erl_mesos_executor:'Event.Message'(), state()) ->
    {ok, state()}.
framework_message(ExecutorInfo, #'Event.Message'{data = <<"finish">>},
                  State) ->
    lager:debug("Finishing riak node"),
    ok = rme_rnp:stop(State#state.rnp_state),
    State1 = State#state{rnp_state = undefined},
    update_task_status(ExecutorInfo, 'TASK_FINISHED', State1);
framework_message(_ExecutorInfo, EventMessage, State) ->
    lager:debug("Got framework message: ~s~n", [EventMessage]),
    {ok, State}.

-spec shutdown(erl_mesos_executor:executor_info(), state()) ->
    {stop, state()}.
shutdown(ExecutorInfo, #state{rnp_state = RNPSt0} = State) ->
    lager:info("Shutting down the executor"),
    ok = rme_rnp:stop(RNPSt0),
    {ok, State1} = update_task_status(ExecutorInfo, 'TASK_KILLED', State),
    {stop, State1}.

-spec error(erl_mesos_executor:executor_info(),
            erl_mesos_executor:'Event.Error'(), state()) ->
    {ok, state()}.
error(_ExecutorInfo, EventError, State) ->
    lager:debug("Got error message: ~p~n", [EventError]),
    {ok, State}.

-spec handle_info(erl_mesos_executor:executor_info(), term(), state()) ->
    {ok, state()}.
handle_info(_ExecutorInfo, _Info, State) ->
    {ok, State}.

-spec terminate(erl_mesos_executor:executor_info(), term(), state()) ->
    ok.
terminate(_ExecutorInfo, Reason, #state{rnp_state = RNPSt0} = _State) ->
    lager:info("Terminating the executor, reasons: ~p~n", [Reason]),
    case RNPSt0 of
        undefined ->
            ok;
        _ ->
            ok = rme_rnp:force_stop(RNPSt0)
    end.

%% Internal functions.

create_task_status(ExecutorInfo, TaskId, AgentId, State) ->
    TaskStatus = #'TaskStatus'{task_id = TaskId,
                               source = 'SOURCE_EXECUTOR',
                               agent_id = AgentId},
    State1 = State#state{task_status = TaskStatus},
    update_task_status(ExecutorInfo, 'TASK_STARTING', State1).

update_task_status(ExecutorInfo, TaskStatusState, State) ->
    update_task_status(ExecutorInfo, TaskStatusState, undefined, State).

update_task_status(ExecutorInfo, TaskStatusState, Data, State) ->
    Timestamp = timestamp(),
    Uuid = erl_mesos_utils:uuid(),
    #state{task_status = TaskStatus, ack_dict = AckDict} = State,
    TaskStatus1 = TaskStatus#'TaskStatus'{state = TaskStatusState,
                                          data = Data,
                                          timestamp = Timestamp,
                                          uuid = Uuid},
    AckDict1 = dict:store(Uuid, TaskStatus1, AckDict),
    ok = erl_mesos_executor:update(ExecutorInfo, TaskStatus1),
    {ok, State#state{task_status = TaskStatus1, ack_dict = AckDict1}}.

-spec timestamp() -> float().
timestamp() ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    float(MegaSecs * 1000000 + Secs).
