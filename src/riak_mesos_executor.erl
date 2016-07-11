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

-record(state,
        {
         riak_node,
         framework_info,
         agent_info,
         task_status,
         rnp_state
        }).

-type state() :: #state{}.

-spec start_link() -> {ok, pid()}.
start_link() ->
    erl_mesos_executor:start_link(?MODULE, ?MODULE, [], []).

-spec init(state()) -> {ok, erl_mesos_executor:'Call.Subscribe'(), state()}.
init(_Options) ->
    {ok, #'Call.Subscribe'{}, #state{}}.

-spec registered(erl_mesos_executor:executor_info(),
                 erl_mesos_executor:'Event.Subscribed'(), state()) ->
    {ok, state()}.
registered(ExecutorInfo, EventSubscribed, State) ->
    lager:info("registered ~p~n", [ExecutorInfo]),
    #'Event.Subscribed'{framework_info = FrameworkInfo,
                        agent_info = AgentInfo} = EventSubscribed,
    {ok, State#state{
                     framework_info = FrameworkInfo,
                     agent_info = AgentInfo
                    }}.

-spec reregister(erl_mesos_executor:executor_info(), state()) ->
    {ok, erl_mesos_executor:'Call.Subscribe'(), state()}.
reregister(_ExecutorInfo, State) ->
    {ok, #'Call.Subscribe'{}, State}.

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
    TaskStatus0 = create_task_status(ExecutorInfo, TaskId, AgentId) ,
    {ok, RNPSetup} = rme_rnp:setup(TaskInfo),
    case rme_rnp:start(RNPSetup) of
        {ok, RNPStarted, Bytes} ->
            TaskStatus = update_task_status(ExecutorInfo, TaskStatus0, 'TASK_RUNNING',
                                    Bytes),
            {ok, State#state{task_status = TaskStatus, rnp_state = RNPStarted}};
        {error,_} = Err ->
            lager:warning("Failed to start: ~p", [Err]),
            TaskStatus = update_task_status(ExecutorInfo, TaskStatus0, 'TASK_FAILED'),
            %% TODO Surely we need to return some other state here...
            {ok, State#state{task_status = TaskStatus}}
    end.

-spec kill_task(erl_mesos_executor:executor_info(),
                erl_mesos_executor:'Event.Kill'(), state()) ->
    {ok, state()}.
kill_task(ExecutorInfo, EventKill,
          #state{task_status = TaskStatus0, rnp_state = RNPSt0} = State) ->
    #'Event.Kill'{task_id = TaskId} = EventKill,
    lager:debug("Killing task: ~p~n", [TaskId]),
    ok = rme_rnp:stop(RNPSt0),
    TaskStatus = update_task_status(ExecutorInfo, TaskStatus0, 'TASK_KILLED'),
    {ok, State#state{task_status = TaskStatus}}.

-spec acknowledged(erl_mesos_executor:executor_info(),
                   erl_mesos_executor:'Event.Acknowledged'(), state()) ->
    {ok, state()}.
acknowledged(_ExecutorInfo, EventAcknowledged, State) ->
    lager:debug("Acknowledged: ~p~n", [EventAcknowledged]),
    {ok, State}.

-spec framework_message(erl_mesos_executor:executor_info(),
                        erl_mesos_executor:'Event.Message'(), state()) ->
    {ok, state()}.
framework_message(ExecutorInfo, #'Event.Message'{data = <<"finish">>},
                  State) ->
    lager:debug("Force finishing riak node"),
    #state{task_status = TaskStatus0, rnp_state = RNPSt0} = State,
    ok = rme_rnp:force_stop(RNPSt0),
    TaskStatus = update_task_status(ExecutorInfo, TaskStatus0,
                                    'TASK_FINISHED'),
    {ok, State#state{task_status = TaskStatus, rnp_state = undefined}};
framework_message(_ExecutorInfo, EventMessage, State) ->
    lager:debug("Got framework message: ~s~n", [EventMessage]),
    {ok, State}.

-spec shutdown(erl_mesos_executor:executor_info(), state()) ->
    {stop, state()}.
shutdown(ExecutorInfo, State) ->
    #state{task_status = TaskStatus0, rnp_state = RNPSt0} = State,
    lager:info("Shutting down the executor"),
    ok = rme_rnp:stop(RNPSt0),
    TaskStatus = update_task_status(ExecutorInfo, TaskStatus0, 'TASK_KILLED'),
    {stop, State#state{task_status=TaskStatus}}.

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
terminate(_ExecutorInfo, Reason, State) ->
    #state{rnp_state = RNPSt0} = State,
    lager:info("Terminating the executor, reasons: ~p~n", [Reason]),
    ok = rme_rnp:force_stop(RNPSt0).

%% Internal functions.

create_task_status(ExecutorInfo, TaskId, AgentId) ->
    Timestamp = timestamp(),
    Uuid = erl_mesos_utils:uuid(),
    TaskStatus = #'TaskStatus'{task_id = TaskId,
                               source = 'SOURCE_EXECUTOR',
                               agent_id = AgentId,
                               timestamp = Timestamp,
                               uuid = Uuid
                               },
    ok = update_task_status(ExecutorInfo, TaskStatus, 'TASK_STARTING'),
    TaskStatus.

update_task_status(ExecutorInfo, TaskStatus0, State) ->
    update_task_status(ExecutorInfo, TaskStatus0, State, undefined).

update_task_status(ExecutorInfo, TaskStatus0, State, Data) ->
    Time = erlang:system_time(seconds),
    TaskStatus = TaskStatus0#'TaskStatus'{state = State,
                                          data = Data,
                                          timestamp = Time
                                          },
    ok = erl_mesos_executor:update(ExecutorInfo, TaskStatus),
    TaskStatus.

-spec timestamp() -> float().
timestamp() ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    float(MegaSecs * 1000000 + Secs).
