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

start_link() ->
    erl_mesos_executor:start_link(?MODULE, ?MODULE, [], []).

init(_Options) ->
    {ok, #'Call.Subscribe'{}, #state{}}.

registered(ExecutorInfo, EventSubscribed, State) ->
    lager:info("registered ~p~n", [ExecutorInfo]),
    #'Event.Subscribed'{framework_info = FrameworkInfo,
                        agent_info = AgentInfo} = EventSubscribed,
    {ok, State#state{
                     framework_info = FrameworkInfo,
                     agent_info = AgentInfo
                    }}.

reregister(_ExecutorInfo, State) ->
    {ok, #'Call.Subscribe'{}, State}.

reregistered(ExecutorInfo, State) ->
    % TODO We need to separate e.g. lager:info/2 call in registered/2 from this
    % so that we can differentiate between info logging and stuff for CLI
    lager:info("Re-registered Executor on slave ~p~n",
               [ExecutorInfo#executor_info.agent_host]),
    {ok, State}.

disconnected(ExecutorInfo, State) ->
    lager:info("Executor disconnected ~p~n", [ExecutorInfo]),
    {ok, State}.

launch_task(ExecutorInfo, #'Event.Launch'{task = TaskInfo}, State) ->
    #'TaskInfo'{task_id = TaskId,
                agent_id = AgentId} = TaskInfo,
    Uuid = erl_mesos_utils:uuid(),
    lager:debug("Launching task: ~p~n", [TaskId]),
    TaskStatus = #'TaskStatus'{task_id = TaskId, state = 'TASK_STARTING',
                               agent_id = AgentId, uuid = Uuid},
    ok = erl_mesos_executor:update(ExecutorInfo, TaskStatus),
    {ok, RNPSetup} = rme_rnp:setup(TaskInfo),
    case rme_rnp:start(RNPSetup) of
        {ok, RNPStarted, Bytes} ->
            ok = erl_mesos_executor:update(ExecutorInfo,
                                           TaskStatus#'TaskStatus'{state = 'TASK_RUNNING',
                                                                   data = Bytes}),
            {ok, State#state{task_status = TaskStatus, rnp_state = RNPStarted}};
        {error,_} = Err ->
            lager:warning("Failed to start: ~p", [Err]),
            ok = erl_mesos_executor:update(ExecutorInfo,
                                           TaskStatus#'TaskStatus'{state = 'TASK_FAILED'}),
            %% TODO Surely we need to return some other state here...
            {ok, State#state{task_status = TaskStatus}}
    end.

kill_task(ExecutorInfo, EventKill,
          #state{task_status = TaskStatus0, rnp_state = RNPSt0} = State) ->
    #'Event.Kill'{task_id = TaskId} = EventKill,
    lager:debug("Killing task: ~p~n", [TaskId]),
    ok = rme_rnp:stop(RNPSt0),
    TaskStatus = TaskStatus0#'TaskStatus'{state = 'TASK_KILLED'},
    ok = erl_mesos_executor:update(ExecutorInfo,
                                   TaskStatus#'TaskStatus'{state = 'TASK_KILLED'}),
    {ok, State#state{task_status = TaskStatus}}.

acknowledged(_ExecutorInfo, EventAcknowledged, State) ->
    lager:debug("Acknowledged: ~p~n", [EventAcknowledged]),
    {ok, State}.

framework_message(ExecutorInfo, #'Event.Message'{data = <<"finish">>},
                  State) ->
    lager:debug("Force finishing riak node"),
    #state{task_status = TaskStatus0, rnp_state = RNPSt0} = State,
    TaskStatus = TaskStatus0#'TaskStatus'{state='TASK_FINISHED'},
    ok = rme_rnp:force_stop(RNPSt0),
    ok = erl_mesos_executor:update(ExecutorInfo, TaskStatus),
    {ok, State#state{task_status = TaskStatus, rnp_state = undefined}};
framework_message(_ExecutorInfo, EventMessage, State) ->
    lager:debug("Got framework message: ~s~n", [EventMessage]),
    {ok, State}.

shutdown(_ExecutorInfo, State) ->
    #state{task_status = TaskStatus0, rnp_state = RNPSt0} = State,
    lager:info("Shutting down the executor"),
    TaskStatus = TaskStatus0#'TaskStatus'{state='TASK_KILLED'},
    ok = rme_rnp:stop(RNPSt0),
    {stop, State#state{task_status=TaskStatus}}.

error(_ExecutorInfo, EventError, State) ->
    lager:debug("Got error message: ~p~n", [EventError]),
    {ok, State}.

handle_info(_ExecutorInfo, _Info, State) ->
    {ok, State}.

terminate(_ExecutorInfo, Reason, State) ->
    #state{rnp_state = RNPSt0} = State,
    lager:info("Terminating the executor, reasons: ~p~n", [Reason]),
    ok = rme_rnp:force_stop(RNPSt0),
    stop.
