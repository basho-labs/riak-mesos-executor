-module(riak_mesos_executor).

-behaviour(executor).
-export([
         init/1,
         registered/4,
         reregistered/2,
         disconnected/1,
         launchTask/2,
         killTask/2,
         frameworkMessage/2,
         shutdown/1,
         error/2
        ]).
-export([start_link/0]).

-include_lib("erlang_mesos/include/mesos_pb.hrl").

-record(state,
        {
         riak_node,
         exec_info,
         framework_info,
         slave_info,
         task_status,
         rnp_state
        }).

start_link() ->
    executor:start_link(?MODULE, []).

%% spec specifications
-spec init(Args :: any()) -> {ok, #state{}}.
init([]) ->
    {ok, #state{}}.

-spec registered( ExecutorInfo :: #'ExecutorInfo'{},
                      FrameworkInfo :: #'FrameworkInfo'{},
                      SlaveInfo :: #'SlaveInfo'{},
                      #state{})-> {ok, #state{}}.
registered(#'ExecutorInfo'{}=ExInfo,
           #'FrameworkInfo'{}=FwInfo,
           #'SlaveInfo'{}=SlInfo, #state{}=State) ->
    lager:info("~p~n~p~n~p~n", [ExInfo, FwInfo, SlInfo]),
    {ok, State#state{
           exec_info=ExInfo,
           framework_info=FwInfo,
           slave_info=SlInfo
          }}.

-spec reregistered(SlaveInfo :: #'SlaveInfo'{}, #state{}) -> {ok, #state{}}.
reregistered(#'SlaveInfo'{}=SlInfo, #state{}=State) ->
    % TODO We need to separate e.g. lager:info/2 call in registered/2 from this
    % so that we can differentiate between info logging and stuff for CLI
    lager:info("Re-registered Executor on slave ~p~n", [#'SlaveInfo'.hostname]),
    {ok, State#state{slave_info=SlInfo}}.

-spec disconnected(#state{}) -> {ok, #state{}}.
disconnected(#state{}=State) ->
    lager:info("Executor disconnected"),
    {ok, State}.

-spec launchTask(TaskInfo :: #'TaskInfo'{}, #state{}) -> {ok, #state{}}.
launchTask(#'TaskInfo'{}=TaskInfo, #state{}=State) ->
    lager:debug("launchTask: ~p~n", [TaskInfo]),
    #'TaskInfo'{ task_id=TaskId }=TaskInfo,
    TaskStatus = #'TaskStatus'{ task_id=TaskId, state='TASK_STARTING' },
    {ok, driver_running} = executor:sendStatusUpdate(TaskStatus),
    {ok, RNPSetup} = riak_mesos_rnp:setup(TaskInfo),
    case riak_mesos_rnp:start(RNPSetup) of
        {ok, RNPStarted, Bytes} ->
            {ok, driver_running} = executor:sendStatusUpdate(TaskStatus#'TaskStatus'{state='TASK_RUNNING', data=Bytes}),
            {ok, State#state{task_status=TaskStatus, rnp_state=RNPStarted}};
        {error,_}=Err ->
            lager:warning("Failed to start: ~p", [Err]),
            {ok, driver_running} = executor:sendStatusUpdate(TaskStatus#'TaskStatus'{state='TASK_FAILED'}),
            %% TODO Surely we need to return some other state here...
            {ok, State#state{task_status=TaskStatus}}
    end.

-spec killTask(TaskID :: #'TaskID'{}, #state{}) -> {ok, #state{}}.
killTask(#'TaskID'{}=TaskId, #state{task_status=TaskStatus0, rnp_state=RNPSt0}=State) ->
    lager:debug("Killing task: ~p~n", [TaskId]),
    ok = riak_mesos_rnp:stop(RNPSt0),
    TaskStatus = TaskStatus0#'TaskStatus'{state='TASK_KILLED'},
    {ok, driver_running} = executor:sendStatusUpdate(TaskStatus),
    {ok, State#state{task_status=TaskStatus}}.

-spec frameworkMessage(Message :: string(), #state{}) -> {ok, #state{}}.
frameworkMessage("finish", #state{task_status=TaskStatus0, rnp_state=RNPSt0}=State) ->
    lager:debug("Force finishing riak node"),
    TaskStatus = TaskStatus0#'TaskStatus'{state='TASK_KILLED'},
    ok = riak_mesos_rnp:force_stop(RNPSt0),
    {ok, driver_running} = executor:sendStatusUpdate(TaskStatus),
    {ok, State#state{task_status=TaskStatus, rnp_state=undefined}};
frameworkMessage(Message, #state{}=State) ->
    lager:debug("Got framework message: ~s~n", [Message]),
    {ok, State}.

-spec shutdown(#state{}) -> {ok, #state{}}.
shutdown(#state{task_status=TaskStatus0, rnp_state=RNPSt0}=State) ->
    lager:info("Shutting down the executor"),
    TaskStatus = TaskStatus0#'TaskStatus'{state='TASK_KILLED'},
    ok = riak_mesos_rnp:stop(RNPSt0),
    %% Remember: the behaviour takes care of stopping the process appropriately
    %% not us.
    {ok, driver_running} = executor:sendStatusUpdate(TaskStatus),
    {ok, State#state{task_status=TaskStatus}}.

-spec error(Message :: string(), #state{}) -> {ok, #state{}}.
error(Message, #state{}=State) ->
    lager:debug("Got error message: ~p~n", [Message]),
    {ok, State}.
