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
    lager:info("launchTask: ~p~n", [TaskInfo]),
    #'TaskInfo'{
       name=_Name,
       task_id=TaskId, % NB: This is a #'TaskID'{}
       slave_id=_SlaveId,
       resources=_,
       executor=_,
       command=_,
       container=_,
       data=_,
       health_check=_,
       labels=_,
       discovery=_
      }=TaskInfo,
    TaskStatus = #'TaskStatus'{ task_id=TaskId, state='TASK_STARTING' },
    {ok, driver_running} = executor:sendStatusUpdate(TaskStatus),
    {ok, RNPSt0} = riak_mesos_rnp:setup(TaskInfo),
    ok = riak_mesos_rnp:start(RNPSt0), %% TODO Update state here
    {ok, driver_running} = executor:sendStatusUpdate(TaskStatus#'TaskStatus'{state='TASK_RUNNING'}),
    {ok, State#state{rnp_state=RNPSt0}}.

-spec killTask(TaskID :: #'TaskID'{}, #state{}) -> {ok, #state{}}.
killTask(#'TaskID'{}=TaskId, #state{rnp_state=RNPSt0}=State) ->
    %% TODO A dilemma: do we send the status update first, then kill it?
    %% Or do we kill it, then send the status update.
    TaskStatus = #'TaskStatus'{task_id=TaskId, state='TASK_KILLED'}, %% TODO Obvs there needs to be more data here
    executor:sendStatusUpdate(TaskStatus),
    riak_mesos_rnp:stop(RNPSt0),
    %% TODO Update the state
    {ok, State}.

-spec frameworkMessage(Message :: string(), #state{}) -> {ok, #state{}}.
frameworkMessage("finish", #state{}=State) ->
    lager:info("Force finishing riak node"),
    riak_mesos_rnp:force_stop(),
    %% TODO Update state
    {ok, State};
frameworkMessage(Message, #state{}=State) ->
    lager:debug("Got framework message: ~s~n", [Message]),
    {ok, State}.

-spec shutdown(#state{}) -> {ok, #state{}}.
shutdown(#state{}=State) ->
    lager:info("Shutting down the executor"),
    riak_mesos_rnp:stop(),
    %% Remember: the behaviour takes care of stopping the process appropriately
    %% not us.
    {ok, State}.

-spec error(Message :: string(), #state{}) -> {ok, #state{}}.
error(Message, #state{}=State) ->
    lager:debug("Got error message: ~p~n", [Message]),
    {ok, State}.
