-module(rme_lifeline).

-behaviour(gen_server).

-export([start_link/0]).
-export([register/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("erl_mesos/include/executor_protobuf.hrl").

-record(state, {
          watching = [] :: list({reference(), pid()})
         }).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register(Pid) ->
    gen_server:call(?MODULE, {register, Pid}).

init([]) ->
    {ok, #state{}}.

handle_call({register, Pid}, _From, State) ->
    #state{watching = W} = State,
    Ref = erlang:monitor(process, Pid),
    {reply, ok, State#state{watching = [ {Ref, Pid} | W ]}};
handle_call(_Request, _From, State) -> Reply = ok, {reply, Reply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({'DOWN', MRef, process, MPid, Reason}, State) ->
    lager:info("rme_lifeline saw process ~p die because ~p", [MPid, Reason]),
    case lists:keysearch(MRef, 1, State#state.watching) of
        {value, {MRef, MPid}} ->
            %% I didn't watch my buddies die face down in the muck so that you... Ugh!
            {stop, Reason, State};
        _ -> {noreply, State}
    end;
handle_info(_Info, State) -> {noreply, State}.

terminate(Reason, #state{}=State) ->
    {stop, Reason, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
