-module(rnp_sup_bridge).
-behavior(supervisor_bridge).

-export([start_link/2]).
-export([erl_pid/1, os_pid/1]).
-export([init/1, terminate/2]).


-define(STABILITY_TIME, 3000).

start_link(Exec, Options) ->
    supervisor_bridge:start_link(?MODULE, {Exec, Options}).

erl_pid(Pid) ->
    {Out, _} = exec_info(Pid),
    Out.

os_pid(Pid) ->
    {_, Out} = exec_info(Pid),
    Out.

init({Exec, Options}) ->
    block_until_stable(Exec, Options).

terminate(_Why, OsPid) ->
    lager:info("supervisor_bridge terminating: ~p~n", [_Why]),
    exec:stop(OsPid).

%% @ignore
% erl exec starts the process, and as long as the os can start it, exec sees 
% that as a success. However, services (like redis) can exit out shortly after
% starting, like if they try to bind to port already in use. If we just took
% the raw return from exec to supervisor, we would then get restarted, and
% likely result in hitting the restart intensity. So, we start as usual, but 
% catch any exits within the first few arbitrary length time segments. In short,
% we try to ensure the service we ask exec to start is 'stable'.
block_until_stable(Exec, Options) ->
    block_until_stable(exec:run_link(Exec, Options)).

block_until_stable({ok, Pid, OsPid} = Out) ->
    lager:info("Blocking..."),
    receive
        {'EXIT', Pid, Why} ->
            lager:warning("Failure to remain up for ~p ms", [?STABILITY_TIME]),
            lager:info("Failure reason: ~p", [Why]),
            {error, {Pid, OsPid, Why}}
    after
        ?STABILITY_TIME ->
            _ = put(exec_info, {Pid, OsPid}),
            Out
    end.

exec_info(Pid) ->
    {dictionary, Dict} = process_info(Pid, dictionary),
    {exec_info, Info} = lists:keyfind(exec_info, 1, Dict),
    Info.
