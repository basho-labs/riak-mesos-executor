-module(rnp_exec_sup).
-behaviour(supervisor).

-export([start_link/0,
         start_link/2,
         start_cmd/2,
         start_cmd/3,
         stop_cmd/1,
         kill_cmd/1,
         log/3]).

-export([init/1]).

start_link() -> start_link(5, 60).
start_link(MaxRestarts, MaxSecsBetweenRestarts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [MaxRestarts, MaxSecsBetweenRestarts]).

init([MaxRestarts, MaxSecBetweenRestarts]) ->
    %% NB: We use one_for_one so that restart_child/2 is valid
    SupFlags = {one_for_one, MaxRestarts, MaxSecBetweenRestarts},
    Exec = {exec, {exec, start_link, [[root]]}, permanent, 10000, worker, [exec]},
    {ok, {SupFlags, [Exec]}}.

start_cmd(Location, Exe, Opts) ->
    start_cmd(Exe, [{cd, Location} | Opts]).
start_cmd(Exe, Opts0) ->
    ChildId =
        case Exe of
            % List of lists i.e. strings
            [[_|_]=Exe0 | _Args] -> {rnpsb, Exe0};
            [_|_] -> {rnpsb, Exe}
        end,
    Opts = ensure_loggers(Opts0),
    Child = {ChildId, {rnp_sup_bridge, start_link, [Exe, Opts]},
             transient, % Restart unless it exits with 'normal'
             300, supervisor, dynamic},
    {ok, ChildPid} = supervisor:start_child(?MODULE, Child),
    OSPid = rnp_sup_bridge:os_pid(ChildPid),
    {ok, ChildPid, OSPid}.

stop_cmd(Pid) when is_pid(Pid) ->
    exec:stop(rnp_sup_bridge:erl_pid(Pid)).

kill_cmd(Pid) when is_pid(Pid) ->
	MaybeChild =
		[ C || {C, P, _, _} <- supervisor:which_children(?MODULE),
						 P == Pid ],
	case MaybeChild of
		[] -> {error, not_found};
		[ChildId] ->
			ChildId,
			ok = supervisor:terminate_child(?MODULE, ChildId),
			supervisor:delete_child(?MODULE, ChildId)
	end.

% Make sure there's a logging fun for both stdout and stderr
% Otherwise erlexec uses something nonsensical
ensure_loggers(Opts) ->
    ensure_logger(stdout, ensure_logger(stderr, Opts)).
ensure_logger(Type, Opts) ->
    case proplists:lookup(Type, Opts) of
        none -> [{Type, fun ?MODULE:log/3} | Opts];
        _ -> Opts
    end.

-spec log(stdout | stderr, integer(), binary()) -> ok.
log(stdout, OSPid, Message) ->
    lager:info(fmtlog(OSPid, Message));
log(stderr, OSPid, Message) ->
    lager:error(fmtlog(OSPid, Message));
log(Eh, OSPid, Message) ->
    lager:info("[~s] ~s", [Eh, fmtlog(OSPid, Message)]).

fmtlog(OSPid, Message) ->
    io_lib:format("<~p> ~s", [OSPid, Message]).
