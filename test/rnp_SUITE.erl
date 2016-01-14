-module(rnp_SUITE).
-export([all/0,
         suite/0,
         groups/0,
         init_per_group/2,
         end_per_group/2,
         init_per_suite/1,
         end_per_suite/1]).

-export([
         t_not_running/1,
         t_start_app/1,
         t_notice_stop/1
        ]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
     {group, rnp_sampler}
    ].

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap, {seconds, 30}}].

groups() ->
    [{rnp_sampler, [],
      [
       t_not_running,
       t_start_app,
       t_notice_stop
      ]}].

init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.

init_per_group(rnp_sampler, Config) ->
    ct:pal("Config: ~p~n", [Config]),
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    %% TODO Extraction of the app should be being handled
    %% by the library really
    SamplerTGZ = filename:join([DataDir, "sampler.tar.gz"]),
    ok = erl_tar:extract(SamplerTGZ, [compressed, {cwd, PrivDir}]),
    Runner = filename:join([PrivDir, "sampler", "bin", "sampler"]),
    Admin = filename:join([PrivDir, "sampler", "bin", "sampler-admin"]),
    [{runner, Runner}, {admin, Admin} | Config];
init_per_group(_, Config) -> Config.

end_per_group(_, _Config) -> ok.

t_not_running(Config) ->
    down = status(Config).

t_start_app(Config) ->
    PrivDir = ?config(priv_dir, Config),
    {ok, _Sup} = rnp_exec_sup:start_link(),
    {ok, _Sampler, _} = rnp_exec_sup:start_cmd(
                      PrivDir, [?config(runner, Config), "console", "-noinput"], []),
    {up, _} = status(Config).

t_notice_stop(Config) ->
    PrivDir = ?config(priv_dir, Config),
    process_flag(trap_exit, true),
    {ok, _Sup} = rnp_exec_sup:start_link(1, 1), %% no restart tolerance
    {ok, Sampler, _} = rnp_exec_sup:start_cmd(
                PrivDir, [?config(runner, Config), "console", "-noinput"], []),
    {up, _} = status(Config),
    _ = stop(Config),
    receive Foo -> ct:pal("receive: ~p~n", [Foo])
    after 1 -> none end,
    ct:pal("Sampler alive:~s~n", [is_process_alive(Sampler)]),
    down = status(Config).

status(Config) ->
    case os:cmd(?config(admin, Config) ++ " status") of
        "Node is not running!\n" -> down;
        [_|_]=Other ->
            %% TODO this dumps a little json - let's parse it
            {up, Other}
    end.

stop(Config) ->
    os:cmd(?config(runner, Config) ++ " stop").
