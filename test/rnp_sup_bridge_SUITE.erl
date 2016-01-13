-module(rnp_sup_bridge_SUITE).

-export([all/0,
         suite/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1]).
-export([
         t_start_stop_app/1,
         t_start_kill_app/1
        ]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
     {group, cmd}
    ].

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap, {seconds, 30}}].

groups() ->
    [
     {cmd, [],
      [
       t_start_stop_app,
       t_start_kill_app
      ]}].

%% TODO This is common to rnp_SUITE too
init_per_suite(Config) ->
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    %% TODO Extraction of the app should be being handled
    %% by the library really
    SamplerTGZ = filename:join([DataDir, "sampler.tar.gz"]),
    ok = erl_tar:extract(SamplerTGZ, [compressed, {cwd, PrivDir}]),
    Runner = filename:join(["sampler", "bin", "sampler"]),
    Admin = filename:join(["sampler", "bin", "sampler-admin"]),
    [{runner, Runner}, {admin, Admin} | Config].
end_per_suite(_Config) -> ok.

t_start_stop_app(Config) ->
    process_flag(trap_exit, true),
    WD = ?config(priv_dir, Config),
    Runner = ?config(runner, Config),
    {ok, _Exec} = exec:start_link([]),
    {ok, SB} = rnp_sup_bridge:start_link([Runner, "console", "-noinput"], [{cd, WD}]),
    % Cleanly stop the app from outside
    os:cmd(WD++"/sampler/bin/sampler stop"),
    % Make sure we are alerted that the application went away 'normal'ly
    receive {'EXIT', SB, normal} -> ok;
            Other -> ct:fail({unexpected_receive, Other})
    after 5000 -> ct:fail(timeout) end.

t_start_kill_app(Config) -> ct:fail(unimplemented).
