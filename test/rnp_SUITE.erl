-module(rnp_SUITE).
-export([all/0,
         suite/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1]).

-export([
         t_start_app/1
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
       t_start_app
      ]}].

init_per_suite(Config) ->
    ct:pal("Config: ~p~n", [Config]),
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    %% TODO Extraction of the app should be being handled
    %% by the library really
    SamplerTGZ = filename:join([DataDir, "sampler.tar.gz"]),
    ok = erl_tar:extract(SamplerTGZ, [compressed, {cwd, PrivDir}]),
    Runner = filename:join([PrivDir, "sampler", "bin", "sampler"]),
    Admin = filename:join([PrivDir, "sampler", "bin", "sampler-admin"]),
    [{runner, Runner}, {admin, Admin} | Config].
end_per_suite(_Config) -> ok.

t_start_app(Config) ->
    PrivDir = ?config(priv_dir, Config),
    {ok, _Sup} = riak_mesos_executor_sup:start_link(),
    {ok, _Sampler} = rnp_exec_sup:start_cmd(
                      PrivDir, ["sampler/bin/sampler", "console", "-noinput"], []),
    ok.
