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
         t_notice_stop/1,
         t_controlled_stop/1,
         t_brutal_stop/1
        ]).
-export([
         t_check_match/1,
         t_check_match_more/1,
         t_check_match_most/1,
         t_small_replace/1,
         t_brace_replace/1,
         t_medium_replace/1,
         t_large_replace/1,
         t_idempotent/1,
         t_holistic/1
        ]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
     {group, template},
     {group, rnp_sampler}
    ].

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap, {seconds, 30}}].

groups() ->
    [{rnp_sampler, [],
      [
       t_not_running,
       t_start_app,
       t_notice_stop,
       t_controlled_stop,
       t_brutal_stop
      ]},
     {template, [sequence],
      [
       t_check_match,
       t_check_match_more,
       t_check_match_most,
       t_small_replace,
       t_brace_replace,
       t_medium_replace,
       t_large_replace,
       t_idempotent,
       t_holistic
      ]}
    ].

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
    {up, _} = wait_for(up, Config).

t_notice_stop(Config) ->
    PrivDir = ?config(priv_dir, Config),
    process_flag(trap_exit, true),
    {ok, _Sup} = rnp_exec_sup:start_link(1, 1), %% no restart tolerance
    {ok, Sampler, _} = rnp_exec_sup:start_cmd(
                PrivDir, [?config(runner, Config), "console", "-noinput"], []),
    {up, _} = wait_for(up, Config),
    _ = stop(Config),
    ct:pal("Sampler alive:~s~n", [is_process_alive(Sampler)]),
    down = wait_for(down, Config).

t_controlled_stop(Config) ->
    process_flag(trap_exit, true),
    PrivDir = ?config(priv_dir, Config),
    {ok, _Sup} = rnp_exec_sup:start_link(1, 1), %% no restart tolerance
    {ok, Sampler, _} = rnp_exec_sup:start_cmd(
                         PrivDir, [?config(runner, Config), "console", "-noinput"], []),
    {up, _} = wait_for(up, Config),
    ok = rnp_exec_sup:stop_cmd(Sampler),
    down = wait_for(down, Config).

t_brutal_stop(Config) ->
    process_flag(trap_exit, true),
    PrivDir = ?config(priv_dir, Config),
    {ok, _Sup} = rnp_exec_sup:start_link(1, 1),
    {ok, Sampler, _} = rnp_exec_sup:start_cmd(
                         PrivDir, [?config(runner, Config), "console", "-noinput"], []),
    {up, _} = wait_for(up, Config),
    ok = rnp_exec_sup:kill_cmd(Sampler),
    down = wait_for(down, Config).

status(Config) ->
    case os:cmd(?config(admin, Config) ++ " status") of
        "Node is not running!\n" -> down;
        [_|_]=Other ->
            case (catch mochijson2:decode(Other)) of
                {'EXIT', {{case_clause, _}, _Stack}} ->
                    % This happens when the sampler app is up but not ready
                    down;
                {struct, _}=JSON ->
                    {up, JSON}
            end
    end.

-define(WAIT_TOTAL, 5000).
-define(WAIT_STEP, 100).
wait_for(Status, Config) ->
    wait_for(Status, Config, ?WAIT_TOTAL).

wait_for(_, _, Empty) when Empty =< ?WAIT_STEP -> {error, timeout};
wait_for(down, Config, Timeout) ->
    case status(Config) of
        down -> down;
        _ ->
            timer:sleep(100),
            wait_for(down, Config, Timeout - ?WAIT_STEP)
    end;
wait_for(up, Config, Timeout) ->
    case status(Config) of
        {up, _}=Status -> Status;
        down ->
            timer:sleep(100),
            wait_for(up, Config, Timeout - ?WAIT_STEP)
    end.

stop(Config) ->
    os:cmd(?config(runner, Config) ++ " stop").

t_check_match(_) ->
    nomatch = rnp_template:match(<<"{{foobarbaz}}">>),
    nomatch = rnp_template:match(<<"{ {{foobarbaz}} }">>),
    {match,_} = rnp_template:match(<<"{{{.FooBarBaz}}}">>).

t_check_match_more(Conf) ->
    {ok, T} = file("advanced-fixed.config", Conf),
    nomatch = rnp_template:match(T).

t_check_match_most(Conf) ->
    {ok, T} = file("riak-fixed.conf", Conf),
    nomatch = rnp_template:match(T).

t_small_replace(_) ->
    Tmpl = <<"{{.FooBarBaz}}">>,
    Result = <<"{{foobarbaz}}">>,
    verify(rnp_template:mustachify(Tmpl), Result).

t_brace_replace(_) ->
    Tmpl = <<"{{{.FooBarBaz}}}">>,
    Result = <<"{ {{foobarbaz}} }">>,
    verify(rnp_template:mustachify(Tmpl), Result).

t_medium_replace(Conf) ->
    {ok, Tmpl} = file("advanced.config", Conf),
    {ok, Result} = file("advanced-fixed.config", Conf),
    Answer = rnp_template:mustachify(Tmpl),
    %ct:pal("Replaced template: ~n~s~n", [Answer]),
    verify(Answer, Result).

t_large_replace(Conf) ->
    {ok, Tmpl} = file("riak.conf", Conf),
    {ok, Result} = file("riak-fixed.conf", Conf),
    Answer = rnp_template:mustachify(Tmpl),
    verify(Answer, Result).

t_idempotent(Conf) ->
    {ok, Result} = file("riak-fixed.conf", Conf),
    Answer0 = rnp_template:mustachify(Result),
    verify(Answer0, Result).

t_holistic(Conf) ->
    {ok, AdvT} = file("advanced-fixed.config", Conf),
    {ok, AdvFinal} = file("advanced-final.config", Conf),

    AdvRender = mustache:render(binary_to_list(AdvT),
                                dict:from_list([{cepmdport, 0}])),
    verify(AdvRender, AdvFinal).


file(Name, Conf) ->
    {ok, _} = file:read_file(?config(data_dir, Conf)  ++ Name ).

verify(Answer, Solution) when is_list(Answer) ->
    verify(list_to_binary(lists:flatten(Answer)), Solution);
verify(Answer, Solution) when is_list(Solution) ->
    verify(Answer, list_to_binary(lists:flatten(Solution)));

verify(Answer, Solution) when is_binary(Answer), is_binary(Solution) ->
    case Answer == Solution of
        true -> ok;
        false ->
            MatchUntil = binary:longest_common_prefix([Answer, Solution]),
            io:format("Differs at (~p): ~n~s~n~s~n", [MatchUntil,
                                                   relevant(Answer, MatchUntil),
                                                   relevant(Solution, MatchUntil)]),
            ct:fail({unmatched_result})
    end.

relevant(Bin, _) when byte_size(Bin) < 100 ->
    Bin;
relevant(Bin, MatchUntil) ->
    binary_part(Bin, MatchUntil-50, 100).
