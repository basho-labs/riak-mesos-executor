-module(riak_mesos_rnp).

%% TODO Tidy up the mochijson2:(en|de)code/1 usage: look at (en|de)code/2
%%
-export([
         setup/1,
         start/1,
         stop/1,
         force_stop/1
        ]).

-include_lib("erlang_mesos/include/mesos_pb.hrl").

-record(taskdata,
        {
         node_name        :: string(),
         zookeepers       :: list(string()),
         framework_name   :: string(),
         cluster_name     :: string(),
         uri              :: string(),
         host             :: string(),
         use_super_chroot :: boolean(),
         http_port        :: non_neg_integer(),
         pb_port          :: non_neg_integer(),
         handoff_port     :: non_neg_integer(),
         disterl_port     :: non_neg_integer()
        }).

-record(state,
        {
         ports = [],
         cepmd_port = 0,
         exes = [],
         task_id,
         taskdata :: #taskdata{},
         md_mgr :: port()
        }).

%% TaskInfo.Data looks like this:
%% {
%%   FullyQualifiedNodeName:riak-default-3@208.43.239.82
%%   Zookeepers:[master.mesos:2181]
%%   FrameworkName:riak
%%   ClusterName:default
%%   URI:http://basho-rmf-sl02:31951
%%   Host:208.43.239.82
%%   UseSuperChroot:true
%%   HTTPPort:31345
%%   PBPort:31346
%%   HandoffPort:0
%%   DisterlPort:31347
%% }

setup(#'TaskInfo'{}=TaskInfo) ->
    lager:debug("TaskInfo: ~n~p~n", [TaskInfo]),
    #'TaskInfo'{
       task_id=TaskId,
       resources=Resources,
       executor=ExecInfo,
       data=RawTData
      }=TaskInfo,
    State0 = process_resources(Resources),
    TD = parse_taskdata(RawTData),
    #state{ports=[CEPMDPort|Ps]}=State1 = filter_ports(TD, State0),
    State2 = State1#state{cepmd_port=CEPMDPort, ports=Ps},
    %% TODO There are cleaner ways to wrangle JSON in erlang
    {struct, TDKV} = mochijson2:decode(RawTData),
    {ok, MDMgr} = mesos_metadata_manager:start_link(TD#taskdata.zookeepers,
                                       TD#taskdata.framework_name),
    % TODO: This location should come from the TaskInfo somehow
    % Probably from one of the #'Resource' records?
    ok = configure("../root/riak/etc/riak.conf",
                   config_uri(TD, "/config"),
                   TDKV),
    ok = configure("../root/riak/etc/advanced.config",
                   config_uri(TD, "/advancedConfig"),
                   [{cepmdport, State2#state.cepmd_port}]),
    {ok, State2#state{task_id=TaskId, taskdata=TD, md_mgr=MDMgr}}.

filter_ports(#taskdata{}=TD, #state{}=State0) ->
    #state{ports=Ps}=State0,
    Preallocated = [TD#taskdata.http_port, TD#taskdata.pb_port, TD#taskdata.handoff_port, TD#taskdata.disterl_port],
    Filtered = Ps -- Preallocated,
    State0#state{ports=Filtered}.

process_resources(Rs) -> process_resources(Rs, #state{}).
process_resources([], #state{}=State) -> State ;
process_resources([#'Resource'{name="ports", type='RANGES', ranges=#'Value.Ranges'{range=Ranges}} | Rs], #state{}=State) ->
    Ports = lists:flatten([ lists:seq(R#'Value.Range'.'begin', R#'Value.Range'.'end') || R <- Ranges ]),
    % TODO We can probably just pre-assign all the ports we need here
    process_resources(Rs, State#state{ports=Ports});
process_resources([_ | Rs], #state{}=State) ->
    process_resources(Rs, State).

%take_port(#state{ports=[P|Ps]}=State) ->
%    {P, State#state{ports=Ps}}.

start(#state{}=State) ->
    #state{cepmd_port=Port,
           task_id=TaskId,
           taskdata=Taskdata,
           exes=Exes} = State,
    % Start CEPMD
    lager:info("Starting CEPMD on port ~s~n", [integer_to_list(Port)]),
    {ok, CEPMD} = rnp_exec_sup:start_cmd("../",
                                          ["cepmd_linux_amd64",
                                              "-name=riak",
                                              "-zk=master.mesos:2181",
                                              "-riak_lib_dir=root/riak/lib",
                                              "-port="++integer_to_list(Port)],
                                          []),
    State1 = State#state{exes=[CEPMD | Exes]},
    %% TODO These should be coming from TaskInfo
    Location = "../root/riak",
    Script = "bin/riak",
    Command = [Script, "console", "-noinput", "-no_epmd"],
    %% TODO This whole process management needs ironing out
    case rnp_exec_sup:start_cmd(Location, Command, []) of
        {ok, Pid} ->
            State2 = State1#state{exes=[Pid | (State1#state.exes) ]},
            %% TODO These arguments are practical but they make little sense.
            case wait_for_healthcheck(fun healthcheck/1, "../root/riak", 60000) of
                ok ->
                    Data = serialise_coordinated_data(Taskdata),
                    {ok, Child, Data} = set_coordinated_child(TaskId, Data),
                    lager:debug("Coordinated data (~p): ~p", [Child, Data]),
                    RexPort = Taskdata#taskdata.http_port,
                    JSON = iolist_to_binary(mochijson2:encode({struct, [{<<"RexPort">>, RexPort}]})),
                    {ok, State2, JSON};
                %% TODO Need to shut it all down here
                %% TODO Perhaps return some other state?
                {error, _}=Err -> Err
            end;
        Error ->
            lager:error("start_cmd returned: ~p~n", [Error])
    end.

serialise_coordinated_data(#taskdata{}=TD) ->
    %% TODO can't we just use the original TDKV?
    RawIO = mochijson2:encode({struct,
                       [
                        {<<"FrameworkName">>, list_to_binary(TD#taskdata.framework_name)},
                        {<<"ClusterName">>,   list_to_binary(TD#taskdata.cluster_name)},
                        {<<"NodeName">>,      list_to_binary(TD#taskdata.node_name)},
                        {<<"DisterlPort">>, TD#taskdata.disterl_port},
                        {<<"PBPort">>,      TD#taskdata.pb_port},
                        {<<"HTTPPort">>,    TD#taskdata.http_port},
                        {<<"Hostname">>, list_to_binary(TD#taskdata.host)}
                       ]}),
    iolist_to_binary(RawIO).

set_coordinated_child(#'TaskID'{}=TaskId, SerialisedData) ->
    {ok, Root, _Data} = mesos_metadata_manager:get_root_node(),
    {ok, Coordinator, _} = ensure_child(Root, "coordinator"),
    {ok, CoordinatedNodes, _} = ensure_child(Coordinator, "coordinatedNodes"),
    {ok, _Child, _} = mesos_metadata_manager:make_child_with_data(CoordinatedNodes, TaskId#'TaskID'.value, SerialisedData, true).

ensure_child(Root, Child) ->
    {ok, Children} = mesos_metadata_manager:get_children(Root),
    case lists:member(Child, Children) of
        true ->
            lager:debug("Using existing child: ~p", [Child]),
            mesos_metadata_manager:get_node(filename:join([Root, Child]));
        false ->
            lager:debug("Creating new child: ~p", [Child]),
            mesos_metadata_manager:make_child(Root, Child)
    end.

healthcheck(Dir) ->
    %% TODO Rid ourselves of special snowflakes
    Logfile = filename:join([Dir, "log", "console.log"]),
    %Admin = filename:join([Dir, "bin", "riak-admin"]),
    log_exists(Logfile)
        andalso log_contains(Logfile, <<"Wait complete for service riak_kv">>).
        %andalso services_available(Admin).

log_exists(Logfile) ->
    filelib:is_file(Logfile).

%% TODO This seems like a Bad Idea™ since this process sticks around for quite some time
log_contains(File, Pattern) ->
    {ok, Bytes} = file:read_file(File),
    case binary:match(Bytes, Pattern) of
        nomatch -> false;
        {_,_} -> true
    end.

%% TODO riak-admin provides a 'wait-for-service <service> [<node>]' command
%% maybe we can reuse that?
%services_available(Admin) ->
%    filelib:is_file(Admin) andalso
%        "[riak_kv,riak_pipe]" ==
%            string:strip(os:cmd(Admin++" services | grep -o '\\[[^]]*\\]'"), both, $\n).

poll({Fun, Args}, Ref, Interval, Timeout) ->
    case Fun(Args) of
        true -> true;
        false ->
            erlang:send_after(Interval, self(), {Ref, interval}),
            receive
                {Ref, timeout} -> false;
                {Ref, interval} ->
                    poll({Fun, Args}, Ref, Interval, Timeout)
            after
                Timeout -> false
            end
    end.


wait_for_healthcheck(Healthcheck, HCArgs, Timeout)
    when is_function(Healthcheck, 1) ->
    Interval = 500,
    Ref = erlang:make_ref(),
    Me  = self(),
    TRef = erlang:send_after(Timeout, Me, {Ref, timeout}),
    _ = spawn(fun() -> Me ! {Ref, poll({Healthcheck, HCArgs}, Ref, Interval, Timeout)} end),
    Result =
        receive
            {Ref, true} -> ok;
            {Ref, timeout} -> {error, healthcheck_timeout};
            {Ref, Failure} ->    {error, {healthcheck_failed, Failure}}
        end,
    _false = erlang:cancel_timer(TRef),
    lager:debug("healthcheck returned: ~p~n", [Result]),
    Result.
    %% TODO Return a #'TaskInfo'{} probably

stop(#state{}=St) ->
    lager:info("rme:stop: ~p~n", [St]),
    ok.

force_stop(#state{}=St) ->
    #state{exes=Exes}=St,
    [ begin
          Ret = supervisor:terminate_child(riak_mesos_executor_sup, E),
          %% TODO supervisor:delete_child/2
          lager:debug("terminate ret: ~p", [Ret])
    end || {ok, E, _} <- Exes ],
    ok.

%install(Location, URI) ->
%    %% TODO Move this all into run_node_package
%    {ok, 200, _Hdrs, Resp} = hackney:get(URI, [], <<>>, []),
%    {ok, TGZ} = hackney:body(Resp), %% TODO Actually Resp is a ref but whatever
%    %% TODO Do we need to close the hackney client at all?
%    ok = erl_tar:extract({binary, TGZ}, [compressed, {cwd, Location}]).

configure(Location, ConfigURI, TD) ->
    {ok, 200, _, Resp} = hackney:get(ConfigURI, [], <<>>, []),
    {ok, ConfigTmpl} = hackney:body(Resp),
    Template = binary_to_list(mustachify(ConfigTmpl)),
    Rendered = mustache:render(Template,
                    dict:from_list(smooth_raw_taskdata(TD))),
    ok = file:write_file(Location, Rendered),
    ok.

%% TODO This. Is. TYRANNY.
%% mustache template variables have to:
%%  - start lowercase (i.e. not with a '.')
%%  - be passed as atom keys
%%  - be in a dict
mustachify(Tmpl) ->
    mustachify(match(Tmpl), Tmpl).

match(Tmpl) ->
    re:run(Tmpl, << "{{\\.[^}]+}}" >>).

%% TODO This might be removable if we change the templates in the golang scheduler
mustachify(nomatch, Tmpl) -> Tmpl;
mustachify({match, [{Start, Len}]}, Tmpl) ->
    Key = binary:part(Tmpl, Start, Len),
    %% TODO Mustache has a bug: if the template variable is immediately followed by a }
    %% that } gets eaten.
    %% i.e. { foobar, {{foobarbaz}}}
    %% becomes { foobar, baz
    %% not { foobar, baz}
    NoDot = binary:replace(Key, <<"{{\.">>, <<"{{">>),
    Space0 = binary:replace(NoDot, <<"{{{">>, <<"{ {{">>),
    Space1 = binary:replace(Space0, <<"}}}">>, <<"}} }">>),
    Lower = list_to_binary(string:to_lower(binary_to_list(Space1))),
    mustachify(binary:replace(Tmpl, Key, Lower)).

smooth_raw_taskdata([]) -> [];
smooth_raw_taskdata([{<<"Zookeepers">>, List} | Rest]) ->
    [ {zookeepers, [ binary_to_list(B) || B <- List]} | smooth_raw_taskdata(Rest) ];
smooth_raw_taskdata([{ K, V } | Rest ]) when is_binary(K), is_binary(V) ->
    [{list_to_atom(string:to_lower(binary_to_list(K))), binary_to_list(V)}
     | smooth_raw_taskdata(Rest) ];
smooth_raw_taskdata([{ K, V } | Rest ]) when is_binary(K) ->
    [{list_to_atom(string:to_lower(binary_to_list(K))), V } | smooth_raw_taskdata(Rest)];
smooth_raw_taskdata([{K,V} | Rest]) ->
    [{K, V} | smooth_raw_taskdata(Rest)].

-spec config_uri(#taskdata{}, string()) -> string().
config_uri(#taskdata{uri=URI, cluster_name=Cluster}, Path) ->
    URI ++ "/api/v1/clusters/" ++ Cluster ++  Path.

-spec parse_taskdata(binary()) -> #taskdata{}.
parse_taskdata(JSON) when is_binary(JSON) ->
    {struct, Data} = mochijson2:decode(JSON),
    #taskdata{
       node_name        = binary_to_list(proplists:get_value(<<"FullyQualifiedNodeName">>, Data)),
       zookeepers       = [ begin
                                [Host, Port] = string:tokens(binary_to_list(B), ":"),
                                {Host, erlang:list_to_integer(Port)}
                            end || B <- proplists:get_value(<<"Zookeepers">>, Data)],
       framework_name   = binary_to_list(proplists:get_value(<<"FrameworkName">>, Data)),
       cluster_name     = binary_to_list(proplists:get_value(<<"ClusterName">>, Data)),
       uri              = binary_to_list(proplists:get_value(<<"URI">>, Data)),
       host             = binary_to_list(proplists:get_value(<<"Host">>, Data)),
       use_super_chroot = proplists:get_value(<<"UseSuperChroot">>, Data),
       http_port        = proplists:get_value(<<"HTTPPort">>, Data),
       pb_port          = proplists:get_value(<<"PBPort">>, Data),
       handoff_port     = proplists:get_value(<<"HandoffPort">>, Data),
       disterl_port     = proplists:get_value(<<"DisterlPort">>, Data)
      }.
