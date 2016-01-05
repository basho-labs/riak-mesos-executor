-module(riak_mesos_rnp).

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
    lager:info("TaskInfo: ~n~p~n", [TaskInfo]),
    #'TaskInfo'{
       resources=Resources,
       executor=ExecInfo,
       data=RawTData % TODO Is this a json binary string?
      }=TaskInfo,
    #'ExecutorInfo'{
       source="riak", %% TODO Is this really a string?
       command=CmdInfo
      }=ExecInfo,
    #'CommandInfo'{
       shell = _Shell
      }=CmdInfo,
    lager:info("CommandInfo.shell: ~p~n", [_Shell]),
    State0 = process_resources(Resources),
    TD = parse_taskdata(RawTData),
    %% TODO This desperately needs some TLC
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
                   [{cepmdport, State0#state.cepmd_port}]),
    {ok, State0#state{md_mgr=MDMgr}}.

process_resources(Rs) -> process_resources(Rs, #state{}).
process_resources([], #state{}=State) -> State ;
process_resources([#'Resource'{name="ports", type='RANGES', ranges=#'Value.Ranges'{range=Ranges}} | Rs], #state{}=State) ->
    Ports = lists:flatten([ lists:seq(R#'Value.Range'.'begin', R#'Value.Range'.'end') || R <- Ranges ]),
    [ CEPMDPort | Ps ] = Ports, % TODO We can probably just pre-assign all the ports we need here
    process_resources(Rs, State#state{cepmd_port=CEPMDPort, ports=Ps});
process_resources([_ | Rs], #state{}=State) ->
    process_resources(Rs, State).

start(#state{}=State) ->
    #state{cepmd_port=Port} = State,
    % Start CEPMD
    lager:info("Starting CEPMD on port ~p~n", [Port]),
    Ret  = riak_mesos_executor_sup:start_cmd("../",
                                          ["cepmd_linux_amd64",
                                          "-name=riak",
                                              " -zk=master.mesos:2181",
                                              " -riak_lib_dir=root/riak/lib",
                                              " -port="++integer_to_list(Port)],
                                          []), ok.
    %, start("../root/riak", "./bin/riak").

stop(#state{}=St) ->
    lager:info("rme:stop: ~p~n", [St]),
    ok.

force_stop(#'TaskInfo'{}=TaskInfo) ->
    lager:info("rme:force_stop: ~p~n", [TaskInfo]),
    ok.

%install(Location, URI) ->
%    %% TODO Move this all into run_node_package
%    {ok, 200, _Hdrs, Resp} = hackney:get(URI, [], <<>>, []),
%    {ok, TGZ} = hackney:body(Resp), %% TODO Actually Resp is a ref but whatever
%    %% TODO Do we need to close the hackney client at all?
%    ok = erl_tar:extract({binary, TGZ}, [compressed, {cwd, Location}]).

%% TODO Ascertain scope for all of this:
%%  - riak-mesos
%%  - node_package (riak-specific)
%%  - or node_package (generic)

% - [ ] fetch config data from framework at
% `${TaskInfo.Data.URI}/api/v1/clusters/%{clustername}/config`
% - [ ] populate a template from config? see riak-mesos/executor/node.go:116
% - [ ] write config to 'root/riak/etc/riak.conf' (defined by caller)
% - [ ] fetch advanced config from framework at
% `${TaskInfo.Data.URI}/api/v1/clusters/${clustername}/advancedConfig`
% - [ ] populate advanced template from data
% - [ ] write to 'root/riai/etc/advanced.config'
    % We should probably do this outside of rnp, right?
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

% Current implementation follows approximately the following:
% TODO CEPMD
% - [ ] start cepmd based on metadataManager config?
% - [ ] install cepmd into kernel dirs(?) 'root/riak/lib/kernel*'
% - [ ] add `-no_epmd` flag
% - [ ] write epmd port to ${kernel_dirs}/priv/cepmd_port
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

