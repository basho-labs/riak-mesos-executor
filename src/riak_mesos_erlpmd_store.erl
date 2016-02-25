-module(riak_mesos_erlpmd_store).

-behaviour(erlpmd_store).

-export([
         init/1,
         register_node/5,
         node_port/2,
         names/1,
         dump/2,
         node_stopped/2,
         remove_node/2
        ]).

-record(state,
        {
         zookeepers = [] :: list(string()),
         framework  :: string(),
         ports_names = [] :: list()
        }).

%% TODO Check binary vs string for return types throughout

init([]) ->
    %% TODO Pull ZK and FW name from somewhere else
    {ok, #state{}}.

register_node(NodeName,
              {PortNo, NodeType, Protocol, HighestVersion, LowestVersion, Extra}=Payload,
              Fd, Creation, State) ->
    #state{ports_names=Ports}=State,
    lager:debug("Node ~p (~p) connected with payload ~p~n", [NodeName, Fd, Payload]),
    %% TODO Store all the things
    %% TODO Later: maybe we can utilise Extra (from erl_epmd.erl) to store
    %% all the stuff the CEPMD was storing previously?
    Data = term_to_binary({PortNo, NodeType, Protocol, HighestVersion, LowestVersion, Extra, Fd, Creation}),
    %% TODO We might be able to do something with a hostname here to bypass
    %% needing to know more than the nodename?
    %% TODO Does the EPMD protocol allow for returning the hostname when
    %% looking up a node's port?
    {ok, _, _} = add_registered_node("todo-localhost", NodeName, Data),
    {ok, State#state{ports_names=[{Fd, NodeName} | Ports]}}.

add_registered_node(_HostName, NodeName, ExtraData) ->
    {ok, NodesNode, _} = get_zk_node(),
    % Node      , ChildId , Data          , Ephemeral?
    TargetNode = filename:join(NodesNode, NodeName),
    %% TODO This seems somewhat naive and will probably bite me later
    case mesos_metadata_manager:get_node(TargetNode) of
        {error, no_node} ->
            ok;
        {ok, _, _} ->
            ok = mesos_metadata_manager:delete_node(TargetNode)
    end,
    {ok, _Child, _} = mesos_metadata_manager:make_child_with_data(NodesNode, NodeName, ExtraData, false).

get_zk_node() ->
    {ok, Root, _Data} = mesos_metadata_manager:get_root_node(),
    {ok, ErlPMDNode, _} = ensure_child(Root, "erlpmd"),
    {ok, _, _} = ensure_child(ErlPMDNode, "nodes").

ensure_child(Root, Child) ->
    {ok, Children} = mesos_metadata_manager:get_children(Root),
    case lists:member(Child, Children) of
        true ->
            mesos_metadata_manager:get_node(filename:join([Root, Child]));
        false ->
            mesos_metadata_manager:make_child(Root, Child)
    end.

node_port(NodeName, State) ->
    lager:debug("~p:node_port(~p, ~p)", [?MODULE, NodeName, State]),
    {ok, NodesNode, _} = get_zk_node(),
    case mesos_metadata_manager:get_node(filename:join(NodesNode, NodeName)) of
        {ok, _NodeNode, Binload} ->
            Payload = {PortNo, _, _, _, _, _, _, _} = binary_to_term(Binload),
            lager:debug("~p:node_port -> ~p", [?MODULE, PortNo]),
            {ok, {NodeName, Payload}};
        {error, no_node} ->
            lager:debug("~p:node_port -> no_node", [?MODULE]),
            {error, not_found};
        {error,_}=Error ->
            Error
    end.

names(St0) ->
    lager:debug("~p:names(~p)", [?MODULE, St0]),
    {ok, NodesNode, _} = get_zk_node(),
    {ok, Children} = mesos_metadata_manager:get_children(NodesNode),
    Res = [ begin
          {ok, {N, {Port, _, _, _, _, _, _, _}}} = node_port(N, St0),
          {N, Port}
      end || N <- Children ],
    lager:debug("~p:names -> ~p", [?MODULE, Res]),
    {ok, Res}.

%% TODO TODO TODO TODO TODO :)
dump(all, St0) ->
    Children = get_dump(St0),
    Res = [ {N, Port, Fd} || {_, N, Port, Fd} <- Children ],
    lager:debug("~p:dump -> ~p", [?MODULE, Res]),
    {ok, Res};
dump(Type, St0) ->
    Children = get_dump(St0),
    Res = [ {N, Port, Fd} || {Type0, N, Port, Fd} <- Children, Type0 == Type],
    lager:debug("~p:dump -> ~p", [?MODULE, Res]),
    {ok, Res}.

get_dump(St0) ->
    {ok, NodesNode, _} = get_zk_node(),
    {ok, Children} = mesos_metadata_manager:get_children(NodesNode),
    [ begin
          {ok, {N, {Port, Type, _, _, _, _, Fd, _}}} = node_port(N, St0),
          {Type, N, Port, Fd}
      end || N <- Children ].

%% TODO This presents us with a problem: if someone force-stops a node with
%% e.g. 'epmd -port 11111 -stop riak-default-4' BUT that node actually lives
%% on a different host (which is possible because now we sync node data)
%% This only happens if relaxed_command_check is set to true, though.
%% TODO Support relaxed_command_check mode
node_stopped(Fd, State) ->
    #state{ports_names=Ps} = State,
    lager:debug("~p:node_stopped(~p, ~p)", [?MODULE, Fd, State]),
    case proplists:get_value(Fd, Ps) of
        undefined -> ok;
        NodeName ->
            remove_node(NodeName, State)
    end.

remove_node(NodeName, #state{ports_names=Ps0}=St0) ->
    lager:debug("~p:remove_node(~p, _)", [?MODULE, NodeName]),
    {ok, NodesNode, _} = get_zk_node(),
    case mesos_metadata_manager:delete_node(filename:join(NodesNode, NodeName)) of
        ok -> {ok, St0#state{ports_names=(lists:keydelete(NodeName, 2, Ps0))}};
        {error,_}=Error -> Error
    end.
