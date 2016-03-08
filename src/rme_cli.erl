%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Basho Technologies Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(rme_cli).
-behaviour(clique_handler).

-export([command/1]).
-export([register/0,
         load_schema/0]).
-export([status/3]).

-export([register_cli/0]).

command(Cmd) ->
    clique:run(Cmd).

register() ->
    clique:register([?MODULE]).

load_schema() ->
    case application:get_env(riak_mesos_executor, schema_dirs) of
        {ok, Directories} ->
            ok = clique_config:load_schema(Directories);
        _ ->
            ok = clique_config:load_schema([code:lib_dir()])
    end.

%% TODO This needs to be more intelligent but it's a start.
status(_Command, [], []) ->
    Status = whereis(riak_mesos_executor) /= undefined,
    [clique_status:text(io_lib:format("{\"alive\": ~p}", [Status]))].

%%%===================================================================
%%% Callbacks
%%%===================================================================

register_cli() ->
    clique:register_usage(["riak_mesos_executor-admin"], usage()),
    clique:register_command(["riak_mesos_executor-admin", "status"],[],
        [{status, [{shortname, "s"}, {longname, "status"}]}],fun status/3).

%%%===================================================================
%%% Private
%%%===================================================================

usage() ->
    [
     "riak_mesos_executor-admin <sub-command>\n\n",
     "  Interact with a running riak_mesos_executor app.\n\n",
     "  Sub-commands:\n",
     "    status          Get the current status.\n",
     "  Use --help after a sub-command for more details.\n"
    ].
