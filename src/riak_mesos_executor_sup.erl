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

-module(riak_mesos_executor_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([start_cmd/3]).
-export([start_cmd/2]).
-export([init/1]).
-export([log/3]).

%% ===================================================================
%% API
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Callbacks
%% ===================================================================

init([]) ->
    Exec = {exec, {exec, start_link, [[]]}, permanent, 10000, worker, [exec]},
    Mod = riak_mesos_executor,
    Executor = {Mod, {Mod, start_link, []}, permanent, 300, worker, dynamic},
    {ok, { {one_for_one, 5, 10}, [Exec, Executor]} }.

start_cmd(Location, Exe, Opts) ->
    start_cmd(Exe, [{cd, Location} | Opts]).
start_cmd(Exe, Opts0) ->
    %% TODO This is probably asking for trouble, but....
    ChildId =
        case Exe of
            % List of lists i.e. strings
            [[_|_]=Exe0 | _Args] -> {rnpsb, Exe0};
            [_|_] -> {rnpsb, Exe}
        end,
    %% TODO Hopefully we can rid ourselves of this eventually..
    Opts = ensure_loggers(Opts0),
    supervisor:start_child(
      ?MODULE,
      {ChildId, {rnp_sup_bridge, start_link, [Exe, Opts]},
       permanent, 300, supervisor, dynamic}).

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
