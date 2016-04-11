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

-module(rme_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ExecSup = {rnp_exec_sup, {rnp_exec_sup, start_link, []}, permanent, 300, supervisor, dynamic},
    Lifeline = {rme_lifeline, {rme_lifeline, start_link, []}, permanent, 300, worker, dynamic},
    Executor = {riak_mesos_executor, {riak_mesos_executor, start_link, []}, permanent, infinity, worker, dynamic},
    %% Allow 0 restarts: this way, when rme_lifeline terminates, we bring down the application
    {ok, { {one_for_one, 0, 1}, [ExecSup, Lifeline, Executor]} }.
