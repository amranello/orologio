%% @author Oleg Krivosheev <amranello@gmail.com>
%% @copyright 2010 Oleg Krivosheev

%% Copyright 2010 Oleg Krivosheev
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(orologio_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

-include("include/orologio.hrl").

start_link(Args) ->
  orologio_sups:connect_nodes(orologio_utils:get_opt(join, [])),
  Ret = supervisor:start_link({local, ?MODULE}, ?MODULE, Args),
  orologio_sups:connect_nodes(orologio_utils:get_elems([])),
  Ret.

init(_Args) ->
  {ok, {{one_for_one, 20, 60}, lists:map(fun childs/1, orologio_utils:get_opt(mods, []))}}.

childs(Name) ->
  {list_to_binary(orologio_utils:check_str(Name) ++ "_sup"), {orologio_sups, start_link, [Name]}, permanent, 20000, supervisor, [orologio_sups]}.
