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
