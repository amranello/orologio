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

-module(orologio_sups).
-behaviour(supervisor).

-export([connect_nodes/1, add_nod/1, del_nod/1, restart_nod/1, reload_cfg/1, event/2]).
-export([start_link/1, init/1]).

-include("include/orologio.hrl").

start_link(Name) ->
  supervisor:start_link({local, list_to_atom(orologio_utils:check_str(Name) ++ "_sup")}, ?MODULE, Name).

init(Name) ->
  Nods = get_nod({mod, Name}),
  CNods = check_nods(Name, Nods),
  Subs = get_subs(Name, Nods),
  subs_act(create, Name, Subs),
  {ok, {{one_for_one, 10, 60}, CNods}}.

connect_nodes(Nodes) ->
  plists:foreach(
                 fun(Nod) ->
                   net_kernel:connect_node(orologio_utils:check_atom(Nod))
                 end,
                 Nodes
                ).

subs_act(Act, Mod, Nods) ->
  plists:foreach(fun({{Host, El}, _, _, _, _, _}) -> orologio_utils:event(Act, {Mod, Host, El}) end, Nods).

get_subs(Mod, Nods) ->
  plists:filter(fun({{Host, El}, _, _, _, _, _}) -> 
                   orologio_utils:get_conf([Host, El], {Mod, subsriber, false})
                   end, Nods).

check_nods(Mod, Nods) ->
  plists:filter(fun({{Host, El}, _, _, _, _, _}) -> 
                   orologio_utils:get_conf([Host, El], {Mod, ignore, false}) == 'false'
                   end, Nods).

get_nod({mod, Name}) ->
  lists:flatten(plists:map(fun(Host) -> get_nod({mod_host, Name, Host}) end, orologio_utils:get_elems([])));
get_nod({host, Host}) ->
  lists:flatten(plists:map(fun(Name) -> get_nod({mod_host, Name, Host}) end, orologio_utils:get_opt(mods, [])));
get_nod({mod_host, Name, Host}) ->
  lists:map(fun(El) -> get_nod({all, Name, Host, El}) end, orologio_utils:get_elems([Host]));
get_nod({host_elem, Host, Elem}) ->
  get_nod({all, orologio_utils:get_opt(mods, []), Host, Elem});
get_nod({all, Names, Host, Elem}) when is_list(Names) ->
  lists:map(fun(Name) -> get_nod({all, Name, Host, Elem}) end, Names);
get_nod({all, Name, Host, Elem}) ->
  ModName = list_to_atom(?MOD_PART ++ orologio_utils:check_str(Name)),
  {{Host, Elem}, {ModName, start_link, [{Host, Elem}]}, permanent, 5000, worker,
    [ModName]}.

add_nod({mod, Name}) ->
  add_nod(Name, get_nod({mod, Name}));
add_nod({host, Host}) ->
  plists:foreach(fun(Name) -> add_nod(Name, get_nod({mod_host, Name, Host})) end, orologio_utils:get_opt(mods, []));
add_nod({mod_host, Name, Host}) ->
  add_nod(Name, get_nod({mod_host, Name, Host}));
add_nod({host_elem, Host, El}) ->
  add_nod({all, orologio_utils:get_opt(mods, []), Host, El});
add_nod({all, Names, Host, El}) when is_list(Names) ->
  plists:foreach(
                fun(Name) ->
                  add_nod({all, Name, Host, El})
                end,
                Names
               );
add_nod({all, Name, Host, Elem}) ->
  add_nod(Name, [get_nod({all, Name, Host, Elem})]).

add_nod(Name, Nods) ->
  CNods = check_nods(Name, Nods),
  Subs = get_subs(Name, Nods),
  subs_act(create, Name, Subs),
  plists:foreach(
                 fun(Nod) ->
                   supervisor:start_child(list_to_atom(orologio_utils:check_str(Name) ++ "_sup"), Nod),
                   {{Host, _Elem}, _, _, _, _, _} = Nod,
                   net_kernel:connect_node(orologio_utils:check_atom(Host))
                 end,
                 CNods
                ).

del_nod({mod, Name}) ->
  plists:foreach(fun(Host) -> del_nod({mod_host, Name, Host}) end, orologio_utils:get_elems([]));
del_nod({host, Host}) ->
  plists:foreach(fun(Name) -> del_nod({mod_host, Name, Host}) end, orologio_utils:get_opt(mods, []));
del_nod({mod_host, Name, Host}) ->
  plists:foreach(
                 fun(El) ->
                   del_nod({all, Name, Host, El})
                 end,
                 orologio_utils:get_elems([Host])
                );
del_nod({host_elem, Host, Elem}) ->
  del_nod({all, orologio_utils:get_opt(mods, []), Host, Elem});
del_nod({all, Names, Host, Elem}) when is_list(Names) ->
  plists:foreach(
                 fun(Name) ->
                   del_nod({all, Name, Host, Elem})
                 end,
                 Names
                );
del_nod({all, Name, Host, Elem}) ->
  SupName = list_to_atom(orologio_utils:check_str(Name) ++ "_sup"),
  Subs = get_subs(Name, [{{Host, Elem}, 1, 1, 1, 1, 1}]),
  subs_act(delete, Name, Subs),
  supervisor:terminate_child(SupName, {Host, Elem}),
  supervisor:delete_child(SupName, {Host, Elem}).

restart_nod({mod, Name}) ->
  plists:foreach(
                 fun(Host) ->
                   restart_nod({mod_host, Name, Host})
                 end,
                 orologio_utils:get_elems([])
                );
restart_nod({host, Host}) ->
  plists:foreach(
                 fun(Name) ->
                   restart_nod({mod_host, Name, Host})
                 end,
                 orologio_utils:get_opt(mods, [])
                );
restart_nod({mod_host, Name, Host}) ->
  plists:foreach(
                 fun(Elem) ->
                   restart_nod({all, Name, Host, Elem})
                 end,
                 orologio_utils:get_elems([Host])
                );
restart_nod({host_elem, Host, Elem}) ->
  restart_nod({all, orologio_utils:get_opt(mods, []), Host, Elem});
restart_nod({all, Names, Host, El}) when is_list(Names) ->
  plists:foreach(
                 fun(Name) ->
                   restart_nod({all, Name, Host, El})
                 end,
                 Names
                );
restart_nod({all, Name, Host, El}) ->
  SupName = list_to_atom(orologio_utils:check_str(Name) ++ "_sup"),
  supervisor:restart_child(SupName, {Host, El}).

reload_cfg({mod, Name}) ->
  plists:foreach(
                 fun(Ht) ->
                   reload_cfg({mod_host, Name, Ht})
                 end,
                 orologio_utils:get_elems([])
                );
reload_cfg({host, Host}) ->
  plists:foreach(
                 fun(Name) ->
                   reload_cfg({mod_host, Name, Host})
                 end,
                 orologio_utils:get_opt(mods, [])
                );
reload_cfg({mod_host, Name, Host}) ->
  plists:foreach(
                 fun(El) ->
                   reload_cfg({all, Name, Host, El})
                 end,
                 orologio_utils:get_elems([Host])
                );
reload_cfg({host_elem, Host, Elem}) ->
  plists:foreach(
                 fun(Name) ->
                   reload_cfg({all, Name, Host, Elem})
                 end,
                 orologio_utils:get_opt(mods, [])
                );
reload_cfg({all, Names, Host, El}) when is_list(Names) ->
  plists:foreach(
                 fun(Name) ->
                   reload_cfg({all, Name, Host, El})
                 end,
                 Names
                );
reload_cfg({all, Name, Host, El}) ->
  event({local, Name, Host, El}, recfg).

event({local, Name, Host, Elem}, Msg) ->
  SupName = list_to_atom(orologio_utils:check_str(Name) ++ "_sup"),
  plists:foreach(
                fun({{Ht, El}, Pid, _, _}) when (Ht == Host) and (El == Elem) ->
                    gen_server:cast(Pid, Msg);
                   (_AEl) ->
                    ok
                end,
                supervisor:which_children(SupName)
               ).
