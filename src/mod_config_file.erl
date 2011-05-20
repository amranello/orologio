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

-module(mod_config_file).
-behaviour(gen_config).

-export([get_conf_all/3, get_conf/3, get_conf/2, get_elems/2]).

-include("include/orologio.hrl").

get_conf(Typ, Conf, Mods) when is_list(Mods) ->
  Cfg = get_conf(Typ, Conf),
  lists:map(fun({Mod, Def}) ->
              case lists:keyfind(Mod, 1, Cfg) of
                {Mod, Val} -> {Mod, Val};
                _ -> {Mod, Def}
              end
            end, Mods);
get_conf(Typ, [], {Mod, Opt, Def}) ->
  case lists:keyfind(Opt, 1, get_conf(Typ, [], {Mod, []})) of
    {Opt, Val} -> Val;
    _ ->
      Def
  end;
get_conf(Typ, Conf, {Mod, Opt, Def}) ->
  case lists:keyfind(Opt, 1, get_conf(Typ, Conf, {Mod, []})) of
    {Opt, Val} -> Val;
    _ ->
      get_conf(Typ, lists:delete(lists:last(Conf), Conf), {Mod, Opt, Def})
  end;
get_conf(Typ, Conf, {Opt, Def}) ->
  case lists:keyfind(Opt, 1, get_conf(Typ, Conf)) of
    {Opt, Val} -> Val;
    _ -> Def
  end.

get_conf_all(Typ, Conf, {Mod, Opt, Def}) ->
  lists:flatten(plists:map(fun(El) ->
                               proplists:get_value(Opt, get_conf(Typ, El, {Mod, []}), Def)
                              end,
                           get_conf_list(Conf)));
get_conf_all(Typ, Conf, {Mod, Def}) ->
  lists:flatten(plists:map(fun(El) ->
                               proplists:get_value(Mod, get_conf(Typ, El), Def)
                              end,
                           get_conf_list(Conf))).

get_conf(Typ, Conf) ->
  {ok, Cfg} = application:get_env(?NAME_APP, Typ),
  Fn = filename:join(lists:append([Cfg], lists:map(fun(El) when is_binary(El) -> binary_to_list(El); (El) -> El end, Conf))),
  Fnd = case filelib:is_dir(Fn) of
          false ->
            Fn;
          true ->
            filename:join([Fn, binary_to_list(?DEF_CFG)])
        end,
  {ok, Ret} = file:consult(Fnd),
  Ret.

get_conf_list([]) ->
  [[]];
get_conf_list(Conf) ->
  [Conf|get_conf_list(lists:delete(lists:last(Conf), Conf))].

get_elems(Typ, Conf) ->
  {ok, Cfg} = application:get_env(?NAME_APP, Typ),
  {ok, Ret} = file:list_dir(filename:join(lists:append([Cfg], lists:map(fun(El) when is_binary(El) -> binary_to_list(El); (El) -> El end, Conf)))),
  lists:map(fun(El) -> list_to_binary(El) end, lists:delete(binary_to_list(?DEF_CFG), Ret)).
