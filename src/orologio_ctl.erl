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

-module(orologio_ctl).
-export([start/0, stop/0]).

start() ->
  [NodeStr, Command | Args] = init:get_plain_arguments(),
  Node = list_to_atom(NodeStr),
  io:format("Calling ~p: ~s(~p)~n", [Node, Command, Args]),
  handle(Node, list_to_atom(Command), Args),
  halt(0).

handle(Node, stop, _Args) ->
  rpc:call(Node, init, stop, []);

handle(Node, Cmd, Arg) when (Cmd == restart_nod) or (Cmd == reload_cfg)  ->
  {ok, Scan, _} = erl_scan:string(lists:flatten(Arg)),
  {ok, Name} = erl_parse:parse_term(Scan),
  rpc:call(Node, orologio_sups, Cmd, [Name]);

handle(Node, graph, [Host, Elem, Img | Opts]) ->
  {ok, Scan, _} = erl_scan:string(lists:flatten(Opts)),
  {ok, EOpts} = erl_parse:parse_term(Scan),
  %io:format("grapg args: ~p~n", [EOpts]);
  rpc:call(Node, orologio_graph, graph, [{list_to_binary(Host), list_to_binary(Elem)}, Img, EOpts]);

handle(Node, graph_all, [Img | Opts]) ->
  {ok, Scan, _} = erl_scan:string(lists:flatten(Opts)),
  {ok, EOpts} = erl_parse:parse_term(Scan),
  %io:format("grapg args: ~p~n", [EOpts]);
  rpc:call(Node, orologio_graph, graph, [all, Img, EOpts]);

handle(_Node, _Com, _Args) ->
  io:format("Command not found~n", []).

stop() ->
  ok.