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