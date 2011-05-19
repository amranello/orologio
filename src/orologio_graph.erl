-module(orologio_graph).

-behaviour(gen_server).

% public API
-export([
		start_link/1,
%		stop/1,
		create/2,
		update/2,
		graph/2,
		graph/3
]).

% gen_server callbacks
-export([init/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		terminate/2,
		code_change/3
]).

-include("include/orologio.hrl").

-record(graph_state, {host, elem, fn, port, mem, rrd}).

% public API

start_link({Host, Elem}) ->
  gen_server:start_link(?MODULE, {Host, Elem}, []).

create({Host, Elem}, {Datastores, RRAs, Options}) ->
  orologio_sups:event({local, graph, Host, Elem}, {create, Datastores, RRAs, Options}).

update({Host, Elem}, {DatastoreValues}) ->
  orologio_sups:event({local, graph, Host, Elem}, {update, DatastoreValues, n});
update({Host, Elem}, {DatastoreValues, Time}) ->
  orologio_sups:event({local, graph, Host, Elem}, {update, DatastoreValues, Time}).

graph({Host, Elem}, {FImg, DDef, VDef, GElem, Options}) ->
  orologio_sups:event({local, graph, Host, Elem}, {graph, FImg, DDef, VDef, GElem, Options}).

graph({Host, Elem}, ImgDir, Opts) ->
  ImgFmt = proplists:get_value(imgfmt, Opts, ".png"),
  Img = filename:join([ImgDir, binary_to_list(Host) ++ "_" ++ binary_to_list(Elem) ++ ImgFmt]),
  graph({Host, Elem}, {Img,
                       orologio_utils:get_conf([Host, Elem], {graph, def, []}),
                       orologio_utils:get_conf([Host, Elem], {graph, vdef, []}),
                       orologio_utils:get_conf([Host, Elem], {graph, gelem, []}),
                       orologio_utils:get_conf([Host, Elem], {graph, options_graph, []}) ++ Opts});
graph(all, ImgDir, Opts) ->
  Nods = plists:zipp(
                     fun(Host) ->
                       orologio_utils:get_elems([Host])
                     end,
                     fun(Host, Elem) ->
                       {Host, Elem}
                     end,
                     orologio_utils:get_elems([])
                    ),
  plists:foreach(
                 fun(Elem) ->
                   graph(Elem, ImgDir, Opts)
                 end,
                 Nods
                ).

% gen_server callbacks

%% @hidden
init({Host, Elem}) ->
  process_flag(trap_exit, true),
  gen_server:cast(self(), {init, <<"Started">>}),
  {ok, #graph_state{host = Host, elem= Elem}}.

%% @hidden

handle_cast({create, Datastores, RRAs, Options}, State) ->
  Fn = filename:join([binary_to_list(State#graph_state.fn), orologio_utils:check_str(State#graph_state.elem) ++ ".rrd"]),
  Command = ["create '", Fn, "' ", format_create_options(Options), " ",
             string:join(format_datastores(Datastores), " "), " ",
             string:join(format_archives(RRAs), " "), "\n"],
  send_port(State#graph_state.mem, State#graph_state.rrd, State#graph_state.port, Command),
  {noreply, State};
handle_cast({update, DatastoreValues, Time}, State) ->
  {Datastores, Values} = format_datastore_values(DatastoreValues),
  Timestamp = case Time of
                n ->
                  "N";
                {Megaseconds, Seconds, _Microseconds} ->
                  integer_to_list((Megaseconds * 1000000) + Seconds);
                Other when is_list(Other) ->
                  Other
              end,
  Fn = filename:join([binary_to_list(State#graph_state.fn), orologio_utils:check_str(State#graph_state.elem) ++ ".rrd"]),
  Command = ["update '", Fn, "' -t ", string:join(Datastores, ":"), " ", Timestamp, ":",
             string:join(Values, ":"), "\n"],
  %io:format("Graph: ~s~n", [string:join(Command, "")]),
  send_port(State#graph_state.mem, State#graph_state.rrd, State#graph_state.port, Command),
  {noreply, State};
handle_cast({graph, FImg, DDef, VDef, GElem, Options}, State) ->
  Fn = filename:join([binary_to_list(State#graph_state.fn), orologio_utils:check_str(State#graph_state.elem) ++ ".rrd"]),
  Command = ["graph '", FImg, "' ", format_create_options(Options), " ",
             string:join(format_definition(DDef, Fn), " "), " ",
             string:join(format_vdefinition(VDef), " "), " ",
             string:join(format_graph(GElem), " "), "\n"],
  %io:format("rrd graph: ~s~n", [string:join(Command, "")]),
  send_port(State#graph_state.mem, State#graph_state.rrd, State#graph_state.port, Command),
  {noreply, State};

handle_cast({init, Act}, State) ->
  Host = State#graph_state.host,
  Elem = State#graph_state.elem,
  orologio_utils:event(join, {{fetch, Host, Elem}, self()}),
  RRDTool = orologio_utils:get_opt(rrdtool, os:find_executable("rrdtool")),
  Cfg = orologio_utils:get_conf_all([Host, Elem], {graph, []}),
  Dir = filename:absname(proplists:get_value(dir, Cfg, "rrddb")),
  NDir = filename:join([Dir, orologio_utils:check_str(Host)]),
  os:cmd("mkdir '" ++ NDir ++ "'"),
  gen_server:cast(self(), {create, proplists:get_value(elems, Cfg, []), proplists:get_value(archives, Cfg, []),
         proplists:get_value(options_create, Cfg, [])}),
  Mem = proplists:get_value(mem, Cfg, false),
  Port = start_port(Mem, RRDTool),
  orologio_utils:log_report(info, [{orologio_utils:gen_name_mod({graph, State#graph_state.host, State#graph_state.elem}),
                                   Act}]),
  {noreply, State#graph_state{fn = list_to_binary(NDir), port = Port, mem = Mem,
                              rrd = orologio_utils:check_binary(RRDTool)}};

handle_cast(recfg, State) ->
  orologio_utils:event(leave, {{fetch, State#graph_state.host, State#graph_state.elem}, self()}),
  catch port_command(State#graph_state.port, "quit\n"),
  catch port_close(State#graph_state.port),
  handle_cast({init, <<"Config reloaded">>}, State);

handle_cast(_Msg, State) ->
  {noreply, State}.

%% @hidden
handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(Request, _From, State) ->
  {reply, {unknown_call, Request}, State}.

%% @hidden
handle_info({_Port, {data, {eol, Data}}}, State) ->
  case io_lib:write_string(Data) of
    "\"OK"++_ ->
      ok;
    "\"ERROR:"++Message ->
      orologio_utils:log_report(warning, [{orologio_utils:gen_name_mod({graph, State#graph_state.host, State#graph_state.elem})
                                , Message}]);
    _Err ->
      ok
  end,
  {noreply, State};
handle_info(_Info, State) ->
  %io:format("info: ~p~n", [Info]),
  {noreply, State}.

%% @hidden
terminate(_Reason, State) ->
  catch port_command(State#graph_state.port, "quit\n"),
  catch port_close(State#graph_state.port),
  orologio_utils:log_report(info, [{orologio_utils:gen_name_mod({graph, State#graph_state.host, State#graph_state.elem}),
                                   <<"Stoped">>}]),
  ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% internal functions

start_port(true, RRDTool) ->
  open_port({spawn_executable, orologio_utils:check_str(RRDTool)}, [{line, ?PORT_LINE}, {args, ["-"]}]);
start_port(false, _RRDTool) ->
  0.

send_port(true, _RRDTool, Port, Comm) ->
  port_command(Port, Comm);
send_port(false, RRDTool, _Pt, Comm) ->
  Port = open_port({spawn_executable, orologio_utils:check_str(RRDTool)}, [{line, 1024}, {args, ["-"]}]),
  port_command(Port, Comm),
  port_command(Port, "quit\n").
  %port_close(Port).

format_datastores(DTS) ->
  lists:map(
            fun({Name, DST, Arguments}) ->
              io_lib:format("DS:~s:~s:~s", [Name, atom_to_list(DST), format_arguments(DST, Arguments)])
            end,
            DTS
           ).

format_arguments(DST, Arguments) ->
  case DST of
    'COMPUTE' ->
      Arguments;
    _ ->
      case Arguments of
        [Heartbeat, Min, Max] when is_integer(Heartbeat), is_integer(Min), is_integer(Max) ->
          io_lib:format("~B:~B:~B", [Heartbeat, Min, Max]);
        [Heartbeat, undefined, undefined] when is_integer(Heartbeat) ->
          io_lib:format("~B:U:U", [Heartbeat]);
        _ ->
          ok
      end
  end.

format_archives(Arc) ->
  lists:map(
            fun({CF, Xff, Steps, Rows}) ->
              io_lib:format("RRA:~s:~.2f:~B:~B", [CF, Xff, Steps, Rows])
            end,
            Arc
           ).

format_datastore_values(DSV) ->
  format_datastore_values(DSV, [], []).

format_datastore_values([], TAcc, Acc) ->
  {lists:reverse(TAcc), lists:reverse(Acc)};
format_datastore_values([{Name, Value} | T], TAcc, Acc) ->
  format_datastore_values(T, [Name | TAcc], [orologio_utils:check_str(Value) | Acc]).

format_definition(Defs, Filename) ->
  lists:map(
            fun({Name, Agr}) ->
              io_lib:format("'DEF:~s=~s:~s:~s'", [Name,Filename,Name,Agr])
            end,
            Defs
           ).

format_vdefinition(VDefs) ->
  lists:map(
            fun({Name, VName, Agr}) ->
              io_lib:format("VDEF:~s=~s,~s", [VName,Name,Agr])
            end,
            VDefs
           ).

format_graph(Gr) ->
  lists:map(
            fun({Typ, Args}) ->
              "'" ++ atom_to_list(Typ) ++ format_graph_args(Args) ++ "'"
            end,
            Gr
           ).

format_graph_args(Ga) ->
  lists:map(
            fun(El) ->
              case El of
                {width, Width} ->
                  orologio_utils:check_str(Width);
                {text, Txt} ->
                  ":" ++ orologio_utils:check_str(Txt);
                {value, Val} ->
                  ":" ++ orologio_utils:check_str(Val);
                {color, Col} ->
                  "#" ++ orologio_utils:check_str(Col);
                {display, Dis} ->
                  ":" ++ orologio_utils:check_str(Dis);
                _ ->
                  []
              end
            end,
            Ga
           ).

format_create_options(Options) ->
  Noverwrite = format_create_option(noverwrite, {"--no-overwrite "}, Options),
  StepOpt = format_create_option(step, {"--step ", " "}, Options),
  StartOpt = format_create_option(start, {"--start ", " "}, Options),
  StopOpt = format_create_option(stop, {"--end ", " "}, Options),
  LabelOpt = format_create_option(label, {"--title '", "' "}, Options),
  VLabelOpt = format_create_option(vlabel, {"--vertical-label '", "' "}, Options),
  DynLabel = format_create_option(dynlabel, {"--dynamic-labels "}, Options),
  ScaleOpt = format_create_option(scale, {"--alt-autoscale "}, Options),
  FzMod = format_create_option(fzmode, {"--full-size-mode "}, Options),
  ImgOpt = format_create_option(imgfmt, {"--imgformat ", " "}, Options),
  LLimitOpt = format_create_option(llimit, {"--lower-limit ", " "}, Options),
  ULimitOpt = format_create_option(ulimit, {"--upper-limit ", " "}, Options),
  BaseOpt = format_create_option(base, {"--base ", " "}, Options),
  lists:flatten([Noverwrite, StepOpt, StartOpt, StopOpt, LabelOpt, VLabelOpt, DynLabel, ScaleOpt, FzMod, ImgOpt,
                 LLimitOpt, ULimitOpt, BaseOpt]).

format_create_option(Tag, {St, En}, Opts) ->
  case proplists:get_value(Tag, Opts) of
    undefined ->
      [];
    Val ->
      [St, orologio_utils:check_str(Val), En]
  end;
format_create_option(Tag, {St}, Opts) ->
  case proplists:get_value(Tag, Opts) of
    undefined ->
      [];
    _Val ->
      [St]
  end.

