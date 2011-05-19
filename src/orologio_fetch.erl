-module(orologio_fetch).
-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([parse_elem_munin/1, parse_elem/1]).

-include("include/orologio.hrl").

-record(fetch_state, {host, elem, tm = 60000, typ = spawn, env, parse_elem = parse_elem_munin}).

start_link({Host, Elem}) ->
  gen_server:start_link(?MODULE, {Host, Elem}, []).

init({Host, Elem}) ->
  process_flag(trap_exit, true),
  gen_server:cast(self(), {init, <<"Started">>}),
  {ok, #fetch_state{host = Host, elem = Elem}}.

handle_call(stop, _From, State) ->
  {stop, stop, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({init, Act}, State) ->
  Host = State#fetch_state.host,
  Elem = State#fetch_state.elem,
  Cfg = orologio_utils:get_conf_all([Host, Elem], {fetch, []}),
  NState = lists:foldl(fun gen_state/2, State, Cfg),
  Tm = timer:send_interval(NState#fetch_state.tm, self(), {fetch, NState#fetch_state.typ}),
  orologio_utils:log_report(info, [{orologio_utils:gen_name_mod({fetch, State#fetch_state.host, State#fetch_state.elem}),
                                   Act}]),
  {noreply, NState#fetch_state{tm = Tm}};

handle_cast(recfg, State) ->
  timer:cancel(State#fetch_state.tm),
  handle_cast({init, <<"Config reloaded">>}, State);
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({fetch, spawn}, State) ->
  Host = State#fetch_state.host,
  Elem = State#fetch_state.elem,
  spawn(binary_to_atom(Host, latin1), ?MOD_SPAWN, start, [{self(), Elem, State#fetch_state.env}]),
  {noreply, State};

handle_info({data_spawn_oth, {Typ, Data}}, State) ->
  orologio_utils:log_report(info, [{fetch, {State#fetch_state.host, State#fetch_state.elem, {Typ, Data}}}]),
  {noreply, State};

handle_info({data_spawn, Data, Time}, State) ->
  Fn = State#fetch_state.parse_elem,
  orologio_utils:event(send, {{fetch, State#fetch_state.host, State#fetch_state.elem}, {update, lists:flatten(lists:map(fun(El) -> ?MODULE:Fn(El) end, Data)), Time}}),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  orologio_utils:log_report(info, [{orologio_utils:gen_name_mod({fetch, State#fetch_state.host, State#fetch_state.elem}),
                            <<"Stoped">>}]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

gen_state({Elem, Conf}, State) ->
  case Elem of
    time -> State#fetch_state{tm = Conf};
    type -> State#fetch_state{typ = Conf};
    parse_elem -> State#fetch_state{parse_elem = Conf};
    env -> State#fetch_state{env = Conf};
    _ -> State
  end.

parse_elem_munin(Elem) ->
  Re = "^[a-z0-9]*\\" ++ [?ELEM_DELIM1]  ++ "{1}[a-z]*\\s[0-9,\\.]*$",
  case re:run(Elem, list_to_binary(Re)) of
    {match, _} ->
      {Field, FVal} = lists:splitwith(fun(El) -> El /= ?ELEM_DELIM1 end, binary_to_list(Elem)),
      {Field, orologio_utils:check_number(FVal -- ?ELEM_DELIM2)};
    _ ->
      orologio_utils:log_report(error, [{<<"Can not parse: ">>, Elem}]),
      []
  end.

parse_elem(Elem) ->
  orologio_utils:parse_term(binary_to_list(Elem)).
