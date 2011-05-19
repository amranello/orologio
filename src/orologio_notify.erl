-module(orologio_notify).
-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("include/orologio.hrl").

-record(notify_state, {host, elem, dir, fmsg, allow, allow_ign}).

start_link({Host, Elem}) ->
  gen_server:start_link(?MODULE, {Host, Elem}, []).

init({Host, Elem}) ->
  process_flag(trap_exit, true),
  gen_server:cast(self(), {init, <<"Started">>}),
  {ok, #notify_state{host = Host, elem = Elem}}.

handle_call(stop, _From, State) ->
  {stop, stop, ok, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({event, Msg, Time}, State) ->
  Dir = filename:absname(binary_to_list(State#notify_state.dir)),
  {Dt, Tm} = calendar:local_time(),
  DtBn = erl_eval:add_binding('Date', Dt, erl_eval:new_bindings()),
  TmBn = erl_eval:add_binding('Time', Tm, DtBn),
  AlertProgs = case erl_eval:exprs(State#notify_state.allow, TmBn) of
                 {value, true, _} ->
                   {ok, AlPr} = file:list_dir(Dir),
                   AlPr;
                 _ ->
                   State#notify_state.allow_ign
               end,
  SMsg = orologio_utils:parse_term(State#notify_state.fmsg,
                                   [{"$host", State#notify_state.host}, {"$elem", State#notify_state.elem},
                                    {"$time", Time}, {"$msg", io_lib:format("~p", [Msg])}]
                                  ),
  plists:foreach(fun(Ag) -> os:cmd(filename:join([Dir, Ag]) ++ " " ++ SMsg) end, AlertProgs),
  {noreply, State};
handle_cast({init, Act}, State) ->
  Host = State#notify_state.host,
  Elem = State#notify_state.elem,
  orologio_utils:event(join, {{notify, Host, Elem}, self()}),
  orologio_utils:event(join, {{limit, Host, Elem}, self()}),
  Cfg = orologio_utils:get_conf_all([Host, Elem], {notify, []}),
  Dir = proplists:get_value(dir, Cfg, "agents"),
  FMsg = proplists:get_value(fmsg, Cfg, ""),
  Allow = proplists:get_value(allow, Cfg, "true."),
  AllowIgnore = proplists:get_value(allow_ignore, Cfg, []),
  orologio_utils:log_report(info, [{orologio_utils:gen_name_mod({notify, State#notify_state.host, State#notify_state.elem}),
                                   Act}]),
  {noreply, State#notify_state{dir = list_to_binary(Dir), fmsg = FMsg,
                               allow = orologio_utils:parse_exprs(Allow), allow_ign = AllowIgnore}};
handle_cast(recfg, State) ->
  orologio_utils:event(leave, {{fetch, State#notify_state.host, State#notify_state.elem}, self()}),
  orologio_utils:event(leave, {{limit, State#notify_state.host, State#notify_state.elem}, self()}),
  handle_cast({init, <<"Config reloaded">>}, State);
handle_cast(_Request, State) ->
  %io:format("Notif: ~p~n", [Request]),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  orologio_utils:log_report(info, [{orologio_utils:gen_name_mod({notify, State#notify_state.host, State#notify_state.elem}),
                            <<"Stoped">>}]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

