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

-module(orologio_limit).
-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("include/orologio.hrl").

-record(limit_state, {host, elem, exprs, last, rep, msg, label}).

start_link({Host, Elem}) ->
  gen_server:start_link(?MODULE, {Host, Elem}, []).

init({Host, Elem}) ->
  process_flag(trap_exit, true),
  gen_server:cast(self(), {init, <<"Started">>}),
  {ok, #limit_state{host = Host, elem = Elem}}.

handle_call(stop, _From, State) ->
  {stop, stop, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({update, Elems, Time}, State) ->
  NLast = plists:map(fun({Nm, Dat}) ->
                       Bn = erl_eval:add_binding('Val', Dat, erl_eval:new_bindings()),
                       Expr = case lists:keyfind(Nm, 1, State#limit_state.exprs) of
                                {Nm, E} -> E;
                                _ -> orologio_utils:parse_exprs("Val > 0.")
                              end,
                       {value, Val, _} = erl_eval:exprs(Expr, Bn),
                       {Nm, Val}
                     end,
                     Elems
                    ),
  {Last, CLast} = case State#limit_state.rep  of
                    no ->
                      CLst = plists:filter(
                                           fun({Nm, Val}) ->
                                             case lists:keyfind(Nm, 1, State#limit_state.last) of
                                               {Nm, Val} -> false;
                                               _ -> true
                                             end
                                           end,
                                           NLast
                                          ),
                      Lst = plists:map(
                                       fun({Nm, Val}) ->
                                         case lists:keyfind(Nm, 1, NLast) of
                                           {Nm, Val} -> {Nm, Val};
                                           {Nm, Ret} -> {Nm, Ret};
                                           _ -> {Nm, Val}
                                         end
                                       end,
                                       State#limit_state.last
                                      ),
                      {Lst, CLst};
                    yes ->
                      {State#limit_state.last, NLast}
                  end,
  SLast = plists:map(
                     fun({Nm, Val}) ->
                       {proplists:get_value(Nm, State#limit_state.label, Nm), Val}
                     end,
                     CLast
                    ),
  case {State#limit_state.msg, length(SLast)} of
    {grp, Len} when Len > 0 ->
      orologio_utils:event(send, {{limit, State#limit_state.host, State#limit_state.elem}, {event, SLast, Time}});
    {_Oth, Len} when Len > 0 ->
      lists:foreach(fun(Msg) -> orologio_utils:event(send, {{limit, State#limit_state.host, State#limit_state.elem}, {event, Msg, Time}}) end, SLast);
    _ ->
     ok
  end,
  NState = State#limit_state{last = Last},
  {noreply, NState};
handle_cast({init, Act}, State) ->
  Host = State#limit_state.host,
  Elem = State#limit_state.elem,
  orologio_utils:event(join, {{fetch, Host, Elem}, self()}),
  Cfg = orologio_utils:get_conf_all([Host, Elem], {limit, []}),
  Exprs = plists:map(
                    fun({Nm, Expr}) -> 
                      {Nm, orologio_utils:parse_exprs(Expr)}
                    end,
                    proplists:get_value(exprs, Cfg, [])
                   ),
  Last = proplists:get_value(exprs_def, Cfg, []),
  orologio_utils:log_report(info, [{orologio_utils:gen_name_mod({limit, State#limit_state.host, State#limit_state.elem}),
                                   Act}]),
  {noreply, State#limit_state{exprs = Exprs,
                    rep = proplists:get_value(repeat, Cfg, no),
                    last = Last,
                    msg = proplists:get_value(msg, Cfg, no),
                    label = proplists:get_value(label, Cfg, [])
                   }};
handle_cast(recfg, State) ->
  orologio_utils:event(join, {{fetch, State#limit_state.host, State#limit_state.elem}, self()}),
  handle_cast({init, <<"Config reloaded">>}, State);
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  orologio_utils:log_report(info, [{orologio_utils:gen_name_mod({limit, State#limit_state.host, State#limit_state.elem}),
                            <<"Stoped">>}]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
