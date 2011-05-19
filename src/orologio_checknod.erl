-module(orologio_checknod).
-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("include/orologio.hrl").

start_link({_Host, _Elem}) ->
  gen_server:start_link(?MODULE, [], []).

init(_Host) ->
  process_flag(trap_exit, true),
  gen_server:cast(self(), {init, <<"Started">>}),
  {ok, []}.

handle_call(stop, _From, State) ->
  {stop, stop, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({init, Act}, State) ->
  net_kernel:monitor_nodes(true),
  orologio_utils:log_report(info, [{orologio_utils:gen_name_mod({checknod, <<"default">>, <<"default">>}),
                                   Act}]),
  {noreply, State};
handle_cast(recfg, State) ->
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({Ev, Nod}, State) ->
  orologio_utils:event(send, {{notify, <<"default">>, <<"default">>}, {event, {Nod, Ev}, now()}}),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  orologio_utils:log_report(info, [{orologio_utils:gen_name_mod({checknod, <<"default">>, <<"default">>}),
                            <<"Stoped">>}]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
