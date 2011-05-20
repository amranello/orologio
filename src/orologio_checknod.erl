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
