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

-module(plists).
-export([foreach/2, map/2, filter/2, zipp_foreach/3, zipp/3]).
-define(TMOUT, 5000).

foreach(Fun, List) ->
  [spawn(fun() -> Fun(El) end) || El <- List],
  ok.

map(Fun, List) ->
  Parent = self(),
  Pids = [spawn(fun() ->
                    Parent ! {self(), Fun(El)}
                   end) || El <- List],
  [map_r(Pid) || Pid <- Pids].

map_r(Pid) ->
  receive
    {Pid, El} -> El
  after ?TMOUT ->
    case is_process_alive(Pid) of
      'true' ->
        map_r(Pid);
      _ ->
       []
    end
  end.

filter(Fun, List) ->
  Parent = self(),
  Pids = [spawn(fun() ->
                    Parent ! {self(), El, Fun(El)}
                   end) || El <- List],
  lists:flatten([filter_r(Pid) || Pid <- Pids]).

filter_r(Pid) ->
  receive
    {Pid, El, true} -> El;
    {Pid, _El, _} -> []
  after ?TMOUT ->
    case is_process_alive(Pid) of
      'true' ->
        filter_r(Pid);
      _ ->
       []
    end
  end.

zipp_foreach(Comb, Ret, List1) ->
  foreach(
          fun(El1) ->
            foreach(
                    fun(El2) ->
                      Ret(El1, El2)
                    end,
                    Comb(El1)
                   )
          end,
          List1
         ).

zipp(Comb, Ret, List1) ->
  lists:flatten(map(
                    fun(El1) ->
                      map(
                          fun(El2) ->
                            Ret(El1, El2)
                          end,
                          Comb(El1)
                         )
                    end,
                    List1
                   )
               ).
