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

-module(mod_event_pg2).

-export([create/1, delete/1, join/1, leave/1, send/1]).

create(Mod) ->
  catch pg2:create(Mod).

delete(Mod) ->
  catch pg2:delete(Mod).

join({Mod, Pid}) ->
  pg2:join(Mod, Pid).

leave({Mod, Pid}) ->
  pg2:leave(Mod, Pid).

send({Mod, Msg}) ->
  lists:foreach(fun(Pid) -> 
                  gen_server:cast(Pid, Msg)
                  end,
                pg2:get_members(Mod)
               ).