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

-module(mod_log_file).
-behaviour(gen_log).
-export([open/1, close/0, report/2, report/3]).

-include("include/orologio.hrl").

open(File) ->
  error_logger:logfile({open, File}),
  error_logger:tty(false).

close() ->
  error_logger:logfile(close).

report(debug, LogMsg) ->
  error_logger:info(LogMsg);
report(Typ, LogMsg) ->
  Fun = list_to_atom(atom_to_list(Typ) ++ "_report"),
  error_logger:Fun(LogMsg).
report(Typ, LogMsg, Allow) ->
  case lists:member(Typ, Allow) of
    'true' ->
      report(Typ, LogMsg);
    _ ->
      ok
  end.
