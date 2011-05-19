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
