-module(orologio_app).
-behaviour(application).
-export([start/2, stop/1, config_change/3]).

-include("include/orologio.hrl").

start(_Typ, _Args) ->
  Log = orologio_utils:get_opt(mod_log, mod_log_file),
  Log:open(orologio_utils:parse_term(orologio_utils:get_opt(log, "orologio"), orologio_utils:gen_time_fmt(now()))),
  orologio_sup:start_link([]).

stop(_State) ->
  Log = orologio_utils:get_opt(mod_log, mod_log_file),
  Log:close(),
  ok.

config_change(_Changed, _New, _Removed) ->
  ok.