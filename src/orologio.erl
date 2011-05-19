-module(orologio).
-export([start/0, stop/0]).

start() ->
  application:start(orologio).

stop() ->
  application:stop(orologio).