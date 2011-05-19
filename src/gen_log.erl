-module(gen_log).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
  [{open, 0},
   {report, 2},
   {close, 0}
  ];
behaviour_info(_Other) ->
  undefined.
