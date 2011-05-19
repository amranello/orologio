-module(gen_config).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
  [
   {get_conf, 3},
   {get_conf_all, 3},
   {get_conf, 2},
   {get_elems, 2}
  ];
behaviour_info(_Other) ->
  undefined.
