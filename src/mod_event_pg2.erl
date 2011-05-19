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