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
