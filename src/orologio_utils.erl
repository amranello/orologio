-module(orologio_utils).
-export([get_opt/2, log_report/2, event/2, gen_name_mod/1, get_conf_all/2, get_conf_all/3, get_conf/2, get_conf/3, get_conf/1, get_elems/1, get_elems/2, check_str/1, check_atom/1, check_number/1, check_binary/1, parse_term/2, parse_term/3, gen_time_fmt/1, parse_term/1, parse_exprs/1]).

-include("include/orologio.hrl").

log_report(Typ, LogMsg) ->
  Fun = get_opt(mod_log, mod_log_file),
  Fun:report(Typ, LogMsg, get_opt(log_level, [])).

event(Typ, Msg) ->
  Fun = get_opt(mod_event, mod_event_pg2),
  Fun:Typ(Msg).

get_elems(Conf) ->
  get_elems(agent_cfg, Conf).

get_elems(Typ, Conf) ->
  Fun = get_opt(mod_config, mod_config_file),
  Fun:get_elems(Typ, Conf).

get_conf(Conf) ->
  get_conf(agent_cfg, Conf).

get_conf(Conf, Opt) when is_list(Conf) ->
  get_conf(agent_cfg, Conf, Opt);
get_conf(Typ, Conf) ->
  Fun = get_opt(mod_config, mod_config_file),
  Fun:get_conf(Typ, Conf).

get_conf(Typ, Conf, Opt) ->
  Fun = get_opt(mod_config, mod_config_file),
  Fun:get_conf(Typ, Conf, Opt).

get_conf_all(Conf, Opt) ->
  get_conf_all(agent_cfg, Conf, Opt).
get_conf_all(Typ, Conf, Opt) ->
  Fun = get_opt(mod_config, mod_config_file),
  Fun:get_conf_all(Typ, Conf, Opt).

get_opt(Name, Def) ->
  case application:get_env(?NAME_APP, Name) of
    {ok, Opt} -> Opt;
    _ -> Def
  end.

gen_name_mod({Mod, Host, Elem}) ->
  list_to_binary(check_str(Host) ++ "_" ++ check_str(Mod) ++ "_" ++ check_str(Elem));
gen_name_mod(Name) ->
  Name.

check_str(Val) when is_atom(Val) ->
  atom_to_list(Val);
check_str(Val) when is_binary(Val) ->
  binary_to_list(Val);
check_str(Val) when is_integer(Val) ->
  integer_to_list(Val);
check_str(Val) when is_float(Val) ->
  io_lib:format("~.3f", [Val]);
check_str({_Meg, _Sec, _Mic} = Now) ->
  parse_term("$year-$month-$day $hour:$min:$sec", gen_time_fmt(Now));
check_str(Val) when is_list(Val) ->
  Val.

check_atom(Val) when is_list(Val) ->
  list_to_atom(Val);
check_atom(Val) when is_binary(Val) ->
  binary_to_atom(Val, latin1);
check_atom(Val) when is_atom(Val) ->
  Val.

check_number(Val) when is_list(Val) ->
  case catch list_to_float(Val) of
    {'EXIT', {badarg, _}} ->
      case catch list_to_integer(Val) of
        {'EXIT', {badarg, _}} -> 0;
        Num -> Num
      end;
    Num -> Num
  end;
check_number(Val) when is_number(Val) ->
  Val.

check_binary(Val) when is_list(Val) ->
  list_to_binary(Val);
check_binary(Val) when is_atom(Val) ->
  atom_to_binary(Val, latin1);
check_binary(Val) when is_binary(Val) ->
  Val.

gen_time_fmt({_Meg, _Sec, _Mic} = Now) ->
  {{Y, M, D}, {H, Mn, S}} = calendar:now_to_local_time(Now),
  [{"$year", Y}, {"$month", M}, {"$day", D}, {"$hour", H}, {"$min", Mn}, {"$sec", S}].

parse_term(FTerm, RTerm) ->
  parse_term(FTerm, RTerm, "(:|;|-|\\.|,|_|\\s)").
parse_term(FTerm, RTerm, Delim) ->
  {_, PTerm} = lists:foldl(fun(El, {Dat, Ret}) ->
                              case lists:keyfind(El, 1, Dat) of
                                {El, Val} -> {Dat, Ret ++ orologio_utils:check_str(Val)};
                                _ -> {Dat, Ret ++ orologio_utils:check_str(El)}
                              end
                             end, {RTerm, ""}, re:split(FTerm, Delim, [{return, list}])),
  string:strip(PTerm).

parse_term(Term) ->
  {ok, Scaned, _} = erl_scan:string(Term),
  {ok, Parsed} = erl_parse:parse_term(Scaned),
  Parsed.

parse_exprs(Exprs) ->
  {ok, Scaned, _} = erl_scan:string(Exprs),
  {ok, Parsed} = erl_parse:parse_exprs(Scaned),
  Parsed.
