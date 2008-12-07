-module(retem).

-export([parse/1, match/2]).

parse({string,String}) ->
  {ok, Scanned, _EndLine} = retem_scan:string(binary_to_list(String)),
  {ok, Parsed} = retem_parse:parse(Scanned),
  Parsed.

match({_, V} = Vars, {_, {identifier, Id}}) -> %% remove this as soon as r2e and e2r become automatic
  dict:fetch(Id, V);

match({_, V} = Vars, {identifier, Id}) ->
  dict:fetch(Id, V);

match(Vars, {arithmetic, "+", Expr1, Expr2}) ->
  match(Vars, Expr1) + match(Vars, Expr2).

