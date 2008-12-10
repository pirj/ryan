-module(retemplate).

-export([parse/1]).

parse({string,String}) ->
  {ok, Scanned, _EndLine} = retem_scan:string(binary_to_list(String)),
  {ok, Parsed} = retem_parse:parse(Scanned),
  Parsed.
