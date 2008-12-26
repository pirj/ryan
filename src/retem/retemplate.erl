-module(retemplate).
-export([parse/1, scan/1]).

scan({string,String}) ->
  {ok, Scanned, _EndLine} = retem_scan:string(binary_to_list(String)),
  Scanned.

parse({string, _String} = Input) ->
  parse(scan(Input));
parse(Scanned) ->
  {ok, Parsed} = retem_parse:parse(Scanned),
  Parsed.
