-module(retem).

-export([parse/1]).

parse(String) ->
  {ok, Scanned, _EndLine} = retem_scan:string(String),
  {ok, Parsed} = retem_parse:parse(Scanned),
  Parsed.
