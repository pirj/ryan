-module(retemplate).
-export([parse/1, scan/1]).

% Yes, it's a bad trick, we should get rid of that, but currently there's no way to scan with text in the beginning, thus prepending with braces
scan({string, String}) ->
  {ok, Scanned, _EndLine} = retem_scan:string([${, $} | binary_to_list(String)]),
  Scanned.

parse({string, _String} = Input) ->
  parse(scan(Input));
parse(Scanned) ->
  {ok, Parsed} = retem_parse:parse(Scanned),
  Parsed.
