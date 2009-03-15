-module(mochi_shim).
-export([out/1, start_mochi/1]).

start_mochi(Options) ->
	mochiweb_http:start([{loop, fun out/1} | Options]).

out(Req) ->
    Headers = Req:get(headers),
    Method = Req:get(method),

    Abs_Path = Req:get(path),
	[Full_Path|_] = string:tokens(Abs_Path, [$?]),
	PathParts = string:tokens(Full_Path, [$/]),

    Params = Req:parse_post(),

    Accept = Req:get_header_value("Accept"),
    io:format("~p request for ~p with headers ~p~n", [Method, Abs_Path, Accept]),

%    reia_erl:r2e(reia:apply('Ryan', out, [Abs_Path, Method, PathParts, nil, Params]))
	
	Req:respond({200, [{"Content-Type", "text/plain"}], "ololo mochi is here!"}).
