-module(mochi_shim).
-export([out/1, start_mochi/1]).

out(Req) ->
    Method = Req:get(method),

    Abs_Path = Req:get(path),
	[Full_Path|_] = string:tokens(Abs_Path, [$?]),
	PathParts = string:tokens(Full_Path, [$/]),

	Params = case Method of
		'POST' -> Req:parse_post();
		_      -> Req:parse_qs()
	end,

    Accept = Req:get_header_value("Accept"),
	Cookie = Req:parse_cookie(),

	{Token, Session} = session(Cookie),

    io:format("~p request for ~p with headers ~p cookie:~p~n", [Method, Abs_Path, Accept, Cookie]),

	Result = reia_erl:r2e(reia:apply('Ryan', out, [Abs_Path, Method, PathParts, Token, Params])),

	Req:respond({200, [{"Content-Type", "text/plain"}] ++ Session, "ololo mochi is here!"}).

session(Cookies) ->
	SID = [X || {"sid", X} <- Cookies],
	case SID of
		[] -> 
			Token = ryan_misc:create_session_token(),
			{Token, [mochiweb_cookies:cookie("sid", Token)]};
		[Token] ->
			{Token, []}
	end.

start_mochi(Port) ->
	mochiweb_http:start([{loop, fun out/1}, {port, Port}]),
	loop().

loop() ->
	receive
	after 1000 ->
		loop()
	end.
