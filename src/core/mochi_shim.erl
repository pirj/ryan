-module(mochi_shim).
-export([out/1, start_mochi/1]).

out(Req) ->
    Method = Req:get(method),

    Abs_Path = Req:get(path),
	[Full_Path|_] = string:tokens(Abs_Path, [$?]),
	io:format("fp:~p~n", [Full_Path]),
	PathParts = string:tokens(Full_Path, [$/]),

	Params = case Method of
		'POST' -> Req:parse_post();
		_      -> Req:parse_qs()
	end,

    Accept = Req:get_header_value("Accept"),
	Cookie = Req:parse_cookie(),

	{Token, Session} = session(Cookie),
	
	io:format("~p~n", [Full_Path]),
	[App|_T] = PathParts,
	case App of
		"app" -> serve_dynamic(Req, Abs_Path, Method, PathParts, Token, Params, Session);
		_ -> serve_static(Req, Full_Path)
	end.
	
serve_dynamic(Req, Abs_Path, Method, PathParts, Token, Params, Session) ->
	Result = reia_erl:r2e(reia:apply('Ryan', out, [Abs_Path, Method, PathParts, Token, Params])),
	
	Response = case Result of
		{html, Contents} -> {200, [{"Content-Type", "text/html"}] ++ Session, Contents};
		{status, Status} -> {Status, [Session]};
		_				-> {}
	end,
	Req:respond(Response).

serve_static(Req, Full_Path) ->
	Req:serve_file("", "/users/philpirj/source/ryan/application_example/public" ++ Full_Path).

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
