-module(mochi_shim).
-export([out/2, start_mochi/1]).

out(Req, Public) ->
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
	
	[App|_T] = PathParts,
	case App of
		"app" -> serve_dynamic(Req, Abs_Path, Method, PathParts, Token, Params, Session);
		_ -> serve_static(Req, Public, Full_Path)
	end.
	
serve_dynamic(Req, Abs_Path, Method, PathParts, Token, Params, Session) ->
	Result = reia_erl:r2e(reia:apply('Ryan', out, [Abs_Path, Method, PathParts, Token, Params])),
	
	Response = case Result of
		{html, Contents} -> {200, [{"Content-Type", "text/html"}] ++ Session, Contents};
		{status, Status} -> {Status, [Session]};
		_				-> {}
	end,
	Req:respond(Response).

serve_static(Req, Public, Full_Path) ->
	io:format("pub:~p, file:~p~n",[Public, Full_Path]),
	[H|T] = Full_Path,
	case H of
		47 -> Req:serve_file(T, Public);
		_  -> Req:serve_file(Full_Path, Public)
	end.

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
	{ok, ApplicationPath} = file:get_cwd(),
	io:format("Starting up Mochiweb to run in ~s~n", [ApplicationPath]),
	Public = filename:join(ApplicationPath, "public"),
	Loop = fun(Req) -> out(Req, Public) end,
	mochiweb_http:start([{loop, Loop}, {port, Port}]),
	loop().

loop() ->
	receive
	after 1000 ->
		loop()
	end.
