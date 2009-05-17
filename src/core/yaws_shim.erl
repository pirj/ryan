-module(yaws_shim).
-export([out/1, init_yaws/1]).
-include("../../deps/yaws/yaws_api.hrl").
-include("../../deps/yaws/yaws.hrl").

out(Arg) ->
	Req = Arg#arg.req,
	Headers = Arg#arg.headers,
	Cookie = Headers#headers.cookie,
	Method = reia_erl:e2r(Req#http_request.method),

	{abs_path, Abs_Path} = Req#http_request.path,
	[Full_Path|_] = string:tokens(Abs_Path, [$?]),
	PathParts = string:tokens(Full_Path, [$/]),

	Params = case Method of
		'POST' -> yaws_api:parse_post(Arg);
		_      -> yaws_api:parse_query(Arg)
	end,

	{Token, Session} = session(Cookie),

	Result = reia_erl:r2e(reia:apply('Ryan', out, [Abs_Path, Method, PathParts, Token, Params])),

	case Session of
		nil -> Result;
		_   -> [Session, Result]
	end.

session(Cookie) ->
	case yaws_api:find_cookie_val("sid", Cookie) of
		[] -> 
			Token = ryan_misc:create_session_token(),
			{Token, yaws_api:setcookie("sid", Token, "/")};
		Token ->
			{Token, nil}
	end.

init_yaws(Port) ->
	YawsHome = "/usr/local/lib/yaws/",
	YawsLib = filename:join(YawsHome, "ebin"),
	code:add_patha(YawsLib),
	{ok, ApplicationPath} = file:get_cwd(),
	io:format("Starting up YAWS to run in ~s~n", [ApplicationPath]),
	Public = filename:join(ApplicationPath, "public"),
	yaws:start_embedded(Public,
		[
			{servername, "localhost"},
			{port, Port},
			{listen, {0,0,0,0}},
			{appmods, [{"/app/", yaws_shim}]}
		],
		[
			{logdir, "logs"},
			{cache_refresh_secs, 0}
		]
	),
	loop().

loop() ->
	receive
	after 1000 ->
		loop()
	end.
