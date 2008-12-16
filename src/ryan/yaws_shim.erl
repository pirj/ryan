-module(yaws_shim).
-export([out/1, call_action/5, init_yaws/0]).
-include("../yaws/yaws_api.hrl").
-include("../yaws/yaws.hrl").

out(Arg) ->
	Req = Arg#arg.req,
	Headers = Arg#arg.headers,

	Method = reia_erl:e2r(Req#http_request.method),

	{abs_path, Abs_Path} = Req#http_request.path,
	[Full_Path|_] = string:tokens(Abs_Path, [$?]),
	[Application, Controller, Action] = string:tokens(Full_Path, [$/]),
	
	Cookie = Headers#headers.cookie,

	Params = case Method of
		'POST' -> yaws_api:parse_post(Arg);
		_      -> yaws_api:parse_query(Arg)
	end,

	Result = 'Ryan':out(Abs_Path, Method, Application, Controller, Action, 
		reia_erl:e2r(Cookie), 
		reia_erl:e2r(Params)),
	reia_erl:r2e(Result).
	
call_action([First|Rest] = _Controller, Action, Parameters, Cookies, HTTPMethod) ->
	Module = unwrap_binary(string:to_upper([First]) ++ Rest),
	Method = unwrap_binary(Action),
	Args = [reia_erl:e2r(A) || A <- [Parameters, Cookies, HTTPMethod]],
	reia_erl:e2r(apply(Module, Method, Args)).

unwrap_binary(A) -> list_to_atom(reia_erl:r2e(A)).

init_yaws() ->
	YawsHome = "/usr/local/lib/yaws/",
	YawsLib = filename:join(YawsHome, "ebin"),
	code:add_patha(YawsLib),
	{ok, ApplicationPath} = file:get_cwd(),
	io:format("Starting up YAWS to run in ~s~n", [ApplicationPath]),
	Public = filename:join(ApplicationPath, "public"),
	yaws:start_embedded(Public, [
		{servername, "localhost"},
		{port, 8001},
		{listen, {0,0,0,0}},
		{appmods, [{"/app/", yaws_shim}]}
		]),
	loop().

loop() ->
	receive
	after 1000 ->
		loop()
	end.
