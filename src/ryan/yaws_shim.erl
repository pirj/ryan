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
	Path = string:tokens(Full_Path, [$/]),
	
	Cookie = Headers#headers.cookie,

    Params = case Method of
        'POST' -> yaws_api:parse_post(Arg);
        _      -> yaws_api:parse_query(Arg)
    end,

	reia_erl:r2e('rYAN':out(reia_erl:e2r(Abs_Path), reia_erl:e2r(Method), reia_erl:e2r(Path), reia_erl:e2r(Cookie), reia_erl:e2r(Params))).
	
call_action([First|Rest] = _Controller, Action, Parameters, Cookies, HTTPMethod) ->
	Module = unwrap_binary(string:to_upper([First]) ++ Rest),
	Method = unwrap_binary(Action),
	Args = [reia_erl:e2r(A) || A <- [Parameters, Cookies, HTTPMethod]],
	reia_erl:e2r(apply(Module, Method, Args)).

unwrap_binary(A) -> list_to_atom(reia_erl:r2e(A)).

init_yaws() ->
	{ok, ApplicationPath} = file:get_cwd(),
    io:format("Starting up YAWS to run in ~s~n", [ApplicationPath]),
	Public = lists:flatten([ApplicationPath, "/public"]),
    yaws:start_embedded(Public, [{servername, "localhost"}, {port, 8001}, {listen, {0,0,0,0}},
						{appmods, '<\"/\", yaws_shim>'}]).
