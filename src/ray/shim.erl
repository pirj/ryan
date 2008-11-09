-module(shim).
-export([out/1, run_method/3]).
-include("yaws_api.hrl").

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

	reia_erl:r2e('Ray':out(reia_erl:e2r(Method), reia_erl:e2r(Path), reia_erl:e2r(Cookie), reia_erl:e2r(Params))).
	
run_method([First|Rest] = _Module, Method, Args) ->
	Mod = unwrap_binary(string:to_upper([First]) ++ Rest),
	Met = unwrap_binary(Method),
	reia_erl:e2r(apply(Mod, Met, Args)).

unwrap_binary(A) -> list_to_atom(reia_erl:r2e(A)).