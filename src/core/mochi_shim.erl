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
    # io:format("~p request for ~p with headers ~p~n", [Method, Abs_Path, Accept]),

    reia_erl:r2e(reia:apply('Ryan', out, [Abs_Path, Method, PathParts, nil, Params]))
	
	Req:respond({200, [{"Content-Type", "text/plain"}], "ololo mochi is here!"}).

session(Cookie) ->
	case yaws_api:find_cookie_val("sid", Cookie) of
		[] -> 
			Token = create_session_token(),
			{Token, yaws_api:setcookie("sid", Token, "/")};
		Token ->
			{Token, nil}
	end.

create_session_token() ->
	random_seed(),
	RandomOne = integer_to_list(random:uniform(100000000000)),
	RandomTwo = integer_to_list(random:uniform(100000000000)),
	MD5One = binary_to_list(erlang:md5(RandomOne)),
	MD5Two = binary_to_list(erlang:md5(RandomTwo)),
	base64:encode_to_string(MD5One ++ MD5Two). % ++ Secret !!!!

random_seed() -> % Hat tip to Joe Armstrong!
	{_,_,X} = erlang:now(),
	{H,M,S} = time(),
	H1 = H * X rem 32767,
	M1 = M * X rem 32767,
	S1 = S * X rem 32767,
	put(random_seed, {H1,M1,S1}).

start_mochi(Port) ->
	mochiweb_http:start([{loop, fun out/1}, {port, Port}]),
	loop().

loop() ->
	receive
	after 1000 ->
		loop()
	end.
