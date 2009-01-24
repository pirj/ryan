-module(yaws_shim).
-export([out/1, init_yaws/0, read_file/1, up_to_date/2]).
-include("../yaws/yaws_api.hrl").
-include("../yaws/yaws.hrl").

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

read_file(Filename) ->
	Absname = filename:absname(Filename),
	Data = file:read_file(Absname),
	case Data of
		{ok, Contents} -> Contents;
		{error, enoent} -> "The file does not exist.";
		{error, eacces} -> "Missing permission for reading the file, or for searching one of the parent directories.";
		{error, eisdir} -> "The named file is a directory.";
		{error, enotdir} -> "A component of the file name is not a directory. On some platforms, enoent is returned instead.";
		{error, enomem} -> "There is not enough memory for the contents of the file."
	end.

up_to_date({string, Module}, {string, ReiaFile}) ->
%	false;
	code:is_loaded(list_to_atom(binary_to_list(Module)));
%	up_to_date(code:is_loaded(list_to_atom(binary_to_list(Module))), {string, ReiaFile});
up_to_date(false, _) ->
	false;
up_to_date({file, BeamFile}, {string, ReiaFile}) ->
	{ok, {file_info,_,_,_,_,BeamFileLastModified,_,_,_,_,_,_,_,_}}=file:read_file_info(BeamFile),
	{ok, {file_info,_,_,_,_,ReiaFileLastModified,_,_,_,_,_,_,_,_}}=file:read_file_info(ReiaFile),
	BeamFileLastModified >= ReiaFileLastModified.

init_yaws() ->
	YawsHome = "/usr/local/lib/yaws/",
	YawsLib = filename:join(YawsHome, "ebin"),
	code:add_patha(YawsLib),
	{ok, ApplicationPath} = file:get_cwd(),
	io:format("Starting up YAWS to run in ~s~n", [ApplicationPath]),
	ets:new(sessions, [named_table, public]),
	Public = filename:join(ApplicationPath, "public"),
	yaws:start_embedded(Public,
		[
			{servername, "localhost"},
			{port, 8001},
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
