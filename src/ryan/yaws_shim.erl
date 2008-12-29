-module(yaws_shim).
-export([out/1, init_yaws/0, read_file/1]).
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

	{string, Result} = reia:apply('Ryan',out, [Abs_Path, Method, PathParts, Cookie, Params]),
	{html, Result}.
	
read_file({string, Filename}) ->
	Absname = filename:absname(binary_to_list(Filename)),
	Data = file:read_file(Absname),
	case Data of
		{ok, Contents} -> Contents;
		{error, enoent} -> "The file does not exist.";
		{error, eacces} -> "Missing permission for reading the file, or for searching one of the parent directories.";
		{error, eisdir} -> "The named file is a directory.";
		{error, enotdir} -> "A component of the file name is not a directory. On some platforms, enoent is returned instead.";
		{error, enomem} -> "There is not enough memory for the contents of the file."
	end.

init_yaws() ->
	YawsHome = "/usr/local/lib/yaws/",
	YawsLib = filename:join(YawsHome, "ebin"),
	code:add_patha(YawsLib),
	{ok, ApplicationPath} = file:get_cwd(),
	io:format("Starting up YAWS to run in ~s~n", [ApplicationPath]),
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
