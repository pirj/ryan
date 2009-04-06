-module(ryan_misc).
-export([create_session_token/0]).

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

