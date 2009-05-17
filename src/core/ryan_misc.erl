-module(ryan_misc).
-export([create_session_token/0]).

create_session_token() ->
	random_seed(),
	Random = integer_to_list(random:uniform(100000000000000000000000000000000000000)),
	MD5 = binary_to_list(erlang:md5(Random)),
	base64:encode_to_string(MD5).

random_seed() -> % Hat tip to Joe Armstrong!
	{_,_,X} = erlang:now(),
	{H,M,S} = time(),
	H1 = H * X rem 32767,
	M1 = M * X rem 32767,
	S1 = S * X rem 32767,
	put(random_seed, {H1,M1,S1}).
