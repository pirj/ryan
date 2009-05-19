%% @author Dmitry Chernyak <losthost@narod.ru>

%% @doc Yet another JSON (RFC 4627) library for Erlang. engejson works
%%      with binaries as strings, arrays as lists (without an {array, _})
%%      wrapper and it only knows how to decode UTF-8 (and ASCII).
%%      
%%      Original code was written by Bob Ippolito bob@mochimedia.com, copyright 2007 Mochi Media, Inc.
%%      This version also has some modified includes from "struct.erl" originally written by BeeBole.
%%
%%      This version introduces more compact form instead of using 'obj' or 'struct'.
%%      Also this version uses utf8 encoding instead of \uXXXX. \uXXXX is converted to utf8

-module(engejson).
-author('losthost@narod.ru').
-export([encoder/1, encode/1]).
-export([decoder/1, decode/1]).
-export([get_value/2, get_values/2]).
-export([test/0]).

% This is a macro to placate syntax highlighters..
-define(Q, $\"). %% "
-define(ADV_COL(S, N), S#decoder{offset=N+S#decoder.offset,
                                 column=N+S#decoder.column}).
-define(INC_COL(S), S#decoder{offset=1+S#decoder.offset,
                              column=1+S#decoder.column}).
-define(INC_LINE(S), S#decoder{offset=1+S#decoder.offset,
                               column=1,
                               line=1+S#decoder.line}).
-define(INC_CHAR(S, C),
        case C of
            $\n ->
                S#decoder{column=1,
                          line=1+S#decoder.line,
                          offset=1+S#decoder.offset};
            _ ->
                S#decoder{column=1+S#decoder.column,
                          offset=1+S#decoder.offset}
        end).
-define(IS_WHITESPACE(C),
        (C =:= $\s orelse C =:= $\t orelse C =:= $\r orelse C =:= $\n)).

%% @type iolist() = [char() | binary() | iolist()]
%% @type json_string() = atom | binary()
%% @type json_number() = integer() | float()
%% @type json_array() = [json_term()]
%% @type json_object() = [{json_string(), json_term()}]
%% @type json_empty() = {empty}. Empty object {}
%% @type json_term() = json_string() | json_number() | json_array() |
%%                     json_object()

-record(encoder, {handler=null}).

-record(decoder, {object_hook=null,
                  offset=0,
                  line=1,
                  column=1,
                  state=null}).

%% @spec encoder([encoder_option()]) -> function()
%% @doc Create an encoder/1 with the given options.
encoder(Options) ->
    State = parse_encoder_options(Options, #encoder{}),
    fun (O) -> json_encode(O, State) end.

%% @spec encode(json_term()) -> iolist()
%% @doc Encode the given as JSON to an iolist.
encode(Any) ->
    json_encode(Any, #encoder{}).

%% @spec decoder([decoder_option()]) -> function()
%% @doc Create a decoder/1 with the given options.
decoder(Options) ->
    State = parse_decoder_options(Options, #decoder{}),
    fun (O) -> json_decode(O, State) end.

%% @spec decode(iolist()) -> json_term()
%% @doc Decode the given iolist to Erlang terms.
decode(S) ->
    json_decode(S, #decoder{}).

test() ->
    test_all().

%% Internal API

parse_encoder_options([], State) ->
    State;
parse_encoder_options([{handler, Handler} | Rest], State) ->
    parse_encoder_options(Rest, State#encoder{handler=Handler}).

parse_decoder_options([], State) ->
    State;
parse_decoder_options([{object_hook, Hook} | Rest], State) ->
    parse_decoder_options(Rest, State#decoder{object_hook=Hook}).

json_encode(true, _State) ->
    <<"true">>;
json_encode(false, _State) ->
    <<"false">>;
json_encode(null, _State) ->
    <<"null">>;
json_encode(I, _State) when is_integer(I) andalso I >= -2147483648 andalso I =< 2147483647 ->
    %% Anything outside of 32-bit integers should be encoded as a float
    integer_to_list(I);
json_encode(I, _State) when is_integer(I) ->
    mochinum:digits(float(I));
json_encode(F, _State) when is_float(F) ->
    mochinum:digits(F);
json_encode(S, State) when is_binary(S); is_atom(S) ->
    json_encode_string(S, State);
json_encode(Props = [{_K, _V} | _T], State) ->
    json_encode_proplist(Props, State);
json_encode({empty}, State) ->
    json_encode_empty(State);
json_encode(Array, State) when is_list(Array) ->
    json_encode_array(Array, State);
json_encode(Bad, #encoder{handler=null}) ->
    exit({json_encode, {bad_term, Bad}});
json_encode(Bad, State=#encoder{handler=Handler}) ->
    json_encode(Handler(Bad), State).

json_encode_array([], _State) ->
    <<"[]">>;
json_encode_array(L, State) ->
    F = fun (O, Acc) ->
                [$,, json_encode(O, State) | Acc]
        end,
    [$, | Acc1] = lists:foldl(F, "[", L),
    lists:reverse([$\] | Acc1]).

json_encode_empty(_State) ->
    <<"{}">>.

% XXX rest only for a case
json_encode_proplist([], _State) ->
    <<"{}">>;

json_encode_proplist(Props, State) ->
    F = fun ({K, V}, Acc) ->
                KS = json_encode_string(K, State),
                VS = json_encode(V, State),
                [$,, VS, $:, KS | Acc]
        end,
    [$, | Acc1] = lists:foldl(F, "{", Props),
    lists:reverse([$\} | Acc1]).

json_encode_string(A, _State) when is_atom(A) ->
    L = atom_to_list(A),
    case json_string_is_safe(L) of
        true ->
            [?Q, L, ?Q];
        false ->
            json_encode_string_unicode(xmerl_ucs:from_utf8(L), [?Q])
    end;
json_encode_string(B, _State) when is_binary(B) ->
    case json_bin_is_safe(B) of
        true ->
            [?Q, B, ?Q];
        false ->
            json_encode_string_unicode(xmerl_ucs:from_utf8(B), [?Q])
    end;
json_encode_string(I, _State) when is_integer(I) ->
    [?Q, integer_to_list(I), ?Q];
json_encode_string(L, _State) when is_list(L) ->
    case json_string_is_safe(L) of
        true ->
            [?Q, L, ?Q];
        false ->
            json_encode_string_unicode(L, [?Q])
    end.

json_string_is_safe([]) ->
    true;
json_string_is_safe([C | Rest]) ->
    case C of
        ?Q ->
            false;
        $\\ ->
            false;
        $\b ->
            false;
        $\f ->
            false;
        $\n ->
            false;
        $\r ->
            false;
        $\t ->
            false;
        C when C >= 0, C < $\s; C >= 16#7f, C =< 16#10FFFF ->
            false;
        C when C < 16#7f ->
            json_string_is_safe(Rest);
        _ ->
            false
    end.

json_bin_is_safe(<<>>) ->
    true;
json_bin_is_safe(<<C, Rest/binary>>) ->
    case C of
        ?Q ->
            false;
        $\\ ->
            false;
        $\b ->
            false;
        $\f ->
            false;
        $\n ->
            false;
        $\r ->
            false;
        $\t ->
            false;
        C when C >= 0, C < $\s; C >= 16#7f, C =< 16#10FFFF ->
            false;
        C when C < 16#7f ->
            json_bin_is_safe(Rest);
        _ ->
            false
    end.

json_encode_string_unicode([], Acc) ->
    lists:reverse([$\" | Acc]); %% "
json_encode_string_unicode([C | Cs], Acc) ->
    Acc1 = case C of
               ?Q ->
                   [?Q, $\\ | Acc];
               %% Escaping solidus is only useful when trying to protect
               %% against "</script>" injection attacks which are only
               %% possible when JSON is inserted into a HTML document
               %% in-line. mochijson2 does not protect you from this, so
               %% if you do insert directly into HTML then you need to
               %% uncomment the following case or escape the output of encode.
               %%
               $/ ->
                  [$/, $\\ | Acc];
              
               $\\ ->
                   [$\\, $\\ | Acc];
               $\b ->
                   [$b, $\\ | Acc];
               $\f ->
                   [$f, $\\ | Acc];
               $\n ->
                   [$n, $\\ | Acc];
               $\r ->
                   [$r, $\\ | Acc];
               $\t ->
                   [$t, $\\ | Acc];
               C when C >= 0, C < $\s ->
                   [unihex(C) | Acc];
               C when C >= 16#7f, C =< 16#10FFFF ->
                   [xmerl_ucs:to_utf8(C) | Acc];
               C when C < 16#7f ->
                   [C | Acc];
               _ ->
                   exit({json_encode, {bad_char, C}})
           end,
    json_encode_string_unicode(Cs, Acc1).

hexdigit(C) when C >= 0, C =< 9 ->
    C + $0;
hexdigit(C) when C =< 15 ->
    C + $a - 10.

unihex(C) when C < 16#10000 ->
    <<D3:4, D2:4, D1:4, D0:4>> = <<C:16>>,
    Digits = [hexdigit(D) || D <- [D3, D2, D1, D0]],
    [$\\, $u | Digits];
unihex(C) when C =< 16#10FFFF ->
    N = C - 16#10000,
    S1 = 16#d800 bor ((N bsr 10) band 16#3ff),
    S2 = 16#dc00 bor (N band 16#3ff),
    [unihex(S1), unihex(S2)].

json_decode(L, S) when is_list(L) ->
    json_decode(iolist_to_binary(L), S);
json_decode(B, S) ->
    {Res, S1} = decode1(B, S),
    {eof, _} = tokenize(B, S1#decoder{state=trim}),
    Res.

decode1(B, S=#decoder{state=null}) ->
    case tokenize(B, S#decoder{state=any}) of
        {{const, C}, S1} ->
            {C, S1};
        {start_array, S1} ->
            decode_array(B, S1);
        {start_object, S1} ->
            decode_object(B, S1)
    end.

make_object(V, #decoder{object_hook=null}) ->
    V;
make_object(V, #decoder{object_hook=Hook}) ->
    Hook(V).

decode_object(B, S) ->
    decode_object(B, S#decoder{state=key}, []).

decode_object(B, S=#decoder{state=key}, Acc) ->
    case tokenize(B, S) of
        {end_object, S1} ->
            Props = case Acc of
                [] -> {empty};
                _ -> lists:reverse(Acc)
            end,
            V = make_object(Props, S1),
            {V, S1#decoder{state=null}};
        {{const, K}, S1} ->
            {colon, S2} = tokenize(B, S1),
            {V, S3} = decode1(B, S2#decoder{state=null}),
            decode_object(B, S3#decoder{state=comma}, [{K, V} | Acc])
    end;
decode_object(B, S=#decoder{state=comma}, Acc) ->
    case tokenize(B, S) of
        {end_object, S1} ->
            V = make_object(lists:reverse(Acc), S1),
            {V, S1#decoder{state=null}};
        {comma, S1} ->
            decode_object(B, S1#decoder{state=key}, Acc)
    end.

decode_array(B, S) ->
    decode_array(B, S#decoder{state=any}, []).

decode_array(B, S=#decoder{state=any}, Acc) ->
    case tokenize(B, S) of
        {end_array, S1} ->
            {lists:reverse(Acc), S1#decoder{state=null}};
        {start_array, S1} ->
            {Array, S2} = decode_array(B, S1),
            decode_array(B, S2#decoder{state=comma}, [Array | Acc]);
        {start_object, S1} ->
            {Array, S2} = decode_object(B, S1),
            decode_array(B, S2#decoder{state=comma}, [Array | Acc]);
        {{const, Const}, S1} ->
            decode_array(B, S1#decoder{state=comma}, [Const | Acc])
    end;
decode_array(B, S=#decoder{state=comma}, Acc) ->
    case tokenize(B, S) of
        {end_array, S1} ->
            {lists:reverse(Acc), S1#decoder{state=null}};
        {comma, S1} ->
            decode_array(B, S1#decoder{state=any}, Acc)
    end.

tokenize_string(B, S=#decoder{offset=O}) ->
    case tokenize_string_fast(B, O) of
        {escape, O1} ->
            Length = O1 - O,
            S1 = ?ADV_COL(S, Length),
            <<_:O/binary, Head:Length/binary, _/binary>> = B,
            tokenize_string(B, S1, lists:reverse(binary_to_list(Head)));
        O1 ->
            Length = O1 - O,
            <<_:O/binary, String:Length/binary, ?Q, _/binary>> = B,
            {{const, String}, ?ADV_COL(S, Length + 1)}
    end.

tokenize_string_fast(B, O) ->
    case B of
        <<_:O/binary, ?Q, _/binary>> ->
            O;
        <<_:O/binary, C, _/binary>> when C =/= $\\ ->
            tokenize_string_fast(B, 1 + O);
        _ ->
            {escape, O}
    end.

tokenize_string(B, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary, ?Q, _/binary>> ->
            {{const, iolist_to_binary(lists:reverse(Acc))}, ?INC_COL(S)};
        <<_:O/binary, "\\\"", _/binary>> ->
            tokenize_string(B, ?ADV_COL(S, 2), [$\" | Acc]); % "
        <<_:O/binary, "\\\\", _/binary>> ->
            tokenize_string(B, ?ADV_COL(S, 2), [$\\ | Acc]); % "
        <<_:O/binary, "\\/", _/binary>> ->
            tokenize_string(B, ?ADV_COL(S, 2), [$/ | Acc]);
        <<_:O/binary, "\\b", _/binary>> ->
            tokenize_string(B, ?ADV_COL(S, 2), [$\b | Acc]);
        <<_:O/binary, "\\f", _/binary>> ->
            tokenize_string(B, ?ADV_COL(S, 2), [$\f | Acc]);
        <<_:O/binary, "\\n", _/binary>> ->
            tokenize_string(B, ?ADV_COL(S, 2), [$\n | Acc]);
        <<_:O/binary, "\\r", _/binary>> ->
            tokenize_string(B, ?ADV_COL(S, 2), [$\r | Acc]);
        <<_:O/binary, "\\t", _/binary>> ->
            tokenize_string(B, ?ADV_COL(S, 2), [$\t | Acc]);
        <<_:O/binary, "\\u", C3, C2, C1, C0, _/binary>> ->
            %% coalesce UTF-16 surrogate pair?
            C = erlang:list_to_integer([C3, C2, C1, C0], 16),
            Acc1 = lists:reverse(xmerl_ucs:to_utf8(C), Acc),
            tokenize_string(B, ?ADV_COL(S, 6), Acc1);
        <<_:O/binary, C, _/binary>> ->
            tokenize_string(B, ?INC_CHAR(S, C), [C | Acc])
    end.

tokenize_number(B, S) ->
    case tokenize_number(B, sign, S, []) of
        {{int, Int}, S1} ->
            {{const, list_to_integer(Int)}, S1};
        {{float, Float}, S1} ->
            {{const, list_to_float(Float)}, S1}
    end.

tokenize_number(B, sign, S=#decoder{offset=O}, []) ->
    case B of
        <<_:O/binary, $-, _/binary>> ->
            tokenize_number(B, int, ?INC_COL(S), [$-]);
        _ ->
            tokenize_number(B, int, S, [])
    end;
tokenize_number(B, int, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary, $0, _/binary>> ->
            tokenize_number(B, frac, ?INC_COL(S), [$0 | Acc]);
        <<_:O/binary, C, _/binary>> when C >= $1 andalso C =< $9 ->
            tokenize_number(B, int1, ?INC_COL(S), [C | Acc])
    end;
tokenize_number(B, int1, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary, C, _/binary>> when C >= $0 andalso C =< $9 ->
            tokenize_number(B, int1, ?INC_COL(S), [C | Acc]);
        _ ->
            tokenize_number(B, frac, S, Acc)
    end;
tokenize_number(B, frac, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary, $., C, _/binary>> when C >= $0, C =< $9 ->
            tokenize_number(B, frac1, ?ADV_COL(S, 2), [C, $. | Acc]);
        <<_:O/binary, E, _/binary>> when E =:= $e orelse E =:= $E ->
            tokenize_number(B, esign, ?INC_COL(S), [$e, $0, $. | Acc]);
        _ ->
            {{int, lists:reverse(Acc)}, S}
    end;
tokenize_number(B, frac1, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary, C, _/binary>> when C >= $0 andalso C =< $9 ->
            tokenize_number(B, frac1, ?INC_COL(S), [C | Acc]);
        <<_:O/binary, E, _/binary>> when E =:= $e orelse E =:= $E ->
            tokenize_number(B, esign, ?INC_COL(S), [$e | Acc]);
        _ ->
            {{float, lists:reverse(Acc)}, S}
    end;
tokenize_number(B, esign, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary, C, _/binary>> when C =:= $- orelse C=:= $+ ->
            tokenize_number(B, eint, ?INC_COL(S), [C | Acc]);
        _ ->
            tokenize_number(B, eint, S, Acc)
    end;
tokenize_number(B, eint, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary, C, _/binary>> when C >= $0 andalso C =< $9 ->
            tokenize_number(B, eint1, ?INC_COL(S), [C | Acc])
    end;
tokenize_number(B, eint1, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary, C, _/binary>> when C >= $0 andalso C =< $9 ->
            tokenize_number(B, eint1, ?INC_COL(S), [C | Acc]);
        _ ->
            {{float, lists:reverse(Acc)}, S}
    end.

tokenize(B, S=#decoder{offset=O}) ->
    case B of
        <<_:O/binary, C, _/binary>> when ?IS_WHITESPACE(C) ->
            tokenize(B, ?INC_CHAR(S, C));
        <<_:O/binary, "{", _/binary>> ->
            {start_object, ?INC_COL(S)};
        <<_:O/binary, "}", _/binary>> ->
            {end_object, ?INC_COL(S)};
        <<_:O/binary, "[", _/binary>> ->
            {start_array, ?INC_COL(S)};
        <<_:O/binary, "]", _/binary>> ->
            {end_array, ?INC_COL(S)};
        <<_:O/binary, ",", _/binary>> ->
            {comma, ?INC_COL(S)};
        <<_:O/binary, ":", _/binary>> ->
            {colon, ?INC_COL(S)};
        <<_:O/binary, "null", _/binary>> ->
            {{const, null}, ?ADV_COL(S, 4)};
        <<_:O/binary, "true", _/binary>> ->
            {{const, true}, ?ADV_COL(S, 4)};
        <<_:O/binary, "false", _/binary>> ->
            {{const, false}, ?ADV_COL(S, 5)};
        <<_:O/binary, "\"", _/binary>> ->
            tokenize_string(B, ?INC_COL(S));
        <<_:O/binary, C, _/binary>> when (C >= $0 andalso C =< $9)
                                         orelse C =:= $- ->
            tokenize_number(B, S);
        <<_:O/binary>> ->
            trim = S#decoder.state,
            {eof, S}
    end.

% additional part from struct.erl by BeeBole

%% @spec get_value(path() | key(), struct()) -> value()
get_value(_, undefined) ->
    undefined;
get_value(_, {empty}) ->
    undefined;
get_value(Path, Struct) when is_tuple(Path) ->
    L = tuple_to_list(Path),
    get_val(L, Struct);
get_value(Key, Struct) when is_list(Key) ->
        get_value(list_to_binary(Key),Struct);
get_value(Key, Struct) ->
    proplists:get_value(Key, Struct).

get_val(_, undefined) ->
    undefined;
get_val([Key], Struct) ->
    get_value(Key, Struct);
get_val([Key | T], Struct) ->
    NewStruct = get_value(Key, Struct),
    get_val(T, NewStruct).


%% @spec get_values([path() | key()], struct()) -> [value()]
get_values(Keys, Struct) ->
    get_values(Keys, Struct, []).

get_values([Key | Keys], Struct, Acc) ->
    get_values(Keys, Struct, [get_value(Key, Struct) | Acc]);

get_values([], _Struct, Acc) ->
    lists:reverse(Acc).

%% testing constructs borrowed from the Yaws JSON implementation.

%% Create an object from a list of Key/Value pairs.

obj_new() ->
    {empty}.

is_obj(Props) ->
    F = fun ({K, _}) when is_binary(K) ->
                true;
            (_) ->
                false
        end,    
    lists:all(F, Props).

obj_from_list(Props) ->
    case is_obj(Props) of
        true -> Props;
        false -> exit({json_bad_object, Props})
    end.

%% Test for equivalence of Erlang terms.
%% Due to arbitrary order of construction, equivalent objects might
%% compare unequal as erlang terms, so we need to carefully recurse
%% through aggregates (tuples and objects).

equiv({empty}, {empty}) -> true;
equiv(Props1 = [{_K1, _V1} | _T1], Props2 = [{_K2, _V2} | _T2]) ->
    equiv_object(Props1, Props2);
equiv(L1, L2) when is_list(L1), is_list(L2) ->
    equiv_list(L1, L2);
equiv(N1, N2) when is_number(N1), is_number(N2) -> N1 == N2;
equiv(B1, B2) when is_binary(B1), is_binary(B2) -> B1 == B2;
equiv(true, true) -> true;
equiv(false, false) -> true;
equiv(null, null) -> true.

%% Object representation and traversal order is unknown.
%% Use the sledgehammer and sort property lists.

equiv_object(Props1, Props2) ->
    L1 = lists:keysort(1, Props1),
    L2 = lists:keysort(1, Props2),
    Pairs = lists:zip(L1, L2),
    true = lists:all(fun({{K1, V1}, {K2, V2}}) ->
                             equiv(K1, K2) and equiv(V1, V2)
                     end, Pairs).

%% Recursively compare tuple elements for equivalence.

equiv_list([], []) ->
    true;
equiv_list([V1 | L1], [V2 | L2]) ->
    case equiv(V1, V2) of
        true ->
            equiv_list(L1, L2);
        false ->
            false
    end.

test_all() ->
    [1199344435545.0, 1] = decode(<<"[1199344435545.0,1]">>),
    test_one(e2j_test_vec(utf8), 1),
    test_get().

test_one([], _N) ->
    %% io:format("~p tests passed~n", [N-1]),
    ok;
test_one([{E, J} | Rest], N) ->
    %% io:format("[~p] ~p ~p~n", [N, E, J]),
    true = equiv(E, decode(J)),
    true = equiv(E, decode(encode(E))),
    test_one(Rest, 1+N).

e2j_test_vec(utf8) ->
    [
     {1, "1"},
     {3.1416, "3.14160"}, %% text representation may truncate, trail zeroes
     {-1, "-1"},
     {-3.1416, "-3.14160"},
     {12.0e10, "1.20000e+11"},
     {1.234E+10, "1.23400e+10"},
     {-1.234E-10, "-1.23400e-10"},
     {10.0, "1.0e+01"},
     {123.456, "1.23456E+2"},
     {10.0, "1e1"},
     {<<"foo">>, "\"foo\""},
     {<<"foo", 5, "bar">>, "\"foo\\u0005bar\""},
     {<<"">>, "\"\""},
     {<<"\n\n\n">>, "\"\\n\\n\\n\""},
     {<<"\" \b\f\r\n\t\"">>, "\"\\\" \\b\\f\\r\\n\\t\\\"\""},
     {obj_new(), "{}"},
     {obj_from_list([{<<"foo">>, <<"bar">>}]), "{\"foo\":\"bar\"}"},
     {obj_from_list([{<<"foo">>, <<"bar">>}, {<<"baz">>, 123}]),
      "{\"foo\":\"bar\",\"baz\":123}"},
     {[], "[]"},
     {[[]], "[[]]"},
     {[1, <<"foo">>], "[1,\"foo\"]"},
     
     %% json array in a json object
     {obj_from_list([{<<"foo">>, [123]}]),
      "{\"foo\":[123]}"},
     
     %% json object in a json object
     {obj_from_list([{<<"foo">>, obj_from_list([{<<"bar">>, true}])}]),
      "{\"foo\":{\"bar\":true}}"},
     
     %% fold evaluation order
     {obj_from_list([{<<"foo">>, []},
                     {<<"bar">>, obj_from_list([{<<"baz">>, true}])},
                     {<<"alice">>, <<"bob">>}]),
      "{\"foo\":[],\"bar\":{\"baz\":true},\"alice\":\"bob\"}"},
     
     %% json object in a json array
     {[-123, <<"foo">>, obj_from_list([{<<"bar">>, []}]), null],
      "[-123,\"foo\",{\"bar\":[]},null]"}
    ].

test_get() ->
    case equiv(<<"v3-2">>,
               get_value({<<"o2-1">>, <<"o3-2">>},
                         [{<<"o1-1">>, <<"v1-1">>},
                          {<<"o2-1">>, [{<<"o3-1">>, {empty}},
                                        {<<"o3-2">>, <<"v3-2">>}]}]))
    of
        true -> ok;
        _ -> fail
    end.
