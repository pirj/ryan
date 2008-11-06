%
% reia_list: Methods for the List pseudo-class
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_list).
-export([funcall/3, funcall/4, to_erl/1]).

%%
%% Functions which don't take a block
%%

%% List#[]
%%   Retrieve an element in the list
funcall({list, {Reverse, Forward}}, '[]', [Index]) ->
  FwLength = length(Forward),
  RvLength = length(Reverse),
  if
    Index < 0 ->
      funcall({list, {Forward, Reverse}}, '[]', [-Index-1]);
    Index >= FwLength + RvLength ->
      nil;
    FwLength > Index ->
      lists:nth(Index + 1, Forward);
    true ->
      lists:nth(RvLength - (Index - FwLength), Reverse)
  end;
      
%% List#reverse
%%   Reverse the order of a list
funcall({list, {Reverse, Forward}}, reverse, []) ->
  {list, {Forward, Reverse}};
  
%% List#push
%%   Add an element to the tail of a list
funcall({list, {Reverse, Forward}}, push, [Value]) ->
  {list, {[Value|Reverse], Forward}};
  
%% List#pop
%%   Pop an element from the tail of a list
funcall({list, {[], []}}, pop, []) ->
  nil;
funcall({list, {[], Forward}}, pop, []) ->
  Reverse = lists:reverse(Forward),
  [Value|_] = Reverse,
  Value;
funcall({list, {[Value|_Reverse], _Forward}}, pop, []) ->
  Value;

%% List#unshift
%%   Unshift an element onto the front of a list
funcall({list, {Reverse, Forward}}, unshift, [Value]) ->
  {list, {Reverse, [Value|Forward]}};
  
%% List#shift
%%   Shift an element off the front of a list
funcall({list, {[], []}}, shift, []) ->
  nil;
funcall({list, {Reverse, []}}, shift, []) ->
  Forward = lists:reverse(Reverse),
  [Value|_] = Forward,
  Value;
funcall({list, {_Reverse, [Value|_Forward]}}, shift, []) ->
  Value;
  
%% List#size
%%   Number of items in a list
funcall({list, {Reverse, Forward}}, size, []) ->
  length(Reverse) + length(Forward);
  
%% List#to_tuple
%%   Explicitly cast a list to a tuple
funcall({list, _} = List, to_tuple, []) ->
  {tuple, list_to_tuple(to_erl(List))};
 
%% List#to_string
%%   Explicitly cast a list to a string.  Useful for converting Erlang "strings"
%%   to Reia strings.
funcall({list, _} = List, to_string, []) ->
  {string, list_to_binary(to_erl(List))};

%% List#to_s
%%   Generate a string representation of a list
funcall({list, _} = List, to_s, []) ->
  funcall(List, inspect, []);
  
%% List#inspect
%%   Inspect the contents of a list
funcall({list, _} = List, inspect, []) ->
  Elements = [element_to_string2(Element) || Element <- to_erl(List)],
  String = lists:concat(["[", string:join(Elements, ","), "]"]),
  funcall(reia_erl:e2r(String), to_string, []);
  
%% List#join
%%   Join all elements of a list together with the given separator
funcall({list, _} = List, join, []) ->
  funcall(List, join, [{string, <<"">>}]);
funcall({list, _} = List, join, [{string, Sep}]) ->
  Elements = [element_to_string(Element) || Element <- to_erl(List)],
  String = string:join(Elements, binary_to_list(Sep)),
  funcall(reia_erl:e2r(String), to_string, []).

element_to_string({string, Binary}) ->
  binary_to_list(Binary);
element_to_string(Element) ->
  element_to_string2(Element).
  
element_to_string2(Element) ->
  {list, {[], List}} = reia_dispatch:funcall(reia_dispatch:funcall(Element, inspect, []), to_list, []),
  List.
  
%%
%% Functions which take a block
%%

funcall({list, _} = List, each, [], Block) ->
  lists:foreach(Block, to_erl(List)),
  List;

funcall({list, _} = List, map, [], Block) ->
  {list, {[], lists:map(Block, to_erl(List))}};
  
funcall({list, _} = List, filter, [], Block) ->
  {list, {[], lists:filter(Block, to_erl(List))}};
  
funcall({list, _} = List, reduce, [Acc0], Block) ->
  lists:foldl(Block, Acc0, to_erl(List)).
  
%% Convert a Reia list to its Erlang equivalent
to_erl({list, {Reverse, Forward}}) ->
  Forward ++ lists:reverse(Reverse).
