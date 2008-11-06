%
% ire: Interactive Reia, a Read Eval Print Loop for the Reia language
% Copyright (C)2008 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(ire).
-export([init/0, start/0]).

init() ->
  io:format("Reia Interactive Shell (prerelease)~nRunning on "),
  user_drv:start('tty_sl -c -e', {ire, start, []}).

start() ->
  spawn(fun() -> loop() end).

loop() ->
  loop('Eval':new_binding()).
    
loop(Binding) ->
  case read() of
    eof -> io:format("~n"); % print a newline then exit
    String ->
      NewBinding = try
        eval_print(String, Binding)
      catch
        Class:Reason -> print_error(Class, Reason), 
        Binding
      end,
      loop(NewBinding)
  end.
  
read() ->
  read('>> ').
  
read(Prompt) ->
  read(standard_io, Prompt).
  
read(Io, Prompt) ->
  io:get_line(Io, Prompt).
  
read_until_blank(Input, Prompt) ->
  case read(Prompt) of
    "\n" ->
      lists:flatten(lists:reverse(Input));
    Line ->
      read_until_blank([Line|Input], Prompt)
  end.
  
eval_print(String, Binding) ->
  case reia_parse:string(String) of
    {ok, Exprs} ->
      {tuple, {value, Value, NewBinding}} = 'Eval':exprs(Exprs, Binding),
      print(Value),
      NewBinding;
      
    %% Need more tokens
    {error, {999999, _}} ->      
      eval_print(read_until_blank([String], '.. '), Binding);
      
    {error, Error} ->
      parse_error(Error),
      Binding
  end.

print(Value) ->
  {string, String} = reia_dispatch:funcall(Value, inspect, []),
  io:format("=> ~s~n", [binary_to_list(String)]).
  
parse_error({Line, Error}) ->
  io:format("Error: Line ~w: ~s~n", [Line, Error]).
  
print_error(Class, Reason) ->
  PF = fun(Term, I) ->
    io_lib:format("~." ++ integer_to_list(I) ++ "P", [Term, 50]) 
  end,
  StackTrace = erlang:get_stacktrace(),
  StackFun = fun(M, _F, _A) -> (M =:= erl_eval) or (M =:= ?MODULE) end,
  Error = lib:format_exception(1, Class, Reason, StackTrace, StackFun, PF),
  io:format("~s~n", [Error]).