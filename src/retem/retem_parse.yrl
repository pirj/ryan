Nonterminals 
embraced expressions.

Terminals
 '{' '}' identifier arithmetic. %% comparator logical.

Rootsymbol embraced.

embraced -> '{' expressions '}' : '$2'.
expressions -> expressions arithmetic expressions : {arithmetic, '$2', '$1', '$3'}.
%% expressions -> expressions comparator expressions : {comparator, '$1', '$3'}.
%% expressions -> expressions logical expressions : {logical, '$1', '$3'}.
expressions -> identifier : identifier_atom('$1').

Erlang code.

identifier_atom({identifier, _Line, Atom}) -> {identifier, list_to_atom(Atom)}.

