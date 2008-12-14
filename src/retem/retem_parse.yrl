Nonterminals 
blocks embraced expressions.

Terminals
 '{' '}' identifier arithmetic comparator logical not text.

Rootsymbol blocks.

blocks -> embraced : ['$1'].
blocks -> embraced blocks : ['$1'|'$2'].
blocks -> text : ['$1'].
blocks -> text blocks : ['$1'|'$2'].

embraced    -> '{' expressions '}' : '$2'.
expressions -> expressions logical expressions : {logical, unwrap_operator('$2'), '$1', '$3'}.
expressions -> expressions comparator expressions : {comparator, unwrap_operator('$2'), '$1', '$3'}.
expressions -> expressions arithmetic expressions : {arithmetic, unwrap_operator('$2'), '$1', '$3'}.
%% expressions -> not expressions : {not, unwrap_operator('$1'), '$2', '$2'}.
expressions -> identifier : identifier_atom('$1').

Erlang code.

identifier_atom({identifier, Atom}) -> {identifier, list_to_atom(Atom)}.

unwrap_operator({_Category, Operator}) -> Operator.