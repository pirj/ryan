Nonterminals 
template embraced expressions.

Terminals
 '{' '}' identifier arithmetic comparator logical text.

Rootsymbol template.

template    -> embraced.
template    -> text embraced.
template    -> embraced text.
template    -> text.
embraced    -> '{' expressions '}' : '$2'.
expressions -> expressions logical expressions : {logical, unwrap_operator('$2'), '$1', '$3'}.
expressions -> expressions comparator expressions : {comparator, unwrap_operator('$2'), '$1', '$3'}.
expressions -> expressions arithmetic expressions : {arithmetic, unwrap_operator('$2'), '$1', '$3'}.
%% expressions -> not expressions : {not, not, '$2'}.
expressions -> identifier : identifier_atom('$1').

Erlang code.

identifier_atom({identifier, Atom}) -> {identifier, list_to_atom(Atom)}.

unwrap_operator({_Category, Operator}) -> Operator.