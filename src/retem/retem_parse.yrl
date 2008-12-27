Nonterminals 
 list element elements expressions expression.

Terminals
 '{' '}' ',' identifier op text value conditional endc reserved nest forc in dot nt.

Rootsymbol list.

list -> elements : '$1'.
elements -> '$empty' : [].
elements -> elements element : '$1' ++ ['$2'].
element -> '{' conditional expression '}' elements '{' endc '}' : {conditional('$2'), '$3', '$5'}.
element -> '{' forc identifier in identifier '}' elements '{' endc '}' : {forcycle, remove_id('$3'), '$5', '$7'}.
element -> expressions : '$1'.
element -> text : '$1'.

expressions -> '{' expression '}' : '$2'.
expressions -> '{' '}' : nil.

expression -> expression op expression : {category('$2'), operator('$2'), '$1', '$3'}.
expression -> nt expression : {nt, '$2'}.
expression -> identifier dot identifier: {property, '$1', remove_id('$3')}.
expression -> identifier : '$1'.
expression -> value : '$1'.
expression -> reserved : '$1'.
expression -> nest identifier ',' identifier: {nest, remove_id('$2'), remove_id('$4')}.
expression -> nest identifier: {nest, remove_id('$2')}.

Erlang code.

category({_Op, _Line, {Category, _Operator}}) -> Category.
operator({_Op, _Line, {_Category, Operator}}) -> Operator.
conditional({_, _Line, Conditional}) -> Conditional.
remove_id({identifier, _Line, Id}) -> Id.
