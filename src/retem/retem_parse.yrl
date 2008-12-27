Nonterminals 
 syntax blocks block cond_block for_block expressions expression.

Terminals
 '{' '}' ',' identifier op text value conditional endc reserved nest forc in dot nt.

Rootsymbol syntax.

syntax -> blocks : '$1'.
blocks -> '$empty' : [].
blocks -> blocks block : '$1' ++ ['$2'].
block -> cond_block : '$1'.
block -> for_block : '$1'.
block -> expressions : '$1'.
block -> text : '$1'.

cond_block -> '{' conditional expression '}' blocks '{' endc '}' : {conditional('$2'), '$3', '$5'}.
for_block -> '{' forc identifier in identifier '}' blocks '{' endc '}' : {forcycle, remove_id('$3'), '$5', '$7'}.

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

category({_Op, {Category, _Operator}}) -> Category.
operator({_Op, {_Category, Operator}}) -> Operator.
conditional({_, Conditional}) -> Conditional.
remove_id({identifier, Id}) -> Id.
