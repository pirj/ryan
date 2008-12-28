Nonterminals 
 syntax blocks block if_block for_block expressions expression nested.

Terminals
 '{' '}' comma identifier op text value conditional reserved nest for in endc dot nt.

Rootsymbol syntax.

syntax -> blocks : '$1'.
blocks -> '$empty' : [].
blocks -> blocks block : '$1' ++ ['$2'].
block -> if_block : '$1'.
block -> for_block : '$1'.
block -> expressions : '$1'.
block -> text : '$1'.

if_block -> '{' conditional expression '}' blocks '{' endc '}' : {conditional('$2'), '$3', '$5'}.
for_block -> '{' for identifier in identifier '}' blocks '{' endc '}' : {for, remove_id('$3'), '$5', '$7'}.

expressions -> '{' expression '}' : '$2'.
expressions -> '{' '}' : ''.

expression -> expression op expression : parse_opertator('$2', '$1', '$3').
expression -> nt expression : {nt, '$2'}.
expression -> identifier dot identifier: {property, '$1', remove_id('$3')}.
expression -> identifier : '$1'.
expression -> value : '$1'.
expression -> reserved : '$1'.
expression -> nested : '$1'.

nested -> nest identifier identifier comma identifier: {nest, remove_id('$2'), remove_id('$3'), remove_id('$5')}.
nested -> nest identifier identifier: {nest, remove_id('$2'), remove_id('$3')}.

Erlang code.

parse_opertator({_Op, Category, Operator}, LExpression, RExpression) ->
	{Category, Operator, LExpression, RExpression}.
conditional({_, Conditional}) -> Conditional.
remove_id({identifier, Id}) -> Id.
