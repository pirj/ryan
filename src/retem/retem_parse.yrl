Nonterminals 
syntax blocks block if_block for_block expressions expression end.

Terminals
 '{' '}' identifier op text value conditional endc reserved nest for in dot nt.

Rootsymbol syntax.

syntax -> blocks : '$1'.

blocks -> block : ['$1'].
blocks -> block blocks : ['$1'|'$2'].

block -> if_block : '$1'.
block -> for_block : '$1'.
block -> expressions : '$1'.
block -> text : '$1'.

end -> '{' endc '}' : '$1'.

if_block -> '{' conditional expression '}' blocks end : {conditional('$2'), '$3', '$5'}.
if_block -> '{' conditional expression '}' block end : {conditional('$2'), '$3', '$5'}.
if_block -> '{' conditional expression '}' text end : {conditional('$2'), '$3', '$5'}.
if_block -> '{' conditional expression '}' expressions end : {conditional('$2'), '$3', '$5'}.

for_block -> '{' for identifier in identifier '}' blocks end : {for, remove_id('$3'), '$5', ['$7']}.
for_block -> '{' for identifier in identifier '}' block end : {for, remove_id('$3'), '$5', '$7'}.
for_block -> '{' for identifier in identifier '}' text end : {for, remove_id('$3'), '$5', '$7'}.
for_block -> '{' for identifier in identifier '}' expressions end : {for, remove_id('$3'), '$5', '$7'}.

expressions -> '{' expression '}' : '$2'.
expressions -> '{' '}' : ''.

expression -> expression op expression : {category('$2'), operator('$2'), '$1', '$3'}.
expression -> nt expression : {nt, '$2'}.
expression -> identifier dot identifier: {property, '$1', remove_id('$3')}.
expression -> identifier : '$1'.
expression -> value : '$1'.
expression -> reserved : '$1'.
expression -> nest identifier: {nest, remove_id('$2')}.

Erlang code.

category({_Op, Category, _Operator}) -> Category.
operator({_Op, _Category, Operator}) -> Operator.
conditional({_, Conditional}) -> Conditional.
remove_id({identifier, Id}) -> Id.
