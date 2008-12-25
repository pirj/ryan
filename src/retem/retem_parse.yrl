Nonterminals 
syntax blocks block if_block for_block expression.

Terminals
 '{' '}' identifier op text value conditional_op end_op reserved nest for_op in_op dot not_op.

Rootsymbol syntax.

syntax -> blocks : '$1'.

blocks -> block : ['$1'].
blocks -> block blocks : ['$1'|'$2'].

block -> if_block : '$1'.
block -> for_block : '$1'.
block -> '{' expression '}' : '$2'.
block -> '{' '}' : ''.
block -> text : '$1'.

if_block -> '{' conditional_op expression '}' block '{' end_op '}' : {condition, conditional('$2'), '$5'}.
if_block -> '{' conditional_op expression '}' text '{' end_op '}' : {condition, conditional('$2'), '$5'}.
if_block -> '{' conditional_op expression '}' '{' expression '}' '{' end_op '}' : {condition, conditional('$2'), '$6'}.

for_block -> '{' for_op identifier in_op identifier '}' block '{' end_op '}' : {for, remove_id('$3'), '$5', '$7'}.
for_block -> '{' for_op identifier in_op identifier '}' text '{' end_op '}' : {for, remove_id('$3'), '$5', '$7'}.
for_block -> '{' for_op identifier in_op identifier '}' '{' expression '}' '{' end_op '}' : {for, remove_id('$3'), '$5', '$8'}.

expression -> expression op expression : {category('$2'), operator('$2'), '$1', '$3'}.
% expression -> not_op expression : {not_op, '$2'}.
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