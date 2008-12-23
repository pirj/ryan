Nonterminals 
blocks embraced expressions conditional_block. %for_block

Terminals
 '{' '}' identifier arithmetic comparator logical not_op text value conditional_op end_op reserved nest for_op in_op dot.

Rootsymbol blocks.

blocks -> conditional_block : ['$1'].
%blocks -> for_block : ['$1'].
blocks -> embraced : ['$1'].
blocks -> embraced blocks : ['$1'|'$2'].
blocks -> text : ['$1'].
blocks -> text blocks : ['$1'|'$2'].

conditional_block -> '{' conditional_op expressions '}' text '{' end_op '}': {condition, unwrap_operator('$2'), '$3', '$5'}.

% for_block -> '{' for_op identifier in_op identifier '}' text '{' end_op '}': {for, unwrap_operator('$2'), '$3', '$5'}.

embraced    -> '{' expressions '}' : '$2'.
embraced    -> '{' '}' : ''.

expressions -> expressions logical expressions : {logical, unwrap_operator('$2'), '$1', '$3'}.
expressions -> expressions comparator expressions : {comparator, unwrap_operator('$2'), '$1', '$3'}.
expressions -> expressions arithmetic expressions : {arithmetic, unwrap_operator('$2'), '$1', '$3'}.
expressions -> not_op expressions : {not_op, unwrap_operator('$1'), '$2', '$2'}.
expressions -> identifier dot identifier: {property, '$1', remove_id('$3')}.
expressions -> identifier : '$1'.
expressions -> value : '$1'.
expressions -> reserved : '$1'.
expressions -> nest identifier: {nest, remove_id('$2')}.

Erlang code.

unwrap_operator({_Category, Operator}) -> Operator.
remove_id({identifier, Id}) -> Id.