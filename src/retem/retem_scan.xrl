Definitions.

%% TXT = (?<=^|\})[^\}\{]+?(?=\{|$) - bring this back when leex casn understand PCRE
TXT = \}[^\{]*
D	= [0-9]
U	= [A-Z]
L	= [a-z]
UL  = {U}|{L}
H	= {U}|{L}|{D}
A	= ({U}|{L}|{D}|_)
WS	= [\000-\s]
ID  = {UL}{A}*
AR  = (\+|-|\*|/)
CO  = (eq|neq|gt|lt|gteq|lteq)
LO  = (and|or)
NOT = (not)

Rules.
{     : {token,{'{'}}.
}     : {token,{'}'},['}']}.
{TXT} : {token,{text, remove_leading_bracet(TokenChars, TokenLen)}}.
{LO}  : {token,{logical,list_to_atom(TokenChars)}}.
{NOT} : {token,{logical,list_to_atom(TokenChars)}}.
{CO}  : {token,{comparator,list_to_atom(TokenChars)}}.
{AR}  : {token,{arithmetic, arithmetic_to_atom(TokenChars)}}.
{ID}  : {token,{identifier,TokenChars}}.
{WS}+ : skip_token.

Erlang code.

arithmetic_to_atom("+") -> plus;
arithmetic_to_atom("-") -> minus;
arithmetic_to_atom("/") -> divide;
arithmetic_to_atom("*") -> multiply.

remove_leading_bracet(Chars, Len) -> lists:sublist(Chars, 2, Len - 1).