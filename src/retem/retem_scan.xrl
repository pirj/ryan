Definitions.
 
TXT = \}[^\{]*
D  = [0-9]
U  = [A-Z]
L  = [a-z]
UL  = {U}|{L}
H  = {U}|{L}|{D}
A  = ({U}|{L}|{D}|_)
WS  = [\000-\s]
ID  = {UL}{A}*
AR  = (\+|-|\*|/)
RES = (true|false|nil)
CO  = (eq|neq|gt|lt|gteq|lteq)
LO  = (and|or)
NOT = not
COO = (if|unless)
COE = (else|elseif|elsif)
END = end
NST = nest
FLT = -?{D}+\.{D}+
INT = -?{D}+
FOR = for
IN  = in
DOT = \.
 
Rules.
{     : {token, {'{'}}.
}     : {token, {'}'}}.
{TXT} : [{token, {'}'}} , {token, {text, remove_leading_brace(TokenChars, TokenLen)}}].
,     : {token, {','}}.
{RES} : {token, {reserved, list_to_atom(TokenChars)}}.
{LO}  : {token, {op, logical, list_to_atom(TokenChars)}}.
{NOT} : {token, {nt}}.
{CO}  : {token, {op, comparator, list_to_atom(TokenChars)}}.
{AR}  : {token, {op, arithmetic, list_to_atom(TokenChars)}}.
{COO} : {token, {conditional, list_to_atom(TokenChars)}}.
{COE} : {token, {conditional_else, list_to_atom(TokenChars)}}.
{NST} : {token, {nest}}.
{END} : {token, {endc}}.
{FOR} : {token, {for}}.
{IN}  : {token, {in}}.
{DOT} : {token, {dot}}.

{ID}  : {token, {identifier, list_to_atom(TokenChars)}}.
{FLT} : {token, {value, string_to_float(TokenChars)}}.
{INT} : {token, {value, string_to_integer(TokenChars)}}.
{WS}+ : skip_token.
 
Erlang code.

remove_leading_brace(Chars, Len) -> lists:sublist(Chars, 2, Len - 1).

string_to_float([$-|Chars]) -> -list_to_float(Chars);
string_to_float(Chars) -> list_to_float(Chars).
  
string_to_integer([$-|Chars]) -> -list_to_integer(Chars);
string_to_integer(Chars) -> list_to_integer(Chars).
