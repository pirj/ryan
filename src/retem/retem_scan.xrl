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
FLT = -?{D}+\.{D}+
INT = -?{D}+
 
Rules.
{     : {token, {'{'}}.
}     : {token, {'}'}}.
{TXT} : [{token, {'}'}} , {token, {text, remove_leading_brace(TokenChars, TokenLen)}}].

{RES} : {token, {reserved, list_to_atom(TokenChars)}}.
{LO}  : {token, {logical, list_to_atom(TokenChars)}}.
{NOT} : {token, {logical, not_op}}.
{CO}  : {token, {comparator, list_to_atom(TokenChars)}}.
{AR}  : {token, {arithmetic, list_to_atom(TokenChars)}}.
{COO} : {token, {conditional_op, list_to_atom(TokenChars)}}.
{COE} : {token, {conditional_else, list_to_atom(TokenChars)}}.
{END} : {token, {end_op}}.
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

