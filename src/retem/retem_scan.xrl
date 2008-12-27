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
{     : {token, {'{', TokenLine}}.
}     : {token, {'}', TokenLine}}.
{TXT} : [{token, {'}', TokenLine}} , {token, {text, TokenLine, remove_leading_brace(TokenChars, TokenLen)}}].
,     : {token, {',', TokenLine}}.
{RES} : {token, {reserved, TokenLine, list_to_atom(TokenChars)}}.
{LO}  : {token, {op, TokenLine, {logical, list_to_atom(TokenChars)}}}.
{NOT} : {token, {nt, TokenLine}}.
{CO}  : {token, {op, TokenLine, {comparator, list_to_atom(TokenChars)}}}.
{AR}  : {token, {op, TokenLine, {arithmetic, list_to_atom(TokenChars)}}}.
{COO} : {token, {conditional, TokenLine, list_to_atom(TokenChars)}}.
{COE} : {token, {conditional_else, TokenLine, list_to_atom(TokenChars)}}.
{NST} : {token, {nest, TokenLine}}.
{END} : {token, {endc, TokenLine}}.
{FOR} : {token, {forc, TokenLine}}.
{IN}  : {token, {in, TokenLine}}.
{DOT} : {token, {dot, TokenLine}}.

{ID}  : {token, {identifier, TokenLine, list_to_atom(TokenChars)}}.
{FLT} : {token, {value, TokenLine, string_to_float(TokenChars)}}.
{INT} : {token, {value, TokenLine, string_to_integer(TokenChars)}}.
{WS}+ : skip_token.
 
Erlang code.

remove_leading_brace(Chars, Len) -> lists:sublist(Chars, 2, Len - 1).

string_to_float([$-|Chars]) -> -list_to_float(Chars);
string_to_float(Chars) -> list_to_float(Chars).
  
string_to_integer([$-|Chars]) -> -list_to_integer(Chars);
string_to_integer(Chars) -> list_to_integer(Chars).
