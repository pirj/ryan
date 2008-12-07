Definitions.

D	= [0-9]
U	= [A-Z]
L	= [a-z]
UL  = {U}|{L}
H	= {U}|{L}|{D}
A	= ({U}|{L}|{D}|_)
WS	= [\000-\s]
ID  = {UL}{A}*
AR  = (\+|-|\*|/)
%% CO  = (eq|neq|gt|lt|gteq|lteq)
%% LO  = (and|or|not)

Rules.
\{    : {token,{'{',TokenLine}}.
\}    : {token,{'}',TokenLine}}.
{ID}  : {token,{identifier,TokenLine,TokenChars}}.
{AR}  : {token,{arithmetic,TokenLine,TokenChars}}.
%% {CO}  : {token,{comparator,TokenLine,TokenChars}}.
%% {LO}  : {token,{logical,TokenLine,TokenChars}}.
{WS}+ : skip_token.

Erlang code.

