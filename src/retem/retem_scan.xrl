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
{ID}  : {token,{identifier,TokenChars}}.
{AR}  : {token,{arithmetic,TokenChars}}.
%% {CO}  : {token,{comparator,TokenChars}}.
%% {LO}  : {token,{logical,TokenChars}}.
{WS}+ : skip_token.

Erlang code.

