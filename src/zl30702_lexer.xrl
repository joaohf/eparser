Definitions.

COMMENT        = ;[%=:.<>,A-Za-z0-9\s\b\t\n\f\r\\\"\_]*
REGNAME        = ;\s\s[a-z0-9\_]*
VALUE          = 0[xX][0-9A-F]+
TIME           = [0-9]+
COMMAND        = X|W
WS             = [\s\t]
LB             = \n|\r\n|\r

Rules.
,                : skip_token.
{COMMAND}        : {token, {command, TokenLine, command(TokenChars)}}.
{VALUE}          : {token, {value, TokenLine, from_hex(TokenChars)}}.
{TIME}           : {token, {time, TokenLine, list_to_integer(TokenChars)}}.
{REGNAME}        : {token, {regname, TokenLine, to_regname(TokenChars)}}.

{COMMENT}        : skip_token.
{WS}             : skip_token.
{LB}             : skip_token.

Erlang code.

from_hex([$0, $x | Rest]) ->
  erlang:list_to_integer(Rest, 16).

to_regname([$;, $ , $  | Regname]) -> Regname.

command("X") -> write;
command("W") -> wait.
