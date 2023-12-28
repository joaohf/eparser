Definitions.

COMMENT        = #[%=:.<>,A-Za-z0-9\s\b\t\n\f\r\\\"\_]*
BOARD          = \[[a-zA-Z0-9]*\]
KEY            = [a-z\_]*[0-9]
EQUAL          = =
VALUE          = [A-Za-z0-9\_/\-\.\*]*
KEY_OR_VALUE  = [A-Za-z0-9\_/\-\.\*]*
WS             = [\s\t]
LB             = \n|\r\n|\r

DEVICE_PROPERTY = device[0-9]*

Rules.

\[boards\]       : {token, {board_info, TokenLine, to_board(TokenChars, TokenLen)}}.
{BOARD}          : {token, {board, TokenLine, to_board(TokenChars, TokenLen)}}.
num_devices      : {token, {num_devices, TokenLine, list_to_atom(TokenChars)}}.
e2k              : {token, {e2k, TokenLine, to_e2k(TokenChars)}}.
num_boards       : {token, {num_boards, TokenLine, TokenChars}}.
{DEVICE_PROPERTY} : {token, {device, TokenLine, to_device(TokenChars, TokenLen)}}.
{KEY_OR_VALUE}   : {token, {key_or_value, TokenLine, TokenChars}}.
{EQUAL}          : {token, {equal, TokenLine, list_to_atom(TokenChars)}}.
{COMMENT}        : skip_token.
{WS}             : skip_token.
{LB}             : skip_token.

Erlang code.

to_board(TokenChars, TokenLen) ->
    lists:sublist(TokenChars, 2, TokenLen - 2).

to_device(_TokenChars, _TokenLen) ->
    device.

to_e2k(TokenChars) ->
    TokenChars.