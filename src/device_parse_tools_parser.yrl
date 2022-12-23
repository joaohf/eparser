Nonterminals
boards
board_values
board_valuesx
num_boardsx
num_devicesx
devicex
key
values
value.

Terminals
board
num_boards
num_devices
device
board_info
key_or_value
equal.

Rootsymbol
boards.

boards -> board_values : '$1'.

board_values -> board_info num_boardsx : {boards, '$2', []}.
board_values -> board_info num_boardsx values : {boards, '$2' ++ '$3', []}.
board_values -> board_info num_boardsx values board_valuesx : {boards, '$2' ++ '$3', '$4'}.

board_valuesx -> board num_devicesx devicex board_valuesx : [{unwrap('$1'), '$2', '$3'} | '$4'].
board_valuesx -> board num_devicesx devicex : [{unwrap('$1'), '$2', '$3'}].

num_devicesx -> num_devices equal value : [{unwrap('$1'), '$3'}].
num_boardsx -> num_boards equal value : [{unwrap('$1'), '$3'}].
devicex -> device equal value values: [{device('$1'), '$3', '$4'}].
devicex -> device equal value values devicex: [{device('$1'), '$3', '$4'} | '$5'].

values -> key equal value values : [{'$1', '$3'} | '$4'].
values -> key equal value : [{'$1', '$3'}].
values -> key equal values : [{'$1', undefined} | '$3'].

key -> key_or_value : unwrap_key('$1').
value -> key_or_value : unwrap('$1').

Erlang code.

device({_,_,V}) -> V.

unwrap({_,_,V}) -> V.

unwrap_key({_,_,V}) ->
    Keys = [
        "board",
        "alias",
        "model",
        "version",
        "file",
        "md5",
        "devport",
        "adapter",
        "activecard",
        "enabled",
        "checkversion",
        "dependencies",
        "restart_type",
        "estimated_time"
    ],

    Fun = fun(Key, Acc) ->
        case string:find(V, Key, leading) of
            nomatch ->
                Acc;
            _Found ->
                Key
        end
    end,

    case lists:foldl(Fun, nomatch, Keys) of
        nomatch ->
            erlang:exit({key_not_exist, V});
        Key ->
            erlang:list_to_atom(Key)
    end.
    
