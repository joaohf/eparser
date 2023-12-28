-module(device_parse_tools).

-export([parse_file/1]).

-type device_property() ::
    {alias, string()}
    | {model, string()}
    | {version, string()}
    | {file, string()}
    | {md5, string()}
    | {devport, string()}
    | {adapter, string()}
    | {activecard, string()}
    | {enable, string()}
    | {checkversion, string()}
    | {dependencies, string()}
    | {restart_type, string()}
    | {estimated_time, string()}.
-type device_properties() :: [device_property()].
-type device_name() :: string().
-type device() :: {device, device_name(), device_properties()}.
-type num_devices() :: {num_device, string()}.
-type board_meta() :: [num_devices()].
-type board() :: {string(), board_meta(), device()}.
-type boards() :: [board()].
-type board_name() :: {board, string()}.
-type num_boards() :: {num_boards, string()}.
-type board_info() :: [num_boards() | board_name()].
-type devices() :: {boards, board_info(), boards()}.

-export_type([devices/0]).

-spec parse_file(file:filename()) -> {ok, devices()} | {error, any()}.
parse_file(Filename) ->
    do_parser(Filename).

do_parser(ConfigScriptFilename) ->
    L = [
        fun slurp_configuration_script/1,
        fun tokenize/1,
        fun parse_devices/1
    ],
    Execute = fun(Fun, AccIn) ->
        Fun(AccIn)
    end,
    lists:foldl(Execute, ConfigScriptFilename, L).

slurp_configuration_script(Filename) ->
    case file:read_file(Filename) of
        {ok, Content} ->
            {Filename, {ok, binary_to_list(Content)}};
        {error, _Reason} = Error ->
            Error
    end.

tokenize({Filename, {ok, Data}}) ->
    {ok, Tokens, _EndLine} = device_parse_tools_lexer:string(Data),
    {Filename, {ok, Tokens}}.

parse_devices({error, _Reason} = Error) ->
    Error;
parse_devices({_Filename, {ok, Tokens}}) ->
    {ok, ParseTree} = device_parse_tools_parser:parse(Tokens),
    ParseTree.
