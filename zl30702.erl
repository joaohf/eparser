-module(ehal_tool_prv_zl30702).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-include("ehal_tool.hrl").

-define(PROVIDER, zl30702).
-define(DEPS, []).

-spec init(ehal_tool_state:t()) -> {ok, ehal_tool_state:t()}.
init(State) ->
    State1 = ehal_tool_state:add_provider(
        State,
        providers:create([
            {name, ?PROVIDER},
            {module, ?MODULE},
            {bare, true},
            {deps, ?DEPS},
            {example, "ehalcli zl30702"},
            {short_desc, "IEEE 1588 & Synch Ethernet Packet Clock Network Synchronizer tool."},
            {opts, opt_spec_list()}
        ])
    ),
    {ok, State1}.

-spec do(ehal_tool_state:t()) -> {ok, ehal_tool_state:t()} | {error, string()}.
do(State) ->
    {Opts, RestArgs} = ehal_tool_state:command_parsed_args(State),

    {ok, _} = ehal_devices:add_device(spi0, zl30702),

    State1 = zl30702(State, Opts, RestArgs),

    State1.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

-spec opt_spec_list() -> [getopt:option_spec()].
opt_spec_list() ->
    [
        {info, $i, "info", undefined, "Display device general info"},
        {regmap, $p, "regmap", undefined, "Display register map"},
        {dump, $d, "dump", undefined, "Dump registers"},
        {read, $r, "read", undefined, "Read address, requires address. Ex: zl30702 -r 0x300"},
        {write, $w, "write", undefined,
            "Write address, requires address and value. Ex: zl30720 -d 0x300 -w 0xFF00"},
        {load_file, $f, "file", string, "Load program file into the device"}
    ].

zl30702(State, [], _Args) ->
    {ok, State};
zl30702(State, [info], _Args) ->
    {ok, Info} = zl30702:read(info),
    {ok, Id} = zl30702:read(id),
    {ok, Revision} = zl30702:read(revision),

    Result = {ok, {Info, Id, Revision}},

    IsReady =
        case Info of
            V when (V band (1 bsl 7)) == 128 -> "";
            _ -> "not"
        end,

    ?CONSOLE("Device zl30702 ~p ~p is ~s ready", [Id, Revision, IsReady]),

    State1 = ehal_tool_state:set(State, command_result, Result),
    {ok, State1};
zl30702(State, [{load_file, ConfigScriptFilename}], _Args) ->
    {ok, CommandsCount} = configure_zl30702(ConfigScriptFilename),

    ?CONSOLE("Device zl30702 has been configured. Number of commands written: ~w", [CommandsCount]),

    State1 = ehal_tool_state:set(State, command_result, CommandsCount),

    {ok, State1};
zl30702(State, [regmap], _Args) ->
    ?CONSOLE("Not implemented yet.", []),
    {ok, State};
zl30702(State, [write], _Args) ->
    ?CONSOLE("Not implemented yet.", []),
    {ok, State};
zl30702(State, [read], _Args) ->
    ?CONSOLE("Not implemented yet.", []),
    {ok, State};
zl30702(State, [dump], _Args) ->
    ?CONSOLE("Not implemented yet.", []),
    {ok, State}.

configure_zl30702(ConfigScriptFilename) ->
    L = [
        fun slurp_configuration_script/1,
        fun parse_commands/1,
        fun generate_commands/1,
        fun write_commands/1
    ],
    Execute = fun(Fun, AccIn) ->
        Fun(AccIn)
    end,
    lists:foldl(Execute, ConfigScriptFilename, L).

slurp_configuration_script(Filename) ->
    {ok, InFile} = file:open(Filename, [read]),
    Tokens = slurp(InFile),
    ok = file:close(InFile),
    {Filename, Tokens}.

parse_commands({_Filename, {ok, Tokens}}) ->
    parse_command(Tokens, []).

generate_commands(Commands) ->
    lists:map(fun generate_command/1, Commands).

generate_command({write, Address, Data}) ->
    zl30702:prepare_write(Address, Data);
generate_command({wait, Timeout}) ->
    zl30702:prepare_write(wait, Timeout).

write_commands(Commands) ->
    {ok, CommandsCount} = lists:foldl(fun write_command/2, {ok, 0}, Commands),
    {ok, CommandsCount}.

write_command({wait, Timeout}, AccIn) ->
    Ms = round(Timeout / 1000),
    timer:sleep(Ms),
    AccIn;
write_command(Data, {ok, Count}) ->
    ok = zl30702:write(Data),
    {ok, Count + 1}.

slurp(InFile) ->
    slurp(InFile, file:read_line(InFile), []).

slurp(_InFile, eof, Acc) ->
    {ok, Acc};
slurp(InFile, {ok, Data}, Acc) ->
    case ehal_tool_prv_zl30702_lexer:string(Data) of
        {ok, [], _EndLine} ->
            slurp(InFile, file:read_line(InFile), Acc);
        {ok, Tokens, _EndLine} ->
            slurp(InFile, file:read_line(InFile), [Tokens | Acc])
    end;
slurp(_, {error, Reason}, _Acc) ->
    {error, Reason}.

parse_command([], Acc) ->
    Acc;
parse_command([Tokens | Rest], Acc) ->
    {ok, ParseTree} = ehal_tool_prv_zl30702_grammar:parse(Tokens),
    parse_command(Rest, [ParseTree | Acc]).
