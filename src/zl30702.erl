-module(zl30702).

-export([parse_file/1]).

-type address() :: non_neg_integer().
-type data() :: non_neg_integer().
-type write_command() :: {write, address(), data()}.
-type wait_command() :: {wait, non_neg_integer()}.
-type commands() :: [write_command() | wait_command()].

-export_type([commands/0, write_command/0, wait_command/0, data/0, address/0]).

-spec parse_file(file:filename()) -> {ok, commands()} | {error, any()}.
parse_file(Filename) ->
    do_parser(Filename).

do_parser(ConfigScriptFilename) ->
    L = [
        fun slurp_configuration_script/1,
        fun parse_commands/1,
        fun generate_commands/1
    ],
    Execute = fun(Fun, AccIn) ->
        Fun(AccIn)
    end,
    lists:foldl(Execute, ConfigScriptFilename, L).

slurp_configuration_script(Filename) ->
    case file:open(Filename, [read]) of
        {ok, InFile} ->
            Tokens = slurp(InFile),
            ok = file:close(InFile),
            {Filename, Tokens};
        {error, enoent} ->
            {error, file_not_found}
    end.

slurp(InFile) ->
    slurp(InFile, file:read_line(InFile), []).

slurp(_InFile, eof, Acc) ->
    {ok, Acc};
slurp(InFile, {ok, Data}, Acc) ->
    case zl30702_lexer:string(Data) of
        {ok, [], _EndLine} ->
            slurp(InFile, file:read_line(InFile), Acc);
        {ok, Tokens, _EndLine} ->
            slurp(InFile, file:read_line(InFile), [Tokens | Acc])
    end;
slurp(_, {error, Reason}, _Acc) ->
    {error, Reason}.

parse_commands({error, _Reason} = Error) ->
    Error;
parse_commands({_Filename, {ok, Tokens}}) ->
    parse_command(Tokens, []).

parse_command([], Acc) ->
    Acc;
parse_command([Tokens | Rest], Acc) ->
    {ok, ParseTree} = zl30702_parser:parse(Tokens),
    parse_command(Rest, [ParseTree | Acc]).

generate_commands({error, _Reason} = Error) ->
    Error;
generate_commands(Commands) ->
    {ok, lists:map(fun generate_command/1, Commands)}.

generate_command({write, Address, Data}) ->
    {write, Address, Data};
generate_command({wait, Timeout}) ->
    {wait, Timeout}.
