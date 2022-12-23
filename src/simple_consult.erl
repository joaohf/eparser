-module(simple_consult).

-include("include/device.hrl").

-export([parse_file/1]).

-spec parse_file(file:name_all()) -> Reason when
    Reason ::
        {ok, [term()]}
        | {error,
            file:posix()
            | badarg
            | terminated
            | system_limit
            | {Line :: integer(), Mod :: module(), Term :: term()}}.
parse_file(Filename) ->
    file:consult(Filename).
