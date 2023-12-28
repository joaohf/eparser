-module(eparser_SUITE).

%% Test server callbacks
-export([
    suite/0,
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    zl30702_simple_parser/1,
    zl30702_file_not_found/1,
    simple_erlang_like/1,
    device_simple_parser/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

all() ->
    [
        {group, zl30702},
        {group, simple_consult},
        {group, device}
    ].

groups() ->
    [
        {zl30702, [], [
            zl30702_simple_parser,
            zl30702_file_not_found
        ]},
        {simple_consult, [], [
            simple_erlang_like
        ]},
        {device, [], [
            device_simple_parser
        ]}
    ].

suite() ->
    [
        {ct_hooks, []},
        {timetrap, {seconds, 300}}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

zl30702_simple_parser(Config) ->
    Filename = filename:join(?config(data_dir, Config), "zl30702.txt"),
    Result = zl30702:parse_file(Filename),
    Sequence = [
        {write, 1410, 0},
        {write, 1411, 1},
        {write, 1412, 2},
        {wait, 20000},
        {write, 1413, 3},
        {write, 1414, 232},
        {write, 1415, 0},
        {write, 1416, 1},
        {write, 1417, 0},
        {write, 1418, 1},
        {write, 1419, 3},
        {write, 1420, 232},
        {write, 1421, 1},
        {write, 1423, 5},
        {write, 1424, 5},
        {write, 1425, 33},
        {write, 1426, 0},
        {write, 1427, 50},
        {write, 1428, 180},
        {write, 1429, 39},
        {write, 1430, 36},
        {write, 1431, 0},
        {write, 1432, 0},
        {write, 1433, 40},
        {write, 1434, 27},
        {write, 1435, 2},
        {write, 1436, 0},
        {write, 1437, 0},
        {write, 1412, 1},
        {wait, 20000},
        {wait, 1000000},
        {write, 1286, 0},
        {write, 1281, 0},
        {write, 1296, 0},
        {write, 1297, 0}
    ],

    ?assertMatch({ok, Sequence}, Result),

    ok.

zl30702_file_not_found(Config) ->
    Filename = filename:join(?config(data_dir, Config), "zl30702.not_found"),
    Result = zl30702:parse_file(Filename),

    ?assertMatch({error, file_not_found}, Result),

    ok.

simple_erlang_like(Config) ->
    Filename = filename:join(?config(data_dir, Config), "basic_erlang_like.conf"),
    Expect = [{test, ok}, {check, ok}, {path, "/tmp"}],
    Result = simple_consult:parse_file(Filename),

    ?assertMatch({ok, [Expect]}, Result),

    ok.

device_simple_parser(Config) ->
    Filename = filename:join(?config(data_dir, Config), "devices.cfg"),
    Expect =
        {boards, [{"num_boards", "2"}, {board, "b1"}, {board, "b2"}, {board, "b2"}], [
            {"b1", [{"e2k", "x1"}, {num_devices, "2"}], [
                {device, "fpgajic", [
                    {alias, "a"},
                    {model, "m"},
                    {version, "xyz1"},
                    {file, "/tmp/fpga_jic.rpd"},
                    {md5, "e019c0a6a79c526918622b637c3898d2"},
                    {devport, "/dev/mx25u256"},
                    {adapter, "0"},
                    {activecard, "0"},
                    {enabled, "1"},
                    {checkversion, "1"},
                    {dependencies, "b1_fpgacvp/a"},
                    {restart_type, "fpga-reload"},
                    {estimated_time, "30"}
                ]},
                {device, "fpgacvp", [
                    {model, "m"},
                    {alias, "a"},
                    {version, "xyz1"},
                    {file, "/tmp/fpga.core.rbf"},
                    {md5, "01639aac1805ee9f2984b2cf08899152"},
                    {devport, "/dev/cvp"},
                    {adapter, "0"},
                    {activecard, "0"},
                    {enabled, "1"},
                    {checkversion, "0"},
                    {dependencies, "b1_fpgajic/a"},
                    {restart_type, "fpga-reload"},
                    {estimated_time, "10"}
                ]}
            ]},
            {"b2", [{"e2k", "xx1"}, {num_devices, "2"}], [
                {device, "cpld", [
                    {model, "a"},
                    {alias, undefined},
                    {version, "4"},
                    {file, "/tmp/v04.jed"},
                    {md5, "ac4b38fc63716157bfdd6f14680df55d"},
                    {devport, "/dev/i2c-34"},
                    {adapter, "34"},
                    {activecard, "1"},
                    {enabled, "1"},
                    {checkversion, "1"},
                    {dependencies, undefined},
                    {restart_type, "system-reboot"},
                    {estimated_time, "120"}
                ]},
                {device, "55", [
                    {model, "b"},
                    {alias, undefined},
                    {version, "1.0"},
                    {file, "/tmp/1_0.hex"},
                    {md5, "418b571772a384b694e56a42dd2ed0f1"},
                    {devport, "/dev/i2c-34"},
                    {adapter, "34"},
                    {activecard, "1"},
                    {enabled, "1"},
                    {checkversion, "1"},
                    {dependencies, undefined},
                    {restart_type, "system-reboot"},
                    {estimated_time, "120"}
                ]}
            ]},
            {"b2", [{"e2k", "xxb1*"}, {num_devices, "2"}], [
                {device, "cpld", [
                    {model, "a"},
                    {alias, undefined},
                    {version, "4"},
                    {file, "/tmp/v04.jed"},
                    {md5, "ac4b38fc63716157bfdd6f14680df55d"},
                    {devport, "/dev/i2c-34"},
                    {adapter, "34"},
                    {activecard, "1"},
                    {enabled, "1"},
                    {checkversion, "1"},
                    {dependencies, undefined},
                    {restart_type, "system-reboot"},
                    {estimated_time, "120"}
                ]},
                {device, "55", [
                    {model, "b"},
                    {alias, undefined},
                    {version, "1.0"},
                    {file, "/tmp/1_0.hex"},
                    {md5, "418b571772a384b694e56a42dd2ed0f1"},
                    {devport, "/dev/i2c-34"},
                    {adapter, "34"},
                    {activecard, "1"},
                    {enabled, "1"},
                    {checkversion, "1"},
                    {dependencies, undefined},
                    {restart_type, "system-reboot"},
                    {estimated_time, "120"}
                ]}
            ]}
        ]},
    Result = device_parse_tools:parse_file(Filename),

    ct:pal("xxxx ~p", [Result]),

    ?assertMatch(Expect, Result),

    ok.

%%--------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%--------------------------------------------------------------------
