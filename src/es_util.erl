-module(es_util).
-import(eunit).

-export([run_tests/0, unit_test/0]).

run_tests() ->
    {_Passed, Failed, _PassedList, FailedList} = eunit:run(modules(), {suffix, "_test"}),
    io:format("----------------------~n"),
    io:format("Failed tests: ~p ~p~n", [Failed, FailedList]),
    io:format("----------------------~n").

modules() ->
    modules("ebin/egon_server.app").

modules(App_file) ->
    {ok, [{application, _, [{description, _}, {vsn, _}, {modules, Modules}, {registered, _}, {applications, _}, {mod, _}, {env, _}]}]} = file:consult(App_file),
    Modules.

-include_lib("include/es_common.hrl").

unit_test() ->
    ok.