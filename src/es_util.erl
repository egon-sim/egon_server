-module(es_util).

-export([run_tests/0]).

run_tests() ->
    eunit:test(modules()).

modules() ->
    modules("ebin/egon_server.app").

modules(App_file) ->
    {ok, [{application, _, [{description, _}, {vsn, _}, {modules, Modules}, {registered, _}, {applications, _}, {mod, _}, {env, _}]}]} = file:consult(App_file),
    Modules.

-include_lib("eunit/include/eunit.hrl").

unit_test() ->
    ok.
