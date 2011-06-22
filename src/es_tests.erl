-module(es_tests).
-import(eunit).
-compile(export_all).

run() ->
    eunit:run(modules(), {suffix, "_test"}).

modules() ->
    modules("ebin/egon_server.app").

modules(App_file) ->
    {ok, [{application, _, [{description, _}, {vsn, _}, {modules, Modules}, {registered, _}, {applications, _}, {mod, _}]}]} = file:consult(App_file),
    Modules.