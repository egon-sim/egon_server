-module(es_util).
-import(eunit).
-compile(export_all).

run_tests() ->
    eunit:run(modules(), {suffix, "_test"}).

modules() ->
    modules("ebin/egon_server.app").

modules(App_file) ->
    {ok, [{application, _, [{description, _}, {vsn, _}, {modules, Modules}, {registered, _}, {applications, _}, {mod, _}, {env, _}]}]} = file:consult(App_file),
    Modules.

generate_compile() ->
    {ok, FD} = file:open("compile.bat", write),
    lists:foreach(fun(M) -> io:format(FD, "erlc -o ebin src\\~p.erl~n", [M]) end, modules()),
    file:close(FD),
    ok.