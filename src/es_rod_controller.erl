-module(es_rod_controller).
-compile(export_all).

speed() -> gen_server:call(es_rod_controller_server, {get, speed}).
mode() -> gen_server:call(es_rod_controller_server, {get, mode}).
set(Arg, Val) ->
    gen_server:call(es_rod_controller_server, {set, Arg, Val}).
