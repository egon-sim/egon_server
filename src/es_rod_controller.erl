-module(es_rod_controller).
-include_lib("include/es_common.hrl").
-compile(export_all).

speed() -> gen_server:call(es_rod_controller_server, {get, speed}).
mode() -> gen_server:call(es_rod_controller_server, {get, mode}).
set(Arg, Val) ->
    gen_server:call(es_rod_controller_server, {set, Arg, Val}).
