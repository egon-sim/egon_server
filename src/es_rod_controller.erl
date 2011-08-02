-module(es_rod_controller).
-include_lib("include/es_common.hrl").
-compile(export_all).

speed(SimId) -> gen_server:call({global, {SimId, es_rod_controller_server}}, {get, speed}).
mode(SimId) -> gen_server:call({global, {SimId, es_rod_controller_server}}, {get, mode}).
set(SimId, Arg, Val) ->
    gen_server:call({global, {SimId, es_rod_controller_server}}, {set, Arg, Val}).
