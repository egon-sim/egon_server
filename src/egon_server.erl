-module(egon_server).
-compile(export_all).

start() ->
    application:start(egon_server).

stop() ->
    application:stop(egon_server).

restart() ->
    stop(),
    start().

run() ->
    gen_server:call(es_config_server, {unfreaze_sim}).

pause() ->
    gen_server:call(es_config_server, {freaze_sim}).
