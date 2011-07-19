-module(egon_server).
-compile(export_all).

start() ->
    application:start(sasl),
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

general_test() ->
    ok = egon_server:start(),
    {ok,_} = egon_client:start(),
    ok = egon_client:new_sim(),
    "305.0" = egon_client:send("{get, es_core_server, tavg}"),
    "305.0" = egon_client:send("{get, es_w7300_server, tref}"),
    "ok" = egon_client:send("{action, es_rod_position_server, step_in}"),
    "304.9416710346633" = egon_client:send("{get, es_core_server, tavg}"),
    ok.