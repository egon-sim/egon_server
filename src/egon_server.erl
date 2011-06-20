-module(egon_server).
-compile(export_all).

run() ->
    gen_server:call(es_config_server, {unfreaze_sim}).

pause() ->
    gen_server:call(es_config_server, {freaze_sim}).
