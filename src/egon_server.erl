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

run(Sim) ->
    gen_server:call({global, {Sim, es_config_server}}, {unfreaze_sim}).

pause(Sim) ->
    gen_server:call({global, {Sim, es_config_server}}, {freaze_sim}).


new_sim() ->
    new_sim(doc).

new_sim(Name, Desc, User) ->
    new_sim([Name, Desc, User]).

new_sim(doc) ->
    "new_sim(Name, Desc, User)";
new_sim(Params) ->
    gen_server:call(es_simulator_tracker_server, {start_simulator, Params}).

stop_sim(SimId) ->
    gen_server:call(es_simulator_tracker_server, {stop_simulator, SimId}).


list_sims() ->
    {ok, List} = gen_server:call(es_simulator_tracker_server, {get, simulators}),
    List.

sim_loaded(SimId) ->
    supervisor:which_children({global, {SimId, es_simulator_sup}}),
    timer:sleep(100),
    true.

general_test() ->
    ok = egon_server:start(),
    {ok,_} = egon_client:start_link("localhost", 1055, "Test user"),
    ok = egon_client:new_sim("Test sim", "Simulator for purposes of unit testing"),
    ok = egon_client:conn_to_sim(1),
    "305.0" = egon_client:call("{get, es_core_server, tavg}"),
    "305.0" = egon_client:call("{get, es_w7300_server, tref}"),
    "ok" = egon_client:call("{action, es_rod_position_server, step_in}"),
    "304.9416710346633" = egon_client:call("{get, es_core_server, tavg}"),
    egon_client:stop_link(),
    egon_server:stop(),
    ok.
