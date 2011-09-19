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

unit_test() ->
    ok = egon_server:start(),

    {ok,1} = new_sim("Tester", "Test sim 1", "Simulator for purposes of unit testing"),

    {ok,stopped} = stop_sim(1),

    egon_server:stop(),
    ok.
