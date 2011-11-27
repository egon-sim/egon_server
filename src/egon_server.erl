-module(egon_server).
-compile(export_all).

start() ->
    application:start(sasl),
    application:start(egon_server).

stop() ->
    application:stop(egon_server).

shutdown() ->
    es_master_server:shutdown().

restart() ->
    stop(),
    start().

run(Sim) ->
    es_config_server:unfreaze(Sim).

pause(Sim) ->
    es_config_server:freaze(Sim).

new_sim() ->
    new_sim(doc).

new_sim(doc) ->
    "new_sim(Name, Desc, User)".

new_sim(Name, Desc, User) ->
    es_simulator_tracker_server:start_new_simulator(Name, Desc, User).

stop_sim(SimId) ->
    es_simulator_tracker_server:stop_simulator(SimId).

list_sims() ->
    {ok, List} = es_simulator_tracker_server:simulators(),
    List.

sim_loaded(SimId) ->
    supervisor:which_children({global, {SimId, es_simulator_sup}}),
    timer:sleep(100),
    true.


%%%==================================================================
%%% Test functions
%%%==================================================================

-include_lib("eunit/include/eunit.hrl").

unit_test() ->
    ?assertEqual(ok, egon_server:start()),

    ?assertEqual({ok,1}, new_sim("Tester", "Test sim 1", "Simulator for purposes of unit testing")),

    ?assertEqual(ok, egon_server:run(1)),

    ?assertEqual({ok,stopped}, stop_sim(1)),

    ?assertEqual(ok, egon_server:stop()),
    ok.
