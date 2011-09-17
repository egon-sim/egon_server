-module(es_turbine).
-include_lib("include/es_common.hrl").
-import(es_turbine_server).
-compile(export_all).

-define(SERVER(SimId), {global, {SimId, es_turbine_server}}).

start(SimId) ->
    es_turbine_server:start_link(SimId).
stop(SimId) ->
    gen_server:call(?SERVER(SimId), stop).

start_ramp(SimId) ->
    gen_server:call(?SERVER(SimId), {action, ramp, start}).

start_ramp(SimId, Target, Rate) ->
    set_target(SimId, Target),
    set_rate(SimId, Rate),
    start_ramp(SimId).

power(SimId) -> 
    gen_server:call(?SERVER(SimId), {get, power}).

go(SimId) -> 
    gen_server:call(?SERVER(SimId), {get, go}).

target(SimId) -> 
    gen_server:call(?SERVER(SimId), {get, target}).

set_target(SimId, Value) -> 
    gen_server:call(?SERVER(SimId), {set, target, Value}).

rate(SimId) -> 
    gen_server:call(?SERVER(SimId), {get, rate}).

set_rate(SimId, Value) -> 
    gen_server:call(?SERVER(SimId), {set, rate, Value}).


integration_test() ->
    ok = egon_server:start(),
    {ok, SimId} = egon_server:new_sim(["Test_server", "Simulator started by test function", "Tester"]),
    true = egon_server:sim_loaded(SimId),

    egon_server:run(SimId),

    100 = power(SimId),
    false = go(SimId),
    80 = target(SimId),
    1 = rate(SimId),

    start_ramp(SimId, 90, 2),
        
    90 = target(SimId),
    2 = rate(SimId),

    timer:sleep(3500),

    true = go(SimId),

    timer:sleep(3500),

    90 = power(SimId),
    false = go(SimId),
    90 = target(SimId),
    2 = rate(SimId),

    egon_server:stop(),
    ok.
