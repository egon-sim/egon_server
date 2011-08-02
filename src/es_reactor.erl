-module(es_reactor).
-include_lib("include/es_common.hrl").
-import(es_core).
-import(es_rod_position).
-import(es_timer).
-import(es_turbine).
-compile(export_all).

boron() -> es_core:boron().
borate(Ppm) -> es_core:borate(Ppm).
dilute(Ppm) -> es_core:dilute(Ppm).
set_power(Power) -> es_core:set_power(Power).
flux() -> es_core:flux().
burnup() -> es_core:burnup().
mtc() -> es_core:mtc().

rod_pos() -> es_rod_position:position().
step_in() -> es_rod_position:step_in().
step_out() -> es_rod_position:step_out().

power() -> es_turbine:power().

tref(SimId) ->
    gen_server:call({global, {SimId, es_w7300_server}}, {get, tref}).
tavg() -> 
    gen_server:call(es_core_server, {get, tavg}).

status() ->
    es_comm:call(rx_port, "status", []).

status(1) ->
    es_comm:call(rx_port, "status", [1]).
