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
    set(SimId, target, Target),
    set(SimId, rate, Rate),
    start_ramp(SimId).

power(SimId) -> 
    gen_server:call(?SERVER(SimId), {get, power}).

go(SimId) -> 
    gen_server:call(?SERVER(SimId), {get, go}).

target(SimId) -> 
    gen_server:call(?SERVER(SimId), {get, target}).

rate(SimId) -> 
    gen_server:call(?SERVER(SimId), {get, rate}).


