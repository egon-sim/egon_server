-module(es_turbine).
-include_lib("include/es_common.hrl").
-import(es_turbine_server).
-compile(export_all).

start(SimId) ->
    es_turbine_server:start_link(SimId).
stop(SimId) ->
    gen_server:call({global, {SimId, es_turbine_server}}, stop).

start_ramp(SimId) ->
    gen_server:call({global, {SimId, es_turbine_server}}, {action, ramp, start}).

start_ramp(SimId, Target, Rate) ->
    set(SimId, target, Target),
    set(SimId, rate, Rate),
    start_ramp(SimId).

get(SimId, What) ->
    gen_server:call({global, {SimId, es_turbine_server}}, {get, What}).

set(SimId, What, Val) ->
    gen_server:call({global, {SimId, es_turbine_server}}, {set, What, Val}).

power() -> es_turbine:get(power).
go() -> es_turbine:get(go).
target() -> es_turbine:get(target).
rate() -> es_turbine:get(rate).
