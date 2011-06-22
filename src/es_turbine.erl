-module(es_turbine).
-include_lib("include/es_common.hrl").
-import(es_turbine_server).
-compile(export_all).

start() ->
    es_turbine_server:start_link().
stop() ->
    gen_server:call(es_turbine_server, stop).

start_ramp() ->
    gen_server:call(es_turbine_server, {action, ramp, start}).

start_ramp(Target, Rate) ->
    set(target, Target),
    set(rate, Rate),
    start_ramp().

get(What) ->
    gen_server:call(es_turbine_server, {get, What}).

set(What, Val) ->
    gen_server:call(es_turbine_server, {set, What, Val}).

power() -> es_turbine:get(power).
go() -> es_turbine:get(go).
target() -> es_turbine:get(target).
rate() -> es_turbine:get(rate).
