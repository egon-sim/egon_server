-module(es_core).
-include_lib("include/es_common.hrl").
-import(core_server).
-compile(export_all).

start() ->
    core_server:start_link().
stop(SimId) ->
    gen_server:call({global, {SimId, es_core_server}}, stop).


get(SimId, What) ->
    gen_server:call({global, {SimId, es_core_server}}, {get, What}).
get(SimId, What, Arg1) ->
    gen_server:call({global, {SimId, es_core_server}}, {get, What, Arg1}).

set(SimId, flux, Arg1) ->
    gen_server:call({global, {SimId, es_core_server}}, {set, flux, Arg1});
set(SimId, What, Arg1) ->
    gen_server:call({global, {SimId, es_core_server}}, {set, What, Arg1}).

action(SimId, What) ->
    gen_server:call({global, {SimId, es_core_server}}, {action, What}).
action(SimId, What, Arg1) ->
    gen_server:call({global, {SimId, es_core_server}}, {action, What, Arg1}).


boron(SimId) ->
    es_core:get(SimId, boron).
pcms_from_full_power(SimId) ->
    es_core:get(SimId, pcms_from_full_power).
mtc(SimId) ->
    es_core:get(SimId, mtc).
tref_mismatch(SimId) ->
    es_core:get(SimId, tref_mismatch).
burnup(SimId) ->
    es_core:get(SimId, burnup).
flux(SimId) ->
    es_core:get(SimId, flux).
tavg(SimId) ->
    es_core:get(SimId, tavg).

borate(SimId, Ppm) ->
    Boron = boron(SimId),
    gen_server:call({global, {SimId, es_makeup_buffer_server}}, {action, borate, [Boron, Ppm]}).
%    action(borate, Ppm).
dilute(SimId, Ppm) ->
    Boron = boron(SimId),
    gen_server:call({global, {SimId, es_makeup_buffer_server}}, {action, dilute, [Boron, Ppm]}).
%    action(SimId, dilute, Ppm).

set_flux(SimId, Flux) ->
    set(SimId, flux, Flux).
