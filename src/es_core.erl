-module(es_core).
-import(core_server).
-compile(export_all).

start() ->
    core_server:start_link().
stop() ->
    gen_server:call(es_core_server, stop).


get(What) ->
    gen_server:call(es_core_server, {get, What}).
get(What, Arg1) ->
    gen_server:call(es_core_server, {get, What, Arg1}).

set(flux, Arg1) ->
    gen_server:call(es_core_server, {set_now, flux, Arg1});
set(What, Arg1) ->
    gen_server:call(es_core_server, {set, What, Arg1}).

action(What) ->
    gen_server:call(es_core_server, {action, What}).
action(What, Arg1) ->
    gen_server:call(es_core_server, {action, What, Arg1}).


boron() ->
    es_core:get(boron).
pcms_from_full_power() ->
    es_core:get(pcms_from_full_power).
mtc() ->
    es_core:get(mtc).
tref_mismatch() ->
    es_core:get(tref_mismatch).
burnup() ->
    es_core:get(burnup).
flux() ->
    es_core:get(flux).
tavg() ->
    es_core:get(tavg).

borate(Ppm) ->
    Boron = boron(),
    gen_server:call(es_makeup_buffer_server, {action, borate, [Boron, Ppm]}).
%    action(borate, Ppm).
dilute(Ppm) ->
    Boron = boron(),
    gen_server:call(es_makeup_buffer_server, {action, dilute, [Boron, Ppm]}).
%    action(dilute, Ppm).

set_flux(Flux) ->
    set(flux, Flux).
