-module(es_turbine).
-include_lib("include/es_common.hrl").
-import(es_turbine_server).
-compile(export_all).

-define(SERVER(SimId), {global, {SimId, es_turbine_server}}).

start(SimId) ->
    es_turbine_server:start_link(SimId).
stop(SimId) ->
    gen_server:call(?SERVER(SimId), stop).

