-module(es_rod_position).
-include_lib("include/es_common.hrl").
-compile(export_all).

start() ->
    rod_position_server:start_link().
stop(SimId) ->
    gen_server:call({global, {SimId, es_rod_position_server}}, stop).


get(SimId, What) ->
    gen_server:call({global, {SimId, es_rod_position_server}}, {get, What}).

get(SimId, What, Arg1) ->
    gen_server:call({global, {SimId, es_rod_position_server}}, {get, What, Arg1}).

action(SimId, What) ->
    gen_server:call({global, {SimId, es_rod_position_server}}, {action, What}).

%start() ->
%    es_comm:start(rods_port, "rod_position.py").

%stop() ->
%    es_comm:stop(rods_port).

position(SimId) -> es_rod_position:get(SimId, control_position).
position(SimId, Group) -> es_rod_position:get(SimId, control_position, Group).
step_in(SimId) -> action(SimId, step_in).
step_out(SimId) -> action(SimId, step_out).

integral_worth() ->
    es_rod_position:get(integral_worth).
