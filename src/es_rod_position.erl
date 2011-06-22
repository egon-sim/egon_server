-module(es_rod_position).
-include_lib("include/es_common.hrl").
-export([start/0, stop/0, position/0, step_in/0, step_out/0, integral_worth/0]).
-compile(export_all).

start() ->
    rod_position_server:start_link().
stop() ->
    gen_server:call(es_rod_position_server, stop).


get(What) ->
    gen_server:call(es_rod_position_server, {get, What}).

get(What, Arg1) ->
    gen_server:call(es_rod_position_server, {get, What, Arg1}).

action(What) ->
    gen_server:call(es_rod_position_server, {action, What}).

%start() ->
%    es_comm:start(rods_port, "rod_position.py").

%stop() ->
%    es_comm:stop(rods_port).

position() -> es_rod_position:get(control_position).
position(Group) -> es_rod_position:get(control_position, Group).
step_in() -> action(step_in).
step_out() -> action(step_out).

integral_worth() ->
    es_rod_position:get(integral_worth).
