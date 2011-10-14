-module(es_primary_sup).
-include_lib("eunit/include/eunit.hrl").
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

start_link(SimId) ->
   supervisor:start_link({global, {SimId, ?MODULE}}, ?MODULE, [SimId]).

init([SimId]) ->
   Curvebook = {es_curvebook_server, {es_curvebook_server, start_link, [SimId]},
      temporary, 2000, worker, [es_curvebook_server]},
   
   W7300 = {es_w7300_server, {es_w7300_server, start_link, [SimId]},
      temporary, 2000, worker, [es_w7300_server]},
   
   Rod_Position = {es_rod_position_server, {es_rod_position_server, start_link, [SimId]},
      temporary, 2000, worker, [es_rod_position_server]},
   
   Core = {es_core_server, {es_core_server, start_link, [SimId]},
      temporary, 2000, worker, [es_core_server]},
   
   Flux_buffer = {es_flux_buffer_server, {es_flux_buffer_server, start_link, [SimId]},
      temporary, 2000, worker, [es_flux_buffer_server]},
   
   Makeup_buffer = {es_makeup_buffer_server, {es_makeup_buffer_server, start_link, [SimId]},
      temporary, 2000, worker, [es_makeup_buffer_server]},
   
   Rod_controller = {es_rod_controller_server, {es_rod_controller_server, start_link, [SimId]},
      temporary, 2000, worker, [es_rod_controller_server]},
   
   Children = [Curvebook, W7300, Rod_Position, Core, Makeup_buffer, Rod_controller, Flux_buffer],
   RestartStrategy = {one_for_one, 1, 2},
   {ok, {RestartStrategy, Children}}.
