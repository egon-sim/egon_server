-module(es_sup).
-include_lib("include/es_common.hrl").
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Connection) ->
   supervisor:start_link({local, ?SERVER}, ?MODULE, [Connection]).

init([Connection]) ->
   Curvebook = {es_curvebook_server, {es_curvebook_server, start_link, []},
      temporary, 2000, worker, [es_curvebook_server]},
   
   Config = {es_config_server, {es_config_server, start_link, []},
      permanent, 2000, worker, [es_config_server]},
   
   Clock = {es_clock_server, {es_clock_server, start_link, []},
      permanent, 2000, worker, [es_clock_server]},
   
   W7300 = {es_w7300_server, {es_w7300_server, start_link, []},
      temporary, 2000, worker, [es_w7300_server]},
   
   Rod_Position = {es_rod_position_server, {es_rod_position_server, start_link, []},
      temporary, 2000, worker, [es_rod_position_server]},
   
   Core = {es_core_server, {es_core_server, start_link, []},
      temporary, 2000, worker, [es_core_server]},
   
   Flux_buffer = {es_flux_buffer_server, {es_flux_buffer_server, start_link, []},
      temporary, 2000, worker, [es_flux_buffer_server]},
   
   Makeup_buffer = {es_makeup_buffer_server, {es_makeup_buffer_server, start_link, []},
      temporary, 2000, worker, [es_makeup_buffer_server]},
   
   Rod_controller = {es_rod_controller_server, {es_rod_controller_server, start_link, []},
      temporary, 2000, worker, [es_rod_controller_server]},
   
   Interface = {es_interface_server, {es_interface_server, start_link, [Connection]},
      permanent, 2000, worker, [es_interface_server]},
   
   Turbine = {es_turbine_server, {es_turbine_server, start_link, []},
      temporary, 2000, worker, [es_turbine_server]},
   
   Ramper = {es_ramper_server, {es_ramper_server, start_link, []},
      temporary, 2000, worker, [es_ramper_server]},
   
   Children = [Curvebook, Config, Clock, W7300, Rod_Position, Core, Makeup_buffer, Rod_controller, Flux_buffer, Interface, Turbine, Ramper],
   RestartStrategy = {one_for_one, 1, 2},
   {ok, {RestartStrategy, Children}}.
