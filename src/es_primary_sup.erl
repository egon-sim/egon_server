%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Server starting and supervising primary subsystems of a
%%%      power plant model. Is started by es_simulator_sup.
%%% @end
%%%------------------------------------------------------------------
-module(es_primary_sup).

-behaviour(supervisor).
-define(SERVER(SimId), {global, {SimId, ?MODULE}}).

%% API
-export([
	start_link/1
	]).

%% Supervisor callbacks
-export([init/1]).


%%%==================================================================
%%% API
%%%==================================================================

%%-------------------------------------------------------------------
%% @doc Starts the supervisor.
%%
%% @spec start_link(SimId::integer()) -> Result
%% where
%%  Result = {ok,Pid} | ignore | {error,Error}
%%  Pid = pid()
%%  Error = {already_started,Pid}} | shutdown | term()
%% @end
%%-------------------------------------------------------------------
start_link(SimId) ->
   supervisor:start_link(?SERVER(SimId), ?MODULE, [SimId]).


%%%==================================================================
%%% supervisor callbacks
%%%==================================================================

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
