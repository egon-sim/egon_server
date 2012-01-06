%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Server starting and supervising secondary subsystems of a
%%%      power plant model. Is started by es_simulator_sup.
%%% @end
%%%------------------------------------------------------------------
-module(es_secondary_sup).

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
   Turbine = {es_turbine_server, {es_turbine_server, start_link, [SimId]},
      temporary, 2000, worker, [es_turbine_server]},
   
   Ramper = {es_ramper_server, {es_ramper_server, start_link, [SimId]},
      temporary, 2000, worker, [es_ramper_server]},
   
   Children = [Turbine, Ramper],
   RestartStrategy = {one_for_one, 1, 2},
   {ok, {RestartStrategy, Children}}.
