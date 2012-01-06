%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Top supervisor of the application. Starts and supervises
%%%      basic infrastructure for controlling simulator instances and
%%%      communicating with outside world. Is started by es_app.
%%% @end
%%%------------------------------------------------------------------
-module(es_sup).

-behaviour(supervisor).
-define(SERVER, ?MODULE).

%% API
-export([
	start_link/0
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
start_link() ->
   {ok, Port} = application:get_env(egon_server, port),
   supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).


%%%==================================================================
%%% supervisor callbacks
%%%==================================================================

init([Port]) ->
   Master = {es_master_server, {es_master_server, start_link, []},
      permanent, 2000, worker, [es_master_server]},
   
   Connection = {es_connection_server, {es_connection_server, start_link, [Port]},
      permanent, 2000, worker, [es_connection_server]},
   
   Tracker = {es_simulator_tracker_server, {es_simulator_tracker_server, start_link, []},
      permanent, 2000, worker, [es_simulator_tracker_server]},
   
   Simulator_dispatcher = {es_simulator_dispatcher, {es_simulator_dispatcher, start_link, []},
      permanent, 2000, supervisor, [es_simulator_dispatcher]},
   
   
   Children = [Master, Connection, Tracker, Simulator_dispatcher],
   RestartStrategy = {one_for_one, 1, 2},
   {ok, {RestartStrategy, Children}}.
