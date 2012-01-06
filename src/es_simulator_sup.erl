%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Server starting and supervising main subsystems of a
%%%      simulator. Started by es_simulator_dispatcher.
%%% @end
%%%------------------------------------------------------------------
-module(es_simulator_sup).

-behaviour(supervisor).
-define(SERVER(SimId), {global, {SimId, ?MODULE}}).

%% API
-export([
	 start_link/1,
	 children/1
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
   io:format("Starting new simulator (Id: ~p).~n", [SimId]),
   supervisor:start_link(?SERVER(SimId), ?MODULE, [SimId]).

%%-------------------------------------------------------------------
%% @doc Returns list of  simulator supervisor's children for given
%%      simulator ID (in fact calls supervisor:which_children/1).
%%
%% @spec children(SimId) -> [{Id,Child,Type,Modules}]
%% where
%%  Child = pid() | undefined
%%  Type = worker | supervisor
%%  Modules = [atom()] | dynamic
%% @end
%%-------------------------------------------------------------------
children(SimId) ->
    supervisor:which_children(?SERVER(SimId)).

%%%==================================================================
%%% supervisor callbacks
%%%==================================================================

init([SimId]) ->
   Utility_sup = {es_utility_sup, {es_utility_sup, start_link, [SimId]},
      permanent, 2000, supervisor, [es_utility_sup]},

   Primary_sup = {es_primary_sup, {es_primary_sup, start_link, [SimId]},
      permanent, 2000, supervisor, [es_primary_sup]},

   Secondary_sup = {es_secondary_sup, {es_secondary_sup, start_link, [SimId]},
      permanent, 2000, supervisor, [es_secondary_sup]},

   Children = [Utility_sup, Primary_sup, Secondary_sup],
   RestartStrategy = {one_for_one, 1, 2},
   {ok, {RestartStrategy, Children}}.
