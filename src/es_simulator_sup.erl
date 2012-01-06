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
