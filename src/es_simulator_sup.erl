-module(es_simulator_sup).
-include_lib("include/es_common.hrl").
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

start_link(SimId) ->
   io:format("Starting new simulator (Id: ~p).~n", [SimId]),
   supervisor:start_link({global, {SimId, ?MODULE}}, ?MODULE, [SimId]).

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
