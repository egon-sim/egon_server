-module(es_secondary_sup).
-include_lib("include/es_common.hrl").
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

start_link(SimId) ->
   supervisor:start_link({global, {SimId, ?MODULE}}, ?MODULE, [SimId]).

init([SimId]) ->
   Turbine = {es_turbine_server, {es_turbine_server, start_link, [SimId]},
      temporary, 2000, worker, [es_turbine_server]},
   
   Ramper = {es_ramper_server, {es_ramper_server, start_link, [SimId]},
      temporary, 2000, worker, [es_ramper_server]},
   
   Children = [Turbine, Ramper],
   RestartStrategy = {one_for_one, 1, 2},
   {ok, {RestartStrategy, Children}}.
