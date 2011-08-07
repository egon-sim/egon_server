-module(es_utility_sup).
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
   Config = {es_config_server, {es_config_server, start_link, [SimId]},
      permanent, 2000, worker, [es_config_server]},
   
   Clock = {es_clock_server, {es_clock_server, start_link, [SimId]},
      permanent, 2000, worker, [es_clock_server]},
   
   Interface_dispatcher = {es_interface_dispatcher, {es_interface_dispatcher, start_link, [SimId]},
      permanent, 2000, supervisor, [es_interface_dispatcher]},

   Logger = {es_log_server, {es_log_server, start_link, [SimId]},
      temporary, 2000, worker, [es_log_server]},
   
   Children = [Config, Clock, Interface_dispatcher, Logger],
   RestartStrategy = {one_for_one, 1, 2},
   {ok, {RestartStrategy, Children}}.
