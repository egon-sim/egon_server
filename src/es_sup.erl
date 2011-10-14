-module(es_sup).
-include_lib("eunit/include/eunit.hrl").
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
   {ok, Port} = application:get_env(egon_server, port),
   supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

init([Port]) ->
   Connection = {es_connection_server, {es_connection_server, start_link, [Port]},
      permanent, 2000, worker, [es_connection_server]},
   
   Tracker = {es_simulator_tracker_server, {es_simulator_tracker_server, start_link, []},
      permanent, 2000, worker, [es_simulator_tracker_server]},
   
   Simulator_dispatcher = {es_simulator_dispatcher, {es_simulator_dispatcher, start_link, []},
      permanent, 2000, supervisor, [es_simulator_dispatcher]},
   
   
   Children = [Connection, Tracker, Simulator_dispatcher],
   RestartStrategy = {one_for_one, 1, 2},
   {ok, {RestartStrategy, Children}}.
