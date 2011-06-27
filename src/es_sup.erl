-module(es_sup).
-include_lib("include/es_common.hrl").
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
   supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
   Connection = {es_connection_server, {es_connection_server, start_link, [1055]},
      permanent, 2000, worker, [es_connection_server]},
   
   Simulator_dispatcher = {es_simulator_dispatcher, {es_simulator_dispatcher, start_link, []},
      permanent, 2000, supervisor, [es_simulator_dispatcher]},
   
   
   Children = [Connection, Simulator_dispatcher],
   RestartStrategy = {one_for_one, 1, 2},
   {ok, {RestartStrategy, Children}}.
