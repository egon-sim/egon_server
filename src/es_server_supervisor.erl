-module(es_server_supervisor).
-include_lib("include/es_common.hrl").
-behaviour(supervisor).
-export([start_link/0, start_child/1, init/1]).
-define(SERVER, ?MODULE).
%-record(interface_state, {port, lsock, buffer}).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Connection) ->
%    io:format("Starting child~n"),
    supervisor:start_child(?SERVER, [Connection]).

init([]) -> 
    Simulator = {es_simulator_sup, {es_simulator_sup, start_link, []}, temporary, 2000, supervisor, [es_simulator_sup]},
    Children = [Simulator],
    Restart_strategy = {simple_one_for_one, 0, 1},
    {ok, {Restart_strategy, Children}}.
