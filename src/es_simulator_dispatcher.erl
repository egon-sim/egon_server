-module(es_simulator_dispatcher).
-include_lib("include/es_common.hrl").
-behaviour(supervisor).
-export([start_link/0, start_child/1, stop_child/1, init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(SimId) ->
    io:format("Starting child~n"),
    supervisor:start_child(?SERVER, [SimId]).

stop_child(Pid) ->
    io:format("Stopping child~n"),
    supervisor:terminate_child(?SERVER, Pid),
    supervisor:delete_child(?SERVER, Pid).

init([]) -> 
    Simulator = {es_simulator_sup, {es_simulator_sup, start_link, []}, temporary, 2000, supervisor, [es_simulator_sup]},
    Children = [Simulator],
    Restart_strategy = {simple_one_for_one, 0, 1},
    {ok, {Restart_strategy, Children}}.
