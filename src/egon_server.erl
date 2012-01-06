%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2021 Nikola Skoric
%%% @doc Module providing basic API for whole egon_server
%%%      application. Does not implement any functionality, just
%%%      provides handy calls for some important actions.
%%% @end
%%%------------------------------------------------------------------
-module(egon_server).

% API
-export([
	start/0,
	stop/0,
	shutdown/0,
	restart/0,
	run/1,
	pause/1,
	new_sim/0,
	new_sim/1,
	new_sim/3,
	stop_sim/1,
	list_sims/0,
	sim_loaded/1	
	]).


%%%==================================================================
%%% API
%%%==================================================================

%%-------------------------------------------------------------------
%% @doc Starts the egon_server application.
%%
%% @spec start() -> ok | {error, Reason}
%% where
%%  Reason = term()
%% @end
%%-------------------------------------------------------------------
start() ->
    application:start(sasl),
    application:start(egon_server).

%%-------------------------------------------------------------------
%% @doc Stops the egon_server application.
%%
%% @spec stop() -> ok | {error, Reason}
%% where
%%  Reason = term()
%% @end
%%-------------------------------------------------------------------
stop() ->
    application:stop(egon_server).

%%-------------------------------------------------------------------
%% @doc Stops whole distribution (calls init:stop()).
%%
%% @spec shutdown() -> ok
%% @end
%%-------------------------------------------------------------------
shutdown() ->
    es_master_server:shutdown().

%%-------------------------------------------------------------------
%% @doc Restarts the egon_server application.
%%
%% @spec restart() -> ok | {error, Reason}
%% where
%%  Reason = term()
%% @end
%%-------------------------------------------------------------------
restart() ->
    stop(),
    start().

%%-------------------------------------------------------------------
%% @doc Starts clock server (effectively unfreazes the simulation).
%%
%% @spec run(SimId::integer()) -> ok
%% @end
%%-------------------------------------------------------------------
run(Sim) ->
    es_config_server:unfreaze(Sim).

%%-------------------------------------------------------------------
%% @doc Stops clock server (effectively freazes the simulation).
%%
%% @spec pause(SimId::integer()) -> ok
%% @end
%%-------------------------------------------------------------------
pause(Sim) ->
    es_config_server:freaze(Sim).

%%-------------------------------------------------------------------
%% @doc Alias for new_sim(doc).
%%
%% @spec new_sim() -> string()
%% @end
%%-------------------------------------------------------------------
new_sim() ->
    new_sim(doc).

%%-------------------------------------------------------------------
%% @doc Function documenting functionality of new_sim/3
%%
%% @spec new_sim(doc) -> string()
%% @end
%%-------------------------------------------------------------------
new_sim(doc) ->
    "new_sim(Name, Desc, User)".

%%-------------------------------------------------------------------
%% @doc Starts a new simulator instance.
%%
%% @spec new_sim(Name, Description, User) ->
%%       {ok, SimId} | {error, shutdown}
%% where
%%   Name = string(), % name of new simulator
%%   Description = string(), % short description of new simulator
%%   User = string(), % username of user starting the simulator
%%   SimId = integer(), % SimId of newly started simulator
%% @end
%%-------------------------------------------------------------------
new_sim(Name, Desc, User) ->
    es_simulator_tracker_server:start_new_simulator(Name, Desc, User).

%%-------------------------------------------------------------------
%% @doc Stops the simulator instance.
%%
%% @spec stop_sim(SimId::integer()) -> {ok, stopped}
%% @end
%%-------------------------------------------------------------------
stop_sim(SimId) ->
    es_simulator_tracker_server:stop_simulator(SimId).

%%-------------------------------------------------------------------
%% @doc Returns list of simulator instances on this server.
%%
%% @spec simulators() -> [SimMan]
%% where
%%  SimMan = #simulator_manifest % record named simulator_manifest
%%  and defined in es_simulator_tracker_server
%% @end
%%-------------------------------------------------------------------
list_sims() ->
    {ok, List} = es_simulator_tracker_server:simulators(),
    List.

%%-------------------------------------------------------------------
%% @doc Returns after all descendants of simulator supervisor are
%%	spawned. Waits until all children of simulator supervisor are
%%	spawned, and then waits another 100ms. In theory, this assures
%%	that all descendants of simulator supervisor are spawned. This
%%	function should be rewriten in a more sane way.
%%
%% @spec sim_loaded(SimId) -> true
%% @end
%%-------------------------------------------------------------------
sim_loaded(SimId) ->
    es_simulator_sup:children(SimId),
    timer:sleep(100),
    true.


%%%==================================================================
%%% Test functions
%%%==================================================================

-include_lib("eunit/include/eunit.hrl").

unit_test() ->
    ?assertEqual(ok, egon_server:start()),

    ?assertEqual({ok,1}, new_sim("Tester", "Test sim 1", "Simulator for purposes of unit testing")),

    ?assertEqual(ok, egon_server:run(1)),

    ?assertEqual({ok,stopped}, stop_sim(1)),

    ?assertEqual(ok, egon_server:stop()),
    ok.
