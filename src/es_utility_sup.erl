%%%-------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Supervisor starting various servers which support simulation
%%%      execution, but are not part of the model. Examples: clock
%%%      server, servers communicating with outside world, servers
%%%      logging simulator state, configuration server. Started by
%%%      es_simulator_sup.
%%% @end
%%%-------------------------------------------------------------------
-module(es_utility_sup).

-behaviour(supervisor).
-define(SERVER(SimId), {global, {SimId, ?MODULE}}).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

% tests
-export([unit_test/0, integration_test/0]).


%%%===================================================================
%%% API
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc Starts the server.
%%
%% @spec start_link(SimId::integer()) -> {ok, Pid}
%% where
%%  Pid = pid()
%% @end
%%-------------------------------------------------------------------
start_link(SimId) ->
    supervisor:start_link(?SERVER(SimId), ?MODULE, [SimId]).


%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

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


%%%===================================================================
%%% Test functions
%%%===================================================================
-include_lib("include/es_common.hrl").

unit_test() ->
    ok.

integration_test() ->
    ok = egon_server:start(),
    {ok, SimId} = egon_server:new_sim(["Test_server", "Simulator started by test function", "Tester"]),
    true = egon_server:sim_loaded(SimId),

    [
     {es_log_server, _, worker, [es_log_server]},
     {es_interface_dispatcher, _, supervisor, [es_interface_dispatcher]},
     {es_clock_server, _, worker, [es_clock_server]},
     {es_config_server, _, worker, [es_config_server]}
    ] = supervisor:which_children(?SERVER(SimId)),

    egon_server:stop(),
    ok.
