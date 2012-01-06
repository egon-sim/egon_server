%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Server starting and supervising simulator instances. Is
%%%      started by es_sup.
%%% @end
%%%------------------------------------------------------------------
-module(es_simulator_dispatcher).

-behaviour(supervisor).
-define(SERVER, ?MODULE).

%% API
-export([start_link/0, start_child/1, stop_child/1]).

%% Supervisor callbacks
-export([init/1]).


%%%==================================================================
%%% API
%%%==================================================================

%%-------------------------------------------------------------------
%% @doc Starts the supervisor.
%%
%% @spec start_link(SimId::integer()) -> Result
%% where
%%  Result = {ok,Pid} | ignore | {error,Error}
%%  Pid = pid()
%%  Error = {already_started,Pid}} | shutdown | term()
%% @end
%%-------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%-------------------------------------------------------------------
%% @doc Starts new simulator instance.
%%
%% @spec start_child(SimId) -> Result
%% where
%%  Result = {ok,Child} | {ok,Child,Info} | {error,Error}
%%  Child = pid() | undefined
%%  Info = term()
%%  Error = already_present | {already_started,Child} | term()
%% @end
%%-------------------------------------------------------------------
start_child(SimId) ->
    io:format("Starting child~n"),
    supervisor:start_child(?SERVER, [SimId]).

%%-------------------------------------------------------------------
%% @doc Stops a simulator instance.
%%
%% @spec stop_child(Pid) -> Result
%% where
%%  Pid = pid()
%%  Result = ok | {error,Error}
%%  Error = running | not_found | simple_one_for_one
%% @end
%%-------------------------------------------------------------------
stop_child(Pid) ->
    io:format("Stopping child~n"),
    supervisor:terminate_child(?SERVER, Pid),
    supervisor:delete_child(?SERVER, Pid).


%%%==================================================================
%%% supervisor callbacks
%%%==================================================================

init([]) -> 
    Simulator = {es_simulator_sup, {es_simulator_sup, start_link, []}, temporary, 2000, supervisor, [es_simulator_sup]},
    Children = [Simulator],
    Restart_strategy = {simple_one_for_one, 0, 1},
    {ok, {Restart_strategy, Children}}.
