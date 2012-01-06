%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Server starting and supervising TCP interfaces to outside
%%%      world. Is started by es_utility_sup.
%%% @end
%%%------------------------------------------------------------------
-module(es_interface_dispatcher).

-behaviour(supervisor).
-define(SERVER(SimId), {global, {SimId, ?MODULE}}).

%% API
-export([
	start_link/1,
	start_child/2,
	children/1
	]).


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
start_link(SimId) ->
    supervisor:start_link(?SERVER(SimId), ?MODULE, [SimId]).

%%-------------------------------------------------------------------
%% @doc Starts new interface server.
%%
%% @spec start_child(SimId, User) -> {ok, Port}
%% where
%%  Port = integer()
%% @end
%%-------------------------------------------------------------------
start_child(SimId, User) ->
    io:format("Starting interface server.~n"),
    {ok, Child} = supervisor:start_child(?SERVER(SimId), [User]),
    io:format("New interface server: ~p~n", [Child]),
    Port = es_interface_server:port(Child),
    io:format("Got port ~p.~n", [Port]),
    gen_server:cast(Child, {listen}),
    {ok, Port}.

%%-------------------------------------------------------------------
%% @doc Returns list of  simulator supervisor's children for given
%%      simulator ID (in fact calls supervisor:which_children/1).
%%
%% @spec children(SimId) -> [{Id,Child,Type,Modules}]
%% where
%%  Child = pid() | undefined
%%  Type = worker | supervisor
%%  Modules = [atom()] | dynamic
%% @end
%%-------------------------------------------------------------------
children(SimId) ->
    supervisor:which_children(?SERVER(SimId)).

%%%==================================================================
%%% supervisor callbacks
%%%==================================================================

init([SimId]) -> 
%    io:format("Starting interface dispatcher.~n"),
    Interface = {es_interface_server, {es_interface_server, start_link, [SimId]},
       temporary, 2000, worker, [es_interface_server]},

    Children = [Interface],
    Restart_strategy = {simple_one_for_one, 0, 1},
    {ok, {Restart_strategy, Children}}.
