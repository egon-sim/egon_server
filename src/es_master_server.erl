%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Server controlling the whole system. Is started by es_sup.
%%% @end
%%%------------------------------------------------------------------
-module(es_master_server).

-behaviour(gen_server).
-define(SERVER, {global, ?MODULE}).

% API
-export([
	start_link/0,
	stop_link/0,
	shutdown/0
	]).


% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(master_state, {
		      }).

%%%==================================================================
%%% API
%%%==================================================================

%%-------------------------------------------------------------------
%% @doc Starts the server.
%%
%% @spec start_link() -> {ok, Pid}
%% where
%%  Pid = pid()
%% @end
%%-------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?SERVER, ?MODULE, [], []).

%%-------------------------------------------------------------------
%% @doc Stops the server.
%%
%% @spec stop_link() -> stopped
%% @end
%%-------------------------------------------------------------------
stop_link() ->
    gen_server:call(?SERVER, stop).

%%-------------------------------------------------------------------
%% @doc Stops the whole system.
%%
%% @spec shutdown() -> ok
%% @end
%%-------------------------------------------------------------------
shutdown() ->
    gen_server:call(?SERVER, {shutdown}).


%%%==================================================================
%%% gen_server callbacks
%%%==================================================================

init([]) -> 
    {ok, #master_state{}}.

handle_call({shutdown}, _From, State) ->
    shutdown_system(),
    {reply, ok, State}.

%handle_call(_Request, _From, State) -> {reply, Reply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%==================================================================
%%% Internal functions
%%%==================================================================

shutdown_system() ->
    init:stop().

%%%==================================================================
%%% Test functions
%%%==================================================================
-include_lib("eunit/include/eunit.hrl").

unit_test() -> ok.