%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Server for gradually changing flux of the core when step
%%%      change of turbine power occurs. Is started by
%%%      es_primary_sup.
%%% @end
%%%------------------------------------------------------------------
-module(es_flux_buffer_server).

-behaviour(gen_server).
-define(SERVER(SimId), {global, {SimId, ?MODULE}}).

% API
-export([
	start_link/1,
	stop_link/1,
	set_flux/2
	]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% data structures
-record(flux_buffer_state, {simid, target, cycle_len, flux_diff_per_cycle}).


%%%==================================================================
%%% API
%%%==================================================================

%%-------------------------------------------------------------------
%% @doc Starts the server.
%%
%% @spec start_link(SimId::integer()) -> {ok, Pid}
%% where
%%  Pid = pid()
%% @end
%%-------------------------------------------------------------------
start_link(SimId) ->
    gen_server:start_link(?SERVER(SimId), ?MODULE, [SimId], []).

%%-------------------------------------------------------------------
%% @doc Stops the server.
%%
%% @spec stop_link(SimId::integer()) -> stopped
%% @end
%%-------------------------------------------------------------------
stop_link(SimId) ->
    gen_server:call(?SERVER(SimId), stop).

%%-------------------------------------------------------------------
%% @doc Sets new target for flux_buffer_server.
%%
%% @spec set_flux(SimId::integer(), Power:float()) -> ok
%% @end
%%-------------------------------------------------------------------
set_flux(SimId, Power) ->
    gen_server:call(?SERVER(SimId), {set, flux, Power}).


%%%==================================================================
%%% gen_server callbacks
%%%==================================================================

init([SimId]) -> 
    es_clock_server:add_listener(SimId, ?SERVER(SimId)),
    {ok, #flux_buffer_state{simid = SimId, target=undef}}.

handle_call({get, cycle_len}, _From, State) ->
    {reply, State#flux_buffer_state.cycle_len, State};
handle_call({set, cycle_len, Val}, _From, State) ->
    {reply, ok, State#flux_buffer_state{cycle_len=Val}};

handle_call({get, flux_diff_per_cycle}, _From, State) ->
    {reply, State#flux_buffer_state.flux_diff_per_cycle, State};
handle_call({set, flux_diff_per_cycle, Val}, _From, State) ->
    {reply, ok, State#flux_buffer_state{flux_diff_per_cycle=Val}};

handle_call({get, state}, _From, State) ->
    {reply, State, State};

handle_call({set, flux, Target}, _From, State) ->
%    error_logger:info_report(["FBS: Set", {flux, Target}]),
    {reply, Target, State#flux_buffer_state{target=Target}};

handle_call({tick}, _From, State) ->% when From =:= State#flux_buffer_state.timer ->
%    io:format("From: ~w, Timer: ~w~n", [From, State#flux_buffer_state.timer]),
    SimId = State#flux_buffer_state.simid,
    Flux = es_core_server:flux(SimId),
    if
        State#flux_buffer_state.target =:= undef ->
	    Target = Flux;
	true ->
	    Target = State#flux_buffer_state.target
    end,
%    io:format("Flux: ~w, Target: ~w~n", [Flux, Target]),
%    error_logger:info_report(["FBS: Tick", {flux, Flux}, {target, Target}]),
%    io:format("Current: ~w~n", [Flux]),
%    io:format("Target: ~w~n", [Target]),
    if
        Flux > Target ->
            Direction = -1;
        true ->
            Direction = 1
    end,
    if
        Flux == Target ->
	    {reply, tick, State};
	true ->
	    New = Flux + Direction * State#flux_buffer_state.flux_diff_per_cycle,
%	    error_logger:info_report(["FBS: Enter", {flux, Flux}, {target, Target}]),
    	    es_core_server:set_flux(SimId, New),
	    {reply, tick, State}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

%handle_call(_Request, _From, State) -> {reply, Reply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%==================================================================
%%% Test functions
%%%==================================================================
-include_lib("eunit/include/eunit.hrl").

unit_test() -> ok.

integration_test() -> ok.

