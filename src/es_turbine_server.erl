%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Server implementing model of nuclear power plant main
%%%      turbine.
%%% @end
%%%------------------------------------------------------------------
-module(es_turbine_server).

-behaviour(gen_server).
-define(SERVER(SimId), {global, {SimId, ?MODULE}}).

% API
-export([
	start_link/1,
	stop_link/1
	]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% tests
-export([unit_test/0, integration_test/0]).

% data structures
-record(turbine_state, {
		       simid,
		       power,
		       target,
		       rate,
		       go
		       }).


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


%%%==================================================================
%%% gen_server callbacks
%%%==================================================================

init([SimId]) -> {ok, #turbine_state{simid = SimId, power=100, target=100, rate=0, go=false}}.

handle_call({get, power}, _From, State) -> 
    {reply, State#turbine_state.power, State};

handle_call({set, power, Power}, _From, State) ->
    New_state = State#turbine_state{power=Power},
    SimId = State#turbine_state.simid,
    gen_server:call({global, {SimId, es_core_server}}, {set, flux, Power}),
    {reply, New_state#turbine_state.power, New_state};

handle_call({get, go}, _From, State) ->
    {reply, State#turbine_state.go, State};

handle_call({set, go, Go}, _From, State) ->
    New_state = State#turbine_state{go=Go},
    {reply, New_state#turbine_state.go, New_state};

handle_call({get, target}, _From, State) ->
    {reply, State#turbine_state.target, State};

handle_call({set, target, Target}, _From, State) ->
    New_state = State#turbine_state{target=Target},
    {reply, New_state#turbine_state.target, New_state};

handle_call({get, rate}, _From, State) ->
    {reply, State#turbine_state.rate, State};

handle_call({set, rate, Rate}, _From, State) ->
    New_state = State#turbine_state{rate=Rate},
    {reply, New_state#turbine_state.rate, New_state};

handle_call({get, tref}, _From, State) -> 
   Power = State#turbine_state.power,
   SimId = State#turbine_state.simid,
   No_load_Tavg = gen_server:call({global, {SimId, es_curvebook_server}}, {get, pls, [no_load_tavg]}),
   Full_power_Tavg = gen_server:call({global, {SimId, es_curvebook_server}}, {get, pls, [full_power_tavg]}),
   Tref = No_load_Tavg + (Full_power_Tavg - No_load_Tavg) * (Power / 100),
   {reply, Tref, State};

handle_call({action, ramp, start}, _From, State) when State#turbine_state.go =:= false ->
    error_logger:info_report(["Starting turbine motion."]),
    SimId = State#turbine_state.simid,
    gen_server:call({global, {SimId, es_ramper_server}}, {start_ramp, State#turbine_state.power, State#turbine_state.target, State#turbine_state.rate}),
    {reply, ok, State#turbine_state{go=true}};

handle_call({action, ramp, start}, _From, State) when State#turbine_state.go =:= true ->
    error_logger:info_report(["Starting turbine motion failed.", {reason, already_ramping}]),
    {reply, error_already_ramping, State};

handle_call({action, ramp, stop}, _Caller, State) ->
    New_state = State#turbine_state{go=false},
    {reply, ok, New_state};

handle_call(stop, _From, Tab) -> {stop, normal, stopped, Tab}.

%handle_call(_Request, _From, State) -> {reply, Reply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%==================================================================
%%% Test functions
%%%==================================================================
-include_lib("include/es_common.hrl").

unit_test() ->
    SimId = 1,
    {ok, _} = start_link(SimId),

    stopped = stop_link(SimId),
    ok.

integration_test() ->
    ok = egon_server:start(),
    {ok, SimId} = egon_server:new_sim(["Test_server", "Simulator started by test function", "Tester"]),
    true = egon_server:sim_loaded(SimId),

    egon_server:stop(),
    ok.
