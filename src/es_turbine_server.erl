%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Server implementing model of nuclear power plant main
%%%      turbine. Is started by es_secondary_sup.
%%% @end
%%%------------------------------------------------------------------
-module(es_turbine_server).

-behaviour(gen_server).
-define(SERVER(SimId), {global, {SimId, ?MODULE}}).

% API
-export([
	 params/0,
	 start_link/1,
	 start_ramp/1,
	 start_ramp/3,
	 stop_ramp/1,
	 stop_link/1,
	 power/1,
	 set_power/2,
	 go/1,
	 set_go/2,
	 target/1,
	 set_target/2,
	 rate/1,
	 set_rate/2
	]).


% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

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
%% @doc Returns list of available parameters.
%%
%% @spec params() -> [Param]
%% where
%%  Param = {Parameter_id, Parameter_description, Function_name}
%%  Parameter_id = term()
%%  Function_name = term()
%% @end
%%-------------------------------------------------------------------
params() -> [{turbine_power, "Turbine power", power}, {go, "Turbine changing power", go}, {target, "Turbine target power", target}, {rate, "Rate of turbine power change", rate}].

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

start_ramp(SimId) ->
    gen_server:call(?SERVER(SimId), {action, ramp, start}).

start_ramp(SimId, Target, Rate) ->
    Go = go(SimId),
    case Go of
        false ->
	    ok = set_target(SimId, Target),
    	    ok = set_rate(SimId, Rate),
    	    ok = start_ramp(SimId),
	    ok;
	true ->
	    start_ramp(SimId) % returning error message
    end.

stop_ramp(SimId) ->
    gen_server:call(?SERVER(SimId), {action, ramp, stop}).

power(SimId) -> 
    gen_server:call(?SERVER(SimId), {get, power}).

set_power(SimId, Value) -> 
    gen_server:call(?SERVER(SimId), {set, power, Value}).

go(SimId) -> 
    gen_server:call(?SERVER(SimId), {get, go}).

set_go(SimId, Value) -> 
    gen_server:call(?SERVER(SimId), {set, go, Value}).

target(SimId) -> 
    gen_server:call(?SERVER(SimId), {get, target}).

set_target(SimId, Value) -> 
    gen_server:call(?SERVER(SimId), {set, target, Value}).

rate(SimId) -> 
    gen_server:call(?SERVER(SimId), {get, rate}).

set_rate(SimId, Value) -> 
    gen_server:call(?SERVER(SimId), {set, rate, Value}).

%%%==================================================================
%%% gen_server callbacks
%%%==================================================================

init([SimId]) -> {ok, #turbine_state{simid = SimId, power=100, target=100, rate=0, go=false}}.

handle_call({get, power}, _From, State) -> 
    {reply, State#turbine_state.power, State};

handle_call({set, power, Power}, _From, State) ->
    New_state = State#turbine_state{power=Power},
    SimId = State#turbine_state.simid,
    es_flux_buffer_server:set_flux(SimId, Power),
    {reply, ok, New_state};

handle_call({get, go}, _From, State) ->
    {reply, State#turbine_state.go, State};

handle_call({set, go, Go}, _From, State) ->
    New_state = State#turbine_state{go=Go},
    {reply, ok, New_state};

handle_call({get, target}, _From, State) ->
    {reply, State#turbine_state.target, State};

handle_call({set, target, Target}, _From, State) when State#turbine_state.go =:= false ->
    New_state = State#turbine_state{target=Target},
    {reply, ok, New_state};

handle_call({set, target, _Target}, _From, State) when State#turbine_state.go =:= true ->
    {reply, {error, cannot_change_target_while_ramping}, State};

handle_call({get, rate}, _From, State) ->
    {reply, State#turbine_state.rate, State};

handle_call({set, rate, Rate}, _From, State) when State#turbine_state.go =:= false ->
    New_state = State#turbine_state{rate=Rate},
    {reply, ok, New_state};

handle_call({set, rate, _Rate}, _From, State) when State#turbine_state.go =:= true ->
    {reply, {error, cannot_change_rate_while_ramping}, State};

handle_call({get, tref}, _From, State) -> 
    Power = State#turbine_state.power,
    SimId = State#turbine_state.simid,
    No_load_Tavg = es_curvebook_server:pls(SimId, no_load_tavg),
    Full_power_Tavg = es_curvebook_server:pls(SimId, full_power_tavg),
    Tref = No_load_Tavg + (Full_power_Tavg - No_load_Tavg) * (Power / 100),
    {reply, Tref, State};

handle_call({action, ramp, start}, _From, State) when State#turbine_state.go =:= false ->
    error_logger:info_report(["Starting turbine motion."]),
    SimId = State#turbine_state.simid,
    es_ramper_server:start_ramping(SimId, State#turbine_state.power, State#turbine_state.target, State#turbine_state.rate),
    {reply, ok, State#turbine_state{go=true}};

handle_call({action, ramp, start}, _From, State) when State#turbine_state.go =:= true ->
    error_logger:info_report(["Starting turbine motion failed.", {reason, already_ramping}]),
    {reply, {error, already_ramping}, State};

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
-include_lib("eunit/include/eunit.hrl").

unit_test() ->
    SimId = 1,
    {ok, _} = start_link(SimId),

    ?assertEqual(stopped, stop_link(SimId)),
    ok.


integration_test_() -> {timeout, 20, [fun () ->
        ?assertEqual(ok, egon_server:start()),
        {ok, SimId} = egon_server:new_sim("Test_server", "Simulator started by test function", "Tester"),
        ?assertEqual(true, egon_server:sim_loaded(SimId)),

        ?assertEqual(ok, egon_server:run(SimId)),

        ?assertEqual(100, power(SimId)),
        ?assertEqual(false, go(SimId)),
        ?assertEqual(80, target(SimId)),
        ?assertEqual(1, rate(SimId)),

        ?assertEqual(ok, start_ramp(SimId, 70, 5)),

        ?assertEqual(70, target(SimId)),
        ?assertEqual(5, rate(SimId)),

        ?assertEqual({error, already_ramping}, start_ramp(SimId, 51, 5)),
        timer:sleep(4000),
        ?assertEqual({error, already_ramping}, start_ramp(SimId, 52, 5)),

        ?assertEqual(true, go(SimId)),

        timer:sleep(4000),

        ?assertEqual(70, power(SimId)),
        ?assertEqual(false, go(SimId)),
        ?assertEqual(70, target(SimId)),
        ?assertEqual(5, rate(SimId)),

        ?assertEqual(ok, start_ramp(SimId, 85, 2)),
        ?assertEqual(85, target(SimId)),
        ?assertEqual(2, rate(SimId)),
        ?assertEqual({error, already_ramping}, start_ramp(SimId, 50, 5)),
        timer:sleep(4000),
        ?assertEqual({error, already_ramping}, start_ramp(SimId, 50, 5)),
        ?assertEqual(true, go(SimId)),
        timer:sleep(4000),

        ?assertEqual(85, power(SimId)),
        ?assertEqual(false, go(SimId)),
        ?assertEqual(85, target(SimId)),
        ?assertEqual(2, rate(SimId)),
        egon_server:stop(),
        ok end]}.
