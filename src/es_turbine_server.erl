-module(es_turbine_server).
-include_lib("include/es_common.hrl").
-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(turbine_state, {simid, power, target, rate, go}).

start_link(SimId) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [SimId], []).

init([SimId]) -> {ok, #turbine_state{simid = SimId, power=100, target=100, rate=0, go=false}}.

handle_call({get, power}, _From, State) -> {reply, State#turbine_state.power, State};
handle_call({get, go}, _From, State) -> {reply, State#turbine_state.go, State};
handle_call({get, target}, _From, State) -> {reply, State#turbine_state.target, State};
handle_call({get, rate}, _From, State) -> {reply, State#turbine_state.rate, State};
handle_call({get, tref}, _From, State) -> %TODO: move constants to config file/ETS table
   Power = State#turbine_state.power,
   Noload_Tavg = 291.8,
   Fullpower_Tavg = 305.0,
   Tref = Noload_Tavg + (Fullpower_Tavg - Noload_Tavg) * (Power / 100),
   {reply, Tref, State};

handle_call({set, power, Power}, _From, State) ->
    New_state = State#turbine_state{power=Power},
    gen_server:call(es_core_server, {set, flux, Power}),
    {reply, New_state#turbine_state.power, New_state};
handle_call({set, target, Target}, _From, State) ->
    New_state = State#turbine_state{target=Target},
    {reply, New_state#turbine_state.target, New_state};
handle_call({set, rate, Rate}, _From, State) ->
    New_state = State#turbine_state{rate=Rate},
    {reply, New_state#turbine_state.rate, New_state};
handle_call({set, go, Go}, _From, State) ->
    New_state = State#turbine_state{go=Go},
    {reply, New_state#turbine_state.go, New_state};

handle_call({action, ramp, start}, _From, State) when State#turbine_state.go =:= false ->
    gen_server:call(es_ramper_server, {start_ramp, State#turbine_state.power, State#turbine_state.target, State#turbine_state.rate}),
    {reply, ok, State#turbine_state{go=true}};

handle_call({action, ramp, start}, _From, State) when State#turbine_state.go =:= true ->
    {reply, error_already_ramping, State};

handle_call({action, ramp, stop}, _Caller, State) ->
    New_state = State#turbine_state{go=false},
    {reply, ok, New_state};

%handle_call({ramp_start}, _From, State) when State#turbine_state.go =:= false ->
%    gen_server:call(es_ramper_server, {start_ramp, State#turbine_state.power, State#turbine_state.target, State#turbine_state.rate}),
%    {reply, ok, State#turbine_state{go=true}};

%handle_call({ramp_start}, _From, State) when State#turbine_state.go =:= true ->
%    {reply, error_already_ramping, State};

%handle_call({ramp_stop}, _Caller, State) ->
%    New_state = State#turbine_state{go=false},
%    {reply, ok, New_state};

handle_call(stop, _From, Tab) -> {stop, normal, stopped, Tab}.

%handle_call(_Request, _From, State) -> {reply, Reply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

