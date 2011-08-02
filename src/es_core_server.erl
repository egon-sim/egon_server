-module(es_core_server).
-include_lib("include/es_common.hrl").
-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(core_state, {simid, boron, burnup, flux}).

start_link(SimId) -> gen_server:start_link({global, {SimId, ?MODULE}}, ?MODULE, [SimId], []).

init([SimId]) -> 
    {ok, #core_state{simid = SimId}}.

handle_call({get, boron}, _From, State) ->
    {reply, State#core_state.boron, State};
handle_call({set, boron, Val}, _From, State) ->
    {reply, ok, State#core_state{boron=Val}};

handle_call({get, pcms_from_full_power}, _From, State) ->
    Pcms = pcms_from_full_power(State),
    {reply, Pcms, State};

handle_call({get, tref_mismatch}, _From, State) ->
    {reply, tref_mismatch(State), State};

handle_call({get, mtc}, _From, State) ->
    {reply, mtc(State), State};

handle_call({get, burnup}, _From, State) ->
    {reply, State#core_state.burnup, State};
handle_call({set, burnup, Val}, _From, State) ->
    {reply, ok, State#core_state{burnup=Val}};

handle_call({get, flux}, _From, State) ->
    {reply, State#core_state.flux, State};

handle_call({set, flux_now, Flux}, {_From, _}, State) ->
%    error_logger:info_report(["Core: Set_now", {flux, Flux}]),
    {reply, ok, State#core_state{flux=Flux}};

handle_call({set_now, flux, Flux}, {_From, _}, State) ->
%    error_logger:info_report(["Core: Set_now", {flux, Flux}]),
    {reply, ok, State#core_state{flux=Flux}};

handle_call({set, flux, Flux}, {From, _}, #core_state{simid=SimId} = State) when From =:= {SimId, flux_buffer_server} ->
%    error_logger:info_report(["Core: Set from flux_buffer", {flux, Flux}]),
    {reply, ok, State#core_state{flux=Flux}};

handle_call({set, flux, Flux}, _From, State) ->
%    error_logger:info_report(["Core: Set", {flux, Flux}]),
    SimId = State#core_state.simid,
    {reply, gen_server:call({global, {SimId, es_flux_buffer_server}}, {set, flux, Flux}), State};

handle_call({get, tavg}, _From, State) ->
    SimId = State#core_state.simid,
    Tref = gen_server:call({global, {SimId, es_w7300_server}}, {get, tref}),
    Tavg = Tref + tref_mismatch(State),
    {reply, Tavg, State};

handle_call({action, borate, Ppm}, _From, State) ->
    Boron = State#core_state.boron + Ppm,
    {reply, ok, State#core_state{boron=Boron}};

handle_call({action, dilute, Ppm}, _From, State) ->
    if
        State#core_state.boron < Ppm ->
	    Boron = 0;
	true ->
            Boron = State#core_state.boron - Ppm
    end,
    {reply, ok, State#core_state{boron = Boron}};

handle_call({get, state}, _From, State) ->
    {reply, State, State};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

%handle_call(_Request, _From, State) -> {reply, Reply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

pcms_from_full_power(State) ->
    Boron = State#core_state.boron,
    Burnup = State#core_state.burnup,
    Flux = State#core_state.flux,
    SimId = State#core_state.simid,
    Rod_worth = gen_server:call({global, {SimId, es_rod_position_server}}, {get, integral_worth, [Burnup, Flux]}),
    Power_defect_100 = gen_server:call({global, {SimId, es_curvebook_server}}, {get, power_defect, [Burnup, Boron, 100]}),
    Power_defect = gen_server:call({global, {SimId, es_curvebook_server}}, {get, power_defect, [Burnup, Boron, Flux]}),

    Boron_worth = gen_server:call({global, {SimId, es_curvebook_server}}, {get, boron_worth, [Burnup, Boron]}),
    Critical_boron = gen_server:call({global, {SimId, es_curvebook_server}}, {get, critical_boron, [Burnup]}),
    Boron_defect = (Boron - Critical_boron) * Boron_worth,

%    io:format("~w ~w ~w ~w ~w ~w~n", [Burnup, Flux, Rod_worth, Power_defect_100, Power_defect, Boron_defect]),

    Balance = Power_defect_100 - Power_defect + Rod_worth - Boron_defect,
    Balance.

mtc(State) ->
    Boron = State#core_state.boron,
    Burnup = State#core_state.burnup,
    Flux = State#core_state.flux,
    SimId = State#core_state.simid,
    gen_server:call({global, {SimId, es_curvebook_server}}, {get, mtc, [Burnup, Boron, Flux]}).

tref_mismatch(State) ->
    Pcms = pcms_from_full_power(State),
    Pcms / mtc(State).
