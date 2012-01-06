%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Server implementing a model of reactor core. Is started by
%%%    	 es_primary_sup.
%%% @end
%%%------------------------------------------------------------------
-module(es_core_server).

-behaviour(gen_server).
-define(SERVER(SimId), {global, {SimId, ?MODULE}}).

% API
-export([
	params/0,
	start_link/1,
	tavg/1,
	flux/1,
	set_flux/2,
	burnup/1,
	boron/1,
	borate/2,
	dilute/2,
	stop_link/1
	]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% data structures
-record(core_state, {
                    simid, % ID of a simulator to which this server belongs
                    boron, % boron concentration in the core
                    burnup, % burnup of the core
                    flux % neutron flux in the core
                    }).

%%%==================================================================
%%% API
%%%==================================================================

%%-------------------------------------------------------------------
%% @doc Returns list of available parameters.
%%
%% @spec params() -> [Param]
%% where
%%  Param = {Parameter_id, Function_name}
%%  Parameter_id = term()
%%  Function_name = term()
%% @end
%%-------------------------------------------------------------------
params() -> [{tavg, "Average temperature of primary coolant", tavg}, {flux, "Neutron flux in reactor core", flux}, {burnup, "Burnup of reactor core", burnup}, {boron, "Concentration of boron in reactor core", boron}].

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
%% @doc Returns average temperature of reactor coolant.
%%
%% @spec tavg(SimId::integer()) -> float()
%% @end
%%-------------------------------------------------------------------
tavg(SimId) ->
    gen_server:call(?SERVER(SimId), {get, tavg}).

%%-------------------------------------------------------------------
%% @doc Returns neutron flux in reactor core.
%%
%% @spec flux(SimId::integer()) -> float()
%% @end
%%-------------------------------------------------------------------
flux(SimId) ->
    gen_server:call(?SERVER(SimId), {get, flux}).

%%-------------------------------------------------------------------
%% @doc Sets value of neutron flux in reactor core.
%%
%% @spec flux(SimId::integer(), Val::float()) -> ok
%% @end
%%-------------------------------------------------------------------
set_flux(SimId, Val) ->
    gen_server:call(?SERVER(SimId), {set, flux, Val}).

%%-------------------------------------------------------------------
%% @doc Returns burnup of reactor core.
%%
%% @spec burnup(SimId::integer()) -> float()
%% @end
%%-------------------------------------------------------------------
burnup(SimId) ->
    gen_server:call(?SERVER(SimId), {get, burnup}).

%%-------------------------------------------------------------------
%% @doc Returns concentration of boron in reactor coolant.
%%
%% @spec boron(SimId::integer()) -> float()
%% @end
%%-------------------------------------------------------------------
boron(SimId) ->
    gen_server:call(?SERVER(SimId), {get, boron}).

%%-------------------------------------------------------------------
%% @doc Initiates boration of RCS for given ammount of boron.
%%
%% @spec borate(SimId::integer(), Ppm::float()) -> ok
%% @end
%%-------------------------------------------------------------------
borate(SimId, Ppm) ->
    gen_server:call(?SERVER(SimId), {action, borate, Ppm}).

%%-------------------------------------------------------------------
%% @doc Initiates dilution of RCS for given ammount of boron.
%%
%% @spec dilute(SimId::integer(), Ppm::float()) -> ok
%% @end
%%-------------------------------------------------------------------
dilute(SimId, Ppm) ->
    gen_server:call(?SERVER(SimId), {action, dilute, Ppm}).


%%%==================================================================
%%% gen_server callbacks
%%%==================================================================

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

handle_call({set, flux, Flux}, _From, State) ->
    error_logger:info_report(["Core: Set", {flux, Flux}]),
    {reply, ok, State#core_state{flux=Flux}};

handle_call({get, tavg}, _From, State) ->
    SimId = State#core_state.simid,
    Tref = es_w7300_server:tref(SimId),
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


%%%==================================================================
%%% Internal functions
%%%==================================================================

pcms_from_full_power(State) ->
    Boron = State#core_state.boron,
    Burnup = State#core_state.burnup,
    Flux = State#core_state.flux,
    SimId = State#core_state.simid,
    Rod_worth = es_rod_position_server:integral_worth(SimId, Burnup, Flux),
    Power_defect_100 = es_curvebook_server:power_defect(SimId, Burnup, Boron, 100),
    Power_defect = es_curvebook_server:power_defect(SimId, Burnup, Boron, Flux),

    Boron_worth = es_curvebook_server:boron_worth(SimId, Burnup, Boron),
    Critical_boron = es_curvebook_server:critical_boron(SimId, Burnup),
    Boron_defect = (Boron - Critical_boron) * Boron_worth,

%    io:format("~w ~w ~w ~w ~w ~w~n", [Burnup, Flux, Rod_worth, Power_defect_100, Power_defect, Boron_defect]),

    Balance = Power_defect_100 - Power_defect + Rod_worth - Boron_defect,
    Balance.

mtc(State) ->
    Boron = State#core_state.boron,
    Burnup = State#core_state.burnup,
    Flux = State#core_state.flux,
    SimId = State#core_state.simid,
    es_curvebook_server:mtc(SimId, Burnup, Boron, Flux).

tref_mismatch(State) ->
    Pcms = pcms_from_full_power(State),
    Pcms / mtc(State).


%%%==================================================================
%%% Test functions
%%%==================================================================
-include_lib("eunit/include/eunit.hrl").

unit_test() ->
    ok.
