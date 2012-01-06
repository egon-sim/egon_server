%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Server logging various parameters of simulator and stores
%%%      them so they can be retreived and analyzed at later
%%%      time. Started by es_primary_sup.
%%% @end
%%%------------------------------------------------------------------
-module(es_w7300_server).

-behaviour(gen_server).
-define(SERVER(SimId), {global, {SimId, ?MODULE}}).

% API
-export([
	start_link/1,
	params/0,
	tref/1
	]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% data structures
-record(w7300_state, {simid}).


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
%% @doc Returns list of available parameters.
%%
%% @spec params() -> [Param]
%% where
%%  Param = {Parameter_id, Function_name}
%%  Parameter_id = term()
%%  Function_name = term()
%% @end
%%-------------------------------------------------------------------
params() -> [{tref, "Tref", tref}].

%%-------------------------------------------------------------------
%% @doc Returns Tref of the simulator model.
%%
%% @spec tref(SimId::integer()) -> Tref
%% where
%%  Tref = float()
%% @end
%%-------------------------------------------------------------------
tref(SimId) ->
    gen_server:call(?SERVER(SimId), {get, tref}).


%%%==================================================================
%%% gen_server callbacks
%%%==================================================================

init([SimId]) -> {ok, #w7300_state{simid = SimId}}.

handle_call({get, tref}, _From, State) ->
   SimId = State#w7300_state.simid,
   Power = es_turbine_server:power(SimId),
   Noload_Tavg = es_curvebook_server:pls(SimId, no_load_tavg),
   Fullpower_Tavg = es_curvebook_server:pls(SimId, full_power_tavg),
   Tref = Noload_Tavg + (Fullpower_Tavg - Noload_Tavg) * (Power / 100),
   {reply, Tref, State};

handle_call(stop, _From, Tab) -> {stop, normal, stopped, Tab}.

%handle_call(_Request, _From, State) -> {reply, Reply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

