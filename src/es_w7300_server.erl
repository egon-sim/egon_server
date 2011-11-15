-module(es_w7300_server).
-define(SERVER(SimId), {global, {SimId, ?MODULE}}).
-include_lib("eunit/include/eunit.hrl").
-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, params/0, tref/1]).
-record(w7300_state, {simid}).

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

tref(SimId) ->
    gen_server:call(?SERVER(SimId), {get, tref}).

init([SimId]) -> {ok, #w7300_state{simid = SimId}}.

handle_call({get, tref}, _From, State) ->
   SimId = State#w7300_state.simid,
   Power = gen_server:call({global, {SimId, es_turbine_server}}, {get, power}),
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

