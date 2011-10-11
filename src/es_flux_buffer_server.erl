-module(es_flux_buffer_server).
-include_lib("include/es_common.hrl").
-behaviour(gen_server).
-import(timer).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(flux_buffer_state, {simid, target, cycle_len, flux_diff_per_cycle}).

config_list(State) -> [
{cycle_len, State#flux_buffer_state.cycle_len},
{flux_diff_per_cycle, State#flux_buffer_state.flux_diff_per_cycle}
].

status(State) ->
    status(config_list(State), State).
status([Head | Tail], State) ->
    {Key, Val} = Head,
    io:format("~w: ~w~n", [Key, Val]),
    status(Tail, State);
status([], _State) ->
    ok.    

start_link(SimId) -> gen_server:start_link({global, {SimId, ?MODULE}}, ?MODULE, [SimId], []).

init([SimId]) -> 
    gen_server:call({global, {SimId, es_clock_server}}, {add_listener, {global, {SimId, ?MODULE}}}),
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

handle_call({get, status}, _From, State) ->
    {reply, status(State), State};

handle_call({set, flux, Target}, _From, State) ->
%    error_logger:info_report(["FBS: Set", {flux, Target}]),
    {reply, Target, State#flux_buffer_state{target=Target}};

handle_call({tick}, _From, State) ->% when From =:= State#flux_buffer_state.timer ->
%    io:format("From: ~w, Timer: ~w~n", [From, State#flux_buffer_state.timer]),
    SimId = State#flux_buffer_state.simid,
    Flux = gen_server:call({global, {SimId, es_core_server}}, {get, flux}),
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
    	    gen_server:call({global, {SimId, es_core_server}}, {set, flux, New}),
	    {reply, tick, State}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

%handle_call(_Request, _From, State) -> {reply, Reply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

