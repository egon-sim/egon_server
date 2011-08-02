-module(es_makeup_buffer_server).
-include_lib("include/es_common.hrl").
-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(makeup_buffer_state, {simid, buffers, cycle_len, boron_diff_per_cycle}).

start_link(SimId) -> gen_server:start_link({global, {SimId, ?MODULE}}, ?MODULE, [SimId], []).

boron_diff(RCS, TNK, VADD, WADD, W) ->
    (TNK - RCS) * (1 - math:exp(-VADD*WADD/W)).

bor_dil(_SimId, []) ->
    [];
bor_dil(SimId, [{Action, Diff} | Rest]) ->
    io:format("~w ~n", [Diff]),    
    gen_server:call({global, {SimId, es_core_server}}, {action, Action, 1}),
    New_diff = Diff - 1,
    if
        New_diff > 0 ->
	    New_buffers = lists:append([{Action, New_diff}], bor_dil(SimId, Rest));
	true ->
	    New_buffers = bor_dil(SimId, Rest)
    end,
    New_buffers.
   
init([SimId]) -> 
    io:format("ProcName: ~p~n", [process_info(self(), registered_name)]),
    gen_server:call({global, {SimId, es_clock_server}}, {add_listener, {global, {SimId, ?MODULE}}}),
    {ok, #makeup_buffer_state{simid = SimId, buffers=[]}}.

handle_call({get, buffers}, _From, State) ->
    {reply, State#makeup_buffer_state.buffers, State};

handle_call({get, cycle_len}, _From, State) ->
    {reply, State#makeup_buffer_state.cycle_len, State};
handle_call({set, cycle_len, Val}, _From, State) ->
    {reply, ok, State#makeup_buffer_state{cycle_len=Val}};

handle_call({get, boron_diff_per_cycle}, _From, State) ->
    {reply, State#makeup_buffer_state.boron_diff_per_cycle, State};
handle_call({set, boron_diff_per_cycle, Val}, _From, State) ->
    {reply, ok, State#makeup_buffer_state{boron_diff_per_cycle=Val}};

handle_call({action, borate, [RCS, VADD]}, _From, State) ->
    TNK = 7000,
    WADD = 1.00663,
    W = 140933,
    Boron = boron_diff(RCS, TNK, VADD, WADD, W),
    New_buffers = lists:append(State#makeup_buffer_state.buffers, [{borate, Boron}]),
    {reply, Boron, State#makeup_buffer_state{buffers=New_buffers}};

handle_call({action, dilute, [RCS, VADD]}, _From, State) ->
    TNK = 0,
    WADD = 0.99813,
    W = 140933,
    Boron = -boron_diff(RCS, TNK, VADD, WADD, W),
    New_buffers = lists:append(State#makeup_buffer_state.buffers, [{dilute, Boron}]),
    {reply, Boron, State#makeup_buffer_state{buffers=New_buffers}};

handle_call({tick}, _From, State) ->
    Buffers = State#makeup_buffer_state.buffers,
    SimId = State#makeup_buffer_state.simid,
%    io:format("~w~n", [Buffers]),
    New_buffers = bor_dil(SimId, Buffers),

    if
%        New_buffers =:= [] ->
%	    {reply, rem_listener, State#makeup_buffer_state{buffers=New_buffers}};
	true ->
	    {reply, tick, State#makeup_buffer_state{buffers=New_buffers}}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

