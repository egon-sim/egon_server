%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Server borates and dilutes RCS in controlled manner. Change
%%%      of boron concentration in RCS in never a step function, but
%%%      ramp. This module provides required ramp.
%%% @end
%%%------------------------------------------------------------------
-module(es_makeup_buffer_server).

-behaviour(gen_server).
-define(SERVER(SimId), {global, {SimId, ?MODULE}}).

% API
-export([
	start_link/1,
	stop_link/1,
	borate/2,
	dilute/2
	]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% tests
-export([integration_test/0]).

% data structures
-record(makeup_buffer_state, {
			     simid, % ID of a simulator to which this log server belongs
			     buffers, % list of all borations and dilutions currently in process
			     cycle_len, % number of miliseconds between alterations of boron concentration
			     boron_diff_per_cycle % ammount of boron added/removed in eash alteration
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

%%-------------------------------------------------------------------
%% @doc Initiates boration of RCS
%%
%% @spec borate(SimId::integer(), Volume::integer()) -> Boron_diff
%% where
%%  Volume = integer() % volume of boron to add
%%  Boron_diff = integer() % change in boron concentration that will be
%%                           achieved once boration is done
%% @end
%%-------------------------------------------------------------------
borate(SimId, Volume) ->
    gen_server:call(?SERVER(SimId), {action, borate, [none, Volume]}).

%%-------------------------------------------------------------------
%% @doc Initiates dilution of RCS
%%
%% @spec dilute(SimId::integer(), Volume::integer()) -> Boron_diff
%% where
%%  Volume = integer() % volume of water to add
%%  Boron_diff = integer() % change in boron concentration that will be
%%                           achieved once dilution is done
%% @end
%%-------------------------------------------------------------------
dilute(SimId, Volume) ->
    gen_server:call(?SERVER(SimId), {action, dilute, [none, Volume]}).


%%%==================================================================
%%% gen_server callbacks
%%%==================================================================

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

handle_call({action, borate, [_RCS, VADD]}, _From, State) ->
    SimId = State#makeup_buffer_state.simid,
    TNK = 7000,
    WADD = 1.00663,
    W = 140933,
    RCS = gen_server:call({global, {SimId, es_core_server}}, {get, boron}),
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


%%%==================================================================
%%% Internal functions
%%%==================================================================

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

   
%%%==================================================================
%%% Test functions
%%%==================================================================

-include_lib("include/es_common.hrl").

integration_test() ->
    ?assertEqual(ok, egon_server:start()),
    {ok, SimId} = egon_server:new_sim(["Test_server", "Simulator started by test function", "Tester"]),
    ?assertEqual(true, egon_server:sim_loaded(SimId)),

    ?assertEqual(ok, egon_server:run(SimId)),

    ?assertEqual(1515, gen_server:call({global, {SimId, es_core_server}}, {get, boron})),

    borate(SimId, 200),

    timer:sleep(2500),
    ?assertEqual(1517, gen_server:call({global, {SimId, es_core_server}}, {get, boron})),

    timer:sleep(2000),
    ?assertEqual(1519, gen_server:call({global, {SimId, es_core_server}}, {get, boron})),

    ?assertEqual(ok, egon_server:stop()),
    ok.
