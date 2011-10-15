%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Server representing a model of control and shutdown rods
%%%      controlling system. It moves rods in and out of the core
%%%      depending on Tavg - Tref mismatch.
%%% @end
%%%------------------------------------------------------------------
-module(es_rod_controller_server).

-behaviour(gen_server).
-define(SERVER(SimId), {global, {SimId, ?MODULE}}).
-import(es_convert).

% API
-export([
	start_link/1,
	stop_link/1,
	speed/1,
	mode/1,
	set_mode/2
	]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% data structures
-record(rod_controller_state, {
			      simid,
			      mode,
			      speed,
			      in_lockup, % lo | no | hi
			      manual_speed,
			      dead_band,
			      lock_up,
			      ticks_per_second,
			      ticks_left
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

speed(SimId) ->
    gen_server:call(?SERVER(SimId), {get, speed}).

mode(SimId) ->
    gen_server:call(?SERVER(SimId), {get, mode}).

set_mode(SimId, Mode) ->
    gen_server:call(?SERVER(SimId), {set, mode, Mode}).


%%%==================================================================
%%% gen_server callbacks
%%%==================================================================

init([SimId]) -> 
    ok = es_clock_server:add_listener(SimId, ?SERVER(SimId)),
    Manual_speed = es_curvebook_server:pls(SimId, speed_of_rods_in_manual),
    Dead_band = es_curvebook_server:pls(SimId, rod_control_dead_band),
    Lock_up = es_curvebook_server:pls(SimId, rod_control_lock_up),
    {ok, #rod_controller_state{simid = SimId, mode=manual, speed=Manual_speed, in_lockup=no, manual_speed = Manual_speed, dead_band = Dead_band, lock_up = Lock_up, ticks_left = 0}}.

handle_call({get, speed}, _From, State) ->
    {reply, State#rod_controller_state.speed, State};

handle_call({get, mode}, _From, State) ->
    {reply, State#rod_controller_state.mode, State};
handle_call({set, mode, manual}, _From, State) ->
    Speed = State#rod_controller_state.manual_speed,
    {reply, ok, State#rod_controller_state{mode=manual, speed = Speed}};
handle_call({set, mode, auto}, _From, State) ->
    SimId = State#rod_controller_state.simid,
    Second_to_ticks = gen_server:call({global, {SimId, es_clock_server}}, {get, seconds_to_ticks, 1}),
    {In_lockup, Speed} = rod_speed(State),
    {reply, ok, State#rod_controller_state{speed=Speed, in_lockup=In_lockup, mode=auto, ticks_per_second = Second_to_ticks}};

handle_call({tick}, _From, State) when State#rod_controller_state.mode =:= manual ->
    {reply, tick, State};

handle_call({tick}, _From, State) when State#rod_controller_state.mode =:= auto ->
    {In_lockup, New_speed} = rod_speed(State),

    Ticks_left = State#rod_controller_state.ticks_left,
    New_ticks = ticks_to_step(State),

%    io:format("Terr:~p, New:_speed:~p, Ticks_left:~p, Old_ticks:~p~n", [Terr, New_speed, Ticks_left, New_ticks]),

    if
        Ticks_left =< 1 ->
	    step(State, New_speed),
	    New_ticks_left = New_ticks;
        Ticks_left >= New_ticks ->
	    New_ticks_left = New_ticks - 1;
        Ticks_left > 1 ->
	    New_ticks_left = Ticks_left - 1
    end,
    New_state = State#rod_controller_state{speed=New_speed, in_lockup=In_lockup, ticks_left = New_ticks_left},
    {reply, tick, New_state};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%==================================================================
%%% Internal functions
%%%==================================================================
rod_speed(State) ->
    SimId = State#rod_controller_state.simid,
    Dead_band = State#rod_controller_state.dead_band,
    Lock_up = State#rod_controller_state.lock_up,

    In_lockup = State#rod_controller_state.in_lockup,
    Terr = calc_terr(State),
    Old_speed = State#rod_controller_state.speed,

    New_in_lockup = in_lockup(Dead_band, Lock_up, In_lockup, Terr, Old_speed),

    {New_in_lockup, rod_speed(SimId, Terr, New_in_lockup)}.

in_lockup(Dead_band, Lock_up, hi, Terr, _) ->
    Upper_hi_limit = Dead_band,
    Lower_hi_limit = Dead_band - Lock_up,

    Terr_F = es_convert:c2f_delta(Terr),

    if 
        (Terr_F > Lower_hi_limit) and (Terr_F < Upper_hi_limit) ->
	    hi;
	true ->
	    no
    end;

in_lockup(Dead_band, Lock_up, lo, Terr, _) ->
    Upper_lo_limit = -Dead_band + Lock_up,
    Lower_lo_limit = -Dead_band,

    Terr_F = es_convert:c2f_delta(Terr),

    if 
        (Terr_F > Lower_lo_limit) and (Terr_F < Upper_lo_limit) ->
	    lo;
	true ->
	    no
    end;

in_lockup(Dead_band, Lock_up, no, Terr, Old_speed) ->
    Upper_hi_limit = Dead_band,
    Lower_hi_limit = Dead_band - Lock_up,
    Upper_lo_limit = -Dead_band + Lock_up,
    Lower_lo_limit = -Dead_band,

    Terr_F = es_convert:c2f_delta(Terr),

    if 
        Terr_F < Lower_lo_limit ->
	    no;
        Terr_F < Upper_lo_limit ->
	    if
	        Old_speed < 0 ->
		    lo;
		true ->
		    no
	    end;
        Terr_F < Lower_hi_limit ->
	    no;
        Terr_F < Upper_hi_limit ->
	    if
	        Old_speed > 0 ->
		    hi;
		true ->
		    no
	    end;
        Terr_F >= Upper_hi_limit ->
	    no
    end.

rod_speed(SimId, Terr, In_lockup) ->
    Terr_F = es_convert:c2f_delta(Terr),
%    io:format("~w ~w ~w~n", [Terr_F, Terr, In_lockup]),

    Speed = rod_control_program(SimId, Terr_F),

    if
        (In_lockup =:= lo) and (Terr < 0) ->
	    0.0;
        (In_lockup =:= hi) and (Terr > 0) ->
	    0.0;
	true ->
	    Speed
    end.

rod_control_program(SimId, Terr_F) ->
    es_curvebook_server:rod_control_speed_program(SimId, Terr_F).

step(State, Speed) ->
    SimId = State#rod_controller_state.simid,
    if
        Speed < 0 ->
    	    es_rod_position_server:step_in(SimId);
        Speed > 0 ->
	    es_rod_position_server:step_out(SimId);
	true ->
	    speed_is_zero
    end.

ticks_to_step(State) when State#rod_controller_state.speed == 0 ->
    0;
ticks_to_step(State) ->
    Speed = State#rod_controller_state.speed,
    Seconds_to_step = 1 / (abs(Speed) / 60),
    Ticks_per_second = State#rod_controller_state.ticks_per_second,
    Ticks_per_second * Seconds_to_step.

calc_terr(#rod_controller_state{simid = SimId}) ->
    Tavg = gen_server:call({global, {SimId, es_core_server}}, {get, tavg}),
    Tref = gen_server:call({global, {SimId, es_w7300_server}}, {get, tref}),
    Tref - Tavg.


%%%==================================================================
%%% Test functions
%%%==================================================================
-include_lib("eunit/include/eunit.hrl").

in_lockup_test() ->
    ?assertEqual(no, in_lockup(1.5, 0.5, no, -1, 0)),
    ?assertEqual(no, in_lockup(1.5, 0.5, no, -1, -8)),
    ?assertEqual(no, in_lockup(1.5, 0.5, no, -0.7, 0)),
    ?assertEqual(lo, in_lockup(1.5, 0.5, no, -0.7, -1)),
    ?assertEqual(no, in_lockup(1.5, 0.5, no, -0.5, 0)),
    ?assertEqual(no, in_lockup(1.5, 0.5, no, 0, 0)),
    ?assertEqual(no, in_lockup(1.5, 0.5, no, 0.5, 0)),
    ?assertEqual(no, in_lockup(1.5, 0.5, no, 0.7, 0)),
    ?assertEqual(hi, in_lockup(1.5, 0.5, no, 0.7, 1)),
    ?assertEqual(no, in_lockup(1.5, 0.5, no, 1, 0)),
    ?assertEqual(no, in_lockup(1.5, 0.5, no, 1, 8)),

    ?assertEqual(no, in_lockup(1.5, 0.5, lo, -1, 0)),
    ?assertEqual(no, in_lockup(1.5, 0.5, lo, -1, -8)),
    ?assertEqual(lo, in_lockup(1.5, 0.5, lo, -0.7, 0)),
    ?assertEqual(lo, in_lockup(1.5, 0.5, lo, -0.7, -1)),
    ?assertEqual(no, in_lockup(1.5, 0.5, lo, -0.5, 0)),
    ?assertEqual(no, in_lockup(1.5, 0.5, lo, 0, 0)),
    ?assertEqual(no, in_lockup(1.5, 0.5, lo, 0.5, 0)),
    ?assertEqual(no, in_lockup(1.5, 0.5, lo, 0.7, 0)),
    ?assertEqual(no, in_lockup(1.5, 0.5, lo, 1, 0)),
    ?assertEqual(no, in_lockup(1.5, 0.5, lo, 1, 8)),

    ?assertEqual(no, in_lockup(1.5, 0.5, hi, -1, 0)),
    ?assertEqual(no, in_lockup(1.5, 0.5, hi, -1, -8)),
    ?assertEqual(no, in_lockup(1.5, 0.5, hi, -0.7, 0)),
    ?assertEqual(no, in_lockup(1.5, 0.5, hi, -0.5, 0)),
    ?assertEqual(no, in_lockup(1.5, 0.5, hi, 0, 0)),
    ?assertEqual(no, in_lockup(1.5, 0.5, hi, 0.5, 0)),
    ?assertEqual(hi, in_lockup(1.5, 0.5, hi, 0.7, 0)),
    ?assertEqual(hi, in_lockup(1.5, 0.5, hi, 0.7, 1)),
    ?assertEqual(no, in_lockup(1.5, 0.5, hi, 1, 0)),
    ?assertEqual(no, in_lockup(1.5, 0.5, hi, 1, 8)),

    ok.

rod_speed_test() ->
    SimId = 1,
    {ok, _} = es_curvebook_server:start_link(SimId, "priv/curvebook/"),

    ?assertEqual(-72.0, rod_speed(SimId, -3, no)),
    ?assertEqual(-56.0, rod_speed(SimId, -2.5, no)),
    ?assertEqual(-27.2, es_convert:round(rod_speed(SimId, -2, no), 2)),
    ?assertEqual(-8.0, rod_speed(SimId, -1.5, no)),
    ?assertEqual(-8.0, rod_speed(SimId, -1, no)),
    ?assertEqual(-8.0, rod_speed(SimId, -0.7, no)),
    ?assertEqual(0.0, rod_speed(SimId, -0.7, lo)),
    ?assertEqual(-8.0, rod_speed(SimId, -0.7, hi)),
    ?assertEqual(0.0, rod_speed(SimId, -0.5, no)),
    ?assertEqual(0.0, rod_speed(SimId, 0, no)),
    ?assertEqual(0.0, rod_speed(SimId, 0.5, no)),
    ?assertEqual(8.0, rod_speed(SimId, 0.7, no)),
    ?assertEqual(8.0, rod_speed(SimId, 0.7, lo)),
    ?assertEqual(0.0, rod_speed(SimId, 0.7, hi)),
    ?assertEqual(8.0, rod_speed(SimId, 1, no)),
    ?assertEqual(27.2, es_convert:round(rod_speed(SimId, 2, no), 2)),
    ?assertEqual(56.0, rod_speed(SimId, 2.5, no)),
    ?assertEqual(72.0, rod_speed(SimId, 3, no)),

    ?assertEqual(stopped, es_curvebook_server:stop_link(SimId)),
    ok.

unit_test() -> 
    SimId = 1,
    {ok, _} = es_curvebook_server:start_link(SimId, "priv/curvebook/"),
    {ok, _} = es_clock_server:start_link(SimId),
    {ok, _} = es_rod_position_server:start_link(SimId),
    {ok, _} = start_link(SimId),

    ?assertEqual(manual, mode(SimId)),
    ?assertEqual(48.0, speed(SimId)),

    ?assertEqual(stopped, stop_link(SimId)),
    ?assertEqual(stopped, es_curvebook_server:stop_link(SimId)),
    ?assertEqual(stopped, es_rod_position_server:stop_link(SimId)),
    ?assertEqual(stopped, es_clock_server:stop_link(SimId)),

    ok.

integration_test_() -> {timeout, 10, [fun () ->
    ?assertEqual(ok, egon_server:start()),
    {ok, SimId} = egon_server:new_sim(["Test_server", "Simulator started by test function", "Tester"]),
    ?assertEqual(true, egon_server:sim_loaded(SimId)),

    ?assertEqual(ok, egon_server:run(SimId)),

    ?assertEqual(manual, mode(SimId)),
    ?assertEqual(48.0, speed(SimId)),

    ?assertEqual(ok, es_rod_position_server:set_control_position_str(SimId, "D200")),

    ?assertEqual(ok, set_mode(SimId, auto)),

    ?assertEqual(auto, mode(SimId)),
    ?assertEqual(72.0, speed(SimId)),

    timer:sleep(1500),

    ?assertEqual(auto, mode(SimId)),
    ?assertEqual(72.0, speed(SimId)),
    ?assertEqual(585, es_rod_position_server:control_position_counter(SimId)),

    timer:sleep(4000),
    ?assertEqual(41.30, es_convert:round(speed(SimId), 2)),
    ?assertEqual(588, es_rod_position_server:control_position_counter(SimId)),

    ?assertEqual(ok, egon_server:stop()),
    ok end]}.
