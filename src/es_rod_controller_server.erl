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
			      manual_speed,
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
    Manual_speed = gen_server:call({global, {SimId, es_curvebook_server}}, {get, pls, [speed_of_rods_in_manual]}),
    {ok, #rod_controller_state{simid = SimId, mode=manual, speed=Manual_speed, manual_speed = Manual_speed, ticks_left = 0}}.

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
    {reply, ok, State#rod_controller_state{speed=rod_speed(State), mode=auto, ticks_per_second = Second_to_ticks}};

handle_call({tick}, _From, State) when State#rod_controller_state.mode =:= manual ->
    {reply, tick, State};

handle_call({tick}, _From, State) when State#rod_controller_state.mode =:= auto ->
    New_speed = rod_speed(State),

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
    New_state = State#rod_controller_state{speed=New_speed, ticks_left = New_ticks_left},
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
    Terr = calc_terr(State),
    Old_speed = State#rod_controller_state.speed,
    rod_speed(Terr, Old_speed).

rod_speed(Terr, Old_speed) ->
    Terr_F = es_convert:c2f_delta(Terr),
%    io:format("~w ~w ~w~n", [Terr_F, Terr, Old_speed]),
    if
        Terr_F < -5 ->
	    Speed = -72;
        Terr_F =< -3 ->
	    Speed = 32 * es_convert:c2f_delta(Terr) + 88;
        Terr_F =< -1.5 ->
	    Speed = -8;
        Terr_F =< -1 ->
	    if
	        Old_speed < 0 ->
		    Speed = -8;
		true ->
		    Speed = 0
	    end;
        Terr_F =< 1 ->
	    Speed = 0;
        Terr_F =< 1.5 ->
	    if
	        Old_speed > 0 ->
		    Speed = 8;
		true ->
		    Speed = 0
	    end;
        Terr_F =< 3 ->
	    Speed = 8;
        Terr_F =< 5 ->
	    Speed = 32 * es_convert:c2f_delta(Terr) - 88;
        Terr_F > 5 ->
	    Speed = 72
   end,
   Speed.

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

unit_test() -> 
    SimId = 1,
    {ok, _} = es_curvebook_server:start_link(SimId, "priv/curvebook/"),
    {ok, _} = es_clock_server:start_link(SimId),
    {ok, _} = es_rod_position_server:start_link(SimId),
    {ok, _} = start_link(SimId),

    ?assertEqual(manual, mode(SimId)),
    ?assertEqual(48, speed(SimId)),

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
    ?assertEqual(48, speed(SimId)),

    ?assertEqual(ok, es_rod_position_server:set_control_position_str(SimId, "D200")),

    ?assertEqual(ok, set_mode(SimId, auto)),

    ?assertEqual(auto, mode(SimId)),
    ?assertEqual(72, speed(SimId)),

    timer:sleep(1500),

    ?assertEqual(auto, mode(SimId)),
    ?assertEqual(72, speed(SimId)),
    ?assertEqual(585, es_rod_position_server:control_position_counter(SimId)),

    timer:sleep(4000),
    ?assertEqual(41.30, es_convert:round(speed(SimId), 2)),
    ?assertEqual(588, es_rod_position_server:control_position_counter(SimId)),

    ?assertEqual(ok, egon_server:stop()),
    ok end]}.
