-module(es_rod_controller_server).
-include_lib("include/es_common.hrl").
-behaviour(gen_server).
-import(es_convert).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(rod_controller_state, {mode, speed, manual_speed, ticks_per_second, ticks_left}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> 
    gen_server:call(es_clock_server, {add_listener, ?MODULE}),
    Manual_speed = gen_server:call(es_curvebook_server, {get, pls, [speed_of_rods_in_manual]}),
    {ok, #rod_controller_state{speed=0, manual_speed = Manual_speed, ticks_left = 0}}.

handle_call({get, speed}, _From, State) ->
    {reply, State#rod_controller_state.speed, State};

handle_call({get, mode}, _From, State) ->
    {reply, State#rod_controller_state.mode, State};
handle_call({set, mode, manual}, _From, State) ->
    Speed = State#rod_controller_state.manual_speed,
    {reply, ok, State#rod_controller_state{mode=manual, speed = Speed}};
handle_call({set, mode, auto}, _From, State) ->
    Second_to_ticks = gen_server:call(es_clock_server, {get, seconds_to_ticks, 1}),
    {reply, ok, State#rod_controller_state{mode=auto, ticks_per_second = Second_to_ticks}};

handle_call({tick}, _From, State) when State#rod_controller_state.mode =:= manual ->
    {reply, tick, State};

handle_call({tick}, _From, State) when State#rod_controller_state.mode =:= auto ->
    Terr = calc_terr(),

    Old_speed = State#rod_controller_state.speed,
    New_speed = rod_speed(Terr, Old_speed),

    Ticks_left = State#rod_controller_state.ticks_left,
    New_ticks = ticks_to_step(State),

%    io:format("~p, ~p, ~p, ~p~n", [Terr, New_speed, Ticks_left, New_ticks]),

    if
        Ticks_left =< 1 ->
	    step(New_speed),
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

step(Speed) ->
    if
        Speed < 0 ->
    	    gen_server:call(es_rod_position_server, {action, step_in});
        Speed > 0 ->
    	    gen_server:call(es_rod_position_server, {action, step_out});
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

calc_terr() ->
    Tavg = gen_server:call(es_core_server, {get, tavg}),
    Tref = gen_server:call(es_w7300_server, {get, tref}),
    Tref - Tavg.
