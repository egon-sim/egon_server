-module(es_rod_controller_server).
-behaviour(gen_server).
-import(es_convert).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(rod_controller_state, {mode, speed}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> 
    gen_server:call(es_clock_server, {add_listener, ?MODULE}),
    {ok, #rod_controller_state{speed=0}}.

handle_call({get, speed}, _From, State) ->
    {reply, State#rod_controller_state.speed, State};

handle_call({get, mode}, _From, State) ->
    {reply, State#rod_controller_state.mode, State};
handle_call({set, mode, manual}, _From, State) ->
    {reply, ok, State#rod_controller_state{mode=manual, speed=48}};
handle_call({set, mode, auto}, _From, State) ->
    {reply, ok, State#rod_controller_state{mode=auto}};

handle_call({tick}, _From, State) when State#rod_controller_state.mode =:= manual ->
    {reply, tick, State};

handle_call({tick}, _From, State) when State#rod_controller_state.mode =:= auto ->
    Tavg = gen_server:call(es_core_server, {get, tavg}),
    Tref = gen_server:call(es_w7300_server, {get, tref}),
    Terr = Tref - Tavg,
    Speed = State#rod_controller_state.speed,
    New_speed = rod_speed(Terr, Speed),
    {reply, tick, State#rod_controller_state{speed=New_speed}};

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
	    Speed = 72;
        Terr_F =< -3 ->
	    Speed = 32 * es_convert:c2f_delta(Terr) + 88;
        Terr_F =< -1.5 ->
	    Speed = 8;
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
