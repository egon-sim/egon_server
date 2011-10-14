%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Clock server. Server sending ticks with given frequency.
%%% @end
%%%------------------------------------------------------------------
-module(es_clock_server).

-behaviour(gen_server).
-define(SERVER(SimId), {global, {SimId, ?MODULE}}).
-import(timer).

% API
-export([
	start_link/1,
	stop_link/1,
	add_listener/2
	]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% data structures
-record(clock_state, {
		     simid,
		     listeners,
		     timer,
		     cycle_len,
		     cycle_no,
		     status,
		     log_ticks
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

add_listener(SimId, Listener) ->
    gen_server:call(?SERVER(SimId), {add_listener, Listener}).


%%%==================================================================
%%% gen_server callbacks
%%%==================================================================

init([SimId]) -> 
    {ok, #clock_state{simid = SimId, listeners=[], timer=none, cycle_len=none, cycle_no=0, status=stopped, log_ticks=false}}.

handle_call({add_listener, Listener}, _From, State) ->
    error_logger:info_report(["Adding listener.", {listener, Listener}]),
    Old_listeners = State#clock_state.listeners,
    New_listeners = lists:umerge(lists:sort(Old_listeners), [Listener]),
    New_state = State#clock_state{listeners=New_listeners},
    {reply, ok, New_state};

handle_call({rem_listener, Listener}, _From, State) ->
    {reply, ok, rem_listener(Listener, State)};

handle_call({start_ticking}, _From, State) when State#clock_state.cycle_len =/= none ->
    Cycle_len = State#clock_state.cycle_len,
    SimId = State#clock_state.simid,
    {ok, Timer} = timer:apply_interval(Cycle_len, gen_server, call, [{global, {SimId, ?MODULE}}, {tick}]),
    timer:start(),
    {reply, ok, State#clock_state{timer=Timer, status=running}};

handle_call({start_ticking}, _From, State) when State#clock_state.cycle_len =:= none ->
    {reply, error_cycle_len_not_set, State};

handle_call({stop_ticking}, _From, State) ->
    timer:cancel(State#clock_state.timer),
    {reply, ok, State#clock_state{timer=none, status=stopped}};

handle_call({get, status}, _From, State) ->
    Val = State#clock_state.status,
    {reply, Val, State};

handle_call({get, listeners}, _From, State) ->
    Val = State#clock_state.listeners,
    {reply, Val, State};

handle_call({get, cycle_len}, _From, State) ->
    Val = State#clock_state.cycle_len,
    {reply, Val, State};

handle_call({set, cycle_len, _Cycle_len}, _From, State) when State#clock_state.status =:= running ->
    {reply, error_cannot_change_while_running, State};

handle_call({set, cycle_len, Cycle_len}, _From, State) when State#clock_state.status =:= stopped ->
    New_state = State#clock_state{cycle_len=Cycle_len},
    {reply, ok, New_state};

handle_call({get, seconds_to_ticks, Secs}, _From, State) ->
    Cycle_len = State#clock_state.cycle_len,
    Millisecs = Secs * 1000,
    Retval = Millisecs / Cycle_len,
    {reply, Retval, State};

handle_call({get, milliseconds_to_ticks, Millisecs}, _From, State) ->
    Cycle_len = State#clock_state.cycle_len,
    Retval = Millisecs / Cycle_len,
    {reply, Retval, State};

handle_call({get, log_ticks}, _From, State) ->
    Val = State#clock_state.log_ticks,
    {reply, Val, State};

handle_call({set, log_ticks, Value}, _From, State) ->
    New_state = State#clock_state{log_ticks=Value},
    {reply, ok, New_state};

handle_call({tick}, _From, State) when State#clock_state.status =:= running ->
    if
	State#clock_state.log_ticks ->
            io:format("Tick: ~w~n", [State#clock_state.cycle_no]);
    	true ->
            ok
    end,
    New_state = send_ticks(State),
    Cycle_no = New_state#clock_state.cycle_no,
    {reply, ok, New_state#clock_state{cycle_no=Cycle_no + 1}};

handle_call({tick}, _From, State) when State#clock_state.status =:= stopped ->
    if
	State#clock_state.log_ticks ->
            io:format("Tick: ~w~n", [State#clock_state.cycle_no]);
    	true ->
            ok
    end,
    {reply, ok, State};

handle_call({tick}, _From, State) ->
    io:format("corrupt counter"),
    {reply, error, State};

handle_call(stop, _From, State) ->
    timer:cancel(State#clock_state.timer),
    {stop, normal, stopped, State}.

%handle_call(_Request, _From, State) -> {reply, Reply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%==================================================================
%%% Internal functions
%%%==================================================================

send_ticks(State) ->
%    io:format("Listeners: ~w~n", [State#clock_state.listeners]),
    Listeners = State#clock_state.listeners,
    send_tick(Listeners, State).

send_tick([Listener | Rest], State) ->
%    io:format("Listeners: ~w~n", [State#clock_state.listeners]),
    Retval = gen_server:call(Listener, {tick}),
%    io:format("Retval: ~w~n", [Retval]),
    if
        Retval =:= rem_listener ->
	    New_state = rem_listener(Listener, State);
	true ->
	    New_state = State
    end,
    send_tick(Rest, New_state);

send_tick([], State) -> State.

rem_listener(Listener, State) ->
    Old_listeners = State#clock_state.listeners,
    New_listeners = lists:subtract(lists:sort(Old_listeners), [Listener]),
    State#clock_state{listeners=New_listeners}.


%%%==================================================================
%%% Test functions
%%%==================================================================
-include_lib("eunit/include/eunit.hrl").

unit_test() -> ok.

integration_test() -> ok.
