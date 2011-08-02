-module(es_rod_position_server).
-include_lib("include/es_common.hrl").
-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(rod_position_state, {simid, control_position_counter, control_group_position, control_rod_stops, overlap, shutdown_group_position, shutdown_position_counter}).

start_link(SimId) -> gen_server:start_link({global, {SimId, ?MODULE}}, ?MODULE, [SimId], []).

init([SimId]) -> 
    Control_rod_stops = gen_server:call({global, {SimId, es_curvebook_server}}, {get, pls, [control_rod_stops]}),
    Overlap = gen_server:call({global, {SimId, es_curvebook_server}}, {get, pls, [overlap]}),
    {ok, #rod_position_state{simid = SimId, control_rod_stops = Control_rod_stops, overlap = Overlap}}.

handle_call({get, control_position_counter}, _From, State) ->
    {reply, State#rod_position_state.control_position_counter, State};
handle_call({set, control_position_counter, Counter}, _From, State) ->
    Control_group_position = counter_to_position(Counter, State),
    {reply, ok, State#rod_position_state{control_position_counter = Counter, control_group_position = Control_group_position}};

handle_call({get, control_position}, _From, State) ->
    {reply, State#rod_position_state.control_group_position, State};
handle_call({get, control_position, Group}, _From, State) ->
    {reply, lists:nth(Group, State#rod_position_state.control_group_position), State};

handle_call({get, shutdown_position_counter}, _From, State) ->
    {reply, State#rod_position_state.shutdown_position_counter, State};
handle_call({set, shutdown_position_counter, Counter}, _From, State) ->
    SimId = State#rod_position_state.simid,
    No_of_groups = gen_server:call({global, {SimId, es_curvebook_server}}, {get, pls, [shutdown_rod_number]}),
    Rod_length = gen_server:call({global, {SimId, es_curvebook_server}}, {get, pls, [shutdown_rod_length]}),
    if
        Counter > Rod_length ->
	    Counter_1 = Rod_length;
        Counter < 0 ->
	    Counter_1 = 0;
	true ->
	    Counter_1 = Counter
    end,

    Shutdown_group_position = lists:duplicate(No_of_groups, Counter_1),
    {reply, ok, State#rod_position_state{shutdown_position_counter = Counter_1, shutdown_group_position = Shutdown_group_position}};

handle_call({get, shutdown_position}, _From, State) ->
    {reply, State#rod_position_state.shutdown_group_position, State};
handle_call({get, shutdown_position, Group}, _From, State) ->
    {reply, lists:nth(Group, State#rod_position_state.shutdown_group_position), State};

handle_call({action, step_in}, _From, State) ->
    Counter = State#rod_position_state.control_position_counter - 1,
    if
        Counter < 0 ->
	    {reply, ok, State};
	true ->
	    Control_group_position = counter_to_position(Counter, State),
	    {reply, ok, State#rod_position_state{control_position_counter = Counter, control_group_position = Control_group_position}}
    end;

handle_call({action, step_out}, _From, State) ->
    Counter = State#rod_position_state.control_position_counter + 1,
    Max_position = lists:last(State#rod_position_state.control_rod_stops),
    if
        Counter > Max_position ->
	    {reply, ok, State};
	true ->
	    Control_group_position = counter_to_position(Counter, State),
	    {reply, ok, State#rod_position_state{control_position_counter = Counter, control_group_position = Control_group_position}}
    end;

handle_call({get, integral_worth, [Burnup, _Flux]}, _From, State) ->
    Counter = State#rod_position_state.control_position_counter,
    SimId = State#rod_position_state.simid,
    Worth = gen_server:call({global, {SimId, es_curvebook_server}}, {get, rod_worth, [Burnup, Counter]}),
    {reply, Worth, State};

handle_call(stop, _From, Tab) ->
    {stop, normal, stopped, Tab}.

%handle_call(_Request, _From, State) -> {reply, Reply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%

counter_to_position(Counter, State) ->
    Control_rod_stops = State#rod_position_state.control_rod_stops,
    Overlap = State#rod_position_state.overlap,
    counter_to_position(Counter, Control_rod_stops, Overlap, Overlap).

counter_to_position(_, [], _, _) ->
    [];
counter_to_position(Counter, Control_rod_stops, Overlap, Last_head) ->
    if
        Counter - Last_head + Overlap =< 0 ->
            lists:map(fun(_) -> 0 end, Control_rod_stops);
        Counter =< hd(Control_rod_stops) ->
            [Counter - Last_head + Overlap|counter_to_position(Counter, tl(Control_rod_stops), Overlap, hd(Control_rod_stops))];
	true ->
	    [hd(Control_rod_stops) - Last_head + Overlap|counter_to_position(Counter, tl(Control_rod_stops), Overlap, hd(Control_rod_stops))]
    end.
