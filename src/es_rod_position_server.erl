-module(es_rod_position_server).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(rod_position_state, {position_counter, group_position, rod_stops, overlap}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> 
    Rod_stops = gen_server:call(es_curvebook_server, {get, pls, [rod_stops]}),
    Overlap = gen_server:call(es_curvebook_server, {get, pls, [overlap]}),
    {ok, #rod_position_state{rod_stops = Rod_stops, overlap = Overlap}}.

handle_call({get, position_counter}, _From, State) ->
    {reply, State#rod_position_state.position_counter, State};
handle_call({set, position_counter, Counter}, _From, State) ->
    Group_position = counter_to_position(Counter, State),
    {reply, ok, State#rod_position_state{position_counter = Counter, group_position = Group_position}};

handle_call({get, position}, _From, State) ->
    {reply, State#rod_position_state.group_position, State};
handle_call({get, position, Group}, _From, State) ->
    {reply, lists:nth(Group, State#rod_position_state.group_position), State};

handle_call({action, step_in}, _From, State) ->
    Counter = State#rod_position_state.position_counter - 1,
    if
        Counter < 0 ->
	    {reply, ok, State};
	true ->
	    Group_position = counter_to_position(Counter, State),
	    {reply, ok, State#rod_position_state{position_counter = Counter, group_position = Group_position}}
    end;

handle_call({action, step_out}, _From, State) ->
    Counter = State#rod_position_state.position_counter + 1,
    Max_position = lists:last(State#rod_position_state.rod_stops),
    if
        Counter > Max_position ->
	    {reply, ok, State};
	true ->
	    Group_position = counter_to_position(Counter, State),
	    {reply, ok, State#rod_position_state{position_counter = Counter, group_position = Group_position}}
    end;

handle_call({get, integral_worth, [Burnup, _Flux]}, _From, State) ->
    Counter = State#rod_position_state.position_counter,
    Worth = gen_server:call(es_curvebook_server, {get, rod_worth, [Burnup, Counter]}),
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
    Rod_stops = State#rod_position_state.rod_stops,
    Overlap = State#rod_position_state.overlap,
    counter_to_position(Counter, Rod_stops, Overlap, Overlap).

counter_to_position(_, [], _, _) ->
    [];
counter_to_position(Counter, Rod_stops, Overlap, Last_head) ->
    if
        Counter - Last_head + Overlap =< 0 ->
            lists:map(fun(_) -> 0 end, Rod_stops);
        Counter =< hd(Rod_stops) ->
            [Counter - Last_head + Overlap|counter_to_position(Counter, tl(Rod_stops), Overlap, hd(Rod_stops))];
	true ->
	    [hd(Rod_stops) - Last_head + Overlap|counter_to_position(Counter, tl(Rod_stops), Overlap, hd(Rod_stops))]
    end.
