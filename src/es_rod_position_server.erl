%%%-------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Server representing a model of contrl and shutdown rods
%%%      positioning system.
%%% @end
%%%-------------------------------------------------------------------
-module(es_rod_position_server).

-behaviour(gen_server).
-define(SERVER(SimId), {global, {SimId, ?MODULE}}).

% API
-export([
	 start_link/1,
	 stop_link/1,
	 control_position_counter/1,
	 set_control_position_counter/2,
	 control_position/1,
	 set_control_position/2,
	 control_position/2,
	 shutdown_position_counter/1,
	 set_shutdown_position_counter/2,
	 shutdown_position/1,
	 shutdown_position/2,
	 step_in/1,
	 step_out/1,
	 integral_worth/3
	]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% tests
-export([unit_test/0, integration_test/0]).

% data structures
-record(rod_position_state, {
	  simid, % ID of a simulator to which this server belongs
	  control_position_counter, % position counter of control rods
	  control_group_position, % list of positions of each control rod
	  control_rod_stops, % list of positions at which control rods stop
	  overlap, % overlap of control rods
	  shutdown_position_counter, % position counter of shutdown rods
	  shutdown_group_position % list of positions of each shutdown rod
}).

%%%===================================================================
%%% API
%%%===================================================================

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
%% @spec stop_link(SimId::integer()) -> ok
%% @end
%%-------------------------------------------------------------------
stop_link(SimId) ->
    gen_server:call(?SERVER(SimId), stop).

%%-------------------------------------------------------------------
%% @doc Returns value of control rod position counter.
%%
%% @spec control_position_counter(SimId::integer()) -> integer()
%% @end
%%-------------------------------------------------------------------
control_position_counter(SimId) ->
    gen_server:call(?SERVER(SimId), {get, control_position_counter}).

%%-------------------------------------------------------------------
%% @doc Sets value of control rod position counter.
%%
%% @spec set_control_position_counter(SimId::integer(), Val::integer()) -> ok
%% @end
%%-------------------------------------------------------------------
set_control_position_counter(SimId, Val) ->
    gen_server:call(?SERVER(SimId), {set, control_position_counter, Val}).

%%-------------------------------------------------------------------
%% @doc Returns value of control rod group positions as an array.
%%
%% @spec control_position(SimId::integer()) -> [integer()]
%% @end
%%-------------------------------------------------------------------
control_position(SimId) ->
    gen_server:call(?SERVER(SimId), {get, control_position}).

%%-------------------------------------------------------------------
%% @doc Sets value of control rod position as an array.
%%
%% @spec set_control_position(SimId::integer(), Val::[integer()]) -> ok
%% @end
%%-------------------------------------------------------------------
set_control_position(SimId, Val) ->
    gen_server:call(?SERVER(SimId), {set, control_position, Val}).

%%-------------------------------------------------------------------
%% @doc Returns value of control rod group position.
%%
%% @spec control_position(SimId::integer(), Group::integer()) -> integer()
%% @end
%%-------------------------------------------------------------------
control_position(SimId, Group) ->
    gen_server:call(?SERVER(SimId), {get, control_position, Group}).

%%-------------------------------------------------------------------
%% @doc Returns value of shutdown rod position counter.
%%
%% @spec shutdown_position_counter(SimId::integer()) -> integer()
%% @end
%%-------------------------------------------------------------------
shutdown_position_counter(SimId) ->
    gen_server:call(?SERVER(SimId), {get, shutdown_position_counter}).

%%-------------------------------------------------------------------
%% @doc Sets value of shutdown rod position counter.
%%
%% @spec set_shutdown_position_counter(SimId::integer(), Val::integer()) -> integer()
%% @end
%%-------------------------------------------------------------------
set_shutdown_position_counter(SimId, Val) ->
    gen_server:call(?SERVER(SimId), {set, shutdown_position_counter, Val}).

%%-------------------------------------------------------------------
%% @doc Returns value of shutdown rod group positions as an array.
%%
%% @spec shutdown_position(SimId::integer()) -> [integer()]
%% @end
%%-------------------------------------------------------------------
shutdown_position(SimId) ->
    gen_server:call(?SERVER(SimId), {get, shutdown_position}).

%%-------------------------------------------------------------------
%% @doc Returns value of shutdown rod group position.
%%
%% @spec shutdown_position(SimId::integer(), Group::integer()) -> integer()
%% @end
%%-------------------------------------------------------------------
shutdown_position(SimId, Group) ->
    gen_server:call(?SERVER(SimId), {set, shutdown_position, Group}).

%%-------------------------------------------------------------------
%% @doc Moves control rods one step into reactor core.
%%
%% @spec step_in(SimId::integer()) -> ok
%% @end
%%-------------------------------------------------------------------
step_in(SimId) ->
    gen_server:call(?SERVER(SimId), {action, step_in}).

%%-------------------------------------------------------------------
%% @doc Moves control rods one step out of reactor core.
%%
%% @spec step_out(SimId::integer()) -> ok
%% @end
%%-------------------------------------------------------------------
step_out(SimId) ->
    gen_server:call(?SERVER(SimId), {action, step_out}).

%%-------------------------------------------------------------------
%% @doc Returns integral worth of control rods inserted into reactor
%%      core.
%%
%% @spec integral_worth(SimId::integer(), Burnup::integer(), Flux::integer()) -> float()
%% @end
%%-------------------------------------------------------------------
integral_worth(SimId, Burnup, Flux) ->
    gen_server:call(?SERVER(SimId), {get, integral_worth, [Burnup, Flux]}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

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
handle_call({set, control_position, Val}, _From, State) ->
    NewCounter = position_to_counter(Val, State),
    Control_group_position = counter_to_position(NewCounter, State),
    {reply, ok, State#rod_position_state{control_position_counter = NewCounter, control_group_position = Control_group_position}};
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


%%%===================================================================
%%% Internal functions
%%%===================================================================

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

position_to_counter(Position, State) ->
    Control_rod_stops = State#rod_position_state.control_rod_stops,
    Overlap = State#rod_position_state.overlap,
    position_to_counter(Position, Control_rod_stops, 0, Overlap, hd("A")).

position_to_counter([Letter|Number], Control_rod_stops, Last_overlap, Overlap, Current_group) ->
    if
	Letter =:= Current_group ->
	    list_to_integer(Number);
	Letter > Current_group ->
	    Start_of_overlap = hd(Control_rod_stops) - Overlap,
	    Start_of_overlap - Last_overlap + position_to_counter([Letter|Number], tl(Control_rod_stops), Start_of_overlap, Overlap, Current_group + 1);
	true ->
	    error
    end.	

%%%===================================================================
%%% Test functions
%%%===================================================================
-include_lib("include/es_common.hrl").

unit_test() ->
    ok.

integration_test() ->
    ok = egon_server:start(),
    {ok, SimId} = egon_server:new_sim(["Test_server", "Simulator started by test function", "Tester"]),
    true = egon_server:sim_loaded(SimId),

    612 = control_position_counter(SimId),
    [228, 228, 228, 228] = control_position(SimId),
    228 = shutdown_position_counter(SimId),
    228 = shutdown_position(SimId),
    ok = set_control_position(SimId, "A50"),
    [50, 0, 0, 0] = control_position(SimId),
    ok = set_control_position(SimId, "B50"),
    [178, 50, 0, 0] = control_position(SimId),
    ok = set_control_position(SimId, "C50"),
    [228, 178, 50, 0] = control_position(SimId),
    ok = set_control_position(SimId, "D50"),
    [0, 228, 228, 178, 50] = [0|control_position(SimId)],

    ok = egon_server:stop(),
    ok.
