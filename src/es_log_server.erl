%%%-------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Server logging various parameters of simulator and stores
%%%      them so they can be retreived and analyzed at later time.
%%% @end
%%%-------------------------------------------------------------------
-module(es_log_server).

-behaviour(gen_server).
-define(SERVER(SimId), {global, {SimId, ?MODULE}}).

% API
-export([
	start_link/1,
	stop_link/1,
	cycle_len/1,
	set_cycle_len/2,
	parameters/1,
	clear_parameters/1,
	add_parameter/2,
	add_parameters/2,
	status/1,
	database/1,
	csv_dump/1,
	start_logging/1,
	stop_logging/1
	]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% tests
-export([unit_test/0, integration_test/0]).

% data structures
-record(log_state, {
		   simid, % ID of a simulator to which this log server belongs
		   timer, % reference of timer which sends ticks to log server
		   status, % status of log server = running | stopped
		   cycle_len, % number of miliseconds between collecting data
		   parameters, % list of paramteres which to take when
		   	       % collecting data
		   database % list of collected data
		   }).
-record(log_parameter, {
		       name, % string(): name of paramtere
		       mfa, % {Module, Function, Arguments} defines a
		           % call to retreive value of the parameter
		       value % value of given parameter
		       }).
-record(log_entry, {
		   timestamp, % value of erlang:now() at the moment
		              % when value of the parameter is taken
		   parameters % [log_parameter()]
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
%% @doc Returns time between two snapshots in miliseconds.
%%
%% @spec cycle_len(SimId::integer()) -> Cycle_len
%% where
%%  Cycle_len = integer()
%% @end
%%-------------------------------------------------------------------
cycle_len(SimId) ->
    gen_server:call(?SERVER(SimId), {get, cycle_len}).

%%-------------------------------------------------------------------
%% @doc Sets time between two snapshots in miliseconds.
%%
%% @spec set_cycle_len(SimId::integer(), Val::integer()) -> ok
%% @end
%%-------------------------------------------------------------------
set_cycle_len(SimId, Val) ->
    gen_server:call(?SERVER(SimId), {set, cycle_len, Val}).

%%-------------------------------------------------------------------
%% @doc Get list of parameters which logger logs.
%%
%% @spec parameters(SimId::integer()) -> Parameters
%% where
%%  Parameters = [log_parameter()]
%% @end
%%-------------------------------------------------------------------
parameters(SimId) ->
    gen_server:call(?SERVER(SimId), {get, parameters}).

%%-------------------------------------------------------------------
%% @doc Add list of parameters which logger logs.
%%
%% @spec add_parameters(SimId::integer(), Parameters::list()) -> ok
%% where
%%  Parameters = [log_parameter()]
%% @end
%%-------------------------------------------------------------------
add_parameters(_SimId, []) ->
    ok;
add_parameters(SimId, [Head|Rest]) ->
    add_parameter(SimId, Head),
    add_parameters(SimId, Rest).

%%-------------------------------------------------------------------
%% @doc Adds a parameter to list of parameters which logger logs.
%%
%% @spec add_parameter(SimId::integer(), Parameter::log_parameter()) -> ok
%% @end
%%-------------------------------------------------------------------
add_parameter(SimId, Parameter) ->
    gen_server:call(?SERVER(SimId), {action, add_parameter, Parameter}).

%%-------------------------------------------------------------------
%% @doc Clear list of parameters which logger logs.
%%
%% @spec clear_parameters(SimId::integer()) -> ok
%% @end
%%-------------------------------------------------------------------
clear_parameters(SimId) ->
    gen_server:call(?SERVER(SimId), {set, parameters, []}).

%%-------------------------------------------------------------------
%% @doc Returns information whether logger server is running or stopped
%%
%% @spec status(SimId::integer()) -> Status
%% where
%%  Status = running | stopped
%% @end
%%-------------------------------------------------------------------
status(SimId) ->
    gen_server:call(?SERVER(SimId), {get, status}).

%%-------------------------------------------------------------------
%% @doc Returns all information collected up until now
%%
%% @spec database(SimId::integer()) -> Database
%% where
%%  Database = [log_entry()]
%% @end
%%-------------------------------------------------------------------
database(SimId) ->
    gen_server:call(?SERVER(SimId), {get, database}).

%%-------------------------------------------------------------------
%% @doc Returns all information collected up until now given in CSV
%%      (comma separated value) form
%%
%% @spec csv_dump(SimId::integer()) -> string()
%% @end
%%-------------------------------------------------------------------
csv_dump(SimId) ->
    gen_server:call(?SERVER(SimId), {get, csv_dump}).

%%-------------------------------------------------------------------
%% @doc Starts logging parameters
%%
%% @spec start_logging(SimId::integer()) -> ok | {error, ErrMessage}
%% where
%%  ErrMessage = {not_set, cycle_len} | {not_set, parameters}
%% @end
%%-------------------------------------------------------------------
start_logging(SimId) ->
    gen_server:call(?SERVER(SimId), {action, start}).

%%-------------------------------------------------------------------
%% @doc Stops logging parameters
%%
%% @spec stop_logging(SimId::integer()) -> ok
%% @end
%%-------------------------------------------------------------------
stop_logging(SimId) ->
    gen_server:call(?SERVER(SimId), {action, stop}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([SimId]) -> 
    {ok, #log_state{simid = SimId, timer=none, status=stopped, cycle_len=none, parameters=[], database=[]}}.

handle_call({get, cycle_len}, _From, State) ->
    {reply, State#log_state.cycle_len, State};

handle_call({set, cycle_len, Val}, _From, State) when State#log_state.status =:= stopped ->
    {reply, ok, State#log_state{cycle_len=Val}};

handle_call({set, cycle_len, Val}, _From, State) when State#log_state.status =:= running ->
    SimId = State#log_state.simid,
    Old_timer = State#log_state.timer,

    timer:cancel(Old_timer),
    {ok, New_timer} = timer:apply_interval(Val, gen_server, call, [{global, {SimId, ?MODULE}}, {tick}]),
    timer:start(),

    {reply, ok, State#log_state{timer=New_timer, cycle_len=Val}};

handle_call({get, parameters}, _From, State) ->
    {reply, State#log_state.parameters, State};

handle_call({set, parameters, Val}, _From, State) ->
    {reply, ok, State#log_state{parameters=Val}};

handle_call({action, add_parameter, {Name, {Module, Function, Arguments}}}, _From, State) ->
    New_param = #log_parameter{name = Name, mfa = {Module, Function, Arguments}},
    Parameters = State#log_state.parameters,
    {reply, ok, State#log_state{parameters=[New_param|Parameters]}};

handle_call({get, status}, _From, State) ->
    {reply, State#log_state.status, State};

handle_call({get, database}, _From, State) ->
    {reply, lists:reverse(State#log_state.database), State};

handle_call({get, csv_dump}, _From, State) ->
    Database = lists:reverse(State#log_state.database),
    Dump = print_csv_dump(Database),
    {reply, Dump, State};

handle_call({action, start}, _From, State) ->
    SimId = State#log_state.simid,
    Cycle_len = State#log_state.cycle_len,
    if
        Cycle_len =:= none ->
	    {reply, {error, {not_set, cycle_len}}, State};
        State#log_state.parameters =:= [] ->
	    {reply, {error, {not_set, parameters}}, State};
        true ->
	    Cycle_len = State#log_state.cycle_len,
	    {ok, Timer} = timer:apply_interval(Cycle_len, gen_server, call, [{global, {SimId, ?MODULE}}, {tick}]),
	    timer:start(),
	    {reply, ok, State#log_state{timer=Timer, status=running}}
    end;

handle_call({action, stop}, _From, State) ->
    timer:cancel(State#log_state.timer),
    {reply, ok, State#log_state{timer=none, status=stopped}};

handle_call({tick}, _From, State) when State#log_state.status =:= running ->
    New_state = log_parameters(State),
    {reply, ok, New_state};

handle_call({tick}, _From, State) when State#log_state.status =:= stopped ->
    timer:cancel(State#log_state.timer),
    {reply, {error, logger_stopped}, State#log_state{timer=none, status=stopped}};

handle_call({tick}, _From, State) ->
    io:format("corrupt counter"),
    {reply, error, State};

handle_call(stop, _From, State) ->
    timer:cancel(State#log_state.timer),
    {stop, normal, stopped, State#log_state{timer=none, status=stopped}}.

%handle_call(_Request, _From, State) -> {reply, Reply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

log_parameters(State) ->
    Database = State#log_state.database,
    Parameters = State#log_state.parameters,

    Parsed = lists:map(fun(P) -> parse_parameter(P) end, Parameters),

    New_entry = #log_entry{timestamp = erlang:now(), parameters = Parsed},

    State#log_state{database = [New_entry|Database]}.

parse_parameter(#log_parameter{mfa = {M, F, A}} = Parameter) ->
    Value = apply(M, F, A),
    Parameter#log_parameter{value = Value}.

print_cvs_dump(Database) ->
    create_cvs_dump(Database).

create_csv_dump([]) -> [];
create_csv_dump([Head|Rest]) -> 
    create_csv_dump([], [Head|Rest], []).

create_csv_dump(_, [], Acc) ->
    lists:reverse(Acc);
create_csv_dump(Old_header, [Head|Rest], Acc) ->
    Header = get_header(Head),
    if
        Old_header =:= Header ->
	    create_csv_dump(Header, Rest, [csv_entry(Head)|Acc]);
	true ->
	    create_csv_dump(Header, Rest, [csv_entry(Head)|[csv_header(Head)|Acc]])
    end.
    
get_header(Entry) ->
    get_header(Entry#log_entry.parameters, []).
get_header([], Acc) ->
    lists:reverse(Acc);
get_header([Head|Rest], Acc) ->
    #log_parameter{name = Name} = Head,
    get_header(Rest, [Name|Acc]).

csv_header(Header) ->
    ["Timestamp"|get_header(Header)].

csv_entry(#log_entry{timestamp = Timestamp, parameters = Entries}) ->
    csv_entry(Entries, [Timestamp]).

csv_entry([], Acc) ->
    lists:reverse(Acc);
csv_entry([Head|Rest], Acc) ->
    #log_parameter{value = Value} = Head,
    csv_entry(Rest, [Value|Acc]).
    


%%%===================================================================
%%% Test functions
%%%===================================================================
-include_lib("include/es_common.hrl").

unit_test() ->
    SimId = 1,
    {ok, _} = es_log_server:start_link(SimId),

    {error, {not_set, cycle_len}} = start_logging(SimId),
    ok = set_cycle_len(SimId, 1000),
    1000 = cycle_len(SimId),

    {error, {not_set, parameters}} = start_logging(SimId),
    ok = add_parameters(SimId, [{"Node", {erlang, node, []}}]),

    _ = parameters(SimId),

    stopped = status(SimId),
    ok = start_logging(SimId),
    running = status(SimId),
    timer:sleep(2000),

    ok = set_cycle_len(SimId, 500),
    500 = cycle_len(SimId),

    ok = add_parameter(SimId, {"Nodes", {erlang, nodes, []}}),
    _ = parameters(SimId),

    timer:sleep(2000),
    ok = stop_logging(SimId),
    stopped = status(SimId),
    _ = database(SimId),
    Retval = csv_dump(SimId),
    io:format("~p~n", [Retval]),
    stopped = stop_link(SimId),
    ok.

integration_test() ->
    ok = egon_server:start(),
    {ok, SimId} = egon_server:new_sim(["Test_server", "Simulator started by test function", "Tester"]),
    true = egon_server:sim_loaded(SimId),

    ok = set_cycle_len(SimId, 500),

    ok = add_parameters(SimId, [
        {"Core Tavg", {gen_server, call, [{global, {SimId, es_core_server}}, {get, tavg}]}},
        {"Turbine power", {gen_server, call, [{global, {SimId, es_turbine_server}}, {get, power}]}}
    ]),

    ok = start_logging(SimId),
    gen_server:call({global, {SimId, es_rod_position_server}}, {action, step_in}),
    gen_server:call({global, {SimId, es_rod_position_server}}, {action, step_in}),
    timer:sleep(2000),
    gen_server:call({global, {SimId, es_rod_position_server}}, {action, step_in}),
    gen_server:call({global, {SimId, es_rod_position_server}}, {action, step_in}),

    ok = add_parameter(SimId, 
        {"Turbine power", {gen_server, call, [{global, {SimId, es_turbine_server}}, {get, power}]}}
    ),
    _ = parameters(SimId),

    timer:sleep(2000),
    ok = stop_logging(SimId),
    _ = database(SimId),
    Retval = csv_dump(SimId),
    io:format("~p~n", [Retval]),
    egon_server:stop(),
    ok.