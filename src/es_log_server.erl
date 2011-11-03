%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Server logging various parameters of simulator and stores
%%%      them so they can be retreived and analyzed at later
%%%      time. Started by es_utility_sup.
%%% @end
%%%------------------------------------------------------------------
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
	timestamp/1,
	database/1,
	csv_dump/1,
	range_dump/4,
	start_logging/1,
	stop_logging/1
	]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

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
    gen_server:cast(?SERVER(SimId), {set, cycle_len, Val}).

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
    gen_server:cast(?SERVER(SimId), {action, add_parameter, Parameter}).

%%-------------------------------------------------------------------
%% @doc Clear list of parameters which logger logs.
%%
%% @spec clear_parameters(SimId::integer()) -> ok
%% @end
%%-------------------------------------------------------------------
clear_parameters(SimId) ->
    gen_server:cast(?SERVER(SimId), {set, parameters, []}).

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
%% @doc Returns timestamp of current moment
%%
%% @spec timestamp(SimId::integer()) -> {Megasec, Sec, Microsec}
%% where
%%  Megasec = integer()
%%  Sec = integer()
%%  Microsec = integer()
%% @end
%%-------------------------------------------------------------------
timestamp(SimId) ->
    gen_server:call(?SERVER(SimId), {get, timestamp}).

%%-------------------------------------------------------------------
%% @doc Returns all information from StartTimestamp, EndTimestamp
%%      with timespan between points equal to Frequency
%%
%% @spec range(SimId, StartTimestamp, EndTimestamp, Frequency) -> 
%% 	 [Points]
%% where
%%  Points = [Timestamp|[Values]]
%%  Timestamp = {integer(), integer(), integer()}
%%  Values = [term()]
%% @end
%%-------------------------------------------------------------------
range_dump(SimId, StartTimestamp, EndTimestamp, Frequency) ->
    gen_server:call(?SERVER(SimId), {get, range, {StartTimestamp, EndTimestamp, Frequency}}).

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
    gen_server:cast(?SERVER(SimId), {action, stop}).


%%%==================================================================
%%% gen_server callbacks
%%%==================================================================

init([SimId]) -> 
    {ok, #log_state{simid = SimId, timer=none, status=stopped, cycle_len=none, parameters=[], database=[]}}.

handle_call({get, timestamp}, _From, State) ->
    {reply, erlang:now(), State};

handle_call({get, cycle_len}, _From, State) ->
    {reply, State#log_state.cycle_len, State};

handle_call({get, parameters}, _From, State) ->
    {reply, State#log_state.parameters, State};

handle_call({get, status}, _From, State) ->
    {reply, State#log_state.status, State};

handle_call({get, database}, _From, State) ->
    {reply, lists:reverse(State#log_state.database), State};

handle_call({get, range, {StartTimestamp, EndTimestamp, Frequency}}, _From, State) ->
    Range = create_range(State#log_state.database, {StartTimestamp, EndTimestamp, Frequency}),
    {reply, Range, State};

handle_call({get, csv_dump}, _From, State) ->
    Database = lists:reverse(State#log_state.database),
    Dump = create_csv_dump(Database),
    {reply, Dump, State};

handle_call({action, start}, _From, State) when State#log_state.cycle_len =:= none ->
    error_logger:info_report(["Starting log server failed", {reason, "cycle_len not set"}]),
    {reply, {error, {not_set, cycle_len}}, State};

handle_call({action, start}, _From, State) when State#log_state.parameters =:= [] ->
    error_logger:info_report(["Starting log server failed", {reason, "parameters not set"}]),
    {reply, {error, {not_set, parameters}}, State};

handle_call({action, start}, _From, State) ->
    SimId = State#log_state.simid,
    Cycle_len = State#log_state.cycle_len,
    error_logger:info_report(["Starting log server", {cycle_len, Cycle_len}]),
    {ok, Timer} = timer:apply_interval(Cycle_len, gen_server, call, [?SERVER(SimId), {tick}]),
    timer:start(),
    {reply, ok, State#log_state{timer=Timer, status=running}};

handle_call({action, add_parameter, {Name, Server, Call}}, _From, State) ->
    SimId = State#log_state.simid,
    add_parameter(SimId, {Name, {gen_server, call, [{global, {SimId, Server}}, Call]}}),
    {reply, ok, State};

handle_call({tick}, _From, State) when State#log_state.status =:= running ->
    New_state = log_parameters(State),
    {reply, ok, New_state};

handle_call({tick}, _From, State) when State#log_state.status =:= stopped ->
    timer:cancel(State#log_state.timer),
    {reply, {error, logger_stopped}, State#log_state{timer=none, status=stopped}};

handle_call({tick}, _From, State) ->
    error_logger:info_report(["Corrupt log_server", {reason, "log_server status not in [running, stopped]"}]),
    {reply, error, State};

handle_call(stop, _From, State) ->
    timer:cancel(State#log_state.timer),
    {stop, normal, stopped, State#log_state{timer=none, status=stopped}}.

handle_cast({set, cycle_len, Val}, State) when State#log_state.status =:= stopped ->
    {noreply, State#log_state{cycle_len=Val}};

handle_cast({set, cycle_len, Val}, State) when State#log_state.status =:= running ->
    error_logger:info_report(["Setting log server cycle_len", {cycle_len, Val}]),
    SimId = State#log_state.simid,
    Old_timer = State#log_state.timer,

    timer:cancel(Old_timer),
    {ok, New_timer} = timer:apply_interval(Val, gen_server, call, [{global, {SimId, ?MODULE}}, {tick}]),
    timer:start(),

    {noreply, State#log_state{timer=New_timer, cycle_len=Val}};

handle_cast({set, parameters, []}, State) ->
    error_logger:info_report(["Clearing parameters from log server"]),
    {noreply, State#log_state{parameters=[]}};
    
handle_cast({set, parameters, Val}, State) ->
    error_logger:info_report(["Setting parameters to log server", {parameters, Val}]),
    {noreply, State#log_state{parameters=Val}};

handle_cast({action, add_parameter, {Name, {Module, Function, Arguments}}}, State) ->
    New_param = #log_parameter{name = Name, mfa = {Module, Function, Arguments}},
    error_logger:info_report(["Adding parameter to log server", {parameter, New_param}]),
    Parameters = State#log_state.parameters,
    {noreply, State#log_state{parameters=[New_param|Parameters]}};

handle_cast({action, stop}, State) ->
    error_logger:info_report(["Stopping log server"]),
    timer:cancel(State#log_state.timer),
    {noreply, State#log_state{timer=none, status=stopped}}.

%handle_call(_Request, _From, State) -> {reply, Reply, State}.
%handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%==================================================================
%%% Internal functions
%%%==================================================================

log_parameters(State) ->
    Database = State#log_state.database,
    Parameters = State#log_state.parameters,

    Parsed = lists:map(fun(P) -> parse_parameter(P) end, Parameters),

    New_entry = #log_entry{timestamp = erlang:now(), parameters = Parsed},

    State#log_state{database = [New_entry|Database]}.

parse_parameter(#log_parameter{mfa = {M, F, A}} = Parameter) ->
    Value = apply(M, F, A),
    Parameter#log_parameter{value = Value}.

print_csv_dump(Database) ->
    List = create_csv_dump(Database),
    Lines = lists:map(fun(L) -> make_line(L) end, List),
    format(Lines).

make_line(Line) ->
    format(make_line(lists:reverse(Line), [])).
make_line([], Acc) ->
    io_lib:format("~p", [Acc]);
make_line([Head|Rest], Acc) ->
    make_line(Rest, [format(Head)|Acc]).

format(Str) ->
    lists:flatten(io_lib:format("~p", [Str])).

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

create_range(Database, {StartTimestamp, EndTimestamp, Frequency}) ->
    create_range([], Database, {StartTimestamp, EndTimestamp, Frequency}).
    
create_range(Acc, [], _) ->
    Acc;
create_range(Acc, [Head|Rest], {StartTimestamp, EndTimestamp, Frequency}) ->
    Done = compare_timestamp(StartTimestamp, EndTimestamp) > 0,
    if
        Done ->
	    Acc;
	true ->
	    StartOK = (compare_timestamp(Head#log_entry.timestamp, StartTimestamp) >= 0),
	    EndOK = (compare_timestamp(EndTimestamp, Head#log_entry.timestamp) >= 0),
	    if
	        StartOK and EndOK ->
		    New_acc = [csv_entry(Head)|Acc],
		    New_endTimestamp = dec_timestamp(Head#log_entry.timestamp, Frequency);
	        true ->
		    New_acc = Acc,
		    New_endTimestamp = EndTimestamp
	    end,
    	    create_range(New_acc, Rest, {StartTimestamp, New_endTimestamp, Frequency})
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
    

compare(Val1, Val2) ->
    if
        Val1 > Val2 ->
	    1;
        Val1 < Val2 ->
	    -1;
        Val1 =:= Val2 ->
	    0;
        true ->
	    {error, values_not_comparable}
    end.



inc_timestamp({MegaSec, Sec, MicroSec}, SecInc) ->
    New_microSec = MicroSec + trunc((SecInc - trunc(SecInc)) * 1000000),

    New_sec = trunc(Sec + SecInc) rem 1000000,

    New_megaSec = MegaSec + ((Sec + trunc(SecInc)) div 1000000),
    {New_megaSec, New_sec, New_microSec}.

dec_timestamp({MegaSec, Sec, MicroSec}, SecInc) ->
    normalize_timestamp(inc_timestamp({MegaSec, Sec, MicroSec}, -SecInc)).

normalize_timestamp({MegaSec, Sec, MicroSec}) when MicroSec < 0 ->
    {MegaSec, Sec - 1, MicroSec + 1000000};
normalize_timestamp({MegaSec, Sec, MicroSec}) when Sec < 0 ->
    {MegaSec - 1, Sec + 1000000, MicroSec};
normalize_timestamp(TimeStamp) ->
    TimeStamp.

compare_timestamp(Stamp, Stamp) ->
    0;
compare_timestamp({MegaSec, Sec, MicroSec1}, {MegaSec, Sec, MicroSec2}) ->
    compare(MicroSec1, MicroSec2);
compare_timestamp({MegaSec, Sec1, _}, {MegaSec, Sec2, _}) ->
    compare(Sec1, Sec2);
compare_timestamp({MegaSec1, _, _}, {MegaSec2, _, _}) ->
    compare(MegaSec1, MegaSec2).

%%%==================================================================
%%% Test functions
%%%==================================================================
-include_lib("eunit/include/eunit.hrl").

timestamp_test() ->
    ?assertEqual({1, 3, 3}, inc_timestamp({1, 2, 3}, 1)),
    ?assertEqual({1, 2, 3}, dec_timestamp({1, 3, 3}, 1)),
    ?assertEqual({2, 2, 3}, inc_timestamp({1, 999999, 3}, 3)),
    ?assertEqual({1, 999999, 3}, dec_timestamp({2, 2, 3}, 3)),
    ok.


unit_test() ->
    SimId = 1,
    {ok, _} = start_link(SimId),

    ?assertEqual({error, {not_set, cycle_len}}, start_logging(SimId)),
    ?assertEqual(ok, set_cycle_len(SimId, 1000)),
    ?assertEqual(1000, cycle_len(SimId)),

    ?assertEqual({error, {not_set, parameters}}, start_logging(SimId)),
    ?assertEqual(ok, add_parameters(SimId, [{"Node", {erlang, node, []}}])),

    ?assertEqual([#log_parameter{name = "Node", mfa = {erlang, node, []}, value = undefined}], parameters(SimId)),

    ?assertEqual(stopped, status(SimId)),
    ?assertEqual(ok, start_logging(SimId)),
    ?assertEqual(running, status(SimId)),
    timer:sleep(2000),

    ?assertEqual(ok, set_cycle_len(SimId, 500)),
    ?assertEqual(500, cycle_len(SimId)),

    ?assertEqual(ok, add_parameter(SimId, {"Nodes", {erlang, nodes, []}})),
    ?assertEqual([#log_parameter{name = "Nodes", mfa = {erlang, nodes, []}, value = undefined}, 
    #log_parameter{name = "Node", mfa = {erlang, node, []}, value = undefined}], parameters(SimId)),

    timer:sleep(2000),
    ?assertEqual(ok, stop_logging(SimId)),
    ?assertEqual(stopped, status(SimId)),
    Database = database(SimId),
    ?assertEqual(5, length(Database)),

%    ?assertEqual(#log_entry{parameters = [#log_parameter{name = "Nodes", mfa = {erlang, nodes, []}}, 
%    #log_parameter{name = "Node", mfa = {erlang, node, []}}]}, lists:last(Database)),

%    Retval = csv_dump(SimId),
%    io:format("~p~n", [Retval]),

    {Megasec, Sec, Microsec} = erlang:now(),

    Retval = range_dump(SimId, {Megasec, Sec - 3, Microsec}, {Megasec, Sec, Microsec}, 1),
    io:format("~p~n", [Retval]),

    ?assertEqual(stopped, stop_link(SimId)),
    ok.

integration_test() ->
    ?assertEqual(ok, egon_server:start()),
    {ok, SimId} = egon_server:new_sim(["Test_server", "Simulator started by test function", "Tester"]),
    ?assertEqual(true, egon_server:sim_loaded(SimId)),

    ?assertEqual(ok, egon_server:run(SimId)),

    ?assertEqual(ok, set_cycle_len(SimId, 500)),

    ok = add_parameters(SimId, [
        {"Core Tavg", {gen_server, call, [{global, {SimId, es_core_server}}, {get, tavg}]}},
        {"Turbine power", {gen_server, call, [{global, {SimId, es_turbine_server}}, {get, power}]}}
    ]),

    ?assertEqual(ok, start_logging(SimId)),
    es_rod_position_server:step_in(SimId),
    es_rod_position_server:step_in(SimId),
    timer:sleep(2000),
    es_rod_position_server:step_in(SimId),
    es_rod_position_server:step_in(SimId),

    ?assertEqual(ok, add_parameter(SimId, 
        {"Turbine power", {gen_server, call, [{global, {SimId, es_turbine_server}}, {get, power}]}}
    )),

    timer:sleep(2000),
    ?assertEqual(ok, stop_logging(SimId)),
%    Retval = csv_dump(SimId),
%    io:format("~p~n", [Retval]),
    ?assertEqual(ok, egon_server:stop()),
    ok.
