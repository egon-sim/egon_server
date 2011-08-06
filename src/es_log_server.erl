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
-export([start_link/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% tests
-export([simple_test/0, general_test/0]).

% data structure
-record(log_state, {
		   simid, % ID of a simulator to which this log server belongs
		   timer, % reference of timer which sends ticks to log server
		   status, % status of log server = running | stopped
		   cycle_len, % number of miliseconds between collecting data
		   parameters, % list of paramteres which to take when
		   	       % collecting data
		   database % list of collected data
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
%% @doc Get parameters which logger logs.
%%
%% @spec parameters(SimId::integer()) -> Parameters
%% where
%%  Parameters = [Parameter]
%%  Parameter = {Name, {Module, Function, Arguments}}
%%  Name = string()
%%  Module = atom()
%%  Function = atom()
%%  Arguments = [term()]
%% @end
%%-------------------------------------------------------------------
parameters(SimId) ->
    gen_server:call(?SERVER(SimId), {get, parameters}).

%%-------------------------------------------------------------------
%% @doc Set parameters which logger logs.
%%
%% @spec set_parameters(SimId::integer(), Parameters::list()) -> ok
%% where
%%  Parameters = [Parameter]
%%  Parameter = {Name, {Module, Function, Arguments}}
%%  Name = string()
%%  Module = atom()
%%  Function = atom()
%%  Arguments = [term()]
%% @end
%%-------------------------------------------------------------------
set_parameters(SimId, Parameters) ->
    gen_server:call(?SERVER(SimId), {set, parameters, Parameters}).


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
%%  Database = [Entry]
%%  Entry = {Timestamp, Value}
%%  Timestamp = {integer(),integer(),integer()}
%%  Value = {Name, {Module, Function, Arguments}, Result}
%%  Name = string()
%%  Module = atom()
%%  Function = atom()
%%  Arguments = [term()]
%%  Result = term()
%% @end
%%-------------------------------------------------------------------
database(SimId) ->
    gen_server:call(?SERVER(SimId), {get, database}).


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

handle_call({get, status}, _From, State) ->
    {reply, State#log_state.status, State};

handle_call({get, database}, _From, State) ->
    {reply, lists:reverse(State#log_state.database), State};

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

    New_entry = {erlang:now(), Parsed},

    State#log_state{database = [New_entry|Database]}.

parse_parameter({Name, {M, F, A}}) ->
    Value = apply(M, F, A),
    {Name, {M, F, A}, Value}.


%%%===================================================================
%%% Test functions
%%%===================================================================
-include_lib("include/es_common.hrl").

simple_test() ->
    SimId = 1,
    {ok, _} = es_log_server:start_link(SimId),
    ok = set_cycle_len(SimId, 1000),
    1000 = cycle_len(SimId),

    ok = set_parameters(SimId, [{"Node", {erlang, node, []}}]),

    _ = parameters(SimId),

    stopped = status(SimId),
    ok = gen_server:call({global, {SimId, ?MODULE}}, {action, start}),
    running = status(SimId),
    timer:sleep(2000),

    ok = set_cycle_len(SimId, 500),
    500 = cycle_len(SimId),

    ok = set_parameters(SimId, [{"Node", {erlang, node, []}}, {"Nodes", {erlang, nodes, []}}]),
    _ = parameters(SimId),

    timer:sleep(2000),
    ok = gen_server:call({global, {SimId, ?MODULE}}, {action, stop}),
    stopped = status(SimId),
    Retval = database(SimId),
    io:format("~p~n", [Retval]),
    stopped = gen_server:call({global, {SimId, ?MODULE}}, stop),
    ok.

general_test() ->
    ok = egon_server:start(),
    {ok, SimId} = egon_server:new_sim(["Test_server", "Simulator started by test function", "Tester"]),
    true = egon_server:sim_loaded(SimId),

    ok = set_cycle_len(SimId, 500),

    ok = set_parameters(SimId, [
        {"Core Tavg", {gen_server, call, [{global, {SimId, es_core_server}}, {get, tavg}]}},
        {"Turbine power", {gen_server, call, [{global, {SimId, es_turbine_server}}, {get, power}]}}
    ]),

    ok = gen_server:call({global, {SimId, ?MODULE}}, {action, start}),
    gen_server:call({global, {SimId, es_rod_position_server}}, {action, step_in}),
    gen_server:call({global, {SimId, es_rod_position_server}}, {action, step_in}),
    timer:sleep(2000),
    gen_server:call({global, {SimId, es_rod_position_server}}, {action, step_in}),
    gen_server:call({global, {SimId, es_rod_position_server}}, {action, step_in}),

    ok = set_parameters(SimId, [
        {"Core Tavg", {gen_server, call, [{global, {SimId, es_core_server}}, {get, tavg}]}},
        {"Turbine Tref", {gen_server, call, [{global, {SimId, es_w7300_server}}, {get, tref}]}},
        {"Turbine power", {gen_server, call, [{global, {SimId, es_turbine_server}}, {get, power}]}}
    ]),
    _ = gen_server:call({global, {SimId, ?MODULE}}, {get, parameters}),

    timer:sleep(2000),
    ok = gen_server:call({global, {SimId, ?MODULE}}, {action, stop}),
    Retval = database(SimId),
    io:format("~p~n", [Retval]),
    egon_server:stop(),
    ok.