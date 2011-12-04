%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Server for starting and stopping simulators and for
%%%      providing information about simulators.
%%% @end
%%%------------------------------------------------------------------
-module(es_simulator_tracker_server).

-behaviour(gen_server).
-define(SERVER, {global, ?MODULE}).

% API
-export([
	start_link/0,
	stop_link/0,
	simulators/0,
	start_new_simulator/3,
	connect_to_simulator/2,
	sim_info/1,
	sim_clients/1,
	stop_simulator/1
	]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% data structures
-record(tracker_state, {
			simulators,
			next_id
			}).

-record(simulator_manifest, {
	  	  	     id,
	  	  	     sup_pid,
	  	  	     name,
	  	  	     desc,
	  	  	     owner
	  	  	     }).


%%%==================================================================
%%% API
%%%==================================================================

%%-------------------------------------------------------------------
%% @doc Starts the server.
%%
%% @spec start_link() -> {ok, Pid}
%% where
%%  Pid = pid()
%% @end
%%-------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?SERVER, ?MODULE, [], []).

%%-------------------------------------------------------------------
%% @doc Stops the server.
%%
%% @spec stop_link() -> stopped
%% @end
%%-------------------------------------------------------------------
stop_link() ->
    gen_server:call(?SERVER, stop).

%%-------------------------------------------------------------------
%% @doc Returns list of simulator instances on this server.
%%
%% @spec simulators() -> {ok, [#simulator_manifest]}
%% @end
%%-------------------------------------------------------------------
simulators() ->
    gen_server:call(?SERVER, {get, simulators}).

%%-------------------------------------------------------------------
%% @doc Starts a new simulator instance
%%
%% @spec start_new_simulator(Name::string(), Description::string(), 
%%       User::string()) ->
%%       {ok, SimId::integer()} | {error, shutdown}
%% @end
%%-------------------------------------------------------------------
start_new_simulator(Name, Desc, User) ->
    gen_server:call(?SERVER, {start_simulator, Name, Desc, User}).

%%-------------------------------------------------------------------
%% @doc Returns simulator manifest for given simulator.
%%
%% @spec connect_to_simulator(SimId::integer(), User::string()) ->
%%       {connected, SimId::integer(), Port::integer()}
%% @end
%%-------------------------------------------------------------------
connect_to_simulator(SimId, User) ->
    gen_server:call(?SERVER, {connect_to_simulator, SimId, User}).

%%-------------------------------------------------------------------
%% @doc Returns simulator manifest for given simulator.
%%
%% @spec sim_info(SimId::integer()) -> {ok, #simulator_manifest}
%% @end
%%-------------------------------------------------------------------
sim_info(SimId) ->
    gen_server:call(?SERVER, {get, sim_info, SimId}).

%%-------------------------------------------------------------------
%% @doc Returns clients connected to given simulator.
%%
%% @spec sim_clients(SimId::integer()) -> {ok, [{User, Remote, Local}]}
%% where
%%    User = string() % username of client
%%    Remote = inet:peername/1 % peername of client socket
%%    Local = inet:sockname/1 % sockname of client socket
%% @end
%%-------------------------------------------------------------------
sim_clients(SimId) ->
    gen_server:call(?SERVER, {get, sim_clients, SimId}).

%%-------------------------------------------------------------------
%% @doc Stops the simulator instance.
%%
%% @spec stop_simulator(SimId::integer()) -> {ok, stopped}
%% @end
%%-------------------------------------------------------------------
stop_simulator(SimId) ->
    gen_server:call(?SERVER, {stop_simulator, SimId}).


%%%==================================================================
%%% gen_server callbacks
%%%==================================================================

init([]) -> 
    {ok, #tracker_state{simulators = [], next_id = 1}}.

handle_call({start_simulator, Name, Desc, User}, _From, State) -> 
%% State#tracker_state.next_id should never be reused, SimId should be unique for every simulator instance
    Sims = State#tracker_state.simulators,
    SimId = State#tracker_state.next_id,
    case es_simulator_dispatcher:start_child(SimId) of
        {ok, Child} ->
	    NewSim = #simulator_manifest{id = SimId, sup_pid = Child, name = Name, desc = Desc, owner = User},
	    {reply, {ok, SimId}, State#tracker_state{simulators = [NewSim|Sims], next_id = SimId + 1}};
	{error, shutdown} ->
	    io:format("Starting child failed.~n"),
	    {reply, {error, shutdown}, State#tracker_state{next_id = SimId + 1}};
	{error, {already_started, _}} ->
	    io:format("That child is already started.~n"),
	    {reply, {error, already_started}, State#tracker_state{next_id = SimId + 1}};
	Other ->
	    io:format("Other: ~p~n", [Other]),
	    {reply, Other, State#tracker_state{next_id = SimId + 1}}
    end;

handle_call({stop_simulator, SimId}, _From, State) -> 
    Pid = get_pid(SimId, State),
    es_simulator_dispatcher:stop_child(Pid),
    NewState = remove_sim(SimId, State),
    {reply, {ok, stopped}, NewState};

handle_call({connect_to_simulator, SimId, User}, _From, State) -> 
    case sim_info(SimId, State) of
        {ok, _} ->
	    {ok, Port} = es_interface_dispatcher:start_child(SimId, User),
	    {reply, {connected, SimId, Port}, State};
	Other ->
	    {reply, Other, State}
    end;

handle_call({get, simulators}, _From, State) -> 
    {reply, {ok, State#tracker_state.simulators}, State};

handle_call({get, sim_info, SimId}, _From, State) -> 
    {reply, sim_info(SimId, State), State};

handle_call({get, sim_clients, SimId}, _From, State) -> 
    Reply = sim_clients_(SimId),
    {reply, {ok, Reply}, State}.

%handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%==================================================================
%%% Internal functions
%%%==================================================================

sim_info(SimId, State) ->
    Sims = State#tracker_state.simulators,

    {FoundSim, _} = lists:partition(fun(S) -> S#simulator_manifest.id == SimId end, Sims),
    if
        length(FoundSim) == 1 ->
	    [SimInfo] = FoundSim,
	    {ok, SimInfo};
        length(FoundSim) == 0 ->
	    {error, no_such_id};
        length(FoundSim) > 1 ->
	    {error, id_not_unique};
        true ->
	    {error, other}
    end.

sim_clients_(SimId) ->
    Reply = lists:map(
	      fun({_, Pid, _, _}) -> es_interface_server:client_info(Pid) end,
	      es_interface_dispatcher:children(SimId)
	     ),
    io:format("es_simulator_tracker_server:sim_clients: ~p", [Reply]),
    Reply.

get_pid(SimId, State) ->
    Sims = State#tracker_state.simulators,
    get_pid_rec(SimId, Sims).

get_pid_rec(_, []) ->
    none;
get_pid_rec(SimId, [#simulator_manifest{id = SimId, sup_pid = Pid}|_]) ->
    Pid;
get_pid_rec(SimId, [#simulator_manifest{}|Rest]) ->
    get_pid_rec(SimId, Rest);
get_pid_rec(SimId, [_|Rest]) ->
    get_pid_rec(SimId, Rest).

remove_sim(SimId, State) ->
    Sims = State#tracker_state.simulators,
    NewSims = lists:filter(fun(Sim) -> Sim#simulator_manifest.id =/= SimId end, Sims),
    State#tracker_state{simulators = NewSims}.

%%%==================================================================
%%% Test functions
%%%==================================================================
-include_lib("eunit/include/eunit.hrl").

unit_test() -> ok.

integration_test() -> ok.
