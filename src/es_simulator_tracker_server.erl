-module(es_simulator_tracker_server).
-include_lib("include/es_common.hrl").
-import(io_lib).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(tracker_state, {simulators, next_id}).
-record(simulator_manifest, {id, sup_pid, name, desc, owner}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> 
    {ok, #tracker_state{simulators = [], next_id = 1}}.

handle_call({start_simulator, [Name, Desc, User]}, _From, State) -> 
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

handle_call({connect_to_simulator, [SimId, User]}, _From, State) -> 
    case sim_info(SimId, State) of
        {ok, _} ->
	    {ok, Port} = es_interface_dispatcher:start_child(SimId, User),
	    {reply, {ok, [{SimId, none, Port}]}, State};
	Other ->
	    {reply, Other, State}
    end;

handle_call({get, simulators}, _From, State) -> 
    {reply, {ok, State#tracker_state.simulators}, State};

handle_call({get, sim_info, SimId}, _From, State) -> 
    {reply, sim_info(SimId, State), State};

handle_call({get, sim_clients, SimId}, _From, State) -> 
    Reply = sim_clients(SimId),
    {reply, {ok, Reply}, State}.

%handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

sim_clients(SimId) ->
    Reply = lists:map(fun({_, S, _, _}) -> client_info(S) end, supervisor:which_children({global, {SimId, es_interface_dispatcher}})),
    io:format("es_simulator_tracker_server:sim_clients: ~p", [Reply]),
    Reply.

client_info(Pid) ->
    gen_server:call(Pid, {get, client_info}).

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
