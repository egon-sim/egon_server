-module(es_simulator_tracker_server).
-include_lib("include/es_common.hrl").
-import(io_lib).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(tracker_state, {simulators}).
-record(simulator_manifest, {id, name, desc, owner}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> 
    {ok, #tracker_state{simulators = []}}.

handle_call({start_simulator, [Name, Desc, User]}, _From, State) -> 
    Sims = State#tracker_state.simulators,
    SimId = next_id(Sims),
    case es_simulator_dispatcher:start_child(SimId) of
        {ok, Child} ->
	    NewSim = #simulator_manifest{id = SimId, name = Name, desc = Desc, owner = User},
	    {reply, {ok, SimId}, State#tracker_state{simulators = [NewSim|Sims]}};
	{error, shutdown} ->
	    io:format("Starting child failed.~n"),
	    {reply, {error, shutdown}, State};
	Other ->
	    io:format("Other: ~p", [Other]),
	    {reply, Other, State}
    end;

handle_call({connect_to_simulator, [SimId, User]}, _From, State) -> 
    case get_sim(SimId, State) of
        {ok, _} ->
	    {ok, Port} = es_interface_dispatcher:start_child(SimId),
	    {reply, {ok, [{SimId, none, Port}]}, State};
	Other ->
	    {reply, Other, State}
    end;

handle_call({update_port, SimId, Port}, _From, State) -> 
    Sims = State#tracker_state.simulators,
    New_sims = update_sims(Sims, SimId, Port),
    {reply, ok, State#tracker_state{simulators = New_sims}};

handle_call({get, simulators}, _From, State) -> 
    {reply, {ok, State#tracker_state.simulators}, State};

handle_call({get, sim_info, SimId}, _From, State) -> 
    {reply, get_sim(SimId, State), State}.

%handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

next_id(Sims) ->
    if
        Sims == [] ->
	    1;
	true ->
	    lists:max(lists:map(fun(Tuple) -> element(1, Tuple) end, Sims)) + 1
    end.

get_sim(SimId, State) ->
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

update_sims([], _, _) ->
    [];

update_sims([{SimId, Child, _}|Rest], SimId, Port) ->
    [{SimId, Child, Port}|Rest];

update_sims([Sim|Rest], SimId, Port) ->
    [Sim|update_sims(Rest, SimId, Port)].