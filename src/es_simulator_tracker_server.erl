-module(es_simulator_tracker_server).
-include_lib("include/es_common.hrl").
-import(io_lib).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(tracker_state, {simulators}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> 
    {ok, #tracker_state{simulators = []}}.

handle_call({start_simulator, Connection}, _From, State) -> 
    Sims = State#tracker_state.simulators,

    if
        Sims == [] ->
	    SimId = 1;
	true ->
	    SimId = lists:max(lists:map(fun(Tuple) -> element(1, Tuple) end, Sims)) + 1
    end,
    case es_simulator_dispatcher:start_child(SimId, Connection) of
        {ok, Child} ->
	    {reply, ok, State#tracker_state{simulators = [{SimId, Child, unknown}|Sims]}};
	{error, shutdown} ->
	    io:format("Starting child failed.~n"),
	    {reply_sock, Socket} = Connection,
	    gen_tcp:send(Socket, io_lib:fwrite("~p~n", [{error_starting_child}])),
	    {reply, {error, shutdown}, State};
	Other ->
	    io:format("Other: ~p", [Other]),
	    {reply_sock, Socket} = Connection,
	    gen_tcp:send(Socket, io_lib:fwrite("{unknown_error, ~p}~n", [Other])),
	    {reply, Other, State}
    end;

handle_call({connect_to_simulator, Sim, Connection}, _From, State) -> 
    Sims = State#tracker_state.simulators,

    {FoundSim, _} = lists:partition(fun({S, _, _}) -> S == Sim end, Sims),
    if
        length(FoundSim) == 1 ->
	    {reply, {ok, FoundSim}, State};
        length(FoundSim) == 0 ->
	    {reply, {error, no_such_id}, State};
        length(FoundSim) > 1 ->
	    {reply, {error, id_not_unique}, State};
        true ->
	    {reply, {error, other}, State}
    end;

handle_call({update_port, SimId, Port}, _From, State) -> 
    Sims = State#tracker_state.simulators,
    New_sims = update_sims(Sims, SimId, Port),
    {reply, ok, State#tracker_state{simulators = New_sims}};

handle_call({get, simulators}, _From, State) -> 
    {reply, State#tracker_state.simulators, State}.

%handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_sims([], _, _) ->
    [];

update_sims([{SimId, Child, _}|Rest], SimId, Port) ->
    [{SimId, Child, Port}|Rest];

update_sims([Sim|Rest], SimId, Port) ->
    [Sim|update_sims(Rest, SimId, Port)].