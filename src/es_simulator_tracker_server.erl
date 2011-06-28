-module(es_simulator_tracker_server).
-include_lib("include/es_common.hrl").
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
    {ok, Child} = es_simulator_dispatcher:start_child(SimId, Connection),
    {reply, ok, #tracker_state{simulators = [{SimId, Child}|Sims]}};

handle_call({get, simulators}, _From, State) -> 
    {reply, State#tracker_state.simulators, State}.

%handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
