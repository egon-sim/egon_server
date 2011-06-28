-module(es_simulator_tracker_server).
-include_lib("include/es_common.hrl").
-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(tracker_state, {simulators}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> 
    {ok, #tracker_state{simulators = []}}.

handle_call({add, simulator, Val}, _From, State) -> 
    Sims = State#tracker_state.simulators,
    {reply, ok, #tracker_state{simulators = [Val|Sims]}};

handle_call({get, simulators}, _From, State) -> 
    {reply, State#tracker_state.simulators, State}.

%handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
