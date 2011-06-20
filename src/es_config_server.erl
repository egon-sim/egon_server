-module(es_config_server).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(config_state, {}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> 
    {ok, #config_state{}}.

handle_call({freaze_sim}, _From, State) ->
    {reply, gen_server:call(es_clock_server, {stop_ticking}), State};

handle_call({unfreaze_sim}, _From, State) ->
    {reply, gen_server:call(es_clock_server, {start_ticking}), State};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

%handle_call(_Request, _From, State) -> {reply, Reply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

