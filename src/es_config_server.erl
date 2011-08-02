-module(es_config_server).
-include_lib("include/es_common.hrl").
-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(config_state, {simid}).

start_link(SimId) -> gen_server:start_link({global, {SimId, ?MODULE}}, ?MODULE, [SimId], []).

init([SimId]) -> 
    {ok, #config_state{simid = SimId}, 0}.

handle_call({get, runing_servers_list}, _From, State) ->
    W = supervisor:which_children(es_simulator_sup),
    F = lists:filter(fun({_, Def, _, _}) -> is_pid(Def) end, W),
    L = lists:map(fun({Name, _, _, _}) -> Name end, F),
    {reply, L, State};

handle_call({freaze_sim}, _From, State) ->
    SimId = State#config_state.simid,
    {reply, gen_server:call({global, {SimId, es_clock_server}}, {stop_ticking}), State};

handle_call({unfreaze_sim}, _From, State) ->
    SimId = State#config_state.simid,
    {reply, gen_server:call({global, {SimId, es_clock_server}}, {start_ticking}), State};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

%handle_call(_Request, _From, State) -> {reply, Reply, State}.
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(timeout, State) ->
    supervisor:which_children(es_simulator_sup),
    set_up_defaults(State),    
    {noreply, State};

handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_up_defaults(State) ->
    SimId = State#config_state.simid,
    load_snapshot(State, "priv/snapshots/full_power.snapshot"),
    gen_server:call({global, {SimId, es_clock_server}}, {start_ticking}),
    ok.

load_snapshot(#config_state{simid = SimId}, Path) ->
   {ok, [Snapshot]} = file:consult(Path),

   lists:foreach(
       fun({Server, Parm, Val}) ->
       	   gen_server:call({global, {SimId, Server}}, {set, Parm, Val})
       end,
       Snapshot),
   ok.
