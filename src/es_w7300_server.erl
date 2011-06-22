-module(es_w7300_server).
-include_lib("include/es_common.hrl").
-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(w7300_state, {}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> {ok, #w7300_state{}}.

handle_call({get, tref}, _From, State) -> %TODO: move constants to config file/ETS table
   Power = gen_server:call(es_turbine_server, {get, power}),
   Noload_Tavg = 291.8,
   Fullpower_Tavg = 305.0,
   Tref = Noload_Tavg + (Fullpower_Tavg - Noload_Tavg) * (Power / 100),
   {reply, Tref, State};

handle_call(stop, _From, Tab) -> {stop, normal, stopped, Tab}.

%handle_call(_Request, _From, State) -> {reply, Reply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

