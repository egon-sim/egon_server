%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Configuration server. Server loading snapshots and initial
%%%      simlator configuration .
%%% @end
%%%------------------------------------------------------------------
-module(es_config_server).

-behaviour(gen_server).
-define(SERVER(SimId), {global, {SimId, ?MODULE}}).

% API
-export([
	start_link/1,
	stop_link/1,
	freaze/1,
	unfreaze/1,
	running_servers/1
	]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% data structures
-record(config_state, {
	  	      simid
	  	      }).


%%%==================================================================
%%% API
%%%==================================================================

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
%% @doc Stops the server.
%%
%% @spec stop_link(SimId::integer()) -> stopped
%% @end
%%-------------------------------------------------------------------
stop_link(SimId) ->
    gen_server:call(?SERVER(SimId), stop).

%%-------------------------------------------------------------------
%% @doc Stops clock server (effectively freazes the simulation).
%%
%% @spec freaze(SimId::integer()) -> ok
%% @end
%%-------------------------------------------------------------------
freaze(SimId) ->
    gen_server:call(?SERVER(SimId), {freaze_sim}).

%%-------------------------------------------------------------------
%% @doc Starts clock server (effectively unfreazes the simulation).
%%
%% @spec unfreaze(SimId::integer()) -> ok
%% @end
%%-------------------------------------------------------------------
unfreaze(SimId) ->
    gen_server:call(?SERVER(SimId), {unfreaze_sim}).

%%-------------------------------------------------------------------
%% @doc Returns a list of atoms representing all children of
%%      es_simulator_sup.
%%
%% @spec running_servers(SimId::integer()) -> [atom()]
%% @end
%%-------------------------------------------------------------------
running_servers(SimId) ->
    gen_server:call(?SERVER(SimId), {get, running_servers_list}).


%%%==================================================================
%%% gen_server callbacks
%%%==================================================================

init([SimId]) -> 
    {ok, #config_state{simid = SimId}, 0}.

handle_call({get, running_servers_list}, _From, State) ->
    SimId = State#config_state.simid,
    W = supervisor:which_children({global, {SimId, es_simulator_sup}}),
    F = lists:filter(fun({_, Def, _, _}) -> is_pid(Def) end, W),
    L = lists:map(fun({Name, _, _, _}) -> Name end, F),
    {reply, L, State};

handle_call({freaze_sim}, _From, State) ->
    error_logger:info_report(["Pausing simulator execution."]),
    SimId = State#config_state.simid,
    {reply, es_clock_server:stop_ticking(SimId), State};

handle_call({unfreaze_sim}, _From, State) ->
    error_logger:info_report(["Starting simulator execution."]),
    SimId = State#config_state.simid,
    {reply, es_clock_server:start_ticking(SimId), State};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

%handle_call(_Request, _From, State) -> {reply, Reply, State}.
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(timeout, State) ->
    SimId = State#config_state.simid,
    supervisor:which_children({global, {SimId, es_simulator_sup}}),
    set_up_defaults(State),    
    {noreply, State};

handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%==================================================================
%%% Internal functions
%%%==================================================================

set_up_defaults(State) ->
    case application:get_application() of
        {ok, App} ->
	    Priv = es_lib:priv_dir(App),
	    {ok, Snapshots} = application:get_env(App, snapshots),
    	    {ok, Snapshot} = application:get_env(App, default_snapshot),
    	    load_snapshot(State, Priv ++ Snapshots ++ Snapshot),
    	    ok;
	undefined ->
	    {error, application_undefined}
    end.


load_snapshot(#config_state{simid = SimId}, Path) ->
    Is_file = filelib:is_regular(Path),
    case Is_file of
        true ->
   	    {ok, [Snapshot]} = file:consult(Path),
   	    lists:foreach(
       	        fun({Server, Parm, Val}) ->
       	   	    gen_server:call({global, {SimId, Server}}, {set, Parm, Val})
       		end,
            Snapshot),
	    ok;
        false ->
            error_logger:info_report(["Error: snapshot file does not exist.~n", {snapshot_file, Path}]),
            {error, {no_snapshot_file, Path}}
    end.

%%%==================================================================
%%% Test functions
%%%==================================================================
-include_lib("eunit/include/eunit.hrl").

unit_test() -> ok.

integration_test() -> ok.
