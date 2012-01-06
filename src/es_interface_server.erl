%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Server logging various parameters of simulator and stores
%%%      them so they can be retreived and analyzed at later
%%%      time. Started by es_utility_sup.
%%% @end
%%%------------------------------------------------------------------
-module(es_interface_server).

-behaviour(gen_server).
-include_lib("include/es_tcp_states.hrl").

% API
-export([
	call/2,
	start_link/2,
	client_info/1,
	port/1
	]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


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
start_link(SimId, User) ->
    gen_server:start_link(?MODULE, [SimId, User], []).


%%-------------------------------------------------------------------
%% @doc Returns information about the client connected to the
%% 	interface.
%%
%% @spec client_info(Pid) -> {User, Remote, Local}
%% where
%%  Pid = pid()
%%  User = string() % username of the client
%%  Remote = string() % remote name of socket
%%  Local = string() % local name of socket
%% @end
%%-------------------------------------------------------------------
client_info(Pid) ->
    gen_server:call(Pid, {get, client_info}).

%%-------------------------------------------------------------------
%% @doc Returns TCP port on which interface communicates with the
%% 	client.
%%
%% @spec port(Pid) -> Port
%% where
%%  Pid = pid()
%%  Port = integer()
%% @end
%%-------------------------------------------------------------------
port(Child) ->
    gen_server:call(Child, {get, port}).


%%%==================================================================
%%% gen_server callbacks
%%%==================================================================

init([SimId, User]) -> 
    {ok, LSock} = gen_tcp:listen(0, [{active, true}, {reuseaddr, true}]),
    {ok, Port} = inet:port(LSock),
    io:format("server for user ~p listening on port ~p.~n", [User, Port]),
    {ok, #interface_state{simid = SimId, port = Port, user = User, lsock = LSock, buffer=[], client=none}}.

handle_info({tcp, Socket, RawData}, State) ->
     New_state = es_lib_tcp:parse_packet(Socket, RawData, State),
    {noreply, New_state};
    
handle_info({tcp_closed, _Socket}, State) ->
    Port = State#interface_state.port,
    Old_sock = State#interface_state.lsock,
    gen_tcp:close(Old_sock),
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}, {reuseaddr, true}]),
    {ok, Sock} = gen_tcp:accept(LSock),
    {noreply, State#interface_state{lsock = LSock, client = Sock, buffer=[]}}.
    
handle_call({get, port}, _From, #interface_state{lsock = LSock} = State) ->
    {ok, Port} = inet:port(LSock),
    {reply, Port, State};

handle_call({get, client_info}, _From, #interface_state{client = Client, user = User} = State)->
    {ok, Remote} = inet:peername(Client),
    {ok, Local} = inet:sockname(Client),
    {reply, {User, Remote, Local}, State}.

handle_cast({listen}, #interface_state{lsock = LSock} = State) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    {noreply, State#interface_state{client = Sock, buffer=[]}};

handle_cast(stop, State) ->
    Lsock = State#interface_state.lsock,
    gen_tcp:close(Lsock),
    Rsock = State#interface_state.rsock,
    gen_tcp:close(Rsock),
    {stop, normal, State}.

%handle_cast(_Msg, State) -> {noreply, State}.
%handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%==================================================================
%%% Internal functions
%%%==================================================================

call(#interface_state{simid = SimId}, {get, Server, Param}) ->
    gen_server:call({global, {SimId, Server}}, {get, Param});
call(#interface_state{simid = SimId}, {get, Server, Param, Args}) ->
    gen_server:call({global, {SimId, Server}}, {get, Param, Args});
call(#interface_state{simid = SimId}, {set, Server, Param, Args}) ->
    gen_server:call({global, {SimId, Server}}, {set, Param, Args});
call(#interface_state{simid = SimId}, {cast, Server, Param, Args}) ->
    gen_server:cast({global, {SimId, Server}}, {set, Param, Args});
call(#interface_state{simid = SimId}, {action, Server, Param}) ->
    gen_server:call({global, {SimId, Server}}, {action, Param});
call(#interface_state{simid = SimId}, {action, Server, Param, Args}) ->
    gen_server:call({global, {SimId, Server}}, {action, Param, Args});

call(#interface_state{simid = SimId}, {stop_sim, SimId}) ->
    es_simulator_tracker_server:stop_simulator(SimId);

call(#interface_state{simid = SimId}, {ask, sim_info}) ->
    es_simulator_tracker_server:sim_info(SimId);
call(_, {ask, sim_info, SimId}) ->
    es_simulator_tracker_server:sim_info(SimId);

call(#interface_state{simid = SimId} = State, {ask, sim_clients}) ->
    call(State, {ask, sim_clients, SimId});
call(#interface_state{simid = SimId}, {ask, sim_clients, SimId}) ->
    es_simulator_tracker_server:sim_clients(SimId);
call(_, {ask, sim_clients, SimId}) ->
    es_simulator_tracker_server:sim_clients(SimId);

call(_, {ask, list_sims}) ->
    es_simulator_tracker_server:list_sims().
