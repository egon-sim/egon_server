%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Server which implements an EGON client. Knows how to
%%%      communicate with egon_server and preform some basic
%%%      interactions. Is used to test egon_server from the outside,
%%%      using it's TCP/IP interface.
%%% @end
%%%------------------------------------------------------------------
-module(egon_client).

-import(re).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

% API
-export([
	start_link/3,
	stop_link/0,
	new_sim/2,
	conn_to_sim/1,
	disconnect/0,
	call/1,
	sim_info/0,
	sim_info/1,
	sim_clients/0,
	sim_clients/1,
	list_sims/0
]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% tests
-export([client_test/0]).

% data structures
-record(client_state, {
		      host, % server hostname
		      port, % server port
		      server_sock, % socket connected to es_connection_server
		      simulator_sock, % socket connected to particular server's es_interface_server
		      username % client username
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
start_link(Host, Port, Username) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Port, Username], []).

%%-------------------------------------------------------------------
%% @doc Stops the server.
%%
%% @spec stop_link(SimId::integer()) -> stopped
%% @end
%%-------------------------------------------------------------------
stop_link() ->
    gen_server:call(?SERVER, stop).

%%-------------------------------------------------------------------
%% @doc Starts new simulator.
%%
%% @spec new_sim(Name::string(), Desc::string()) -> {connected,
%%       Port} | {error_starting_child} | {unknown_error, Error}
%% where
%%  Port = integer(),
%%  Error = term()
%% @end
%%-------------------------------------------------------------------
new_sim(Name, Desc) ->
    gen_server:call(?SERVER, {new_sim, Name, Desc}).

%%-------------------------------------------------------------------
%% @doc Connects to a simulator.
%%
%% @spec conn_to_sim(SimId::integer()) -> {connected,
%%       Port} | {error, no_such_sim} | {unknown_error, Error}
%% where
%%  Port = integer(),
%%  Error = term()
%% @end
%%-------------------------------------------------------------------
conn_to_sim(SimId) ->
    gen_server:call(?SERVER, {connect_to_sim, SimId}).

%%-------------------------------------------------------------------
%% @doc Connects to a simulator.
%%
%% @spec disconnect(SimId::integer()) -> ok | not_connected
%% @end
%%-------------------------------------------------------------------
disconnect() ->
    gen_server:call(?MODULE, {disconnect}).

%%-------------------------------------------------------------------
%% @doc Sends an arbitrary message to egon_server.
%%
%% @spec call(Message::string()) -> Result
%% where
%%  Result = term()
%% @end
%%-------------------------------------------------------------------
call(Message) ->
    gen_server:call(?MODULE, {send, Message}).

%%-------------------------------------------------------------------
%% @doc Return simulator_manifest for simulator to which egon_client
%%      is currently connected.
%%
%% @spec sim_info() -> Result
%% where
%%  Result = string()
%% @end
%%-------------------------------------------------------------------
sim_info() ->
    call("{ask, sim_info}").

%%-------------------------------------------------------------------
%% @doc Return simulator_manifest for all simulator with id SimId.
%%
%% @spec sim_info(SimId:integer()) -> Result
%% where
%%  Result = string()
%% @end
%%-------------------------------------------------------------------
sim_info(Id) ->
    call("{ask, sim_info, " ++ integer_to_list(Id) ++ "}").

%%-------------------------------------------------------------------
%% @doc Return list of clients connected to simulator to which
%%      egon_client is currently connected.
%%
%% @spec sim_clients() -> Result
%% where
%%  Result = string()
%% @end
%%-------------------------------------------------------------------
sim_clients() ->
    call("{ask, sim_clients}").

%%-------------------------------------------------------------------
%% @doc Return list of clients connected to simulator with id SimId.
%%
%% @spec sim_clients(SimId::integer()) -> Result
%% where
%%  Result = string()
%% @end
%%-------------------------------------------------------------------
sim_clients(Id) ->
    call("{ask, sim_clients, " ++ integer_to_list(Id) ++ "}").

%%-------------------------------------------------------------------
%% @doc Return list of simulators running on server to which
%%      egon_client is currently connected.
%%
%% @spec list_sims() -> Result
%% where
%%  Result = string()
%% @end
%%-------------------------------------------------------------------
list_sims() ->
    call("{ask, list_sims}").

%%%==================================================================
%%% gen_server callbacks
%%%==================================================================

init([Host, PortNo, Username]) -> 
    {ok,Sock} = gen_tcp:connect(Host,PortNo,[{active,false}, {packet,raw}]),
    {ok, #client_state{server_sock = Sock, simulator_sock = undefined, host = Host, port = PortNo, username = Username}}.

handle_call({send, Message}, _From, State) -> 
    {reply, send(Message, State), State};

handle_call({new_sim, Name, Desc}, _From, State) -> 
    Username = State#client_state.username,
    Params = "[\"" ++ Name ++ "\", \"" ++ Desc ++ "\", \"" ++ Username ++ "\"]",
    case parse(send("{ask, start_new_simulator, " ++ Params ++ "}", State)) of
        {connected, _Port} ->
            {reply, ok, State};
	{error_starting_child} ->
	    io:format("Starting new simulator failed.~n"),
	    {reply, {error, shutdown}, State};
	{unknown_error, Error} ->
	    {reply, {unknown_error, Error}, State};
	Other ->
	    {reply, Other, State}
    end;


handle_call({connect_to_sim, SimId}, _From, State) -> 
    if
        State#client_state.simulator_sock =/= undefined ->
	    {reply, {error, already_connected}, State};
	true ->
	    Username = State#client_state.username,
	    Params = "[" ++ integer_to_list(SimId) ++ ", \"" ++ Username ++ "\"]",
    	    Retv = send("{ask, connect_to_simulator, " ++ Params ++ "}", State),
    	    io:format("Retv: ~p.~n", [Retv]),
    	    case parse(Retv) of
                {connected, Port} ->
	    	    io:format("Simulator with id ~p is listening on port ~p.", [SimId, Port]),
	    	    Host = State#client_state.host,
            	    {ok, New_sock} = gen_tcp:connect(Host,Port,[{active,false}, {packet,raw}]),
    	    	    {reply, ok, State#client_state{simulator_sock = New_sock}};
		{error, no_such_sim} ->
	    	    io:format("There is no simulator with id " ++ integer_to_list(SimId) ++ ".~n"),
	    	    {reply, {error, no_such_sim}, State};
		{unknown_error, Error} ->
	    	    io:format("Unknown error: ~p.~n", [Error]),
	    	    {reply, {unknown_error, Error}, State};
		Other ->
	    	    io:format("Unknown error: ~p.~n", [Other]),
	    	    {reply, Other, State}
	    end
    end;

handle_call({connect_to, PortNo}, _From, State) -> 
    Host = State#client_state.host,
    {ok, New_sock} = gen_tcp:connect(Host,PortNo,[{active,false}, {packet,raw}]),
    {reply, ok, State#client_state{simulator_sock = New_sock}};

handle_call({disconnect}, _From, State) -> 
    if
        State#client_state.simulator_sock =:= undefined ->
	    {reply, not_connected, State};
	true ->
	    Sock = State#client_state.simulator_sock,
    	    gen_tcp:close(Sock),
    	    {reply, ok, State#client_state{simulator_sock = undefined}}
    end;

handle_call({get, username}, _From, State) -> 
    {reply, State#client_state.username, State};

handle_call(stop, _From, State) -> 
    Sock = State#client_state.server_sock,
    gen_tcp:close(Sock),
    Sock1 = State#client_state.simulator_sock,
    if
        Sock1 =/= undefined ->
    	    gen_tcp:close(Sock1);
	true ->
	    ok
    end,
    {stop, normal, stopped, State}.

%handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%==================================================================
%%% Internal functions
%%%==================================================================

parse(Buffer) ->
    {match, [Tuple]} =  re:run(Buffer, "^\\W*({.*})\\W*$", [{capture, [1], list}]),
%    io:format("~p~n", [Tuple]),
    {ok, Tokens, _Line} = erl_scan:string("[" ++ Tuple ++ "]."),
%    io:format("~p~n", [Tokens]),
    {ok, [Args]} = erl_parse:parse_term(Tokens),
    Args.

send(Message, State) ->
    if
        State#client_state.simulator_sock =:= undefined ->
	    Sock = State#client_state.server_sock;
	true ->
	    Sock = State#client_state.simulator_sock
    end,
    gen_tcp:send(Sock,Message),
    {ok, A} = gen_tcp:recv(Sock, 0, 2000),
%    {match, [B]} = re:run(A, "^([^\\n]+)\\R*$", [{capture, [1], list}]),
%    {match, [B]} = re:run(A, "^( .+)$", [{capture, [1], list}]),
    A.

    
%%%==================================================================
%%% Test functions
%%%==================================================================
-include_lib("include/es_common.hrl").

client_test() ->
    ok = egon_server:start(),
    {ok,_} = start_link("localhost", 1055, "Test user"),
    ok = new_sim("Test sim 1", "Simulator for purposes of unit testing"),
    ok = new_sim("Test sim 2", "Simulator for purposes of unit testing"),
    ok = new_sim("Test sim 3", "Simulator for purposes of unit testing"),

    not_connected = disconnect(),

    ok = conn_to_sim(1),
    {error, already_connected} = conn_to_sim(2),
    "305.0" = call("{get, es_core_server, tavg}"),
    "[100,1,612]" = call("[{get, es_turbine_server, power}, {get, es_turbine_server, rate}, {get, es_rod_position_server, control_position_counter}]"),
    "ok" = call("{action, es_rod_position_server, step_in}"),
    "304.9416710346633" = call("{get, es_core_server, tavg}"),
    ok = disconnect(),

    ok = conn_to_sim(2),
    "305.0" = call("{get, es_core_server, tavg}"),
    ok = disconnect(),

    ok = conn_to_sim(1),
    "304.9416710346633" = call("{get, es_core_server, tavg}"),
    ok = disconnect(),

    egon_server:stop(),
    stopped = stop_link(),
    ok.
