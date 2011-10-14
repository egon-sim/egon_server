-module(es_connection_server).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/es_tcp_states.hrl").
-behaviour(gen_server).
-export([call/2, start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, sim_info/1, sim_clients/1, list_sims/0]).

start_link(Port) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

init([Port]) -> 
    Retval = gen_tcp:listen(Port, [{active, true}]),
    case Retval of
        {ok, LSock} ->
    	    io:format("Connection server started normally.~n"),
	    {ok, #connection_state{port = Port, lsock = LSock, buffer=[]}, 0};
	{error, eaddrinuse} ->
    	    io:format("Connection server unable to listen: port ~p is in use.~n", [Port]),
    	    error_handler:error_report("Connection server unable to listen: port ~p is in use.~n", [Port]),
	    {stop, eaddrinuse}
    end.

handle_info({tcp, Socket, RawData}, State) ->
    New_state = es_lib_tcp:parse_packet(Socket, RawData, State),
    {noreply, New_state};
    
handle_info({tcp_closed, _Socket}, State) ->
    Port = State#connection_state.port,
    Old_sock = State#connection_state.lsock,
    gen_tcp:close(Old_sock),
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    {ok, _Sock} = gen_tcp:accept(LSock),
    io:format("socket restarted.~n"),
    {noreply, State#connection_state{lsock = LSock, buffer=[]}};
    
handle_info(timeout, #connection_state{lsock = LSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    {noreply, State}.

handle_cast(stop, State) -> {stop, normal, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.
%handle_cast(_Msg, State) -> {noreply, State}.
%handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

call(_State, {ask, start_new_simulator, Params}) ->
    start_new_simulator(Params);
call(_State, {ask, connect_to_simulator, Params}) ->
    connect_to_simulator(Params);
call(_State, {ask, stop_simulator, SimId}) ->
    stop_simulator(SimId);
call(_State, {ask, sim_info}) ->
    sim_info();
call(_State, {ask, sim_info, SimId}) ->
    sim_info(SimId);
call(_State, {ask, sim_clients}) ->
    sim_clients();
call(_State, {ask, sim_clients, SimId}) ->
    sim_clients(SimId);
call(_State, {ask, list_sims}) ->
    list_sims();
call(_State, {stop_sim, SimId}) ->
    gen_server:call(es_simulator_tracker_server, {stop_simulator, SimId});
call(_State, Req) ->
    {unknown_request, Req}.


start_new_simulator(Params) ->
%    io:format("starting children... "),
    case gen_server:call(es_simulator_tracker_server, {start_simulator, Params}) of
        {ok, SimId} ->
	    [_, _, User] = Params,
            connect_to_simulator([SimId, User]);
	{error, shutdown} -> 
	    {error_starting_child}; % TODO: {error, starting_child_failed}
	Other -> 
	    {unknown_error, Other} % TODO: {error, Other}
    end.

connect_to_simulator(Params) ->
    [SimId|_] = Params,
    case gen_server:call(es_simulator_tracker_server, {connect_to_simulator, Params}) of
        {ok, [{SimId, _, Port}]} ->
	    {connected, SimId, Port};
	Other ->
	    Other
    end.

stop_simulator(SimId) ->
    {ok, stopped} = gen_server:call(es_simulator_tracker_server, {stop_simulator, SimId}),
    {ok, stopped}.

sim_info() ->
    not_connected_to_a_simulator.

sim_info(SimId) ->
    {ok, Reply} = gen_server:call(es_simulator_tracker_server, {get, sim_info, SimId}),
    Reply.

sim_clients() ->
    not_connected_to_a_simulator.

sim_clients(SimId) ->
    {ok, Reply} = gen_server:call(es_simulator_tracker_server, {get, sim_clients, SimId}),
    Reply.

list_sims() ->
    {ok, List} = gen_server:call(es_simulator_tracker_server, {get, simulators}),
    List.
