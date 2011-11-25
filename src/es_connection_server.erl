-module(es_connection_server).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/es_tcp_states.hrl").
-behaviour(gen_server).
-export([call/2, start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Port) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

init([Port]) -> 
    Retval = gen_tcp:listen(Port, [{active, true}, {reuseaddr, true}]),
    case Retval of
        {ok, LSock} ->
    	    io:format("Connection server started normally.~n"),
	    {ok, #connection_state{port = Port, lsock = LSock, buffer=[]}, 0};
	{error, eaddrinuse} ->
    	    error_logger:error_report(["Connection server unable to listen: port in use.", {port, Port}]),
	    {stop, {eaddrinuse, {port, Port}}}
    end.

handle_info({tcp, Socket, RawData}, State) ->
    New_state = es_lib_tcp:parse_packet(Socket, RawData, State),
    {noreply, New_state};
    
handle_info({tcp_closed, _Socket}, State) ->
    Port = State#connection_state.port,
    Old_sock = State#connection_state.lsock,
    gen_tcp:close(Old_sock),
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}, {reuseaddr, true}]),
    {ok, _Sock} = gen_tcp:accept(LSock),
    io:format("socket restarted.~n"),
    {noreply, State#connection_state{lsock = LSock, buffer=[]}};
    
handle_info(timeout, #connection_state{lsock = LSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    {noreply, State}.

handle_cast(stop, State) ->
    Sock = State#connection_state.lsock,
    gen_tcp:close(Sock),
    {stop, normal, State}.

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
    es_simulator_tracker_server:stop_simulator(SimId);
call(_State, {ask, sim_info}) ->
    not_connected_to_a_simulator;
call(_State, {ask, sim_info, SimId}) ->
    {ok, Reply} = es_simulator_tracker_server:sim_info(SimId),
    Reply;
call(_State, {ask, sim_clients}) ->
    not_connected_to_a_simulator;
call(_State, {ask, sim_clients, SimId}) ->
    {ok, Reply} = es_simulator_tracker_server:sim_clients(SimId),
    Reply;
call(_State, {ask, list_sims}) ->
    {ok, List} = es_simulator_tracker_server:simulators(),
    List;
call(_State, {stop_sim, SimId}) ->
    es_simulator_tracker_server:stop_simulator(SimId);
call(_State, {shutdown_server}) ->
    es_master_server:shutdown();
call(_State, Req) ->
    {unknown_request, Req}.


start_new_simulator([Name, Desc, User]) ->
%    io:format("starting children... "),
    case es_simulator_tracker_server:start_new_simulator(Name, Desc, User) of
        {ok, SimId} ->
            connect_to_simulator([SimId, User]);
	{error, shutdown} -> 
	    {error_starting_child}; % TODO: {error, starting_child_failed}
	Other -> 
	    {unknown_error, Other} % TODO: {error, Other}
    end.

connect_to_simulator([SimId, User]) ->
    case es_simulator_tracker_server:connect_to_simulator(SimId, User) of
        {ok, [{SimId, _, Port}]} ->
	    {connected, SimId, Port};
	Other ->
	    Other
    end.
