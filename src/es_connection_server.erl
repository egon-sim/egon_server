-module(es_connection_server).
-include_lib("include/es_common.hrl").
-include_lib("include/es_tcp_states.hrl").
-behaviour(gen_server).
-export([call/2, start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, process_data/4, sim_info/1, sim_clients/1, list_sims/0]).

start_link(Port) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

init([Port]) -> 
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
%    io:format("server listening...~n"),
    {ok, #connection_state{port = Port, lsock = LSock, buffer=[]}, 0}.

handle_info({tcp, Socket, RawData}, State) ->
%    io:format("~w ~n", [RawData]),
%    io:format("server received a packet~n"),

%    New_state = process_data(RawData, State, Socket),

    process_flag(trap_exit, true),
    From = proc_lib:spawn_link(?MODULE, process_data, [RawData, State, Socket, self()]),
%    io:format("spawned~n"),
    receive
        {ok, From, Reply} ->
%	     io:format("received ok~n"),
	     New_state = Reply,
	     receive
	         {'EXIT', From, normal} ->
%		     io:format("received normal EXIT~n"),
		     ok
	     end;
        {'EXIT', From, Reason} ->
%	     io:format("received EXIT~n"),
	     gen_tcp:send(Socket, io_lib:fwrite("~p", [{error, Reason}])),
	     New_state = State#connection_state{buffer = []}
    end,
    process_flag(trap_exit, false),
%    io:format("end packet parse~n"),
    {noreply, New_state};
    
handle_info({tcp_closed, _Socket}, State) ->
%    io:format("starting: socket closed.~n"),
    Port = State#connection_state.port,
    Old_sock = State#connection_state.lsock,
    gen_tcp:close(Old_sock),
%    io:format("socket closed.~n"),
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
%    io:format("socket listening.~n"),
    {ok, _Sock} = gen_tcp:accept(LSock),
    io:format("socket restarted.~n"),
    {noreply, State#connection_state{lsock = LSock, buffer=[]}};
%    {noreply, State};
    
handle_info(timeout, #connection_state{lsock = LSock} = State) ->
%    io:format("accepting... "),
    {ok, _Sock} = gen_tcp:accept(LSock),
%    io:format("accepted~n"),
    {noreply, State}.

handle_cast(stop, State) -> {stop, normal, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.
%handle_cast(_Msg, State) -> {noreply, State}.
%handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_data(RawData, State, _Socket, Parent) ->
    New_state = process_data(RawData, State, _Socket),
    Parent ! {ok, self(), New_state}.

process_data(RawData, State, Socket) ->
%    io:format("~p~n", [RawData]),
    Buffer = State#connection_state.buffer,
    {Newline, [CleanData]} = re:run(RawData, "^([^\\R]+)\\R*$", [{capture, [1], list}]),
    if
        Newline =:= match ->
	    Buffer1 = Buffer ++ CleanData,
	    New_state = State#connection_state{buffer = Buffer1},
	    es_lib_tcp:exec_call(New_state, Socket),
	    Buffer2 = [];
	true ->
	    Buffer2 = Buffer ++ RawData
    end,
    State#connection_state{buffer = Buffer2}.

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
call(_State, Req) ->
    {unknown_request, Req}.



start_new_simulator(Params) ->
%    io:format("starting children... "),
    case gen_server:call(es_simulator_tracker_server, {start_simulator, Params}) of
        {ok, SimId} ->
	    [_, _, User] = Params,
            connect_to_simulator([SimId, User]);
	{error, shutdown} -> 
	    {error_starting_child};
	Other -> 
	    {unknown_error, Other}
    end.

connect_to_simulator(Params) ->
    [SimId|_] = Params,
    {ok, [{SimId, _, Port}]} = gen_server:call(es_simulator_tracker_server, {connect_to_simulator, Params}),
    {connected, Port}.

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
