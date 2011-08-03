-module(es_interface_server).
-include_lib("include/es_common.hrl").
-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, process_data/4]).
-record(interface_state, {simid, port, lsock, buffer, rsock}).

start_link(SimId) -> gen_server:start_link(?MODULE, [SimId], []).

init([SimId]) -> 
    {ok, LSock} = gen_tcp:listen(0, [{active, true}]),
    {ok, Port} = inet:port(LSock),
    io:format("server listening on port ~p.~n", [Port]),
    {ok, #interface_state{simid = SimId, port = Port, lsock = LSock, buffer=[]}}.

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
	     gen_tcp:send(Socket, io_lib:fwrite("~p~n", [{error, Reason}])),
	     New_state = State#interface_state{buffer = []}
    end,
    process_flag(trap_exit, false),
%    io:format("end packet parse~n"),
    {noreply, New_state};
    
handle_info({tcp_closed, _Socket}, State) ->
%    io:format("starting: socket closed.~n"),
    Port = State#interface_state.port,
    Old_sock = State#interface_state.lsock,
    gen_tcp:close(Old_sock),
%    io:format("socket closed.~n"),
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
%    io:format("socket listening.~n"),
    {ok, _Sock} = gen_tcp:accept(LSock),
%    io:format("socket restarted.~n"),
    {noreply, State#interface_state{lsock = LSock, buffer=[]}}.
%    {noreply, State};
    
%handle_info(timeout, #interface_state{lsock = LSock} = State) ->
%    {ok, Port} = inet:port(LSock),
%    SimId = State#interface_state.simid,
%    gen_server:call(es_simulator_tracker_server, {update_port, SimId, Port}),
%    {ok, _Sock} = gen_tcp:accept(LSock),
%    io:format("accepted"),
%    {noreply, State}.

handle_call({get, port}, _From, #interface_state{lsock = LSock} = State) ->
    {ok, Port} = inet:port(LSock),
    {reply, Port, State};

handle_call({get, client_info}, _From, #interface_state{lsock = LSock} = State) ->
    
    {reply, client_info(LSock), State}.

handle_cast({listen}, #interface_state{lsock = LSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    {noreply, State};

handle_cast(stop, State) -> {stop, normal, State}.

%handle_cast(_Msg, State) -> {noreply, State}.
%handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_data(RawData, State, _Socket, Parent) ->
    New_state = process_data(RawData, State, _Socket),
    Parent ! {ok, self(), New_state}.

process_data(RawData, State, _Socket) ->
%    io:format("~p~n", [RawData]),
    Buffer = State#interface_state.buffer,
    {Newline, [CleanData]} = re:run(RawData, "^([^\\R]+)\\R*$", [{capture, [1], list}]),
    if
        Newline =:= match ->
	    New_state = State#interface_state{buffer = Buffer ++ CleanData},
	    exec_call(New_state, _Socket),
	    S1 = State#interface_state{buffer = []};
	true ->
	    S1 = State#interface_state{buffer = Buffer ++ RawData}
    end,
    S1.

exec_call(State, Socket) ->
    Buffer = State#interface_state.buffer,
%    io:format("~p~n", [Buffer]),
    {match, [Tuple]} =  re:run(Buffer, "^\\W*({.*})\\W*$", [{capture, [1], list}]),
%    io:format("~p~n", [Tuple]),
    {ok, Tokens, _Line} = erl_scan:string("[" ++ Tuple ++ "]."),
%    io:format("~p~n", [Tokens]),
    {ok, [Args]} = erl_parse:parse_term(Tokens),
%    io:format("~p~n", [Args]),
    Result = call(State, Args),
    gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Result])),
%    io:format("Server sent: ~w~n", [Result]),
    ok.

call(#interface_state{simid = SimId}, {get, Server, Param}) ->
    gen_server:call({global, {SimId, Server}}, {get, Param});
call(#interface_state{simid = SimId}, {get, Server, Param, Args}) ->
    gen_server:call({global, {SimId, Server}}, {get, Param, Args});
call(#interface_state{simid = SimId}, {set, Server, Param, Args}) ->
    gen_server:call({global, {SimId, Server}}, {set, Param, Args});
call(#interface_state{simid = SimId}, {action, Server, Param}) ->
    gen_server:call({global, {SimId, Server}}, {action, Param});
call(#interface_state{simid = SimId}, {action, Server, Param, Args}) ->
    gen_server:call({global, {SimId, Server}}, {action, Param, Args});

call(#interface_state{simid = SimId}, {ask, sim_info}) ->
    es_connection_server:sim_info(SimId);
call(_, {ask, sim_info, SimId}) ->
    es_connection_server:sim_info(SimId);

call(#interface_state{simid = SimId}, {ask, sim_clients}) ->
    call(SimId, {ask, sim_clients, SimId});
call(#interface_state{simid = SimId, lsock = LSock}, {ask, sim_clients, SimId}) ->
    client_info(LSock);
call(_, {ask, sim_clients, SimId}) ->
    es_connection_server:sim_clients(SimId);

call(_, {ask, list_sims}) ->
    es_connection_server:list_sims().

client_info(LSock) ->
    {ok, Remote} = inet:peername(LSock),
    {ok, Local} = inet:sockname(LSock),
    {Remote, Local}.
