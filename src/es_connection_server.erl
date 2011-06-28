-module(es_connection_server).
-include_lib("include/es_common.hrl").
-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, process_data/4]).
-record(connection_state, {port, simulators, buffer, lsock}).

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
	     gen_tcp:send(Socket, io_lib:fwrite("~p~n", [{error, Reason}])),
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

handle_call({relay_port, Socket, Port}, _From, State) -> 
    Result = {started, Port},
    gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Result])),
%    io:format("Server sent: ~w~n", [Result]),
    {reply, ok, State}.

%handle_call(_Request, _From, State) -> {reply, ok, State}.
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
    Buffer = State#connection_state.buffer,
    {Newline, [CleanData]} = re:run(RawData, "^([^\\R]+)\\R*$", [{capture, [1], list}]),
    if
        Newline =:= match ->
	    New_state = State#connection_state{buffer = Buffer ++ CleanData},
	    exec_call(New_state, _Socket),
	    S1 = State#connection_state{buffer = []};
	true ->
	    S1 = State#connection_state{buffer = Buffer ++ RawData}
    end,
    S1.

exec_call(State, Socket) ->
    Buffer = State#connection_state.buffer,
%    io:format("~p~n", [Buffer]),
    {match, [Tuple]} =  re:run(Buffer, "^\\W*({.*})\\W*$", [{capture, [1], list}]),
%    io:format("~p~n", [Tuple]),
    {ok, Tokens, _Line} = erl_scan:string("[" ++ Tuple ++ "]."),
%    io:format("~p~n", [Tokens]),
    {ok, [Args]} = erl_parse:parse_term(Tokens),

    case Args of
        {ask, start_new_simulator} ->
	    start_new_simulator(Socket);
        {ask, connect_to_simulator, Sim} ->
	    connect_to_simulator(Sim)
    end,
    ok.


start_new_simulator(Reply_socket) ->
%    io:format("starting children... "),
    gen_server:call(es_simulator_tracker_server, {start_simulator, {reply_sock, Reply_socket}}),
%    io:format("done.~n"),
    ok.

connect_to_simulator(Sim) ->
    {connected_to, Sim}.

