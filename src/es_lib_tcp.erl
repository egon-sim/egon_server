-module(es_lib_tcp).

-include_lib("include/es_tcp_states.hrl").

-export([parse_packet/3, process_data/4]).

parse_packet(Socket, RawData, #interface_state{} = State) ->
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
	     New_state = State#interface_state{buffer = []}
    end,
    process_flag(trap_exit, false),
%    io:format("end packet parse~n"),
    New_state;

parse_packet(Socket, RawData, #connection_state{} = State) ->
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
    New_state.

process_data(RawData, State, Socket, Parent) ->
    New_state = process_data(RawData, State, Socket),
    Parent ! {ok, self(), New_state}.

process_data(RawData, #connection_state{buffer = Buffer} = State, Socket) ->
%    io:format("~p~n", [RawData]),
    {Newline, [CleanData]} = re:run(RawData, "^([^\\R]+)\\R*$", [{capture, [1], list}]),
    if
        Newline =:= match ->
	    Buffer1 = Buffer ++ CleanData,
	    New_state = State#connection_state{buffer = Buffer1},
	    exec_call(New_state, Socket),
	    Buffer2 = [];
	true ->
	    Buffer2 = Buffer ++ RawData
    end,
    State#connection_state{buffer = Buffer2};

process_data(RawData, #interface_state{buffer = Buffer} = State, Socket) ->
%    io:format("~p~n", [RawData]),
    {Newline, [CleanData]} = re:run(RawData, "^([^\\R]+)\\R*$", [{capture, [1], list}]),
    if
        Newline =:= match ->
	    Buffer1 = Buffer ++ CleanData,
	    New_state = State#interface_state{buffer = Buffer1},
	    exec_call(New_state, Socket),
	    Buffer2 = [];
	true ->
	    Buffer2 = Buffer ++ RawData
    end,
    State#interface_state{buffer = Buffer2}.

parse_call(Buffer) ->
%    io:format("~p~n", [Buffer]),
    {match, [Tuple]} =  re:run(Buffer, "^\\W*({.*})\\W*$", [{capture, [1], list}]),
%    io:format("~p~n", [Tuple]),
    {ok, Tokens, _Line} = erl_scan:string("[" ++ Tuple ++ "]."),
%    io:format("~p~n", [Tokens]),
    {ok, [Args]} = erl_parse:parse_term(Tokens),
%    io:format("~p~n", [Args]),
    Args.

exec_call(#interface_state{buffer = Buffer} = State, Socket) ->
    Args = parse_call(Buffer),
    Result = es_interface_server:call(State, Args),
    gen_tcp:send(Socket, io_lib:fwrite("~p", [Result])),
%    io:format("Server sent: ~w~n", [Result]),
    ok;

exec_call(#connection_state{buffer = Buffer} = State, Socket) ->
    Args = parse_call(Buffer),
    Result = es_connection_server:call(State, Args),
    gen_tcp:send(Socket, io_lib:fwrite("~p", [Result])),
%    io:format("reply: ~p~n", [Result]),
    ok.

-include_lib("include/es_common.hrl").
