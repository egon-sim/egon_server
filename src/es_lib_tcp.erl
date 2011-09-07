-module(es_lib_tcp).

-include_lib("include/es_tcp_states.hrl").

-export([exec_call/2]).

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
