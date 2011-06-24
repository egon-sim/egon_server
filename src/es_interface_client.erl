-module(es_interface_client).
-include_lib("include/es_common.hrl").
-import(re).
-compile(export_all).

start() ->
    Host = "localhost",
    PortNo = 1056,
    {ok,Sock} = gen_tcp:connect(Host,PortNo,[{active,false}, {packet,raw}]),
    Sock.

send_without_recv(Sock, Message) ->
    gen_tcp:send(Sock,Message),
    stop(Sock),
    {ok, A} = gen_tcp:recv(Sock, 0, 2000),
    {match, [B]} = re:run(A, "^([^\\n]+)\\R*$", [{capture, [1], list}]),
    B.

send_with_close(Sock, Message) ->
    gen_tcp:send(Sock,Message),
    {ok, A} = gen_tcp:recv(Sock, 0, 2000),
    stop(Sock),
    {match, [B]} = re:run(A, "^([^\\n]+)\\R*$", [{capture, [1], list}]),
    B.

send(Sock, Message) ->
    gen_tcp:send(Sock,Message),
    {ok, A} = gen_tcp:recv(Sock, 0, 2000),
    {match, [B]} = re:run(A, "^([^\\n]+)\\R*$", [{capture, [1], list}]),
    B.

stop(Sock) ->
    gen_tcp:close(Sock).
