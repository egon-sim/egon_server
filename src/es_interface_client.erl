-module(es_interface_client).
-import(re).
-compile(export_all).

start() ->
    Host = "localhost",
    PortNo1 = 1056,
    PortNo2 = 1057,
    {ok,Sock1} = gen_tcp:connect(Host,PortNo1,[{active,false}, {packet,raw}]),
    {ok,Sock2} = gen_tcp:connect(Host,PortNo2,[{active,false}, {packet,raw}]),
    {Sock1, Sock2}.

get({Sock, _},Message) ->
    gen_tcp:send(Sock,Message),
    {ok, A} = gen_tcp:recv(Sock, 0, 2000),
    {match, [B]} = re:run(A, "^([^\\n]+)\\R*$", [{capture, [1], list}]),
    B.

act({_,Sock},Message) ->
    gen_tcp:send(Sock,Message),
    {ok, A} = gen_tcp:recv(Sock, 0, 2000),
    {match, [B]} = re:run(A, "^([^\\n]+)\\R*$", [{capture, [1], list}]),
    B.

stop({Sock1, Sock2}) ->
    gen_tcp:close(Sock1),
    gen_tcp:close(Sock2).
