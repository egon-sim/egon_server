-module(egon_client).
-include_lib("include/es_common.hrl").
-import(re).
-behaviour(gen_server).
-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(client_state, {host, port, sock}).
-compile(export_all).

start_link(Host, Port) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Port], []).

init([Host, PortNo]) -> 
    {ok,Sock} = gen_tcp:connect(Host,PortNo,[{active,false}, {packet,raw}]),
    {ok, #client_state{sock = Sock, host = Host, port = PortNo}}.

handle_call({send, Message}, _From, State) -> 
    Sock = State#client_state.sock,
    gen_tcp:send(Sock,Message),
    {ok, A} = gen_tcp:recv(Sock, 0, 2000),
    {match, [B]} = re:run(A, "^([^\\n]+)\\R*$", [{capture, [1], list}]),
    {reply, B, State};

handle_call({switch_port, PortNo}, _From, State) -> 
    Sock = State#client_state.sock,
    gen_tcp:close(Sock),
    Host = State#client_state.host,
    {ok, New_sock} = gen_tcp:connect(Host,PortNo,[{active,false}, {packet,raw}]),
    {reply, ok, State#client_state{sock = New_sock, port = PortNo}};

handle_call(stop, _From, State) -> 
    Sock = State#client_state.sock,
    gen_tcp:close(Sock),
    {stop, normal, stopped, State}.

%handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    start_link("localhost", 1055).

new_sim() ->
    case parse(send("{ask, start_new_simulator}")) of
        {connected, Port} ->
	    gen_server:call(?MODULE, {switch_port, Port});
	{error_starting_child} ->
	    io:format("Starting new simulator failed.~n"),
	    {error, shutdown};
	{unknown_error, Error} ->
	    io:format("Unknown error: ~p.~n", [Error]),
	    {unknown_error, Error};
	Other ->
	    io:format("Unknown error: ~p.~n", [Other]),
	    Other
    end.

conn_to_sim(Id) ->
    Retv = send("{ask, connect_to_simulator, " ++ integer_to_list(Id) ++ "}"),
    io:format("Retv: ~p.~n", [Retv]),
    case parse(Retv) of
        {connected, Port} ->
	    io:format("Simulator with id ~p is listening on port ~p.", [Id, Port]),
	    gen_server:call(?MODULE, {switch_port, Port});
	{error, no_such_sim} ->
	    io:format("There is no simulator with id " ++ integer_to_list(Id) ++ ".~n"),
	    {error, no_such_sim};
	{unknown_error, Error} ->
	    io:format("Unknown error: ~p.~n", [Error]),
	    {unknown_error, Error};
	Other ->
	    io:format("Unknown error: ~p.~n", [Other]),
	    Other
    end.

call(Message) ->
    send(Message).

send(Message) ->
    gen_server:call(?MODULE, {send, Message}).

stop() ->
    gen_server:call(?MODULE, stop).


parse(Buffer) ->
    {match, [Tuple]} =  re:run(Buffer, "^\\W*({.*})\\W*$", [{capture, [1], list}]),
%    io:format("~p~n", [Tuple]),
    {ok, Tokens, _Line} = erl_scan:string("[" ++ Tuple ++ "]."),
%    io:format("~p~n", [Tokens]),
    {ok, [Args]} = erl_parse:parse_term(Tokens),
    Args.

client_test() ->
    ok = egon_server:start(),
    {ok, _} = egon_client:start(),
    ok = egon_client:new_sim(),
    "305.0" = egon_client:call("{get, es_core_server, tavg}"),
    "ok" = egon_client:call("{action, es_rod_position_server, step_in}"),
    "304.9416710346633" = egon_client:call("{get, es_core_server, tavg}"),
    egon_server:stop(),
    egon_client:stop(),
    ok.