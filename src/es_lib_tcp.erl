%%%-------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Library for parsing IP/TCP requests. It is written as
%%%      gen_server for purposes of unit testing only. It is not used
%%%      as gen_server but as an library.
%%% @end
%%%-------------------------------------------------------------------
-module(es_lib_tcp).

-behaviour(gen_server).
-include_lib("include/es_tcp_states.hrl").
-define(SERVER, ?MODULE).
-define(PACKET_FORMAT, "^\\W*({.*})\\W*$").

-record(lib_tcp_state, {port, lsock, client, buffer, rsock, results}).

% API
-export([
	parse_packet/3
	]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% tests
-export([unit_test/0, integration_test/0]).

%%%===================================================================
%%% API
%%%===================================================================

parse_packet(Socket, RawData, State) ->
%    io:format("~p~n", [RawData]),
    Buffer = get_buffer(State),
    {Newline, [CleanData]} = re:run(RawData, "^([^\\R]+)\\R*$", [{capture, [1], list}]),
    if
        Newline =:= match ->
	    Buffer1 = Buffer ++ CleanData,
    	    Args = parse_call(Buffer1),
	    New_state = set_buffer(State, Buffer1),
	    exec_call(New_state, Socket, Args),
	    Buffer2 = [];
	true ->
	    Buffer2 = Buffer ++ RawData
    end,
    set_buffer(State, Buffer2).

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_buffer(#interface_state{} = State) ->
    State#interface_state.buffer;
get_buffer(#connection_state{} = State) ->
    State#connection_state.buffer.

set_buffer(#interface_state{} = State, Buffer) ->
    State#interface_state{buffer = Buffer};
set_buffer(#connection_state{} = State, Buffer) ->
    State#connection_state{buffer = Buffer}.

parse_call(Buffer) ->
%    io:format("~p~n", [Buffer]),
    {match, [Tuple]} =  re:run(Buffer, ?PACKET_FORMAT, [{capture, [1], list}]),
%    io:format("~p~n", [Tuple]),
    {ok, Tokens, _Line} = erl_scan:string("[" ++ Tuple ++ "]."),
%    io:format("~p~n", [Tokens]),
    {ok, [Args]} = erl_parse:parse_term(Tokens),
%    io:format("~p~n", [Args]),
    Args.

exec_call(#interface_state{} = State, Socket, Args) ->
    Result = es_interface_server:call(State, Args),
    gen_tcp:send(Socket, io_lib:fwrite("~p", [Result])),
%    io:format("Server sent: ~w~n", [Result]),
    ok;

exec_call(#connection_state{} = State, Socket, Args) ->
    Result = es_connection_server:call(State, Args),
    gen_tcp:send(Socket, io_lib:fwrite("~p", [Result])),
%    io:format("reply: ~p~n", [Result]),
    ok;

exec_call(#lib_tcp_state{} = State, Socket, Args) ->
    Result = call(State, Args),
    gen_tcp:send(Socket, io_lib:fwrite("~p", [Result])),
%    io:format("reply: ~p~n", [Result]),
    ok.

call(State, {M, F, A}) ->
    Result = apply(M, F, A),
    Results = State#lib_tcp_state.results,
    New_state = State#lib_tcp_state{results = Results ++ Result},
    {New_state, Result}.

%%%===================================================================
%%% gen_server callbacks (used only for testing purposes)
%%%===================================================================
init([Port]) -> 
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    {ok, #lib_tcp_state{port = Port, lsock = LSock, buffer=[], results = []}, 0}.

handle_info({tcp, Socket, RawData}, State) ->
    New_state = parse_packet(Socket, RawData, State),
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

handle_call({get, results}, _From, State) -> {reply, State#lib_tcp_state.results, State}.

%handle_call(_Request, _From, State) -> {reply, ok, State}.
%handle_cast(_Msg, State) -> {noreply, State}.
%handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Test functions
%%%===================================================================
-include_lib("include/es_common.hrl").

start_link(Port) ->
    gen_server:start_link(?SERVER, ?MODULE, [Port], []).

stop_link() ->
    gen_server:call(?SERVER, stop).

results() ->
    gen_server:call(?SERVER, {get, results}).

unit_test() ->
    Port = 1055,
    {ok, _} = start_link(Port),

    {ok,Client_sock} = gen_tcp:connect(localhost,Port,[{active,false}, {packet,raw}]),
    gen_tcp:send(Client_sock,{erlang, node, []}),
    {ok, "nonode@nohost"} = gen_tcp:recv(Client_sock, 0, 2000),

    gen_tcp:send(Client_sock,{math, pow, [2, 3]}),
    {ok, "8.0"} = gen_tcp:recv(Client_sock, 0, 2000),

    [nonode@nohost, 8.0] = results(),
    stop_link(),
    ok.


integration_test() ->
    ok.