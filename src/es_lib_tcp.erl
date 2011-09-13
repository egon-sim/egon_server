%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Library for parsing IP/TCP requests. It is written as
%%%      gen_server for purposes of unit testing only. It is not used
%%%      as gen_server but as an library.
%%% @end
%%%------------------------------------------------------------------
-module(es_lib_tcp).

-behaviour(gen_server).
-include_lib("include/es_tcp_states.hrl").
-define(SERVER, ?MODULE).
-define(PACKET_FORMAT, "^\\W*({.*})\\W*$").

-record(lib_tcp_state, {port, lsock, buffer, csock}).

% API
-export([
	parse_packet/3
	]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% tests
-export([
	unit_test/0,
	integration_test/0
	]).

%%%==================================================================
%%% API
%%%==================================================================

parse_packet(Socket, RawData, State) ->
    {Calls_ready, Calls, New_state} = append_data(State, RawData),
    if
        Calls_ready =:= true ->
	    exec_calls(New_state, Socket, Calls),
	    New_state;
	true ->
	    New_state
    end.

%%%==================================================================
%%% Internal functions
%%%==================================================================

append_data(State, RawData) ->
    Buffer = get_buffer(State),
    {Newline, [CleanData]} = re:run(RawData, "^([^\\R]+)\\R*$", [{capture, [1], list}]),
    if
        Newline =:= match ->
	    Buffer1 = Buffer ++ CleanData,
    	    Args = parse_call(Buffer1),
	    {true, Args, set_buffer(State, [])};
	true ->
	    {false, none, set_buffer(State,  Buffer ++ RawData)}
    end.

get_buffer(#interface_state{} = State) ->
    State#interface_state.buffer;
get_buffer(#connection_state{} = State) ->
    State#connection_state.buffer;
get_buffer(#lib_tcp_state{} = State) ->
    State#lib_tcp_state.buffer.

set_buffer(#interface_state{} = State, Buffer) ->
    State#interface_state{buffer = Buffer};
set_buffer(#connection_state{} = State, Buffer) ->
    State#connection_state{buffer = Buffer};
set_buffer(#lib_tcp_state{} = State, Buffer) ->
    State#lib_tcp_state{buffer = Buffer}.

parse_call(Buffer) ->
%    io:format("~p~n", [Buffer]),
    {match, [Tuple]} =  re:run(Buffer, ?PACKET_FORMAT, [{capture, [1], list}]),
%    io:format("~p~n", [Tuple]),
    {ok, Tokens, _Line} = erl_scan:string("[" ++ Tuple ++ "]."),
%    io:format("~p~n", [Tokens]),
    {ok, Args} = erl_parse:parse_term(Tokens),
%    io:format("~p~n", [Args]),
    Args.

exec_calls(State, Socket, Args) ->
    Results = lists:map(fun(A) -> exec_call(State, A) end, Args),
    Len = length(Results),
    if
        Len > 1 ->
	    Response = Results;
	Len =:= 1 ->
	    [Response] = Results;
	true ->
	    Response = {error, response_length_negative}
    end,
    gen_tcp:send(Socket, io_lib:fwrite("~p", [Response])).

exec_call(#interface_state{} = State, Args) ->
    es_interface_server:call(State, Args);

exec_call(#connection_state{} = State, Args) ->
    es_connection_server:call(State, Args);

exec_call(#lib_tcp_state{} = State, Args) ->
    call(State, Args).


call(#lib_tcp_state{} = _State, {M, F, A}) ->
    apply(M, F, A).

%%%==================================================================
%%% gen_server callbacks (used only for testing purposes)
%%%==================================================================
init([Port]) -> 
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    {ok, #lib_tcp_state{port = Port, lsock = LSock, buffer=[]}, 0}.

handle_info({tcp, Socket, RawData}, State) ->
    New_state = parse_packet(Socket, RawData, State),
    {noreply, New_state};
    
handle_info({tcp_closed, _Socket}, State) ->
    Port = State#lib_tcp_state.port,
    Old_sock = State#lib_tcp_state.lsock,
    gen_tcp:close(Old_sock),
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    {ok, CSock} = gen_tcp:accept(LSock),
    io:format("socket restarted.~n"),
    {noreply, State#lib_tcp_state{lsock = LSock, csock = CSock, buffer=[]}};
    
handle_info(timeout, #lib_tcp_state{lsock = LSock} = State) ->
    {ok, CSock} = gen_tcp:accept(LSock),
    {noreply, State#lib_tcp_state{csock = CSock}}.

handle_cast(stop, State) ->
    io:format("Closing sockets... "),
    LSock = State#lib_tcp_state.lsock,
    CSock = State#lib_tcp_state.csock,
    gen_tcp:close(LSock),
    gen_tcp:close(CSock),
    io:format("done.~n"),
    {stop, normal, State}.


handle_call(_Request, _From, State) -> {reply, ok, State}.
%handle_cast(_Msg, State) -> {noreply, State}.
%handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%==================================================================
%%% Test functions
%%%==================================================================
-include_lib("include/es_common.hrl").

start_link(Port) ->
    gen_server:start_link(?MODULE, [Port], []).

stop_link() ->
    gen_server:cast(?SERVER, stop).

send(Client_sock, Call) ->
    gen_tcp:send(Client_sock, Call),
    gen_tcp:recv(Client_sock, 0, 2000).

unit_test() -> ok.
unused() ->
%unit_test() ->
    Port = 1055,
    {ok, _} = start_link(Port),

    {ok,Client_sock} = gen_tcp:connect(localhost,Port,[{active,false}, {packet,raw}]),
    
    {ok, "nonode@nohost"} = send(Client_sock,"{erlang, node, []}"),

    {ok, "8.0"} = send(Client_sock,"{math, pow, [2, 3]}"),
    
    {ok, "[nonode@nohost,8.0]"} = send(Client_sock,"{erlang, node, []},{math, pow, [2, 3]}"),

    gen_tcp:close(Client_sock),
    stop_link(),
    ok.


integration_test() ->
% see egon_client:client_test/0
    ok.
