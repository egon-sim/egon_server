-module(es_set_server).
-include_lib("include/es_common.hrl").
-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(set_state, {port, lsock, buffer}).

start_link(Port) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

init([Port]) -> 
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
%    io:format("server listening...~n"),
    {ok, #set_state{port = Port, lsock = LSock, buffer=[]}, 0}.

handle_info({tcp, Socket, RawData}, State) ->
%    io:format("~w ~n", [RawData]),
%    io:format("server received a packet~n"),
    New_state = process_data(RawData, State, Socket),
%    io:format("end packet parse~n"),
    {noreply, New_state};
    
handle_info({tcp_closed, _Socket}, State) ->
    io:format("socket closed.~n"),
    {noreply, State};
    
handle_info(timeout, #set_state{lsock = LSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    {noreply, State}.

handle_cast(stop, State) -> {stop, normal, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.
%handle_cast(_Msg, State) -> {noreply, State}.
%handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_data(RawData, State, _Socket) ->
%    io:format("~p~n", [RawData]),
    Buffer = State#set_state.buffer,
    {Newline, [CleanData]} = re:run(RawData, "^([^\\R]+)\\R+$", [{capture, [1], list}]),
    if
        Newline =:= match ->
	    New_state = State#set_state{buffer = Buffer ++ CleanData},
	    exec_call(New_state, _Socket),
	    S1 = State#set_state{buffer = []};
	true ->
	    S1 = State#set_state{buffer = Buffer ++ RawData}
    end,
    S1.

exec_call(State, Socket) ->
    Buffer = State#set_state.buffer,
%    io:format("~p~n", [Buffer]),
    {match, [Tuple]} =  re:run(Buffer, "^\\W*({.*})\\W*$", [{capture, [1], list}]),
%    io:format("~p~n", [Tuple]),
    {ok, Tokens, _Line} = erl_scan:string("[" ++ Tuple ++ "]."),
%    io:format("~p~n", [Tokens]),
    {ok, [Args]} = erl_parse:parse_term(Tokens),
%    io:format("~p~n", [Args]),
    Result = call(Args),
    gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Result])),
%    io:format("Server sent: ~w~n", [Result]),
    ok.

call({Server, Param, Args}) ->
    gen_server:call(Server, {set, Param, Args});
call({Server, Param}) ->
    gen_server:call(Server, {set, Param}).
