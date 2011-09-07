-module(es_interface_server).
-include_lib("include/es_common.hrl").
-include_lib("include/es_tcp_states.hrl").
-behaviour(gen_server).
-export([call/2, start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, process_data/4]).

start_link(SimId, User) -> gen_server:start_link(?MODULE, [SimId, User], []).

init([SimId, User]) -> 
    {ok, LSock} = gen_tcp:listen(0, [{active, true}]),
    {ok, Port} = inet:port(LSock),
    io:format("server for user ~p listening on port ~p.~n", [User, Port]),
    {ok, #interface_state{simid = SimId, port = Port, user = User, lsock = LSock, buffer=[], client=none}}.

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
	     gen_tcp:send(Socket, io_lib:fwrite("~p", [{error, Reason}])),
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
    {ok, Sock} = gen_tcp:accept(LSock),
%    io:format("socket restarted.~n"),
    {noreply, State#interface_state{lsock = LSock, client = Sock, buffer=[]}}.
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

handle_call({get, client_info}, _From, #interface_state{client = Client, user = User} = State)->
    {ok, Remote} = inet:peername(Client),
    {ok, Local} = inet:sockname(Client),
    {reply, {User, Remote, Local}, State}.

handle_cast({listen}, #interface_state{lsock = LSock} = State) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    {noreply, State#interface_state{client = Sock, buffer=[]}};

handle_cast(stop, State) -> {stop, normal, State}.

%handle_cast(_Msg, State) -> {noreply, State}.
%handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_data(RawData, State, Socket, Parent) ->
    New_state = process_data(RawData, State, Socket),
    Parent ! {ok, self(), New_state}.

process_data(RawData, State, Socket) ->
%    io:format("~p~n", [RawData]),
    Buffer = State#interface_state.buffer,
    {Newline, [CleanData]} = re:run(RawData, "^([^\\R]+)\\R*$", [{capture, [1], list}]),
    if
        Newline =:= match ->
	    Buffer1 = Buffer ++ CleanData,
	    New_state = State#interface_state{buffer = Buffer1},
	    es_lib_tcp:exec_call(New_state, Socket),
	    Buffer2 = [];
	true ->
	    Buffer2 = Buffer ++ RawData
    end,
    State#interface_state{buffer = Buffer2}.

call(#interface_state{simid = SimId}, {action, es_clock_server, start}) ->
    gen_server:call({global, {SimId, es_clock_server}}, {start_ticking});
call(#interface_state{simid = SimId}, {action, es_clock_server, stop}) ->
    gen_server:call({global, {SimId, es_clock_server}}, {stop_ticking});

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

call(#interface_state{simid = SimId} = State, {ask, sim_clients}) ->
    call(State, {ask, sim_clients, SimId});
call(#interface_state{simid = SimId}, {ask, sim_clients, SimId}) ->
    es_simulator_tracker_server:sim_clients(SimId);
call(_, {ask, sim_clients, SimId}) ->
    es_simulator_tracker_server:sim_clients(SimId);

call(_, {ask, list_sims}) ->
    es_connection_server:list_sims().
