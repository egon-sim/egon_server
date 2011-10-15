%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Server providing access to core design and other files
%%%      defining model of the power plant.
%%% @end
%%%------------------------------------------------------------------
-module(es_curvebook_server).

-behaviour(gen_server).
-define(SERVER(SimId), {global, {SimId, ?MODULE}}).

% API
-export([
	start_link/1,
	start_link/2,
	stop_link/1
	]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% data structures
-record(curvebook_state, {
			 simid,
			 power_defect,
			 boron_worth,
			 mtc,
			 critical_boron,
			 rod_worth,
			 rod_control_speed_program,
			 pls
			 }).


%%%==================================================================
%%% API
%%%==================================================================

%%-------------------------------------------------------------------
%% @doc Starts the server.
%%
%% @spec start_link(SimId::integer()) -> {ok, Pid}
%% where
%%  Pid = pid()
%% @end
%%-------------------------------------------------------------------
start_link(SimId) ->
    gen_server:start_link(?SERVER(SimId), ?MODULE, [SimId], []).

%%-------------------------------------------------------------------
%% @doc Starts the server for testing purposes.
%%
%% @spec start_link(SimId::integer(), Dir::string()) -> {ok, Pid}
%% where
%%  Pid = pid()
%% @end
%%-------------------------------------------------------------------
start_link(SimId, Dir) ->
    gen_server:start_link(?SERVER(SimId), ?MODULE, [SimId, Dir], []).

%%-------------------------------------------------------------------
%% @doc Stops the server.
%%
%% @spec stop_link(SimId::integer()) -> stopped
%% @end
%%-------------------------------------------------------------------
stop_link(SimId) ->
    gen_server:call(?SERVER(SimId), stop).


%%%==================================================================
%%% gen_server callbacks
%%%==================================================================

init([SimId]) -> 
    case fill_curvebook() of
        {Power_defect, Boron_worth, MTC, Critical_boron, Rod_worth, Rod_control_speed_program, Pls} ->
            {ok, #curvebook_state{simid = SimId, power_defect = Power_defect, boron_worth = Boron_worth, mtc = MTC, critical_boron = Critical_boron, rod_worth = Rod_worth, rod_control_speed_program = Rod_control_speed_program, pls = Pls}};
        {error, application_undefined} ->
	    error_logger:info_report(["Curvebook server started with undefined application."]),
            {error, application_undefined};
        {error, _} ->
            {error}
    end;

init([SimId, Dir]) -> 
    case fill_curvebook(Dir) of
        {Power_defect, Boron_worth, MTC, Critical_boron, Rod_worth, Rod_control_speed_program, Pls} ->
            {ok, #curvebook_state{simid = SimId, power_defect = Power_defect, boron_worth = Boron_worth, mtc = MTC, critical_boron = Critical_boron, rod_worth = Rod_worth, rod_control_speed_program = Rod_control_speed_program, pls = Pls}};
        {error, _} ->
            {error}
    end.

handle_call({get, power_defect, Key}, _From, State) ->
    {reply, lookup(State#curvebook_state.power_defect, Key), State};

handle_call({get, boron_worth, Key}, _From, State) ->
    {reply, lookup(State#curvebook_state.boron_worth, Key), State};

handle_call({get, mtc, Key}, _From, State) ->
    {reply, lookup(State#curvebook_state.mtc, Key), State};

handle_call({get, critical_boron, Key}, _From, State) ->
    {reply, lookup(State#curvebook_state.critical_boron, Key), State};

handle_call({get, rod_worth, Key}, _From, State) ->
    {reply, lookup(State#curvebook_state.rod_worth, Key), State};

handle_call({get, rod_control_speed_program, Key}, _From, State) ->
    {reply, lookup(State#curvebook_state.rod_control_speed_program, Key), State};

handle_call({get, pls, Key}, _From, State) ->
    {reply, lookup(State#curvebook_state.pls, Key), State};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

%handle_call(_Request, _From, State) -> {reply, Reply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%==================================================================
%%% Internal functions
%%%==================================================================

fill_curvebook() ->
    case application:get_application() of
        {ok, App} ->
	    {ok, Priv} = application:get_env(App, priv),
    	    {ok, Curvebook} = application:get_env(App, curvebook),
    	    fill_curvebook(Priv ++ Curvebook);
	undefined ->
	    {error, application_undefined}
    end.

fill_curvebook(Dir) ->
    Is_dir = filelib:is_dir(Dir),
    case Is_dir of
        true ->
            Power_defect = fill_table(Dir ++ "power_defect.ets"),
    	    Boron_worth = fill_table(Dir ++ "boron_worth.ets"),
	    MTC = fill_table(Dir ++ "MTC.ets"),
   	    Critical_boron = fill_table(Dir ++ "critical_boron.ets"),
   	    Rod_worth = fill_table(Dir ++ "rod_worth.ets"),
	    Rod_control_speed_program = fill_table(Dir ++ "rod_control_speed_program.ets"),
   	    Pls = fill_pls(Dir ++ "pls.ets"),
	    Tables = [Power_defect, Boron_worth, MTC, Critical_boron, Rod_worth, Rod_control_speed_program, Pls],
	    Tables_OK = lists:all(fun(T) -> is_list(T) end, Tables),
	    if 
	        Tables_OK ->
   	            {Power_defect, Boron_worth, MTC, Critical_boron, Rod_worth, Rod_control_speed_program, Pls};
	     	true ->
	            {error, corrupt_curvebook}
	    end;
        false ->
            io:format("Error: curvebook directory ~p does not exist.~n", [Dir]),
            {error, no_directory}
    end.

fill_table(Path) ->
    case file:consult(Path) of
        {ok, [Table]} ->
            sort_table(Table);
        {error, enoent} ->
	    io:format("Error: curvebook file ~p not found.~n", [Path]),
            {error, enoent};
        Other ->
	    io:format("Error: ~p", [Other]),
	    Other
    end.

fill_pls(Path) ->
    case file:consult(Path) of
        {ok, [Table]} ->
            Table;
        {error, enoent} ->
	    io:format("Error: PLS file ~p not found.~n", [Path]),
            {error, enoent};
        Other ->
	    io:format("Error: ~p", [Other]),
	    Other
    end.

sort_table(Table) when not is_list(Table) ->
    Table;

sort_table(Table) when is_list(Table) ->
    Sorted_table = lists:map(fun(T) -> {element(1, T), sort_table(element(2, T))} end, Table),
    lists:sort(fun(A, B) -> element(1, A) =< element(1, B) end, Sorted_table).

lookup(T, []) ->
%   io:format("lookup: []~n"),
    T;
lookup(Table, Key) ->
%    io:format("lookup: ~p~n", [Key]),
    [Head|Rest] = Key,
    case lists:keymember(Head, 1, Table) of
        true ->
            {value, {_, Val}} = lists:keysearch(Head, 1, Table),
	    lookup(Val, Rest);
        false ->
            interpolate(Table, Key)
    end.

interpolate(Table, Key) ->
    interpolate(Table, Key, 1, length(Table)).

interpolate(Table, Key, Start, End) ->
%    io:format("interpolate: ~p~n", [Key]),
    Middle = (Start + End) div 2,
    [Head|_] = Key,
    {New_head, Val} = lists:nth(Middle, Table),
%    io:format("start-end: ~p~n", [{Start, Middle, End}]),
%    io:format("heads: ~p~n", [{Head, New_head}]),
    if
        Start == End ->
            Val;
        Start + 1 == End ->
            calculate(Table, Key, Start, End);
        Head < New_head ->
            interpolate(Table, Key, Start, Middle);
        New_head < Head ->
            interpolate(Table, Key, Middle, End)
    end.

calculate(Table, Key, Lo, Hi) ->
%    io:format("calculate: ~p~n", [[0|Key]]),
    [Head|Rest] = Key,
    {Head_Lo, Rest_Lo} = lists:nth(Lo, Table),
    {Head_Hi, Rest_Hi} = lists:nth(Hi, Table),
    Val_Lo = lookup(Rest_Lo, Rest),
    Val_Hi = lookup(Rest_Hi, Rest),
%    io:format("head: ~p~n", [{Head_Lo, Head_Hi}]),
%    io:format("calculate: ~p~n", [{Val_Lo, Val_Hi}]),
    if
        is_number(Val_Lo) and is_number(Val_Hi) ->
            Ratio = (Head - Head_Lo) / (Head_Hi - Head_Lo),
	    Val = Ratio * (Val_Hi - Val_Lo) + Val_Lo,
	    Val;
        true ->
            {error, value_not_number}
    end.


%%%==================================================================
%%% Test functions
%%%==================================================================
-include_lib("eunit/include/eunit.hrl").

unit_test() ->
    ok.

integration_test() ->
    ok.