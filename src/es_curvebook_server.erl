-module(es_curvebook_server).
-include_lib("include/es_common.hrl").
-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(curvebook_state, {simid, power_defect, boron_worth, mtc, critical_boron, rod_worth, pls}).
-compile(export_all).

start_link(SimId) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [SimId], []).

init([SimId]) -> 
    {Power_defect, Boron_worth, MTC, Critical_boron, Rod_worth, Pls} = fill_curvebook(),
    {ok, #curvebook_state{simid = SimId, power_defect = Power_defect, boron_worth = Boron_worth, mtc = MTC, critical_boron = Critical_boron, rod_worth = Rod_worth, pls = Pls}}.

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

handle_call({get, pls, Key}, _From, State) ->
    {reply, lookup(State#curvebook_state.pls, Key), State};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

%handle_call(_Request, _From, State) -> {reply, Reply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fill_curvebook() ->
%   Dir = code:priv_dir(egon_server) ++ "curvebook/",
   Dir = "priv/" ++ "curvebook/",
%   io:format("~p~n", [Dir]),
   Power_defect = fill_table(Dir ++ "power_defect.ets"),
   Boron_worth = fill_table(Dir ++ "boron_worth.ets"),
   MTC = fill_table(Dir ++ "MTC.ets"),
   Critical_boron = fill_table(Dir ++ "critical_boron.ets"),
   Rod_worth = fill_table(Dir ++ "rod_worth.ets"),
   Pls = fill_pls(Dir ++ "pls.ets"),
   {Power_defect, Boron_worth, MTC, Critical_boron, Rod_worth, Pls}.

fill_table(Path) ->
    {ok, [Table]} = file:consult(Path),
    sort_table(Table).

fill_pls(Path) ->
    {ok, [Table]} = file:consult(Path),
    Table.

sort_table(Table) when not is_list(Table) ->
    Table;

sort_table(Table) when is_list(Table) ->
    Sorted_table = lists:map(fun(T) -> {element(1, T), sort_table(element(2, T))} end, Table),
    lists:sort(fun(A, B) -> element(1, A) =< element(1, B) end, Sorted_table).

lookup(T, []) ->
%   io:format("lookup: []~n"),
   T;
lookup(Table, Key) ->
%   io:format("lookup: ~p~n", [Key]),
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
%   io:format("interpolate: ~p~n", [Key]),
   Middle = (Start + End) div 2,
   [Head|_] = Key,
   {New_head, Val} = lists:nth(Middle, Table),
%   io:format("start-end: ~p~n", [{Start, Middle, End}]),
%   io:format("heads: ~p~n", [{Head, New_head}]),
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
%   io:format("calculate: ~p~n", [[0|Key]]),
   [Head|Rest] = Key,
   {Head_Lo, Rest_Lo} = lists:nth(Lo, Table),
   {Head_Hi, Rest_Hi} = lists:nth(Hi, Table),
   Val_Lo = lookup(Rest_Lo, Rest),
   Val_Hi = lookup(Rest_Hi, Rest),
%   io:format("head: ~p~n", [{Head_Lo, Head_Hi}]),
%   io:format("calculate: ~p~n", [{Val_Lo, Val_Hi}]),
   Ratio = (Head - Head_Lo) / (Head_Hi - Head_Lo),
   Val = Ratio * (Val_Hi - Val_Lo) + Val_Lo,
   Val.
