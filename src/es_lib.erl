%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Library for functions not belonging to anywhere.
%%% @end
%%%------------------------------------------------------------------
-module(es_lib).

-export([
	priv_dir/1,
	collect_parameters/2
	]).

priv_dir(App) ->
%    io:format("App: ~p~n", [App]),
%    io:format("Cwd: ~p~n", [file:get_cwd()]),
%    io:format("Priv: ~p~n", [code:priv_dir(App)]),
    case code:priv_dir(App) of
        {error, bad_name} ->
	    {ok, Cwd} = file:get_cwd(),
	    Cwd ++ "/" ++ "priv/";
        Priv ->
	    Priv ++ "/"
    end.

collect_parameters(SimId, Log) ->
    case Log of
	{ok, all} ->
	    Modules = collect_modules_sup({global, {SimId, es_simulator_sup}}),
	    Params = lists:merge(lists:map(fun(Module) -> query_module(Module) end, Modules)),
	    {ok, Params};
	_ ->
	    {ok, []}
    end.
    
collect_modules_sup(SupRef) ->
    Children = supervisor:which_children(SupRef),
    lists:flatten(lists:map(fun(ChildSpec) -> collect_modules(ChildSpec) end, Children)).

collect_modules({_,_,worker,Modules}) ->
    Modules;
collect_modules({_,Child,supervisor,_}) ->
    collect_modules_sup(Child).

query_module(Module) ->
    Resp = (catch Module:params()),
    Params = case Resp of
	{'EXIT', _} ->
	    [];
	_ ->
	    Resp
    end,
    lists:map(fun({Id, Desc, Function}) -> {Id, Desc, Module, Function} end, Params).

