%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Library for functions not belonging to anywhere.
%%% @end
%%%------------------------------------------------------------------
-module(es_lib).

-export([
	priv_dir/1
	]).


priv_dir(App) ->
    io:format("App: ~p~n", [App]),
    io:format("Cwd: ~p~n", [file:get_cwd()]),
    io:format("Priv: ~p~n", [code:priv_dir(App)]),
    case code:priv_dir(App) of
        {error, bad_name} ->
	    {ok, Cwd} = file:get_cwd(),
	    Cwd ++ "/" ++ "priv/";
        Priv ->
	    Priv ++ "/"
    end.
