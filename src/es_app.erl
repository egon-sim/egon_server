%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Application behaviour module.
%%% @end
%%%------------------------------------------------------------------
-module(es_app).

-behaviour(application).

-export([
        start/2,
        stop/1
]).

start(_Type, _StartArgs) ->
   case es_sup:start_link() of
      {ok, Pid} ->
         {ok, Pid};
      Other ->
         {error, Other}
   end.

stop(_State) ->
   ok.
