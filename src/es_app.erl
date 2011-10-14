-module(es_app).
-include_lib("eunit/include/eunit.hrl").
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
