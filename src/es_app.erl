-module(es_app).
-include_lib("include/es_common.hrl").
-behaviour(application).
-export([
   start/2,
   stop/1
]).

start(_Type, _StartArgs) ->
   case es_sup:start_link(1056) of
      {ok, Pid} ->
         Retval = {ok, Pid};
      Other ->
         Retval = {error, Other}
   end,
   Retval.

stop(_State) ->
   ok.
