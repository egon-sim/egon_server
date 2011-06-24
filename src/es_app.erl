-module(es_app).
-include_lib("include/es_common.hrl").
-behaviour(application).
-export([
   start/2,
   stop/1
]).

start(_Type, _StartArgs) ->
   case es_server_supervisor:start_link() of
      {ok, Pid} ->
         Retval = {ok, Pid};
      Other ->
         Retval = {error, Other}
   end,
   es_server_supervisor:start_child(1056),
   Retval.

stop(_State) ->
   ok.
