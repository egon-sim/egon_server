-module(es_app).
-behaviour(application).
-export([
   start/2,
   stop/1,
   set_up_defaults/0
]).

start(_Type, _StartArgs) ->
   case es_sup:start_link() of
      {ok, Pid} ->
         Retval = {ok, Pid};
      Other ->
         Retval = {error, Other}
   end,
   set_up_defaults(),
   Retval.

stop(_State) ->
   ok.

set_up_defaults() ->
    timer:sleep(500),
    load_snapshot("priv/snapshots/full_power.snapshot"),
    timer:sleep(500),
    gen_server:call(es_config_server, {unfreaze_sim}),
    ok.

load_snapshot(Path) ->
   {ok, [Snapshot]} = file:consult(Path),

   lists:foreach(
       fun({Server, Parm, Val}) ->
       	   gen_server:call(Server, {set, Parm, Val})
       end,
       Snapshot),
   ok.
