-module(es_interface_dispatcher).
-include_lib("include/es_common.hrl").
-behaviour(supervisor).
-export([start_link/1, start_child/1, init/1]).

start_link(SimId) ->
    supervisor:start_link({global, {SimId, ?MODULE}}, ?MODULE, [SimId]).

start_child(SimId) ->
    io:format("Starting interface server.~n"),
    {ok, Child} = supervisor:start_child({global, {SimId, ?MODULE}}, []),
    io:format("New interface server: ~p~n", [Child]),
    Port = gen_server:call(Child, {get_port}),
    io:format("Got port ~p.~n", [Port]),
    gen_server:cast(Child, {listen}),
    {ok, Port}.

init([SimId]) -> 
%    io:format("Starting interface dispatcher.~n"),
    Interface = {es_interface_server, {es_interface_server, start_link, [SimId]},
       temporary, 2000, worker, [es_interface_server]},

    Children = [Interface],
    Restart_strategy = {simple_one_for_one, 0, 1},
    {ok, {Restart_strategy, Children}}.
