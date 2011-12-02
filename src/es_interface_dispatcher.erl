-module(es_interface_dispatcher).

-behaviour(supervisor).
-define(SERVER(SimId), {global, {SimId, ?MODULE}}).

-export([start_link/1, start_child/2, children/1, init/1]).

start_link(SimId) ->
    supervisor:start_link(?SERVER(SimId), ?MODULE, [SimId]).

start_child(SimId, User) ->
    io:format("Starting interface server.~n"),
    {ok, Child} = supervisor:start_child(?SERVER(SimId), [User]),
    io:format("New interface server: ~p~n", [Child]),
    Port = es_interface_server:port(Child),
    io:format("Got port ~p.~n", [Port]),
    gen_server:cast(Child, {listen}),
    {ok, Port}.

children(SimId) ->
    supervisor:which_children(?SERVER(SimId)).

init([SimId]) -> 
%    io:format("Starting interface dispatcher.~n"),
    Interface = {es_interface_server, {es_interface_server, start_link, [SimId]},
       temporary, 2000, worker, [es_interface_server]},

    Children = [Interface],
    Restart_strategy = {simple_one_for_one, 0, 1},
    {ok, {Restart_strategy, Children}}.
