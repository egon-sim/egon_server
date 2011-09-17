-module(es_ramper_server).
-include_lib("include/es_common.hrl").
-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(ramper_state, {simid, turbine, target, rate, direction}).

start_link(SimId) -> 
    gen_server:start_link({global, {SimId, ?MODULE}}, ?MODULE, [SimId], []).

init([SimId]) ->
    {ok, #ramper_state{simid = SimId, turbine=es_turbine_server, target=none, rate=none, direction=none}}.

stop(SimId, Target, Rate) ->
    ok = gen_server:call({global, {SimId, es_turbine_server}}, {action, ramp, stop}),
    Power = gen_server:call({global, {SimId, es_turbine_server}}, {get, power}),
    error_logger:info_report(["Stopping ramper", {current, Power}, {target, Target}, {rate, Rate}]).

handle_call({start_ramp, Current, Target, Rate}, _From, State) ->
%    error_logger:info_report(["Start ramping."]),
    case Current > Target of
    	 true -> Direction = -1;
         false -> Direction = 1
    end,
    New_state = State#ramper_state{target=Target, rate=Rate, direction=Direction},
    SimId = State#ramper_state.simid,
    gen_server:call({global, {SimId, es_clock_server}}, {add_listener, {global, {SimId, ?MODULE}}}),
    {reply, ok, New_state};

handle_call({tick}, _From, State) ->
%    error_logger:info_report(["Ramper tick."]),
    SimId = State#ramper_state.simid,
    Current = gen_server:call({global, {SimId, es_turbine_server}}, {get, power}),
    Target = State#ramper_state.target,
    Rate = State#ramper_state.rate,
    Direction = State#ramper_state.direction,

    error_logger:info_report(["Progress", {current, Current}, {target, Target}, {rate, Rate}, {direction, Direction}]),

    New = Current + (Direction * Rate),
    case Direction * (New - Target) =< 0 of
        true -> 
		gen_server:call({global, {SimId, es_turbine_server}}, {set, power, New}),
		Retval = ok;
        _ -> 
		gen_server:call({global, {SimId, es_turbine_server}}, {set, power, Target}),
		stop(SimId, Target, Rate),
		Retval = rem_listener
    end,
    {reply, Retval, State};


handle_call(stop, _From, Tab) -> {stop, normal, stopped, Tab}.

%handle_call(_Request, _From, State) -> {reply, Reply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

