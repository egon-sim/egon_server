%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Clock server. Server sending ticks with given frequency.
%%% @end
%%%------------------------------------------------------------------
-module(es_ramper_server).

-behaviour(gen_server).
-define(SERVER(SimId), {global, {SimId, ?MODULE}}).

% API
-export([
	start_link/1,
	stop_link/1,
	start_ramping/4
	]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% data structures
-record(ramper_state, {simid, turbine, target, rate, direction}).


%%%==================================================================
%%% API
%%%==================================================================

%%-------------------------------------------------------------------
%% @doc Starts the server.
%%
%% @spec start_link(SimId::integer()) -> {ok, Pid}
%% where
%%  Pid = pid()
%% @end
%%-------------------------------------------------------------------
start_link(SimId) ->
    gen_server:start_link(?SERVER(SimId), ?MODULE, [SimId], []).

%%-------------------------------------------------------------------
%% @doc Stops the server.
%%
%% @spec stop_link(SimId::integer()) -> stopped
%% @end
%%-------------------------------------------------------------------
stop_link(SimId) ->
    gen_server:call(?SERVER(SimId), stop).

%%-------------------------------------------------------------------
%% @doc Starts ramping.
%%
%% @spec start_ramping(SimId::integer(), Current::float(),
%%       Target::float(), Rate::float()) -> ok
%% @end
%%-------------------------------------------------------------------
start_ramping(SimId, Current, Target, Rate) ->
    gen_server:call(?SERVER(SimId), {start_ramp, Current, Target, Rate}).


%%%==================================================================
%%% gen_server callbacks
%%%==================================================================

init([SimId]) ->
    {ok, #ramper_state{simid = SimId, turbine=es_turbine_server, target=none, rate=none, direction=none}}.

handle_call({start_ramp, Current, Target, Rate}, _From, State) ->
%    error_logger:info_report(["Start ramping."]),
    case Current > Target of
    	 true -> Direction = -1;
         false -> Direction = 1
    end,
    New_state = State#ramper_state{target=Target, rate=Rate, direction=Direction},
    SimId = State#ramper_state.simid,
    es_clock_server:add_listener(SimId, ?SERVER(SimId)),
    {reply, ok, New_state};

handle_call({tick}, _From, State) ->
%    error_logger:info_report(["Ramper tick."]),
    SimId = State#ramper_state.simid,
    Current = es_turbine_server:power(SimId),
    Target = State#ramper_state.target,
    Rate = State#ramper_state.rate,
    Direction = State#ramper_state.direction,

    error_logger:info_report(["Progress", {current, Current}, {target, Target}, {rate, Rate}, {direction, Direction}]),

    New = Current + (Direction * Rate),
    case Direction * (New - Target) =< 0 of
        true -> 
		es_turbine_server:set_power(SimId, New),
		Retval = ok;
        _ -> 
		es_turbine_server:set_power(SimId, Target),
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


%%%==================================================================
%%% Internal functions
%%%==================================================================

stop(SimId, Target, Rate) ->
    ok = es_turbine_server:stop_ramp(SimId),
    Power = es_turbine_server:power(SimId),
    error_logger:info_report(["Stopping ramper", {current, Power}, {target, Target}, {rate, Rate}]).

%%%==================================================================
%%% Test functions
%%%==================================================================
-include_lib("eunit/include/eunit.hrl").

unit_test() -> ok.

integration_test() -> ok.
