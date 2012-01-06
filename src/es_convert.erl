%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Library containing mathematical functions for converting
%%% 	 between units.
%%% @end
%%%------------------------------------------------------------------
-module(es_convert).

% API
-export([
	c2f_delta/1,
	round/2
	]).


%%%==================================================================
%%% API
%%%==================================================================

%%-------------------------------------------------------------------
%% @doc Converts temperature difference in degrees Celsius into
%% 	temperature difference in degrees Farenheit.
%%
%% @spec c2f_delta(Celsius::float()) -> Farenheit::float()
%% @end
%%-------------------------------------------------------------------
c2f_delta(Celsius) ->
    Farenheit = Celsius * 9 / 5,
    Farenheit.

%%-------------------------------------------------------------------
%% @doc Founds a float to given number of decimal places.
%%
%% @spec round(Float::float(), Points::integer()) -> float()
%% @end
%%-------------------------------------------------------------------
round(Float, Points) ->
    Dec = math:pow(10, Points),
    round(Float * Dec) / Dec.


%%%==================================================================
%%% Test functions
%%%==================================================================
-include_lib("eunit/include/eunit.hrl").

unit_test() ->
    ?assertEqual(-9.99, round(c2f_delta(-5.55), 2)),
    ?assertEqual(-4.99, round(c2f_delta(-2.77), 2)),
    ?assertEqual(-2.99, round(c2f_delta(-1.66), 2)),
    ?assertEqual(-1.49, round(c2f_delta(-0.83), 2)),
    ?assertEqual(-0.99, round(c2f_delta(-0.55), 2)),
    ?assertEqual(0.99, round(c2f_delta(0.55), 2)),
    ?assertEqual(1.49, round(c2f_delta(0.83), 2)),
    ?assertEqual(2.99, round(c2f_delta(1.66), 2)),
    ?assertEqual(4.99, round(c2f_delta(2.77), 2)),
    ?assertEqual(9.99, round(c2f_delta(5.55), 2)),

    ok.
