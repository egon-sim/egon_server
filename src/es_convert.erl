-module(es_convert).
-include_lib("include/es_common.hrl").
-compile(export_all).

c2f_delta(Celsius) ->
    Farenheit = Celsius * 9 / 5,
    Farenheit.

round(Float, Points) ->
    Dec = math:pow(10, Points),
    round(Float * Dec) / Dec.

c2f_delta_test() ->
    -9.99 = round(c2f_delta(-5.55), 2),
    -4.99 = round(c2f_delta(-2.77), 2),
    -2.99 = round(c2f_delta(-1.66), 2),
    -1.49 = round(c2f_delta(-0.83), 2),
    -0.99 = round(c2f_delta(-0.55), 2),
    0.99 = round(c2f_delta(0.55), 2),
    1.49 = round(c2f_delta(0.83), 2),
    2.99 = round(c2f_delta(1.66), 2),
    4.99 = round(c2f_delta(2.77), 2),
    9.99 = round(c2f_delta(5.55), 2),

    ok.