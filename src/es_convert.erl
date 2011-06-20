-module(es_convert).
-compile(export_all).

c2f_delta(Celsius) ->
    Farenheit = Celsius * 9 / 5,
    Farenheit.
