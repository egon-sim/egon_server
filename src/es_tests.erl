-module(es_tests).

-include_lib("eunit/include/eunit.hrl").

full_test_() ->
    {application, egon_server}.
