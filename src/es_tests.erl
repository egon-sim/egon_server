%%%------------------------------------------------------------------
%%% @author Nikola Skoric <nskoric@gmail.com>
%%% @copyright 2011 Nikola Skoric
%%% @doc Helper module for testing code. Application itself does not
%%%      use this module, it is used only during developement and
%%%      testing.
%%% @end
%%%------------------------------------------------------------------
-module(es_tests).

-include_lib("eunit/include/eunit.hrl").

full_test_() ->
    {application, egon_server}.
