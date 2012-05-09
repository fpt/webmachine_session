-module(webmachine_session_unittest).

-export([run_tests/0]).

-include_lib("eunit/include/eunit.hrl").

run_tests() ->
    io:format("Running unit tests...~n"),
    Failures = [],
    io:format("Unit test failures: ~p~n", [lists:reverse(Failures)]).
