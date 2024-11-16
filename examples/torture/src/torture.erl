-module(torture).

-export([]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

torture_basic_hello_test() ->
    ?assertEqual(<<"Hello, World!">>, torture_basic:hello()).

torture_basic_hello_ok_test() ->
    ?assertEqual({ok, <<"Hello, World!">>}, torture_basic:hello_ok()).

-endif.
