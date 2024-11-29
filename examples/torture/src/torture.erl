-module(torture).

-export([]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

torture_basic_hello_test() ->
    ?assertEqual(<<"Hello, World!">>, torture_basic:hello()).

torture_basic_hello_atom_test() ->
    ?assertEqual(ok, torture_basic:hello_atom()).

torture_basic_hello_integer_test() ->
    ?assertEqual(1, torture_basic:hello_integer_1()),
    ?assertEqual(-1, torture_basic:hello_integer_2()).

torture_basic_hello_list_test() ->
    ?assertEqual([1, 2, 3, 4], torture_basic:hello_list()).

torture_basic_hello_map_test() ->
    ?assertEqual(#{<<"hello">> => <<"world">>}, torture_basic:hello_map()).

torture_basic_hello_add_test() ->
    ?assertEqual(4, torture_basic:hello_add(2, 2)).

torture_basic_hello_equal_test() ->
    ?assert(torture_basic:hello_equal()).

torture_basic_hello_case_test() ->
    ?assertEqual(one, torture_basic:hello_case(1)),
    ?assertEqual(two, torture_basic:hello_case(2)),
    ?assertEqual(three, torture_basic:hello_case(3)),
    ?assertEqual(something, torture_basic:hello_case(4)),
    ?assertEqual(something, torture_basic:hello_case(-42)).

torture_basic_hello_match_sig_test() ->
    ?assertEqual(one, torture_basic:hello_match_sig(1)),
    ?assertEqual(two, torture_basic:hello_match_sig(2)),
    ?assertEqual(three, torture_basic:hello_match_sig(3)),
    ?assertEqual(something, torture_basic:hello_match_sig(4)),
    ?assertEqual(something, torture_basic:hello_match_sig(-42)).

torture_basic_hello_math_test() ->
    ?assertEqual(0, torture_basic:hello_math(2, 2)),
    ?assertEqual(-3, torture_basic:hello_math(3, 3)).

torture_basic_hello_cons_test() ->
    ?assertEqual([1, 2, 3], torture_basic:hello_cons(1, [2, 3])).

torture_basic_hello_map_update_test() ->
    ?assertEqual(
        #{<<"hello">> => <<"world">>},
        torture_basic:hello_map_update(#{}, <<"hello">>, <<"world">>)
    ),

    ?assertEqual(
        #{<<"hello">> => <<"world">>, <<"foo">> => <<"bar">>},
        torture_basic:hello_map_update2(#{}, <<"hello">>, <<"world">>)
    ).

torture_basic_hello_list_tuples_test() ->
    ?assertEqual(
        [{1, 2}, {3, 4}, {5, 6}],
        torture_basic:hello_list_tuples()
    ).

torture_basic_hello_tuple_list_test() ->
    ?assertEqual(
        {[1, 2], [3, 4], [5, 6]},
        torture_basic:hello_tuple_list()
    ).

torture_basic_hello_list_map_test() ->
    ?assertEqual(
        [#{<<"hello">> => <<"world">>}, #{<<"foo">> => <<"bar">>}],
        torture_basic:hello_list_map()
    ).

torture_basic_hello_map_list_test() ->
    ?assertEqual(
        #{
           <<"hello">> => [1, 2],
           <<"world">> => [3, 4]
        },
        torture_basic:hello_map_list()
    ).

torture_basic_hello_call_priv_test() ->
    ?assertEqual(42, torture_basic:hello_call_priv()).

torture_basic_hello_fact_test() ->
    ?assertEqual(1, torture_basic:hello_fact(0)),
    ?assertEqual(2, torture_basic:hello_fact(2)).

torture_basic_hello_variables_test() ->
    ?assertEqual(9, torture_basic:hello_variables(2, 2)).

torture_basic_hello_list_reverse_test() ->
    ?assertEqual(
      [3, 2, 1],
      torture_basic:hello_list_reverse([1, 2, 3])
    ).

torture_basic_hello_map_match_test() ->
    ?assertEqual(
        <<"world">>, 
        torture_basic:hello_map_match(#{<<"hello">> => <<"world">>})
    ),
    ?assertEqual(
        #{<<"foo">> => <<"bar">>},
        torture_basic:hello_map_match(#{<<"foo">> => <<"bar">>})
    ).

torture_basic_hello_erlang_lists_map_test() ->
    ?assertEqual(
        [3, 4, 5],
        torture_basic:hello_erlang_lists_map([1, 2, 3])
    ).
    
-endif.
