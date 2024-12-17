-module(stress).

-export([]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

stress_basic_hello_test() ->
    ?assertEqual(<<"Hello, World!">>, stress_basic:hello()).

stress_basic_hello_bitstring_test() ->
    ?assertEqual(<<"Hello, World!">>, stress_basic:hello_bitstring()).

stress_basic_hello_atom_test() ->
    ?assertEqual(ok, stress_basic:hello_atom()).

stress_basic_hello_other_atom_test() ->
    ?assertEqual('foo bar', stress_basic:hello_other_atom()).

stress_basic_hello_integer_test() ->
    ?assertEqual(1, stress_basic:hello_integer_1()),
    ?assertEqual(-1, stress_basic:hello_integer_2()).

stress_basic_hello_list_test() ->
    ?assertEqual([1, 2, 3, 4], stress_basic:hello_list()).

stress_basic_hello_map_test() ->
    ?assertEqual(#{<<"hello">> => <<"world">>}, stress_basic:hello_map()).

stress_basic_hello_add_test() ->
    ?assertEqual(4, stress_basic:hello_add(2, 2)).

stress_basic_hello_equal_test() ->
    ?assert(stress_basic:hello_equal()).

stress_basic_hello_case_test() ->
    ?assertEqual(one, stress_basic:hello_case(1)),
    ?assertEqual(two, stress_basic:hello_case(2)),
    ?assertEqual(three, stress_basic:hello_case(3)),
    ?assertEqual(something, stress_basic:hello_case(4)),
    ?assertEqual(something, stress_basic:hello_case(-42)).

stress_basic_hello_match_sig_test() ->
    ?assertEqual(one, stress_basic:hello_match_sig(1)),
    ?assertEqual(two, stress_basic:hello_match_sig(2)),
    ?assertEqual(three, stress_basic:hello_match_sig(3)),
    ?assertEqual(something, stress_basic:hello_match_sig(4)),
    ?assertEqual(something, stress_basic:hello_match_sig(-42)).

stress_basic_hello_math_test() ->
    ?assertEqual(0, stress_basic:hello_math(2, 2)),
    ?assertEqual(-3, stress_basic:hello_math(3, 3)).

stress_basic_hello_cons_test() ->
    ?assertEqual([1, 2, 3], stress_basic:hello_cons(1, [2, 3])).

stress_basic_hello_map_update_test() ->
    ?assertEqual(
        #{<<"hello">> => <<"world">>},
        stress_basic:hello_map_update(#{}, <<"hello">>, <<"world">>)
    ),

    ?assertEqual(
        #{<<"hello">> => <<"world">>, <<"foo">> => <<"bar">>},
        stress_basic:hello_map_update2(#{}, <<"hello">>, <<"world">>)
    ).

stress_basic_hello_list_tuples_test() ->
    ?assertEqual(
        [{1, 2}, {3, 4}, {5, 6}],
        stress_basic:hello_list_tuples()
    ).

stress_basic_hello_tuple_list_test() ->
    ?assertEqual(
        {[1, 2], [3, 4], [5, 6]},
        stress_basic:hello_tuple_list()
    ).

stress_basic_hello_list_map_test() ->
    ?assertEqual(
        [#{<<"hello">> => <<"world">>}, #{<<"foo">> => <<"bar">>}],
        stress_basic:hello_list_map()
    ).

stress_basic_hello_map_list_test() ->
    ?assertEqual(
        #{
           <<"hello">> => [1, 2],
           <<"world">> => [3, 4]
        },
        stress_basic:hello_map_list()
    ).

stress_basic_hello_call_priv_test() ->
    ?assertEqual(42, stress_basic:hello_call_priv()).

stress_basic_hello_fact_test() ->
    ?assertEqual(1, stress_basic:hello_fact(0)),
    ?assertEqual(2, stress_basic:hello_fact(2)).

stress_basic_hello_variables_test() ->
    ?assertEqual(9, stress_basic:hello_variables(2, 2)).

stress_basic_hello_list_reverse_test() ->
    ?assertEqual(
      [3, 2, 1],
      stress_basic:hello_list_reverse([1, 2, 3])
    ).

stress_basic_hello_map_match_test() ->
    ?assertEqual(
        <<"world">>, 
        stress_basic:hello_map_match(#{<<"hello">> => <<"world">>})
    ),
    ?assertEqual(
        #{<<"foo">> => <<"bar">>},
        stress_basic:hello_map_match(#{<<"foo">> => <<"bar">>})
    ).

stress_basic_hello_erlang_lists_map_test() ->
    ?assertEqual(
        [3, 4, 5],
        stress_basic:hello_erlang_lists_map([1, 2, 3])
    ).

stress_basic_hello_fun_val_test() ->
    F = fun () -> 42 end,
    ?assertEqual(42, stress_basic:hello_fun_val(F)).

stress_basic_hello_mod_fun_val_test() ->
    ?assertEqual(42, stress_basic:hello_mod_fun_val(stress_basic, hello_call_me)).

stress_basic_try_catch_test() ->
    ?assertEqual(42, stress_basic:hello_try_catch()).

stress_basic_hello_return_fun_val_test() ->
    F = stress_basic:hello_return_fun_val(),
    ?assertEqual(4, F(2)).

stress_basic_hello_map_put_call_test() ->
    ?assertEqual(#{foo => bar}, stress_basic:hello_map_put_val()).

stress_basic_hello_parens_test() ->
    ?assertEqual(8, stress_basic:hello_parens()).

stress_basic_hello_multiline_fun_test() ->
    ?assertEqual(<<"Hello World!"/utf8>>, stress_basic:hello_multline_fun()).

stress_basic_hello_routes_test() ->
    ?assertEqual(#{<<"pre">> => {nine_cowboy_mid, json_request},
                   <<"post">> => {logger_mid, log},
                   <<"handle">> => {example, api}},
                stress_basic:hello_routes()).

stress_basic_hello_bitstring_matching() ->
    ?assertEqual(<<"bar">>, stress_basic:hello_bitstring_matching()).

stress_statements_add2_test() ->
    ?assertEqual(6, stress_statements:add2_example()).

stress_statements_point_test() ->
    P = stress_statements:point_new(0, 0),
    P2 = stress_statements:point_down(P),
    P3 = stress_statements:point_up(P2),
    P4 = stress_statements:point_left(P3),
    P5 = stress_statements:point_right(P4),

    ?assertEqual({point, 0, 0}, P5).

stress_statements_point_default_test() ->
    P = stress_statements:point_default(),
    ?assertEqual({point, 0, 0}, P).

stress_statements_get_tx_type_test() ->
    ?assertEqual(debit, stress_statements:get_tx_type(42)),
    ?assertEqual(credit, stress_statements:get_tx_type(-42)).

-endif.
