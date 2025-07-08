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

stress_basic_hello_guard_test() ->
    ?assertEqual(4, stress_basic:hello_guard(2)),
    ?assertEqual(<<"hello">>, stress_basic:hello_guard(hello)),
    ?assertEqual(hello, stress_basic:hello_guard(<<"hello">>)),
    ?assertEqual([], stress_basic:hello_guard([])).

stress_basic_hello_string_test() ->
    ?assertEqual(<<"hello, world"/utf8>>, stress_basic:hello_string()).

stress_basic_hello_binary_test() ->
    ?assertEqual(<<"hello, world">>, stress_basic:hello_binary()).

stress_basic_hello_binary_type_test() ->
    ?assertEqual(<<"foo">>, stress_basic:hello_binary_type()).

stress_basic_hello_binary_size_test() ->
    ?assertEqual(<<"foo">>, list_to_binary(stress_basic:hello_binary_size())).

stress_basic_hello_binary_map_test() ->
    ?assertEqual(<<"bar">>, list_to_binary(stress_basic:hello_binary_map())).

stress_basic_hello_guard2_test() ->
    ?assertEqual(5, stress_basic:hello_guard2(4)),
    ?assertEqual(42, stress_basic:hello_guard2(2)).

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

stress_hello_goodbye_test() ->
    ?assertEqual(hello, stress_hello_goodbye:hello()),
    ?assertEqual(200, stress_hello_goodbye:goodbye()).

stress_real_world_example1_test() ->
    ?assertEqual({noreply, []}, stress_real_world:example1(ok, [])),
    ?assertEqual({noreply, []}, stress_real_world:example1({send_async, ok}, [])).


stress_real_world_example2_test() ->
    ?assertEqual({noreply, []}, stress_real_world:example2(ok, [])).

stress_real_world_example3_test() ->
    ?assertEqual(42, stress_real_world:example3(true)),
    ?assertEqual({error, not_found}, stress_real_world:example3(false)).

stress_real_world_example4_test() ->
    ?assertEqual({ok, 2}, stress_real_world:example4()).

stress_real_world_example5_test() ->
    {A, B, C} = stress_real_world:example5(),
    ?assertEqual('GET', A),
    ?assertEqual('GET', B),
    ?assertEqual('GET', C).

stress_real_world_example6_test() ->
    ?assertEqual(<<>>, stress_real_world:example6()).

stress_real_world_example7_test() ->
    ?assertEqual(42, stress_real_world:example7(foo)),
    ?assertEqual(42, stress_real_world:example7(bar)),
    ?assertEqual(43, stress_real_world:example7(ok)).

stress_real_world_example8_test() ->
    {A, B} = stress_real_world:example8(),
    ?assertEqual([{mods, [{}]}], A),
    ?assertEqual([{mods, [{simple_app, []}]}], B).

stress_real_world_example9_test() ->
    ?assertEqual([<<"root">>], stress_real_world:example9(<<"root">>, undefined)),
    ?assertEqual([<<"hello">>, 1], stress_real_world:example9(<<"hello">>, 1)).

stress_math_test() ->
    ?assertEqual(2, stress_math:add(1, 1)),
    ?assertEqual(1, stress_math:sub(2, 1)),
    ?assertEqual(4, stress_math:add2(2)),
    ?assertEqual(5, stress_math:add3(2)).

stress_math_factorial_test() ->
    ?assertEqual(1, stress_math:factorial(0)),
    ?assertEqual(6, stress_math:factorial(3)).

stress_math_fibonacci_test() ->
    ?assertEqual(0, stress_math:fibonacci(0)),
    ?assertEqual(1, stress_math:fibonacci(1)),
    ?assertEqual(1, stress_math:fibonacci(2)),
    ?assertEqual(2, stress_math:fibonacci(3)),
    ?assertEqual(3, stress_math:fibonacci(4)),
    ?assertEqual(5, stress_math:fibonacci(5)).

stress_types_simple_test() ->
    ?assertEqual(42, stress_types:simple(1)),
    ?assertEqual(<<"foobar">>, stress_types:simple(<<"bar">>)).

stress_types_simple2_test() ->
    ?assertEqual(43, stress_types:simple2(1)),
    ?assertEqual(<<"foobar">>, stress_types:simple2(<<"foobar">>)).

stress_types_simple3_test() ->
    ?assertEqual(44, stress_types:simple3(1)),
    ?assertEqual(<<"foobar">>, stress_types:simple3(<<"foobar">>)).

stress_types_simple4_test() ->
    ?assertEqual(45, stress_types:simple4(1)),
    ?assertEqual(<<"foobar">>, stress_types:simple4(<<"foobar">>)).

stress_nested_hello_test() ->
    ?assertEqual(42, stress_nested:hello()).

stress_types_simple5_test() ->
    ?assertEqual(46, stress_types:simple5(1)),
    ?assertEqual(<<"foobar">>, stress_types:simple5(<<"foobar">>)).

-endif.
