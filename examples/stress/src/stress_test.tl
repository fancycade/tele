fun foo(): 42

test:
  include_lib("eunit/include/eunit.hrl")

  funp example_stuff(): 42

  funp example_stuff_test():
    ?assertEqual(42, example_stuff())
    ?assertEqual(42, foo())

