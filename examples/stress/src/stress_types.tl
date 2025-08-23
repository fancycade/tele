include_lib("stress/include/stress.hrl")

nominal id: integer

spec simple(some_type): some_type
fun simple
  (x) when is_integer(x):
    42
  (x) when is_binary(x):
    "foobar"

spec simple2(another_type): another_type
fun simple2
  (x) when is_integer(x):
    43
  (x):
    x

spec simple3(foo_type): foo_type
fun simple3
  (x) when is_integer(x):
    44
  (x):
    x

spec simple4(tele_example_type): tele_example_type
fun simple4
  (x) when is_integer(x):
    45
  (x):
    x

spec simple5(another_type): another_type
fun simple5
  (x) when is_integer(x):
    46
  (x):
    x

spec int_to_id(integer): id
fun int_to_id(n): n

spec simple6(id): #(atom, id)
fun simple6(id): #('foo, id)
