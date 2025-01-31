
include("include/stress.hrl")
include_lib("stress/include/stress2.hrl")
include("stress3.hrl")

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

