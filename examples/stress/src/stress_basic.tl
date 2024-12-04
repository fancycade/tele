// This is a single line comment

/* This is a multiline comment */

fun hello(): // comment here
  "Hello, World!" // and a comment here

fun hello_atom():
  'ok

fun hello_other_atom():
  #'foo bar'

fun hello_integer_1():
  1

fun hello_integer_2():
  -1

fun hello_list():
  [1, 2, 3, 4]

fun hello_map():
  {"hello": "world"}

fun hello_map_put_val():
  maps.put('foo, 'bar, {})

fun hello_add(a, b):
  a + b

fun hello_parens():
  ((2 + 2) + 
   (2 + 2))

fun hello_equal():
  1 == 1

fun hello_case(stuff):
  case stuff:
    1: 'one
    2: 'two
    3: 'three
    _: 'something

fun hello_match_sig
  (1): 'one
  (2): 'two
  (3): 'three
  (_): 'something

fun hello_math(a, b):
  ((a + b) - (a * b))

fun hello_cons(a, l):
  [a | l]

fun hello_map_update(m, k, v):
  {m | k: v}

fun hello_map_update2(m, k, v):
  {m | k: v, "foo": "bar"}

fun hello_list_tuples():
  [#(1, 2), #(3, 4), #(5, 6)]

fun hello_tuple_list():
  #([1, 2], [3, 4], [5, 6])

fun hello_list_map():
  [{"hello": "world"}, {"foo": "bar"}]

fun hello_map_list():
  {
    "hello": [1, 2], 
    "world": [3, 4]
  }

fun hello_call_priv():
  hello_priv()

funp hello_priv(): 42

fun hello_fact
  (0): 1
  (n): n * hello_fact(n - 1)

fun hello_variables(a, b):
  c = a + b
  d = c + 2
  e = d + 3
  e

fun hello_list_reverse(l):
  do_hello_list_reverse(l, [])

funp do_hello_list_reverse
  ([], acc): 
    acc
  ([h | t], acc):
    do_hello_list_reverse(t, [h | acc])

fun hello_map_match
  ({"hello": world}): world
  (m): m

fun hello_erlang_lists_map(l):
  lists.map(
    fun (x):
      x + 2,
    l
  )

fun hello_fun_val(f):
  @f()

fun hello_mod_fun_val(m, f):
  @m.@f()

fun hello_call_me():
  42

fun hello_try_catch():
  try throw("oops!"):
    result: result
  catch:
    _._:
      42

fun hello_pass_me(x): x + 2
fun hello_return_fun_val(): fun hello_pass_me/1

fun hello_multline_fun():
  f =
  fun ():
    a = "Hello World!"
    b = a
    c = b
    c
  @f()

fun hello_routes():
  {"pre": #('nine_cowboy_mid, 'json_request),
   "post": #('logger_mid, 'log),
   "handle": #('example, 'api)}
