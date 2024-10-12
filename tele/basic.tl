//fun foobar(): // hello there
  // oof
//  42 // foo
// bar

//fun foobar2():
//  43
/*
This is a multiline comment that demonstrates

how one does
a
multiline comment
*/

record point: #(x, y)

type stuff: integer | float

fun fooboo():
  a = #point(x=0, y=0)
  a2 = a#point(x=42)
  a2#point.x

fun atom_fun():
  'a

fun fooboo3():
  put('a, 2)

fun fooboo2():
  maps.put({}, 'a, 2)

spec stuff(integer()): integer()
fun stuff(x): x + 2

fun wowzer():
 ((1 + 1) + (1 + 2))

fun wowzer_fun():
  fun wowzer/0

fun woop():
  fun (1, 2): 2 + 2

fun map_over_data():
  lists.map(
    fun (x): x + 2,
    [1, 2, 3]
  )

fun map_over_data2():
  lists.map(fun add2/1, [1, 2, 3])

fun oopsie():
  try oopsie_helper():
    _: 'ok
  catch:
    @error.@e:
      #('error, @error, @e)

fun oopsie_helper():
  'ok

fun foo():
  case bar():
    _result:
      'ok

fun bar():
  'hello_there

fun basic_bin():
  "hello world"

fun hello_there():
  io.format("~s~n", ["Hello, World!"])

fun hello_there2():
  io.format(
    "~s~n",
    ["Hello, World!"]
  )

fun multi_line_arrow():
  fun ():
    a = 2
    b = 3
    a + b

fun multi_line_paren_op():
  (2 + 4) +
  (3 + 4)

fun multi_line_match_equal():
  a =
  case 42:
    _x: 'ok
  a

fun multi_line_arrow_equal():
  a =
  fun ():
    x = 2
    x + 4
  a

fun apply(m, f):
  @m.@f()

fun apply_mod(m):
  @m.run()

fun apply_f(f):
  m.@f()

fun routes():
  {"pre": [#('mid, 'log_mid)],
   "handle": [
      {"path": "/",
       "handle": #('handler, 'handle_index)}
   ],
   "post": [#('mid, 'log_mid)]}

fun alt_atom():
  #'basic.cmd'
