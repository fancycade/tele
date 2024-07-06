//fun foobar(): // hello there
  // oof
//  42 // foo
// bar

//fun foobar2():
//  43

fun fooboo():
  a = #point(x=0, y=0)
  a = a#point(x=42)
  a#point.x

record point: #(a, b)

type stuff: int() | float()

fun fooboo2():
  maps.put('a, 2)

spec stuff(int()): int()
fun stuff(x): x + 2

fun wowzer():
 ((1 + 1) + (1 + 2))

fun wowzer_fun():
  #wowzer/0

fun woop():
  (1, 2) => 2 + 2

fun oopsie():
  try stuff():
    _: 'ok
  catch:
    error.e:
      #('error, error, e)

fun foo():
  match bar():
    _result:
      'ok

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
  () =>
    a = 2
    b = 3
    a + b

fun multi_line_paren_op():
  (2 + 4) +
  (3 + 4)

fun multi_line_match_equal():
  a =
  match 42:
    _x: 'ok
  a

fun multi_line_arrow_equal():
  a =
  () =>
    x = 2
    x + 4
  a
