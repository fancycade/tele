//def foobar(): // hello there
  // oof
//  42 // foo
// bar

//def foobar2():
//  43

def fooboo():
  a = #point(x=0, y=0)
  a = a#point(x=42)
  a#point.x

record point:
  #('a, 'b)

type stuff: int() | float()

def fooboo2():
  maps.put('a, 2)

spec stuff(int()): int()
def stuff(x): x + 2

def wowzer():
 ((1 + 1) + (1 + 2))

def wowzer_fun():
  #wowzer/0

def woop():
  (1, 2) => 2 + 2

def oopsie():
  try stuff():
    _: 'ok
  catch:
    error.e:
      #('error, error, e)

def foo():
  match bar():
    _result:
      'ok

def basic_bin():
  "hello world"

def hello_there():
  io.format("~s~n", ["Hello, World!"])

def hello_there2():
  io.format(
    "~s~n",
    ["Hello, World!"]
  )

def multi_line_arrow():
  () =>
    a = 2
    b = 3
    a + b

def multi_line_paren_op():
  (2 + 4) +
  (3 + 4)

def multi_line_match_equal():
  a =
  match 42:
    _x: 'ok
  a

def multi_line_arrow_equal():
  a =
  () =>
    x = 2
    x + 4
  a
