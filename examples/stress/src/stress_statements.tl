import stress_math(
  add2/1
)

type tx_type: 'debit | 'credit

spec get_tx_type(integer): tx_type
fun get_tx_type(n):
  case n >= 0:
    'true: 'debit
    'false: 'credit

spec add2_example(): integer
fun add2_example():
  add2(4)

record point: #(x = 0, y = 0)

spec point_default(): #point()
fun point_default():
  #point()

spec point_new(integer, integer): #point
fun point_new(x, y):
  #point(x=x, y=y)

spec point_up(#point): #point
fun point_up(p):
  p#point(y = p#point.y - 1)

spec point_down(#point): #point
fun point_down(p):
  p#point(y = p#point.y + 1)

spec point_left(#point): #point
fun point_left(p):
  p#point(x = p#point.x - 1)

spec point_right(#point): #point
fun point_right(p):
  p#point(x = p#point.x + 1)
