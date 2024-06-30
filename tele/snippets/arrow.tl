fun foobar():
  () => 2

fun foobar2():
  (x) => x + 2

fun foobar3(x):
  (y) => x + y

fun foobar4():
  (y) => (x) => x + y

fun foobar5():
  (x, y) => x + y
