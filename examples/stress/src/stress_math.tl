fun add(x, y): x + y
fun sub(x, y): x - y
fun add2(x): x + 2
fun add3(x): x + 3

fun factorial
  (0): 1
  (n): n * factorial(n - 1)

fun fibonacci
  (0): 0
  (1): 1
  (x) when x > 0:
    fibonacci(x - 1) + fibonacci(x - 2)
