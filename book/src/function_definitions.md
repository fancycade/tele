# Function Definitions

Define a function using the `fun` keyword.

```
fun hello_world(): io.format("Hello, World!")
```

A function signature can have params:

```
fun add(a, b): a + b
```

A function body can be many lines:

```
fun some_math(x, y):
  z = x + y
  d = z + 42
  d
```

Function definitions use block syntax. This means that this would be invalid function definition:

```
fun add(a, b):
a + b
```

The body of the function needs to be 1 column ahead of the starting column of `fun` (in simple terms the `f`).

Tele like Erlang, Lisp, Ruby, or Rust has an implicit return with the last value in the function body.

Tele also supports pattern matching in the signature. For example we can implement factorial like this:

```
fun factorial
  (0): 1
  (n): n * factorial(n - 1)
```

This means when factorial is called with 0 it returns 1. If it is called with any value besides 0 it multiplies that number by the recursive call of factorial with n subtracted by 1.

This can be helpful if we want to pattern match on different results. A common pattern might look like this:

```
fun handle_result
  (#('ok, v)): #('ok, v + 2)
  (#('error, _err) = e): e
```

Here if `handle_result` is called with an ok tuple we add 2 to the value. In the case it is called with an error tuple then we simply return that entire tuple.
