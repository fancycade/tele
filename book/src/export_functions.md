# Exporting Functions

To make a Tele module simply make a `.tl` file in your project.

Like Elixir, functions are exported by default. Let's make a simple module called `math.tl`:

```
fun add(a, b): a + b
fun sub(a, b): a - b
fun mul(a, b): a * b
fun div(a, b): a / b
```

Then we can make another module that uses these functions. We can call it `math_stuff.tl`.
To use a function from another module we can simply prefix the function name with the module like this:

```
module.function_name()
```

So if we wanted to use functions from the `math` module we just made we can do it like this:

```
fun do_math():
  x = math.add(1, 2)
  y = math.sub(x, 4)
  z = math.mul(y, 6)
  math.div(z, 3) 
```

This same idea works for modules written in Erlang. This works because Tele modules are Erlang modules.

For example if we wanted to use a method from Erlang's `lists` module in the standard library it might look like this:

```
spec do_map(list): list
fun do_map(l):
  lists.map(fun (x): x + 2, l)
```

This works both ways. So if we wanted to use the `math` module in an Erlang module, it might look like this:

```
-module(math_stuff).
-export([do_math/0]).

do_math() ->
    X = math:add(1, 2),
    Y = math:sub(X, 4),
    Z = math:mul(Y, 6),
    math:div(Z, 3).
```

If you are familiar with Erlang you can see that you would have no idea if the `math` module was written in Tele or Erlang.
