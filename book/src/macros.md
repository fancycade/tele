# Macros

Tele supports Erlang Macros. The definition and usage is similar as well.

You can read more about Erlang macros [here](https://www.erlang.org/doc/system/macros.html#defining-and-using-macros).

Macros are used for a few purposes. One of them is like a constant:

```
define MAGIC_NUMBER: 42
```

Then when we want to use that macro it looks like this:

```
fun math_stuff():
  ?MAGIC_NUMBER + 2
```

Macros can also take parameters like a function.

Let's say we had some tuple shape we like but want to determine that static values later:

```
define MAGIC_TUPLE(a, b):
  #('a, a, 'b, b)
```

We can use this macro like this:

```
fun tuple_stuff():
  ?MAGIC_TUPLE(1, 2)
```

Would expand to:

```
fun tuple_stuff():
  #('a, 1, 'b, 2)
```

There are a number of uses for macros, but keep in mind these macros are less powerful than say Lisp's or Elixir's.
