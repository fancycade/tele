# Tuples

Tuples are a static array of values. While they might not be used as much in other languages, they are conventionally very common in Tele or Erlang.

The syntax for tuples looks like this:
```
#(... , ...)
```

This syntax is actually taken from Gleam!

## Ok/Error tuple

A common convention is to return a result as a tuple with an `'ok` atom or an `'error` atom.

ok:
```
#('ok, some_value)
```

error:
```
#('error, 'error_message)
```

The different tuples can be handled with a case statement:

```
case some_function():
  #('ok, v): v
  #('error, _err) = e: e
```

Here we return the variable `v` if `some_function` returns an ok tuple, and we return the error tuple itself in that condition.
