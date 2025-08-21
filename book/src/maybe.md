# Maybe

Erlang introduced maybe expressions to deal with excessively nested case statements.

Take this nested case statement:

```
case foobar():
  #('ok, v):
    case barfoo(v):
      #('ok, v2):
        case final_foo(v2):
          #('ok, v3): v3
          #('error, _err) = e: e
      #('error, _err) = e: e
  #('error, _err) = e: e
```

This may seem like a corner case but it is more common than you think since the ok/error tuple pattern
is very common in Erlang.

Let's see how we can refactor that with a [maybe](https://www.erlang.org/doc/system/expressions.html#maybe) expression.

```
maybe:
  #('ok, v) ?= foobar()
  #('ok, v2) ?= barfoo(v)
  #('ok, v3) ?= final_foo(v2)
  v3
else:
  #('error, _err) = e: e
```

We can see that this is a lot easier to read. Not the `?=` which is called the conditional match operator.
If the result of the function call matches the ok tuple then it continues. If not, it short circuits to the `else`
and goes from there.
