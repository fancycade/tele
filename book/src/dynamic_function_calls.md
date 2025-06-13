# Dynamic Function Calls

Sometimes in Tele we want to do wild things like passing a function an atom that is then used to do a function call.

Imagine we have a list of function definitions like this:

```
fun foobar(): 7

fun barfoo(): 8

fun do_thing(): [1, 2, 3]
```

In this scenario we also might not know ahead of time how we want to call these functions. So we can figure it out at runtime.

```
fun caller(f):
  @f()
```

```
[7, 8, [1, 2, 3]] = lists.map(fun caller/1, ['foobar, 'barfoo, 'do_thing])
```
