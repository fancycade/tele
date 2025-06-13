# Anonymous Functions

In Tele anonymous functions look exactly like function definitions without a name (hence anonymous).

For example:

```
z = 42
f = fun (a): a + z
```

Since `f` is a variable we need to tell the Tele compiler we are doing a function call on a variable not a function defined as `f`.
We can use an absolute variable to do that.

```
output = @f(2)
output = 44
```

If we tried:
```
output = f(2)
```

We would get an error about how the function `f` isn't found.

Anonymous functions are values and can be passed into other functions. A common example is using a function to map over a list:

```
[3, 4, 5] = lists.map(fun (x): x + 2, [1, 2, 3])
```

Or if we wanted to indent it:

```
[3, 4, 5] = lists.map(
  fun (x):
    x + 2,
  [1, 2, 3]
)
```

Pattern matching is also possible with anonymous functions.

We could mix up the example above like this:

```
[0, 777, 5] = lists.map(
  fun
    (1): 0
    (2): 777
    (x): x + 2,
  [1, 2, 3]
)
```
