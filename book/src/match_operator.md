# Match Operator

In most programming languages, `=` means assignment. In Erlang, and therefore Tele, `=` is the match operator.

Pattern matching is an important aspect of Tele (Erlang).

Let's say we make a variable like this:

```
a = 2
```

Then we try to do this:

```
a = 3
```

We will get an error like this:

```
** exception error: no match of right hand side value 3
```

Not only are the variables immutable, but the match fails because `a` is already `2`. It worked the first time because `a` did not have a value yet.

Pattern matching works for things like data structures as well.

This works:
```
[1, 2, 3] = [1, 2, 3]
```

This doesn't:
```
[1, 2, 3] = [4, 5, 6]
```

It will throw this error message:
```
** exception error: no match of right hand side value [4,5,6]
```

## Destructuring

We know that pattern matching works at assigning values to variables and it works on data structures.
It is a common pattern to access values in a data structure. We can do this in Erlang in a few ways depending on the data structure.

Let's say we have a list like this:

```
l = [1, 2, 3]
```

And we want to get the first value of the list:

```
[head | tail] = l
```

This is a very common pattern in functional programming languages using the `cons` operator.

```
head = 1
```

and

```
tail = [2, 3]
```

If we wanted to just get the head and ignore the tail we can use the `_` to ignore it.

```
[head | _] = l
```

or

```
[head | _tail] = l
```

Sometimes that can be helpful to describe the value we want to ignore.

Lists are a bit special with their use of the cons operator, but we will see how tuples and maps are even easier to match on.

An example with a tuple might look like this:

```
t = #('ok, 42)

#('ok, v) = t

v = 42
```

Here we see that we are matching `t` to a tuple with the atom `'ok` and the variable `v`. The `'ok` atom is important because if were to do something else then the pattern match would fail.

```
#('error, v) = t
```

Would throw an error.

Finally maps work similarly. Here is an example of how to destructure a map:

```
m = {"hello": "world", "foo": "bar"}

{"hello": world, "foo": some_value} = m

world = "world"

some_value = "bar" 
```

We could even just grab one key value pair instead of all of it:

```
{"hello": world} = m
world = "hello"
```

This works because `m` matches on a map with the field `"hello"`.
