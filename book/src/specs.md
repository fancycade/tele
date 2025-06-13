# Specs

In Tele we can specify types for our functions using specs.

For example:
```
fun add(a, b): a + b
```

We can add a spec for it like this:

```
spec add(integer, integer): integer
fun add(a, b): a + b
```

This means it takes two arguments of type `integer` and returns an `integer`.
