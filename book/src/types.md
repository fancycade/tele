# Type Expressions

In tele we can define our own types:

```
type point: #(integer, integer)
```

This defines a type called point that is a tuple with two integers.

We can use it in a a function spec.

```
spec move_up(point): point
fun move_up(#(x, y)): #(x, y - 1)
```
