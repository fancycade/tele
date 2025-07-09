# Types

## Type Expressions

Erlang comes with a variety of builtin [types](https://www.erlang.org/doc/system/typespec.html).

With Tele parentheses are optional for type expressions. For example the `integer` type can be represented like this:

```
integer()
```

OR

```
integer
```

### Type Parameters

Some types can take other types as parameters for more specificity. For example the list type:

```
list
```

Can be more specific to be a list of integers:

```
list(integer)
```

## Type Definitions

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

## Type Variables

Since Tele uses lowercase letters for both types and variables it can make type variables ambiguous. This is solved by using an absolute variable.

Let's see how we can define a type with a type variable and use it.

Let's make a result type like so:

```
type result(@data): #('ok, @data) | #('error, any)
```

This is a type that can either be an ok tuple with some specified type or an error tuple with any type.

Let's see how we can use it for a function spec:

```
spec some_api_call(): result(list(map))
fun some_api_call():
  call_some_api()
```

Here we are specifying that the function `some_api_call` will return an okay tuple with a list of maps or an error tuple.
