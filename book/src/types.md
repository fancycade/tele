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

### Private Type Definitions

Types are exported from the module by default. Use `typep` if you do not want the type to be exported.

```
typep internal_id: integer
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

## Nominal Types

As of Erlang 28, nominal types are supported. Nominal types are user-defined types that are distinguished by their name,
not their structure.

In Tele a nominal type definition looks like this:

```
nominal id: integer
```

Now function definitions like this would fail a type check:

```
spec add2(integer): integer
fun add2(a): a + 2

spec get_todo(id): map
fun get_todo(id): ...

fun get_todo2():
  get_todo(add2(0))
```

`get_todo(add2(0))` would fail the type check because `get_todo` accepts an id as an input not an integer.
