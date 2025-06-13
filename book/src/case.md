# Case

Tele uses case statements to handle conditional statements. Think of it as a switch statement/if-else statement on steroids.

Let's see an example:

```
case some_function():
  1: 'one
  2: 'two
  3: 'three
  _: 'undefined
```

Here a function `some_function` is called that might return 1, 2, 3, or something else.

The equivalent to an if statement in Tele is something like this:
```
case 1 =:= 1:
  'true: 'ok
  'false: #('error, 'math_broken)
```

## Equivalence with Function Definitions

A key thing to know about Tele is that case statements are equivalent to pattern matching with function definitions.

This case statement:

```
case some_variable:
  1: 'one
  2: 'two
  3: 'three
  _: 'undefined
```
Is equivalent to this:

```
fun handle_result
  (1): 'one
  (2): 'two
  (3): 'three
  (_): 'undefined
```

We can handle `some_variable` with the function call:

```
handle_result(some_variable)
```
