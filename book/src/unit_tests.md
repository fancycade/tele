# Unit Tests

Tele leverages Erlang's unit test framework called [eunit](https://www.erlang.org/doc/apps/eunit/chapter.html).

Let's say we have a module with this function:

```
fun add2(x): x + 2
```

This is a function that adds 2 to the given input value.

Now we want to make a unit test for it.

Here we will introduce the `test` attribute block.

Anything under this block will only be compiled during the testing stage.

Let's add the `eunit` header.

```
test:
  include_lib("eunit/include/eunit.hrl")
```

Eunit works by checking functions with the `_test` suffix in the name.

Let's make our unit test function.

```
test:

  include_lib("eunit/include/eunit.hrl")

  fun add2_test():
    ?assertEqual(4, add2(2))
```

Now run `tele test` to run the unit tests. The `?assertEqual` macro is provided by the header we included. It provides some useful error messages when testing.

## Named Test Blocks

Instead of making functions with the `_test` suffix we can use named test blocks. This is a syntax sugar that Tele provides.

So let's change the example above to look like this:

```
test:
  include_lib("eunit/include/eunit.hrl")

test add2:
  ?assertEqual(4, add2(2))
```

This feature is inspired by Zig's unit testing approach. A benefit of this is that we can put the unit test right next to the function we are testing.
So the Tele way of writing this module would look like this:

```
test:
  include_lib("eunit/include/eunit.hrl")

fun add2(x): x + 2

test add2:
  ?assertEqual(4, add2(2))
  ?assertEqual(5, add2(3))

fun add3(x): x + 3

test add3:
  ?assertEqual(4, add3(1))
  ?assertEqual(5, add3(2))
```

In the above example we added another function `add3` to stress the point of how to organize functions with their associated unit tests.
