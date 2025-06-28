# Guards

Guards are modifiers for pattern matching. Let's see an example with a function definition.

```
fun foobar
  (x) when x > 2: 0
  (x): x + 2
```

Notice the `when` keyword as part of the function definition pattern matching for the first clause. This means that
any number greater than two will return a result of zero. Anything else will be that number plus two.

Now to see it in action:

```
[3, 4, 0, 0, 0] = lists.map(fun foobar/1, [1, 2, 3, 4, 5])
```

## Guard Sequences

While this is rare, there are times when you might want to do multiple guards for a single pattern match. For this scenario we can use guard sequences.
The syntax for this leverages block syntax rules with the `when` keyword.

```
fun foobar (x, y) when x > 2
                  when y < 3:
  42
```

Note that the newline is required to separate the `when` blocks.

This is a derived example. You would probably prefer something like this instead:

```
fun foobar(x, y) when x > 2 and y < 3: 42
```
