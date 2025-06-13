# Lists

Tele's lists have the exact same syntax as [Erlang](https://www.erlang.org/doc/system/data_types.html#list).

```
[1, 2, 3]
```

In functional languages it is common to "cons" or make a new list with an element prepended to the original.

The syntax for that looks like this:

```
l = [2, 3]
l2 = [1 | l]
```

`l2` will now look like this:
```
[1, 2, 3]
```

We can cons multiple elements as well:

```
l = [4, 5, 6]
[1, 2, 3 | l]
```


