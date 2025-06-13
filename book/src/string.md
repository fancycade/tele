# Strings

Tele has a basic "string" datatype that looks like this:

```
"Hello, World!"
```

While this looks like a string in other programming languages, under the hood it is actually what we call a [binary](https://www.erlang.org/doc/system/data_types.html#bit-strings-and-binaries).

In simple term's it is basically a static string. There is more nuance to them that, but for now keep it simple.

When the above string gets compiled to Erlang it gets compiled to this:

```
<<"Hello, World!"/utf8>>
```

This sane default is actually taken from Elixir.

## Erlang String's

What can be confusing is that Erlang has it's own string datatype. This string is a list of characters.
In practice, the binary is more in line with the popular notion of static string hence why it is preferred by tele.

If you really need an Erlang string (a list of characters) you can always do this:

```
binary_to_list("Hello, World!")
```

When coding in Tele keep this in mind because while they look like the strings you might be familiar with in Javascript and Python they have different semantics at times.
