# Strings

Tele has a basic "string" datatype that looks like this:

```
"Hello, World!"
```

While this looks like a string in other programming languages, under the hood it is actually what we call a [binary](https://www.erlang.org/doc/system/data_types.html#bit-strings-and-binaries).

In simple term's it is basically a static string but you can do a lot more with binaries. We will address that later.

When the above string gets compiled to Erlang it gets compiled to this:

```
<<"Hello, World!"/utf8>>
```

If you code in Elixir this might be familiar as the same approach was taken.

If you really need an Erlang string (a list of characters) you can always do this:

```
binary_to_list("Hello, World!")
```

When coding in Tele keep this in mind because while they look like the strings you might be familiar with in Javascript and Python they have different semantics at times.
