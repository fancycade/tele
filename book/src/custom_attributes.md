# Custom Attributes

Sometimes there are attributes that Tele does not have builtin. These might come from libraries or a new feature from Erlang not yet supported.

This can be supported by using the `attr` keyword.

If the Erlang attribute looks like this:

```
-some_attr(hello)
```

The Tele equivalent would be:

```
attr some_attr('hello)
```

Notice we hade to use the atom syntax for `'hello`. This is because we have to be explicit with the syntax.

The builtin attributes can work around this with syntax sugar.
