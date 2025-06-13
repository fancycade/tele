# Atoms

Atom's are data type literals. Tele borrows the orginal Lisp syntax for atoms with a little bit of extra.

They look like this:

```
'foo
```

Since it is common for autocomplete to wrap single quotes when making an atom, a surrounding quote is also supported:

```
'foo'
```

Keep in mind that two single quotes will not work with spaces as it does in Erlang.

You can't do this:
```
'foo bar'
```

This is because the Tele tokenizer uses spaces or newlines to delimit the end of the atom in the case when only one single quote is used.

However, there is a special syntax to support this case:

```
#'foo bar'
```

TODO: Don't support this case?

One can always do this:
```
binary_to_atom("foo bar")
```

## Booleans

Atom's are also how booleans are represented. Simply use these atoms:

```
'true
'false
```
