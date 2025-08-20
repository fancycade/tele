# Atoms

Atom's are data type literals. Tele borrows the orginal Lisp syntax for atoms with a little bit of extra.

They look like this:

```
'foo
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

## Booleans

Atom's are also how booleans are represented. Simply use these atoms:

```
'true
'false
```
