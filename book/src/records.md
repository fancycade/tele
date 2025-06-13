# Records

Records in Tele have a similar syntax as tuples. This is appropriate because records are semantically just named tuples.

## Definitions

```
record point: #(x, y)
```

You can define records with types:

```
record point: #(x: integer, y: integer)
```

And even default values:

```
record point: #(x = 0, y = 0)
```

Putting it all together:

```
record point: #(x: integer = 0, y: integer = 0)
```

Record definitions use block syntax so to put the definition on multiple lines we can do something like this:

```
record point: 
  #(
    x: integer = 0,
    y: integer = 0 
  )
```

## Instantiating

Once a record is defined we can use this syntax to instantiate a new record:

```
#point(x = 42, y = 43)
```

If we only want to set some of the fields we can:
```
#point(x = 42)
```

## Updating

To update a field in a record we need to assign our record to a variable first.

This creates a point record with no fields defined. If we set our default values like we did before then x and y are both 0. 
```
p = #point()
```

To update `p` we do this:

```
p2 = p#point(x = 24)
```

## Accessing Fields

After we instantiate a record we will need to get access to its fields. Make sure it is assigned to a variable. Then we can do this:

```
p = #point(x = 42, y = 42)

p#point.x
```

Notice we had to include the record type in the name. That's because the Erlang compiler needs to know what type of record it is for the field access to work.
