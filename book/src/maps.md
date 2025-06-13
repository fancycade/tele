# Maps

Maps in Erlang use the familiar JSON notation for dictionaries and objects:

An empty map looks like this:
```
{}
```

A map with keys and values looks like this:

```
{"key": "value"}
```

## Map Update

There is syntax to update a map:

```
m = {"key": "value"}
m2 = {m | "key2": "value2"}
```

It's possible to update multiple keys at a time as well:

```
m2 = {m | "key": "new_value", 
          "key2": "value2",
          "key3": "value3"}
```

This syntax was inspired by Elm's record update syntax.
