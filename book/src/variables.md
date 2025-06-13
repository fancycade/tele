# Variables

Variables in Tele are conventionally lowercase and can include underscores.

Example:
```
a = 2
hello_world = "Hello, World!"
```

## Absolute Variables

There are some edge cases in the syntax where it is ambiguous if it is a variable or not. To ensure the compiler knows we are using a variable Tele introduces
the idea of an absolute variable. Think of it like we want the compiler to be absolutely sure that value is a variable.

Absolute variables can be used anywhere a variable is used. Later in this book there will be shown actual uses of absolute variables.

For now you only need to that they exist and they look like this:

```
@a = 2
```
