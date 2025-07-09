# Hello World

Let's make sure everything is working by making a simple 'Hello, World!' program.

Make a file called `hello.tl`:

```
fun hello_world():
  io.format("Hello, World!~n", [])
```

Then we compile the program:

```
tele compile hello.tl
```

This will make a file `hello.erl`:

```
-module(hello).
-export([hello_world/0]).

hello_world() ->
    io:format(<<"Hello, World!~n"/utf8>>, []).
```

We can fire up the Erlang REPL and call the function of this Erlang module we made:

Start the REPL like this:
```
erl
```

Compile the Erlang module `hello.erl`.
```
c(hello).
```

Then call our `hello_world` method from the `hello` module.

```
hello:hello_world().
=> Hello, World!

```
