# Getting Started

If you have Tele [installed](./installation.md) you'll want to do more than [hello_world](./hello_world.md).

In this section we will show you how to setup an Erlang library and write the code with Tele.

## Create A New Project

Use Rebar3 to create a new Erlang project.

```
rebar3 new lib example
```

There will be a few files of note. You can read more about `rebar.config` and `src/example.app.src` in the Rebar3 [docs](https://rebar3.org/docs/basic_usage/).

Since we are coding in Tele we can go ahead and delete `src/example.erl` and make a new tele file in it's place.

```
rm -f src/example.erl
touch src/example.tl
```

Inside of `src/example.tl` make a module like this:

```
fun add2(x): x + 2
```

Run `tele build`. Then start the shell, `rebar3 shell`.

In the shell we can run our module:

```
example:add2(4).
=> 6
```
