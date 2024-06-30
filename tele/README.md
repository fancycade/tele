# tele

tele is an alternate syntax for Erlang. tele code compiles directly to Erlang code.

tele does not have a standard library or build tool as it is meant to be seamlessly integrated into Erlang projects.

For more detailed info see `SPEC.md`.

## Install

Install latest version of Zig from [here](https://ziglang.org/download/).

Install Erlang, on Debian it would look like this:

    apt install erlang

## Build

Building tele is simple:

    zig build

The executable will be located at `./zig-out/bin/tele`.

## Test

    zig build test

## Usage

Make a simple tele module like this:

    fun hello_world():
      io.format("~s~n", ["Hello, World!"])

Name the file something like `hello.tl`.

Then use tele to compile it to Erlang:

    ./zig-out/bin/tele hello.tl

You can run the code by starting up an Erlang shell:

    erl

In the Erlang shell you can compile the Erlang code:

    c(hello).

Then you can call the `hello_world` function:

    hello:hello_world().

## License

Apache V2
