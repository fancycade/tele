# Installation

## Install Binary

Download the binary for your OS and Architecture [here](https://telelang.org/artifacts).

Rename the binary to `tele` and put it in your path.

Run `tele help` to see that it is working.

## Build From Source

Install Zig 0.14 from [here](https://ziglang.org/download/).

Download the Tele [code](https://github.com/fancycade/tele).

```
cd tele/tele
zig build
```

The binary will be at `zig-out/bin/tele`.

## Dependencies

The Tele binary only compiles Tele code to Erlang code. To actually run the code or do anything useful you will need Erlang and the build tool Rebar3.


### Install Erlang/Rebar3:

Install Erlang from [here](https://www.erlang.org/downloads).

Install Rebar3 from [here](https://www.rebar3.org/docs/getting-started/).
