# hello_tele

An example erlang project that with tele files as source code. 

## Install

Ensure `erlang` and `rebar3` are installed.

## Build

Make sure a built tele binary is at `../../tele/zig-out/bin/`.

    source setup.sh
    tele build

## Usage

After building the code startup a rebar3 shell.

    rebar3 shell

Inside the shell run this function:

    hello_tele:hello_world().
