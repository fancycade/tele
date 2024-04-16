build:
        zig build

test:
        zig build test

basic:
        ./zig-out/bin/tele basic.tl
        cat basic.erl

clean:
        rm -f *.erl
        rm -f *.beam
