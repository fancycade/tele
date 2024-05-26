build:
        zig build

test:
        zig build test

basic:
        ./zig-out/bin/tele basic.tl
        cat basic.erl
        cat other.tl

clean:
        rm -f *.erl
        rm -f *.beam
