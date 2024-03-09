const std = @import("std");
const test_allocator = std.testing.allocator;
const eql = std.mem.eql;
const expect = std.testing.expect;

const TokenMode = enum { word, op, hash, bit_string, none };

fn read_token(r: anytype, l: *std.ArrayList(u8), leftover: *u8) !bool {
    var mode: TokenMode = TokenMode.none;

    if (leftover.* != 0) {
        if (special_char(leftover.*)) {
            try l.append(leftover.*);
            if (leftover.* == '#') {
                mode = TokenMode.hash;
            } else if (!op_char(leftover.*)) {
                leftover.* = 0;
                return false;
            } else if (op_char(leftover.*)) {
                mode = .op;
            }
        } else {
            try l.append(leftover.*);
            mode = .word;
        }
        leftover.* = 0;
    }

    while (true) {
        const b = r.readByte() catch |err| switch (err) {
            error.EndOfStream => {
                return true;
            },
            else => |e| return e,
        };

        switch (mode) {
            .none => {
                if (special_char(b)) {
                    try l.append(b);

                    if (b == '#') {
                        mode = .hash;
                    } else if (op_char(b)) {
                        mode = .op;
                    } else {
                        return false;
                    }
                } else {
                    try l.append(b);
                    mode = .word;
                }
            },
            .word => {
                if (special_char(b)) {
                    if (b != '#') {
                        leftover.* = b;
                        return false;
                    }
                }
                try l.append(b);
            },
            .op => {
                if (op_char(b)) {
                    try l.append(b);

                    if (b == '<') {
                        mode = .bit_string;
                    }
                } else {
                    leftover.* = b;
                    return false;
                }
            },
            .hash => {
                try l.append(b);
                if (b == '(') {
                    return false;
                }
            },
            .bit_string => {
                if (b == '>') {
                    if (l.items[l.items.len - 1] == '>') {
                        try l.append(b);
                        return false;
                    }
                }

                try l.append(b);
            },
        }
    }

    return false;
}

fn special_char(c: u8) bool {
    switch (c) {
        '(', ')', '{', '}', '[', ']', ':', ',', '\n', '#', '-', '+', '/', '*', '<', '>', '|', '_', ' ', '=' => {
            return true;
        },
        else => {},
    }
    return false;
}

fn op_char(c: u8) bool {
    switch (c) {
        '-', '+', '/', '*', '<', '>', '=' => {
            return true;
        },
        else => {},
    }
    return false;
}

test "read token" {
    const file = try std.fs.cwd().openFile(
        "snippets/tokens.tl",
        .{ .mode = .read_only },
    );
    defer file.close();

    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var leftover: u8 = 0;
    const end_file = try read_token(file.reader(), &list, &leftover);

    try expect(!end_file);
    try expect(eql(u8, list.items, "("));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, ")"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "\n"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "["));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "]"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "\n"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "{"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "}"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "\n"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "foobar"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "\n"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "1"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "\n"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "$c"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "\n"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "42.42"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "\n"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "="));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "\n"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "+"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "\n"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "-"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "\n"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "/"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "\n"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "=="));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "\n"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, ">="));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "\n"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "<="));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "\n"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "->"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "\n"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "|"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "\n"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "#person("));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, ")"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "\n"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "#("));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, ")"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "\n"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "16#FFFF"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "\n"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "1"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "+"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "1"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "\n"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "<<\"binary\">>"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "\n"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "\"string\""));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "\n"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "io.read"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "\n"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "{"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "foo"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, ":"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "bar"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "}"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "\n"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "{"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, " "));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "foo"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, " "));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, ":"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, " "));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "bar"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, " "));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "}"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "\n"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "["));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "1"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, ","));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, " "));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "2"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, ","));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, " "));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "3"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "]"));
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &leftover);
    try expect(eql(u8, list.items, "\n"));
    list.clearAndFree();

    const eof = try read_token(file.reader(), &list, &leftover);
    try expect(eof);
}
