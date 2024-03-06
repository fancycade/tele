const std = @import("std");
const test_allocator = std.testing.allocator;

const Token = union(enum) { lparen, rparen, open_list, close_list, open_tuple, rsquiggle, open_map, quote, equal, plus, minus, divide, multiply, greater, lesser, greater_equal, lesser_equal, open_record: []const u8, whitespace: usize, word: []const u8 };

fn next_token(r: anytype, t: *Token) !void {
    const res = try r.readByte();

    if (res == '(') {
        t.* = Token.lparen;
    }
}

test "next token" {
    const stdout = std.io.getStdOut();
    const stdin = std.io.getStdIn();

    try stdout.writeAll();

    var t: Token = undefined;
    _ = try next_token(stdin.reader(), &t);
    //var buf = std.mem.zeroes([256]u8);
}
