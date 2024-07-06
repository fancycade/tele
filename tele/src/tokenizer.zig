const std = @import("std");
const test_allocator = std.testing.allocator;
const eql = std.mem.eql;
const expect = std.testing.expect;

const TokenQueueError = error{MissingHead};

const TokenQueueNode = struct { next: ?*TokenQueueNode, body: []const u8, line: usize, col: usize };

// FIFO Queue
pub const TokenQueue = struct {
    const Self = @This();

    head: ?*TokenQueueNode,
    tail: ?*TokenQueueNode,
    len: usize,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !*Self {
        const queue = try allocator.create(Self);
        queue.*.allocator = allocator;
        queue.*.head = null;
        queue.*.tail = null;
        queue.*.len = 0;
        return queue;
    }

    pub fn deinit(self: *Self) void {
        var cur = self.head;

        while (cur != null) {
            const next = cur.?.next;
            self.allocator.free(cur.?.body);
            self.allocator.destroy(cur.?);
            cur = next;
        }

        self.allocator.destroy(self);
    }

    pub fn push(self: *Self, token_body: []const u8, line_number: usize, col_number: usize) !void {
        const node = try self.allocator.create(TokenQueueNode);
        node.*.next = null;
        node.*.body = token_body;
        node.*.line = line_number;
        node.*.col = col_number;

        if (self.head == null) {
            self.head = node;
            self.tail = node;
        } else {
            self.tail.?.next = node;
            self.tail = self.tail.?.next;
        }
        self.len += 1;
    }

    pub fn pop(self: *Self) !*TokenQueueNode {
        if (self.len == 0) {
            return TokenQueueError.MissingHead;
        }

        if (self.head == null) {
            return TokenQueueError.MissingHead;
        } else {
            const node = self.head;
            self.head = self.head.?.next;
            node.?.next = null;
            return node.?;
        }
        self.len -= 1;
    }

    pub fn peek(self: *Self) !*TokenQueueNode {
        if (self.head == null) {
            return TokenQueueError.MissingHead;
        } else {
            return self.head.?;
        }
    }

    pub fn empty(self: *Self) bool {
        return self.head == null;
    }

    pub fn count(self: *Self) usize {
        return self.len;
    }
};

test "token queue" {
    const queue = try TokenQueue.init(test_allocator);
    defer queue.deinit();

    const buf: []u8 = try test_allocator.alloc(u8, 13);
    std.mem.copyForwards(u8, buf, "hello, world!");

    try queue.push(buf, 0, 0);

    try std.testing.expect(!queue.empty());

    const node_ptr = try queue.peek();
    try std.testing.expect(std.mem.eql(u8, node_ptr.*.body, "hello, world!"));

    const node = try queue.pop();

    try std.testing.expect(std.mem.eql(u8, node.*.body, "hello, world!"));

    try std.testing.expect(queue.empty());

    test_allocator.free(node.*.body);
    test_allocator.destroy(node);
}

const TokenContext = struct { leftover: u8, line_number: usize, col_number: usize, next_line_number: usize, next_col_number: usize };

pub fn read_tokens(r: anytype, allocator: std.mem.Allocator) !*TokenQueue {
    const queue: *TokenQueue = try TokenQueue.init(allocator);

    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var ctx = TokenContext{ .leftover = 0, .line_number = 0, .col_number = 0, .next_line_number = 0, .next_col_number = 0 };

    while (!try read_token(r, &buffer, &ctx)) {
        if (buffer.items.len > 0) {
            try queue.push(try buffer.toOwnedSlice(), ctx.line_number, ctx.col_number);
        }
    }

    return queue;
}

const TokenMode = enum { word, op, hash, bit_string, binary, none };

fn read_token(r: anytype, l: *std.ArrayList(u8), ctx: *TokenContext) !bool {
    ctx.line_number = ctx.next_line_number;
    ctx.col_number = ctx.next_col_number;
    var mode: TokenMode = TokenMode.none;

    if (ctx.*.leftover != 0) {
        if (special_char(ctx.*.leftover)) {
            if (ctx.*.leftover == '/') {
                const b2 = r.readByte() catch |err| switch (err) {
                    error.EndOfStream => {
                        return true;
                    },
                    else => |e| return e,
                };

                if (b2 == '/') {
                    ctx.*.leftover = 0;
                    // Parse comments
                    while (true) {
                        const b3 = r.readByte() catch |err| switch (err) {
                            error.EndOfStream => {
                                return true;
                            },
                            else => |e| return e,
                        };

                        if (b3 == '\n') {
                            return false;
                        }

                        ctx.*.next_line_number += 1;
                        ctx.*.next_col_number = 0;
                    }
                } else {
                    try l.append(ctx.*.leftover);
                    ctx.*.leftover = b2;
                    ctx.*.next_col_number += 1;
                    return false;
                }
            }

            try l.append(ctx.*.leftover);

            if (ctx.*.leftover == '#') {
                mode = TokenMode.hash;
            } else if (ctx.*.leftover == '"') {
                mode = .binary;
            } else if (!op_char(ctx.*.leftover)) {
                ctx.*.leftover = 0;
                ctx.*.next_col_number += 1;
                return false;
            } else if (op_char(ctx.*.leftover)) {
                mode = .op;
                ctx.*.next_col_number += 1;
            }
        } else if (whitespace(ctx.*.leftover)) {
            ctx.next_col_number += 1;
            ctx.col_number += 1;
            // Skip
        } else if (newline(ctx.*.leftover)) {
            ctx.*.next_line_number += 1;
            ctx.*.line_number += 1;
            ctx.*.col_number = 0;
            ctx.*.next_col_number = 0;
        } else {
            try l.append(ctx.*.leftover);
            ctx.*.next_col_number += 1;
            mode = .word;
        }
        ctx.*.leftover = 0;
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
                    if (b == '/') {
                        const b2 = r.readByte() catch |err| switch (err) {
                            error.EndOfStream => {
                                return true;
                            },
                            else => |e| return e,
                        };

                        if (b2 == '/') {
                            ctx.*.leftover = 0;
                            // Parse comments
                            while (true) {
                                const b3 = r.readByte() catch |err| switch (err) {
                                    error.EndOfStream => {
                                        return true;
                                    },
                                    else => |e| return e,
                                };

                                if (b3 == '\n') {
                                    return false;
                                }

                                ctx.*.next_line_number += 1;
                                ctx.*.next_col_number = 0;
                            }
                        } else {
                            ctx.*.leftover = b2;
                        }
                    }

                    try l.append(b);
                    ctx.*.next_col_number += 1;

                    if (b == '/') {
                        return false;
                    } else if (b == '"') {
                        mode = .binary;
                    } else if (b == '#') {
                        mode = .hash;
                    } else if (op_char(b)) {
                        mode = .op;
                    } else {
                        return false;
                    }
                } else if (whitespace(b)) {
                    ctx.*.next_col_number += 1;
                    ctx.*.col_number += 1;
                } else if (newline(b)) {
                    ctx.*.line_number += 1;
                    ctx.*.col_number = 0;
                    ctx.*.next_line_number += 1;
                    ctx.*.next_col_number = 0;
                } else {
                    try l.append(b);
                    ctx.*.next_col_number += 1;
                    mode = .word;
                }
            },
            .word => {
                if (special_char(b)) {
                    if (b != '#') {
                        ctx.*.leftover = b;
                        return false;
                    }
                } else if (newline(b)) {
                    ctx.*.leftover = b;
                    //ctx.*.next_line_number += 1;
                    //ctx.*.next_col_number += 1;
                    return false;
                } else if (whitespace(b)) {
                    //ctx.*.next_col_number += 1;
                    ctx.*.leftover = b;
                    return false;
                }
                try l.append(b);
                ctx.*.next_col_number += 1;
            },
            .op => {
                if (op_char(b)) {
                    try l.append(b);
                    ctx.*.next_col_number += 1;

                    if (b == '<') {
                        mode = .bit_string;
                    }
                } else if (whitespace(b)) {
                    ctx.*.leftover = b;
                    return false;
                } else if (newline(b)) {
                    ctx.*.next_col_number = 0;
                    ctx.*.next_line_number += 1;
                    return false;
                } else {
                    ctx.*.leftover = b;
                    return false;
                }
            },
            .hash => {
                if (whitespace(b)) {
                    ctx.*.leftover = b; // TODO: Is this needed?
                    return false;
                } else if (newline(b)) {
                    ctx.*.next_col_number = 0;
                    ctx.*.next_line_number += 1;
                    return false;
                }

                try l.append(b);
                ctx.*.next_col_number += 1;
                if (b == '(') { // Is tuple start
                    return false;
                }
            },
            .bit_string => {
                if (b == '>') {
                    if (l.items[l.items.len - 1] == '>') {
                        try l.append(b);
                        ctx.*.next_col_number += 1;
                        return false;
                    }
                }

                try l.append(b);
                ctx.*.next_col_number += 1;
            },
            .binary => {
                try l.append(b);
                ctx.*.next_col_number += 1;
                if (b == '"') {
                    return false;
                }
            },
        }
    }

    return false;
}

fn whitespace(c: u8) bool {
    return c == ' ' or c == '\t';
}

fn newline(c: u8) bool {
    return c == '\n' or c == '\r';
}

fn special_char(c: u8) bool {
    switch (c) {
        '(', ')', '{', '}', '[', ']', ':', ',', '#', '-', '+', '/', '*', '<', '>', '|', '=', '"' => {
            return true;
        },
        else => {},
    }
    return false;
}

fn op_char(c: u8) bool {
    switch (c) {
        '-', '+', '/', '*', '<', '>', '=', '|' => {
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

    var ctx = TokenContext{ .leftover = 0, .line_number = 0, .col_number = 0, .next_line_number = 0, .next_col_number = 0 };
    const end_file = try read_token(file.reader(), &list, &ctx);

    try expect(!end_file);
    try expect(eql(u8, list.items, "("));
    try expect(ctx.line_number == 0);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_col_number == 1);
    try expect(ctx.next_line_number == 0);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, ")"));
    try expect(ctx.line_number == 0);
    try expect(ctx.col_number == 1);
    try expect(ctx.next_col_number == 2);
    try expect(ctx.next_line_number == 0);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "["));
    try expect(ctx.line_number == 1);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 1);
    try expect(ctx.next_col_number == 1);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "]"));
    try expect(ctx.line_number == 1);
    try expect(ctx.col_number == 1);
    try expect(ctx.next_line_number == 1);
    try expect(ctx.next_col_number == 2);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "{"));
    try expect(ctx.line_number == 2);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 2);
    try expect(ctx.next_col_number == 1);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "}"));
    try expect(ctx.line_number == 2);
    try expect(ctx.col_number == 1);
    try expect(ctx.next_line_number == 2);
    try expect(ctx.next_col_number == 2);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "foobar"));
    try expect(ctx.line_number == 3);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 3);
    try expect(ctx.next_col_number == 6);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "1"));
    try expect(ctx.line_number == 4);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 4);
    try expect(ctx.next_col_number == 1);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "$c"));
    try expect(ctx.line_number == 5);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 5);
    try expect(ctx.next_col_number == 2);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "42.42"));
    try expect(ctx.line_number == 6);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 6);
    try expect(ctx.next_col_number == 5);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "="));
    try expect(ctx.line_number == 7);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 8);
    try expect(ctx.next_col_number == 0);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "+"));
    try expect(ctx.line_number == 8);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 9);
    try expect(ctx.next_col_number == 0);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "-"));
    try expect(ctx.line_number == 9);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 10);
    try expect(ctx.next_col_number == 0);
    list.clearAndFree();

    try expect(ctx.leftover == 0);
    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "/"));
    try expect(ctx.line_number == 10);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 10);
    try expect(ctx.next_col_number == 1);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "=="));
    try expect(ctx.line_number == 11);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 12);
    try expect(ctx.next_col_number == 0);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, ">="));
    try expect(ctx.line_number == 12);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 13);
    try expect(ctx.next_col_number == 0);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "<="));
    try expect(ctx.line_number == 13);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 14);
    try expect(ctx.next_col_number == 0);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "->"));
    try expect(ctx.line_number == 14);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 15);
    try expect(ctx.next_col_number == 0);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "|"));
    try expect(ctx.line_number == 15);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 16);
    try expect(ctx.next_col_number == 0);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "#person("));
    try expect(ctx.line_number == 16);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 16);
    try expect(ctx.next_col_number == 8);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, ")"));
    try expect(ctx.line_number == 16);
    try expect(ctx.col_number == 8);
    try expect(ctx.next_line_number == 16);
    try expect(ctx.next_col_number == 9);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "#("));
    try expect(ctx.line_number == 17);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 17);
    try expect(ctx.next_col_number == 2);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, ")"));
    try expect(ctx.line_number == 17);
    try expect(ctx.col_number == 2);
    try expect(ctx.next_line_number == 17);
    try expect(ctx.next_col_number == 3);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "16#FFFF"));
    try expect(ctx.line_number == 18);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 18);
    try expect(ctx.next_col_number == 7);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "1"));
    try expect(ctx.line_number == 19);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 19);
    try expect(ctx.next_col_number == 1);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "+"));
    try expect(ctx.line_number == 19);
    try expect(ctx.col_number == 1);
    try expect(ctx.next_line_number == 19);
    try expect(ctx.next_col_number == 2);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "1"));
    try expect(ctx.line_number == 19);
    try expect(ctx.col_number == 2);
    try expect(ctx.next_line_number == 19);
    try expect(ctx.next_col_number == 3);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "<<\"binary\">>"));
    try expect(ctx.line_number == 20);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 20);
    try expect(ctx.next_col_number == 12);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "\"string\""));
    try expect(ctx.line_number == 21);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 21);
    try expect(ctx.next_col_number == 8);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "io.read"));
    try expect(ctx.line_number == 22);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 22);
    try expect(ctx.next_col_number == 7);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "{"));
    try expect(ctx.line_number == 23);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 23);
    try expect(ctx.next_col_number == 1);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "foo"));
    try expect(ctx.line_number == 23);
    try expect(ctx.col_number == 1);
    try expect(ctx.next_line_number == 23);
    try expect(ctx.next_col_number == 4);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, ":"));
    try expect(ctx.line_number == 23);
    try expect(ctx.col_number == 4);
    try expect(ctx.next_line_number == 23);
    try expect(ctx.next_col_number == 5);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "bar"));
    try expect(ctx.line_number == 23);
    try expect(ctx.col_number == 5);
    try expect(ctx.next_line_number == 23);
    try expect(ctx.next_col_number == 8);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "}"));
    try expect(ctx.line_number == 23);
    try expect(ctx.col_number == 8);
    try expect(ctx.next_line_number == 23);
    try expect(ctx.next_col_number == 9);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "{"));
    try expect(ctx.line_number == 24);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 24);
    try expect(ctx.next_col_number == 1);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "foo"));
    try expect(ctx.line_number == 24);
    try expect(ctx.col_number == 2);
    try expect(ctx.next_line_number == 24);
    try expect(ctx.next_col_number == 5);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, ":"));
    try expect(ctx.line_number == 24);
    try expect(ctx.col_number == 6);
    try expect(ctx.next_line_number == 24);
    try expect(ctx.next_col_number == 7);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "bar"));
    try expect(ctx.line_number == 24);
    try expect(ctx.col_number == 8);
    try expect(ctx.next_line_number == 24);
    try expect(ctx.next_col_number == 11);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "}"));
    try expect(ctx.line_number == 24);
    try expect(ctx.col_number == 12);
    try expect(ctx.next_line_number == 24);
    try expect(ctx.next_col_number == 13);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "["));
    try expect(ctx.line_number == 25);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 25);
    try expect(ctx.next_col_number == 1);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "1"));
    try expect(ctx.line_number == 25);
    try expect(ctx.col_number == 1);
    try expect(ctx.next_line_number == 25);
    try expect(ctx.next_col_number == 2);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, ","));
    try expect(ctx.line_number == 25);
    try expect(ctx.col_number == 2);
    try expect(ctx.next_line_number == 25);
    try expect(ctx.next_col_number == 3);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "2"));
    list.clearAndFree();
    try expect(ctx.line_number == 25);
    try expect(ctx.col_number == 4);
    try expect(ctx.next_line_number == 25);
    try expect(ctx.next_col_number == 5);

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, ","));
    try expect(ctx.line_number == 25);
    try expect(ctx.col_number == 5);
    try expect(ctx.next_line_number == 25);
    try expect(ctx.next_col_number == 6);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "3"));
    try expect(ctx.line_number == 25);
    try expect(ctx.col_number == 7);
    try expect(ctx.next_line_number == 25);
    try expect(ctx.next_col_number == 8);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "]"));
    try expect(ctx.line_number == 25);
    try expect(ctx.col_number == 8);
    try expect(ctx.next_line_number == 25);
    try expect(ctx.next_col_number == 9);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "++"));
    try expect(ctx.line_number == 26);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 27);
    try expect(ctx.next_col_number == 0);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "a#person"));
    try expect(ctx.line_number == 27);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 27);
    try expect(ctx.next_col_number == 8);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "("));
    try expect(ctx.line_number == 27);
    try expect(ctx.col_number == 8);
    try expect(ctx.next_line_number == 27);
    try expect(ctx.next_col_number == 9);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "x"));
    try expect(ctx.line_number == 27);
    try expect(ctx.col_number == 9);
    try expect(ctx.next_line_number == 27);
    try expect(ctx.next_col_number == 10);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "="));
    try expect(ctx.line_number == 27);
    try expect(ctx.col_number == 10);
    try expect(ctx.next_line_number == 27);
    try expect(ctx.next_col_number == 11);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "2"));
    try expect(ctx.line_number == 27);
    try expect(ctx.col_number == 11);
    try expect(ctx.next_line_number == 27);
    try expect(ctx.next_col_number == 12);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, ")"));
    try expect(ctx.line_number == 27);
    try expect(ctx.col_number == 12);
    try expect(ctx.next_line_number == 27);
    try expect(ctx.next_col_number == 13);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "#foo/2"));
    try expect(ctx.line_number == 28);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 29);
    try expect(ctx.next_col_number == 0);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "\"foo bar\""));
    try expect(ctx.line_number == 29);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 29);
    try expect(ctx.next_col_number == 9);
    list.clearAndFree();

    const eof = try read_token(file.reader(), &list, &ctx);
    try expect(eof);
}

test "tokenize functions" {
    const file = try std.fs.cwd().openFile(
        "snippets/functions.tl",
        .{ .mode = .read_only },
    );
    defer file.close();

    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var ctx = TokenContext{ .leftover = 0, .line_number = 0, .col_number = 0, .next_line_number = 0, .next_col_number = 0 };
    _ = try read_token(file.reader(), &list, &ctx);

    try expect(eql(u8, list.items, "fun"));
    try expect(ctx.line_number == 0);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_col_number == 3);
    try expect(ctx.next_line_number == 0);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "foo"));
    try expect(ctx.line_number == 0);
    try expect(ctx.col_number == 4);
    try expect(ctx.next_col_number == 7);
    try expect(ctx.next_line_number == 0);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "("));
    try expect(ctx.line_number == 0);
    try expect(ctx.col_number == 7);
    try expect(ctx.next_col_number == 8);
    try expect(ctx.next_line_number == 0);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, ")"));
    try expect(ctx.line_number == 0);
    try expect(ctx.col_number == 8);
    try expect(ctx.next_col_number == 9);
    try expect(ctx.next_line_number == 0);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, ":"));
    try expect(ctx.line_number == 0);
    try expect(ctx.col_number == 9);
    try expect(ctx.next_col_number == 10);
    try expect(ctx.next_line_number == 0);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "a"));
    try expect(ctx.line_number == 1);
    try expect(ctx.col_number == 2);
    try expect(ctx.next_col_number == 3);
    try expect(ctx.next_line_number == 1);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "+"));
    try expect(ctx.line_number == 1);
    try expect(ctx.col_number == 4);
    try expect(ctx.next_col_number == 5);
    try expect(ctx.next_line_number == 1);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "b"));
    try expect(ctx.line_number == 1);
    try expect(ctx.col_number == 6);
    try expect(ctx.next_col_number == 7);
    try expect(ctx.next_line_number == 1);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "fun"));
    try expect(ctx.line_number == 3);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_col_number == 3);
    try expect(ctx.next_line_number == 3);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "bar"));
    try expect(ctx.line_number == 3);
    try expect(ctx.col_number == 4);
    try expect(ctx.next_col_number == 7);
    try expect(ctx.next_line_number == 3);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "("));
    try expect(ctx.line_number == 3);
    try expect(ctx.col_number == 7);
    try expect(ctx.next_col_number == 8);
    try expect(ctx.next_line_number == 3);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, ")"));
    try expect(ctx.line_number == 3);
    try expect(ctx.col_number == 8);
    try expect(ctx.next_col_number == 9);
    try expect(ctx.next_line_number == 3);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, ":"));
    try expect(ctx.line_number == 3);
    try expect(ctx.col_number == 9);
    try expect(ctx.next_col_number == 10);
    try expect(ctx.next_line_number == 3);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "c"));
    try expect(ctx.line_number == 4);
    try expect(ctx.col_number == 2);
    try expect(ctx.next_col_number == 3);
    try expect(ctx.next_line_number == 4);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "+"));
    try expect(ctx.line_number == 4);
    try expect(ctx.col_number == 4);
    try expect(ctx.next_col_number == 5);
    try expect(ctx.next_line_number == 4);
    list.clearAndFree();

    _ = try read_token(file.reader(), &list, &ctx);
    try expect(eql(u8, list.items, "d"));
    try expect(ctx.line_number == 4);
    try expect(ctx.col_number == 6);
    try expect(ctx.next_col_number == 7);
    try expect(ctx.next_line_number == 4);
    list.clearAndFree();
}
