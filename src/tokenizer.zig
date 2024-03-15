const std = @import("std");
const test_allocator = std.testing.allocator;
const eql = std.mem.eql;
const expect = std.testing.expect;

const TokenQueueError = error{MissingHead};

const TokenQueueNode = struct { next: ?*TokenQueueNode, body: []const u8 };

// FIFO Queue
pub const TokenQueue = struct {
    const Self = @This();

    head: ?*TokenQueueNode,
    tail: ?*TokenQueueNode,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !*Self {
        const queue = try allocator.create(Self);
        queue.*.allocator = allocator;
        queue.*.head = null;
        queue.*.tail = null;
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

    pub fn push(self: *Self, token_body: []const u8) !void {
        const node = try self.allocator.create(TokenQueueNode);
        node.*.next = null;
        node.*.body = token_body;

        if (self.head == null) {
            self.head = node;
            self.tail = node;
        } else {
            self.tail.?.next = node;
            self.tail = self.tail.?.next;
        }
    }

    pub fn pop(self: *Self) !*TokenQueueNode {
        if (self.head == null) {
            return TokenQueueError.MissingHead;
        } else {
            const node = self.head;
            self.head = self.head.?.next;
            node.?.next = null;
            return node.?;
        }
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
};

test "token queue" {
    const queue = try TokenQueue.init(test_allocator);
    defer queue.deinit();

    const buf: []u8 = try test_allocator.alloc(u8, 13);
    std.mem.copyForwards(u8, buf, "hello, world!");

    try queue.push(buf);
    const node = try queue.pop();

    try std.testing.expect(std.mem.eql(u8, node.*.body, "hello, world!"));

    test_allocator.free(node.*.body);
    test_allocator.destroy(node);
}

pub fn read_tokens(r: anytype, allocator: std.mem.Allocator) !*TokenQueue {
    const queue: *TokenQueue = try TokenQueue.init(allocator);

    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var leftover: u8 = 0;
    while (try read_token(r, &buffer, &leftover)) {
        try queue.push(try buffer.toOwnedSlice());
    }

    return queue;
}

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
