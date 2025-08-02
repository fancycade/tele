const std = @import("std");
const test_allocator = std.testing.allocator;
const eql = std.mem.eql;
const expect = std.testing.expect;

const TokenQueueError = error{ MissingHead, MissingNode };

pub const TokenQueueNode = struct { next: ?*TokenQueueNode, body: []const u8, line: usize, col: usize };

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
        while (!self.empty()) {
            const n = self.pop() catch {
                return;
            };
            self.allocator.free(n.*.body);
            self.allocator.destroy(n);
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

    pub fn pushHead(self: *Self, token_body: []const u8, line_number: usize, col_number: usize) !void {
        const node = try self.allocator.create(TokenQueueNode);
        node.*.next = null;
        node.*.body = token_body;
        node.*.line = line_number;
        node.*.col = col_number;

        if (self.head == null) {
            self.head = node;
            self.tail = node;
        } else {
            const tmp_node = self.head;
            node.*.next = tmp_node;
            self.head = node;
        }
        self.len += 1;
    }

    pub fn pop(self: *Self) !*TokenQueueNode {
        if (self.len == 0) {
            return TokenQueueError.MissingHead;
        }

        if (self.head == null) {
            return TokenQueueError.MissingHead;
        }

        const node = self.head;
        self.len -= 1;

        if (node.?.next == null) {
            self.head = null;
            self.tail = null;
        } else {
            self.head = node.?.next;
        }

        return node orelse TokenQueueError.MissingNode;
    }

    pub fn peek(self: *Self) !*TokenQueueNode {
        if (self.head == null) {
            return TokenQueueError.MissingHead;
        } else {
            return self.head.?;
        }
    }

    pub fn empty(self: *Self) bool {
        return self.count() == 0;
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

test "empty token queue" {
    // Test that normal run doesn't result in memory leak
    const queue = try TokenQueue.init(test_allocator);
    queue.deinit();
}

const Tokenizer = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    buffer: [256]u8,
    buffer_index: usize,
    eof: bool,
    dirty: bool,

    pub fn init(allocator: std.mem.Allocator) !*Self {
        const t = try allocator.create(Self);
        t.*.allocator = allocator;
        t.*.buffer_index = 256;
        t.*.dirty = true;
        t.resetBuffer();
        return t;
    }

    pub fn deinit(self: *Self) void {
        self.allocator.destroy(self);
    }

    pub fn peekChar(self: *Self, r: anytype) !u8 {
        if (self.*.eof) {
            return error.EndOfStream;
        }
        try self.readBuffer(r);

        const c = self.*.buffer[self.*.buffer_index];
        return c;
    }

    pub fn nextChar(self: *Self, r: anytype) !u8 {
        if (self.*.eof) {
            return error.EndOfStream;
        }
        try self.readBuffer(r);

        const c = self.*.buffer[self.*.buffer_index];
        self.*.buffer_index += 1;

        if (self.*.buffer_index < self.*.buffer.len) {
            if (self.*.buffer[self.*.buffer_index] == 0) {
                self.*.eof = true;
            }
        }
        return c;
    }

    fn readBuffer(self: *Self, r: anytype) !void {
        if (self.*.buffer_index >= self.*.buffer.len) {
            self.resetBuffer();
        }

        if (self.*.buffer_index == 0 and !self.*.dirty) {
            _ = try r.read(&self.*.buffer);
            self.*.dirty = true;
        }
    }

    fn resetBuffer(self: *Self) void {
        var i: usize = 0;
        while (i < self.*.buffer.len) {
            self.*.buffer[i] = 0;
            i += 1;
        }
        self.*.buffer_index = 0;
        self.*.dirty = false;
    }
};

test "tokenizer file read" {
    const file = try std.fs.cwd().openFile(
        "snippets/tokenizer.tl",
        .{ .mode = .read_only },
    );
    defer file.close();

    const rdr = file.reader();

    var t = try Tokenizer.init(test_allocator);
    defer t.deinit();

    try std.testing.expect(try t.nextChar(rdr) == '1');
    try std.testing.expect(try t.nextChar(rdr) == ' ');
    try std.testing.expect(try t.nextChar(rdr) == '+');
    try std.testing.expect(try t.nextChar(rdr) == ' ');
    try std.testing.expect(try t.nextChar(rdr) == '1');
    try std.testing.expect(try t.nextChar(rdr) == '\n');

    try std.testing.expect(t.*.eof);

    try std.testing.expect(t.nextChar(rdr) == error.EndOfStream);
}

test "tokenizer file read peeking" {
    const file = try std.fs.cwd().openFile(
        "snippets/tokenizer.tl",
        .{ .mode = .read_only },
    );
    defer file.close();

    const rdr = file.reader();

    var t = try Tokenizer.init(test_allocator);
    defer t.deinit();

    try std.testing.expect(try t.peekChar(rdr) == '1');
    try std.testing.expect(try t.nextChar(rdr) == '1');
    try std.testing.expect(try t.peekChar(rdr) == ' ');
    try std.testing.expect(try t.nextChar(rdr) == ' ');
    try std.testing.expect(try t.peekChar(rdr) == '+');
    try std.testing.expect(try t.nextChar(rdr) == '+');
    try std.testing.expect(try t.peekChar(rdr) == ' ');
    try std.testing.expect(try t.nextChar(rdr) == ' ');
    try std.testing.expect(try t.peekChar(rdr) == '1');
    try std.testing.expect(try t.nextChar(rdr) == '1');
    try std.testing.expect(try t.peekChar(rdr) == '\n');
    try std.testing.expect(try t.nextChar(rdr) == '\n');

    try std.testing.expect(t.*.eof);

    try std.testing.expect(t.peekChar(rdr) == error.EndOfStream);
    try std.testing.expect(t.nextChar(rdr) == error.EndOfStream);
}

const TokenContext = struct { line_number: usize, col_number: usize, next_line_number: usize, next_col_number: usize };

pub fn readTokens(r: anytype, allocator: std.mem.Allocator) !*TokenQueue {
    const queue: *TokenQueue = try TokenQueue.init(allocator);

    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var ctx = TokenContext{ .line_number = 0, .col_number = 0, .next_line_number = 0, .next_col_number = 0 };

    var t = try Tokenizer.init(allocator);
    defer t.deinit();

    while (!try readToken(r, &buffer, &ctx, t)) {
        if (buffer.items.len > 0) {
            try queue.push(try buffer.toOwnedSlice(), ctx.line_number, ctx.col_number);
        }
    }

    return queue;
}

const TokenMode = enum { word, op, hash, bit_string, binary, atom, short_atom, none };

fn readToken(r: anytype, l: *std.ArrayList(u8), ctx: *TokenContext, tokenizer: *Tokenizer) !bool {
    ctx.line_number = ctx.next_line_number;
    ctx.col_number = ctx.next_col_number;
    var mode: TokenMode = TokenMode.none;

    while (!tokenizer.*.eof) {
        const pb = try tokenizer.peekChar(r);

        switch (mode) {
            .none => {
                if (specialChar(pb)) {
                    if (pb == '/') {
                        _ = try tokenizer.nextChar(r);
                        const pb2 = try tokenizer.peekChar(r);

                        if (pb2 == '/') {
                            ctx.*.next_col_number = 0;
                            try removeComment(r, tokenizer, ctx);
                            continue;
                        } else if (pb2 == '*') {
                            ctx.*.next_col_number = 0;
                            _ = try tokenizer.nextChar(r);
                            try removeMultiLineComment(r, tokenizer, ctx);
                            continue;
                        } else { // Is / char
                            try l.append(pb);
                            ctx.*.next_col_number += 1;
                            return false;
                        }
                    }

                    try l.append(try tokenizer.nextChar(r));
                    ctx.*.next_col_number += 1;

                    if (pb == '"') {
                        mode = .binary;
                    } else if (pb == '#') {
                        mode = .hash;
                    } else if (pb == '\'') {
                        mode = .short_atom;
                    } else if (pb == '?') {
                        const pb2 = try tokenizer.peekChar(r);
                        if (pb2 == '=') {
                            mode = .op;
                        } else {
                            mode = .word;
                        }
                    } else if (opChar(pb)) {
                        mode = .op;
                    } else {
                        return false;
                    }
                } else if (whitespace(pb)) {
                    if (pb == '\t') {
                        ctx.*.next_col_number += 4;
                        ctx.*.col_number += 4;
                    } else {
                        ctx.*.next_col_number += 1;
                        ctx.*.col_number += 1;
                    }
                    _ = try tokenizer.nextChar(r);
                } else if (newline(pb)) {
                    ctx.*.line_number += 1;
                    ctx.*.col_number = 0;
                    ctx.*.next_line_number += 1;
                    ctx.*.next_col_number = 0;
                    _ = try tokenizer.nextChar(r);
                } else {
                    try l.append(try tokenizer.nextChar(r));
                    ctx.*.next_col_number += 1;
                    mode = .word;
                }
            },
            .word => {
                if (specialChar(pb)) {
                    if (pb != '#') {
                        return false;
                    }
                } else if (newline(pb)) {
                    return false;
                } else if (whitespace(pb)) {
                    return false;
                }
                try l.append(try tokenizer.nextChar(r));
                ctx.*.next_col_number += 1;
            },
            .short_atom => {
                if (newline(pb)) {
                    return false;
                } else if (whitespace(pb)) {
                    return false;
                    // TODO: Be more selective of which special char to end with
                } else if (specialChar(pb)) {
                    return false;
                }
                try l.append(try tokenizer.nextChar(r));
                ctx.*.next_col_number += 1;
            },
            .op => {
                if (opChar(pb)) {
                    try l.append(try tokenizer.nextChar(r));
                    ctx.*.next_col_number += 1;
                } else {
                    return false;
                }
            },
            .hash => {
                if (whitespace(pb)) {
                    return false;
                } else if (newline(pb)) {
                    return false;
                } else if (specialChar(pb)) {
                    if (pb == '\'') {
                        mode = TokenMode.atom;
                    } else if (pb != '(') {
                        return false;
                    }
                }

                try l.append(try tokenizer.nextChar(r));
                ctx.*.next_col_number += 1;
                if (pb == '(') { // Is tuple start
                    return false;
                }
            },
            .atom => {
                try l.append(try tokenizer.nextChar(r));
                ctx.*.next_col_number += 1;
                if (pb == '\'') {
                    const pb2 = tokenizer.peekChar(r) catch |err| switch (err) {
                        error.EndOfStream => {
                            return false;
                        },
                        else => {
                            return err;
                        },
                    };

                    if (pb2 == '(') {
                        try l.append(try tokenizer.nextChar(r));
                        ctx.*.next_col_number += 1;
                        return false;
                    }

                    return false;
                }
            },
            .bit_string => {
                if (pb == '>') {
                    if (l.items[l.items.len - 1] == '>') {
                        try l.append(try tokenizer.nextChar(r));
                        ctx.*.next_col_number += 1;
                        return false;
                    }
                }

                try l.append(try tokenizer.nextChar(r));
                ctx.*.next_col_number += 1;
            },
            .binary => {
                try l.append(try tokenizer.nextChar(r));
                ctx.*.next_col_number += 1;
                if (pb == '"') {
                    // TODO: Make sure this doesnt buffer underflow
                    if (l.items[l.items.len - 2] != '\\') {
                        return false;
                    }
                }
            },
        }
    }

    return true;
}

fn removeComment(r: anytype, tokenizer: *Tokenizer, ctx: *TokenContext) !void {
    while (!tokenizer.*.eof) {
        const c = try tokenizer.nextChar(r);
        if (c == '\n') {
            ctx.*.next_line_number += 1;
            break;
        }
    }
}

fn removeMultiLineComment(r: anytype, tokenizer: *Tokenizer, ctx: *TokenContext) !void {
    while (!tokenizer.*.eof) {
        const c = try tokenizer.nextChar(r);
        if (c == '*') {
            const pc = try tokenizer.peekChar(r);
            if (pc == '/') {
                _ = try tokenizer.nextChar(r);
                break;
            }
        } else if (c == '\n') {
            ctx.*.next_line_number += 1;
        }
    }
}

fn whitespace(c: u8) bool {
    return c == ' ' or c == '\t';
}

fn newline(c: u8) bool {
    return c == '\n' or c == '\r';
}

fn specialChar(c: u8) bool {
    switch (c) {
        '(', ')', '{', '}', '[', ']', ':', ',', '#', '-', '+', '/', '*', '<', '>', '|', '=', '"', '\'', '!', '?' => {
            return true;
        },
        else => {},
    }
    return false;
}

fn opChar(c: u8) bool {
    switch (c) {
        '-', '+', '/', '*', '<', '>', '=', '|', ':', '!', '?' => {
            return true;
        },
        else => {},
    }
    return false;
}

test "remove comments" {
    const file = try std.fs.cwd().openFile(
        "snippets/comments.tl",
        .{ .mode = .read_only },
    );
    defer file.close();

    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var ctx = TokenContext{ .line_number = 0, .col_number = 0, .next_line_number = 0, .next_col_number = 0 };

    var tokenizer = try Tokenizer.init(test_allocator);
    defer tokenizer.deinit();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "1"));
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "+"));
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "2"));
    list.clearAndFree();
}

test "read token" {
    const file = try std.fs.cwd().openFile(
        "snippets/tokens.tl",
        .{ .mode = .read_only },
    );
    defer file.close();

    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var ctx = TokenContext{ .line_number = 0, .col_number = 0, .next_line_number = 0, .next_col_number = 0 };
    var tokenizer = try Tokenizer.init(test_allocator);
    defer tokenizer.deinit();
    const end_file = try readToken(file.reader(), &list, &ctx, tokenizer);

    try expect(!end_file);
    try expect(eql(u8, list.items, "("));
    try expect(ctx.line_number == 0);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_col_number == 1);
    try expect(ctx.next_line_number == 0);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, ")"));
    try expect(ctx.line_number == 0);
    try expect(ctx.col_number == 1);
    try expect(ctx.next_col_number == 2);
    try expect(ctx.next_line_number == 0);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "["));
    try expect(ctx.line_number == 1);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 1);
    try expect(ctx.next_col_number == 1);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "]"));
    try expect(ctx.line_number == 1);
    try expect(ctx.col_number == 1);
    try expect(ctx.next_line_number == 1);
    try expect(ctx.next_col_number == 2);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "{"));
    try expect(ctx.line_number == 2);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 2);
    try expect(ctx.next_col_number == 1);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "}"));
    try expect(ctx.line_number == 2);
    try expect(ctx.col_number == 1);
    try expect(ctx.next_line_number == 2);
    try expect(ctx.next_col_number == 2);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "foobar"));
    try expect(ctx.line_number == 3);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 3);
    try expect(ctx.next_col_number == 6);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "1"));
    try expect(ctx.line_number == 4);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 4);
    try expect(ctx.next_col_number == 1);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "$c"));
    try expect(ctx.line_number == 5);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 5);
    try expect(ctx.next_col_number == 2);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "42.42"));
    try expect(ctx.line_number == 6);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 6);
    try expect(ctx.next_col_number == 5);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "="));
    try expect(ctx.line_number == 7);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 7);
    try expect(ctx.next_col_number == 1);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "+"));
    try expect(ctx.line_number == 8);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 8);
    try expect(ctx.next_col_number == 1);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "-"));
    try expect(ctx.line_number == 9);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 9);
    try expect(ctx.next_col_number == 1);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "/"));
    try expect(ctx.line_number == 10);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 10);
    try expect(ctx.next_col_number == 1);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "=="));
    try expect(ctx.line_number == 11);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 11);
    try expect(ctx.next_col_number == 2);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, ">="));
    try expect(ctx.line_number == 12);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 12);
    try expect(ctx.next_col_number == 2);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "<="));
    try expect(ctx.line_number == 13);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 13);
    try expect(ctx.next_col_number == 2);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "->"));
    try expect(ctx.line_number == 14);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 14);
    try expect(ctx.next_col_number == 2);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "|"));
    try expect(ctx.line_number == 15);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 15);
    try expect(ctx.next_col_number == 1);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "#person("));
    try expect(ctx.line_number == 16);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 16);
    try expect(ctx.next_col_number == 8);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, ")"));
    try expect(ctx.line_number == 16);
    try expect(ctx.col_number == 8);
    try expect(ctx.next_line_number == 16);
    try expect(ctx.next_col_number == 9);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "#("));
    try expect(ctx.line_number == 17);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 17);
    try expect(ctx.next_col_number == 2);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, ")"));
    try expect(ctx.line_number == 17);
    try expect(ctx.col_number == 2);
    try expect(ctx.next_line_number == 17);
    try expect(ctx.next_col_number == 3);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "16#FFFF"));
    try expect(ctx.line_number == 18);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 18);
    try expect(ctx.next_col_number == 7);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "1"));
    try expect(ctx.line_number == 19);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 19);
    try expect(ctx.next_col_number == 1);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "+"));
    try expect(ctx.line_number == 19);
    try expect(ctx.col_number == 1);
    try expect(ctx.next_line_number == 19);
    try expect(ctx.next_col_number == 2);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "1"));
    try expect(ctx.line_number == 19);
    try expect(ctx.col_number == 2);
    try expect(ctx.next_line_number == 19);
    try expect(ctx.next_col_number == 3);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "<<"));
    try expect(ctx.line_number == 20);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 20);
    try expect(ctx.next_col_number == 2);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "\"binary\""));
    try expect(ctx.line_number == 20);
    try expect(ctx.col_number == 2);
    try expect(ctx.next_line_number == 20);
    try expect(ctx.next_col_number == 10);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, ">>"));
    try expect(ctx.line_number == 20);
    try expect(ctx.col_number == 10);
    try expect(ctx.next_line_number == 20);
    try expect(ctx.next_col_number == 12);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "\"string\""));
    try expect(ctx.line_number == 21);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 21);
    try expect(ctx.next_col_number == 8);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "io.read"));
    try expect(ctx.line_number == 22);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 22);
    try expect(ctx.next_col_number == 7);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "{"));
    try expect(ctx.line_number == 23);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 23);
    try expect(ctx.next_col_number == 1);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "foo"));
    try expect(ctx.line_number == 23);
    try expect(ctx.col_number == 1);
    try expect(ctx.next_line_number == 23);
    try expect(ctx.next_col_number == 4);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, ":"));
    try expect(ctx.line_number == 23);
    try expect(ctx.col_number == 4);
    try expect(ctx.next_line_number == 23);
    try expect(ctx.next_col_number == 5);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "bar"));
    try expect(ctx.line_number == 23);
    try expect(ctx.col_number == 5);
    try expect(ctx.next_line_number == 23);
    try expect(ctx.next_col_number == 8);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "}"));
    try expect(ctx.line_number == 23);
    try expect(ctx.col_number == 8);
    try expect(ctx.next_line_number == 23);
    try expect(ctx.next_col_number == 9);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "{"));
    try expect(ctx.line_number == 24);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 24);
    try expect(ctx.next_col_number == 1);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "foo"));
    try expect(ctx.line_number == 24);
    try expect(ctx.col_number == 2);
    try expect(ctx.next_line_number == 24);
    try expect(ctx.next_col_number == 5);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, ":"));
    try expect(ctx.line_number == 24);
    try expect(ctx.col_number == 6);
    try expect(ctx.next_line_number == 24);
    try expect(ctx.next_col_number == 7);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "bar"));
    try expect(ctx.line_number == 24);
    try expect(ctx.col_number == 8);
    try expect(ctx.next_line_number == 24);
    try expect(ctx.next_col_number == 11);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "}"));
    try expect(ctx.line_number == 24);
    try expect(ctx.col_number == 12);
    try expect(ctx.next_line_number == 24);
    try expect(ctx.next_col_number == 13);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "["));
    try expect(ctx.line_number == 25);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 25);
    try expect(ctx.next_col_number == 1);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "1"));
    try expect(ctx.line_number == 25);
    try expect(ctx.col_number == 1);
    try expect(ctx.next_line_number == 25);
    try expect(ctx.next_col_number == 2);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, ","));
    try expect(ctx.line_number == 25);
    try expect(ctx.col_number == 2);
    try expect(ctx.next_line_number == 25);
    try expect(ctx.next_col_number == 3);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "2"));
    list.clearAndFree();
    try expect(ctx.line_number == 25);
    try expect(ctx.col_number == 4);
    try expect(ctx.next_line_number == 25);
    try expect(ctx.next_col_number == 5);

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, ","));
    try expect(ctx.line_number == 25);
    try expect(ctx.col_number == 5);
    try expect(ctx.next_line_number == 25);
    try expect(ctx.next_col_number == 6);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "3"));
    try expect(ctx.line_number == 25);
    try expect(ctx.col_number == 7);
    try expect(ctx.next_line_number == 25);
    try expect(ctx.next_col_number == 8);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "]"));
    try expect(ctx.line_number == 25);
    try expect(ctx.col_number == 8);
    try expect(ctx.next_line_number == 25);
    try expect(ctx.next_col_number == 9);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "++"));
    try expect(ctx.line_number == 26);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 26);
    try expect(ctx.next_col_number == 2);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "a#person"));
    try expect(ctx.line_number == 27);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 27);
    try expect(ctx.next_col_number == 8);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "("));
    try expect(ctx.line_number == 27);
    try expect(ctx.col_number == 8);
    try expect(ctx.next_line_number == 27);
    try expect(ctx.next_col_number == 9);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "x"));
    try expect(ctx.line_number == 27);
    try expect(ctx.col_number == 9);
    try expect(ctx.next_line_number == 27);
    try expect(ctx.next_col_number == 10);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "="));
    try expect(ctx.line_number == 27);
    try expect(ctx.col_number == 10);
    try expect(ctx.next_line_number == 27);
    try expect(ctx.next_col_number == 11);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "2"));
    try expect(ctx.line_number == 27);
    try expect(ctx.col_number == 11);
    try expect(ctx.next_line_number == 27);
    try expect(ctx.next_col_number == 12);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, ")"));
    try expect(ctx.line_number == 27);
    try expect(ctx.col_number == 12);
    try expect(ctx.next_line_number == 27);
    try expect(ctx.next_col_number == 13);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "\"foo bar\""));
    try expect(ctx.line_number == 28);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 28);
    try expect(ctx.next_col_number == 9);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "::"));
    try expect(ctx.line_number == 29);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 29);
    try expect(ctx.next_col_number == 2);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "#'foo bar'"));
    try expect(ctx.line_number == 30);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 30);
    try expect(ctx.next_col_number == 10);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "#'basic.ok'("));
    try expect(ctx.line_number == 31);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 31);
    try expect(ctx.next_col_number == 12);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, ")"));
    try expect(ctx.line_number == 31);
    try expect(ctx.col_number == 12);
    try expect(ctx.next_line_number == 31);
    try expect(ctx.next_col_number == 13);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "'ok"));
    try expect(ctx.line_number == 32);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 32);
    try expect(ctx.next_col_number == 3);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, ","));
    try expect(ctx.line_number == 32);
    try expect(ctx.col_number == 3);
    try expect(ctx.next_line_number == 32);
    try expect(ctx.next_col_number == 4);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "#point"));
    try expect(ctx.line_number == 33);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 33);
    try expect(ctx.next_col_number == 6);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, ")"));
    try expect(ctx.line_number == 33);
    try expect(ctx.col_number == 6);
    try expect(ctx.next_line_number == 33);
    try expect(ctx.next_col_number == 7);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, ":"));
    try expect(ctx.line_number == 33);
    try expect(ctx.col_number == 7);
    try expect(ctx.next_line_number == 33);
    try expect(ctx.next_col_number == 8);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "\"foo\\\"bar\\\"\""));
    try expect(ctx.line_number == 34);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 34);
    try expect(ctx.next_col_number == 12);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "?="));
    try expect(ctx.line_number == 35);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 35);
    try expect(ctx.next_col_number == 2);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "?assertEqual"));
    try expect(ctx.line_number == 36);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 36);
    try expect(ctx.next_col_number == 12);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "!="));
    try expect(ctx.line_number == 37);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_line_number == 37);
    try expect(ctx.next_col_number == 2);
    list.clearAndFree();

    const eof = try readToken(file.reader(), &list, &ctx, tokenizer);
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

    var ctx = TokenContext{ .line_number = 0, .col_number = 0, .next_line_number = 0, .next_col_number = 0 };
    var tokenizer = try Tokenizer.init(test_allocator);
    defer tokenizer.deinit();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);

    try expect(eql(u8, list.items, "fun"));
    try expect(ctx.line_number == 0);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_col_number == 3);
    try expect(ctx.next_line_number == 0);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "foo"));
    try expect(ctx.line_number == 0);
    try expect(ctx.col_number == 4);
    try expect(ctx.next_col_number == 7);
    try expect(ctx.next_line_number == 0);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "("));
    try expect(ctx.line_number == 0);
    try expect(ctx.col_number == 7);
    try expect(ctx.next_col_number == 8);
    try expect(ctx.next_line_number == 0);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, ")"));
    try expect(ctx.line_number == 0);
    try expect(ctx.col_number == 8);
    try expect(ctx.next_col_number == 9);
    try expect(ctx.next_line_number == 0);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, ":"));
    try expect(ctx.line_number == 0);
    try expect(ctx.col_number == 9);
    try expect(ctx.next_col_number == 10);
    try expect(ctx.next_line_number == 0);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "a"));
    try expect(ctx.line_number == 1);
    try expect(ctx.col_number == 2);
    try expect(ctx.next_col_number == 3);
    try expect(ctx.next_line_number == 1);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "+"));
    try expect(ctx.line_number == 1);
    try expect(ctx.col_number == 4);
    try expect(ctx.next_col_number == 5);
    try expect(ctx.next_line_number == 1);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "b"));
    try expect(ctx.line_number == 1);
    try expect(ctx.col_number == 6);
    try expect(ctx.next_col_number == 7);
    try expect(ctx.next_line_number == 1);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "fun"));
    try expect(ctx.line_number == 3);
    try expect(ctx.col_number == 0);
    try expect(ctx.next_col_number == 3);
    try expect(ctx.next_line_number == 3);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "bar"));
    try expect(ctx.line_number == 3);
    try expect(ctx.col_number == 4);
    try expect(ctx.next_col_number == 7);
    try expect(ctx.next_line_number == 3);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "("));
    try expect(ctx.line_number == 3);
    try expect(ctx.col_number == 7);
    try expect(ctx.next_col_number == 8);
    try expect(ctx.next_line_number == 3);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, ")"));
    try expect(ctx.line_number == 3);
    try expect(ctx.col_number == 8);
    try expect(ctx.next_col_number == 9);
    try expect(ctx.next_line_number == 3);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, ":"));
    try expect(ctx.line_number == 3);
    try expect(ctx.col_number == 9);
    try expect(ctx.next_col_number == 10);
    try expect(ctx.next_line_number == 3);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "c"));
    try expect(ctx.line_number == 4);
    try expect(ctx.col_number == 2);
    try expect(ctx.next_col_number == 3);
    try expect(ctx.next_line_number == 4);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "+"));
    try expect(ctx.line_number == 4);
    try expect(ctx.col_number == 4);
    try expect(ctx.next_col_number == 5);
    try expect(ctx.next_line_number == 4);
    list.clearAndFree();

    _ = try readToken(file.reader(), &list, &ctx, tokenizer);
    try expect(eql(u8, list.items, "d"));
    try expect(ctx.line_number == 4);
    try expect(ctx.col_number == 6);
    try expect(ctx.next_col_number == 7);
    try expect(ctx.next_line_number == 4);
    list.clearAndFree();
}
