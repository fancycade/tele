const std = @import("std");
const ast = @import("ast.zig");
const Ast = ast.Ast;
const AstType = ast.AstType;
const fs = std.fs;
const test_allocator = std.testing.allocator;

pub const Codegen = struct {
    const Self = @This();

    writer: fs.File.Writer,

    pub fn init(writer: anytype) Self {
        return Self{ .writer = writer };
    }

    pub fn write(self: *Self, a: *const Ast) !void {
        try write_ast(self.writer, a);
    }
};

fn write_ast(w: anytype, a: *const Ast) !void {
    switch (a.ast_type) {
        .int => {
            try write_value(w, a);
        },
        .float => {
            try write_value(w, a);
        },
        .binary => {
            try write_binary(w, a);
        },
        .atom => {
            try write_value(w, a);
        },
        .record => {},
        .tuple => {},
        .list => {
            try write_list(w, a);
        },
        .map => {},
        .module => {},
        .function => {},
        .case => {},
        .op => {},
    }
}

// Need to make static functions for easy testing. Wrap these for public API in Codegen struct

fn write_value(w: anytype, a: *const Ast) !void {
    _ = try w.write(a.body);
}

test "write value" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    try write_value(list.writer(), &Ast{ .body = "1", .ast_type = AstType.int, .children = null });
    try std.testing.expect(std.mem.eql(u8, list.items, "1"));
    list.clearAndFree();

    try write_value(list.writer(), &Ast{ .body = "100_000", .ast_type = AstType.int, .children = null });
    try std.testing.expect(std.mem.eql(u8, list.items, "100_000"));
    list.clearAndFree();

    try write_value(list.writer(), &Ast{ .body = "1.0", .ast_type = AstType.int, .children = null });
    try std.testing.expect(std.mem.eql(u8, list.items, "1.0"));

    // TODO: Scientific notation
    // TODO: Normal Atom
    // TODO: Atom with double quotes
}

fn write_binary(w: anytype, a: *const Ast) !void {
    _ = try w.write("<<");
    _ = try w.write(a.body);
    _ = try w.write(">>");
}

test "write binary" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    try write_binary(list.writer(), &Ast{ .body = "\"foobar\"", .ast_type = AstType.binary, .children = null });

    try std.testing.expect(std.mem.eql(u8, list.items, "<<\"foobar\">>"));
}

fn write_tuple(w: anytype, a: *const Ast) error{OutOfMemory}!void {
    _ = try w.write("{");
    var i: usize = 0;
    for (a.children.?.items) |c| {
        _ = try write_ast(w, c);

        if (i + 1 < a.children.?.items.len) {
            _ = try w.write(", ");
        }

        i += 1;
    }
    _ = try w.write("}");
}

test "write tuple" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "1", .ast_type = AstType.int, .children = null });
    try children.append(&Ast{ .body = "2", .ast_type = AstType.int, .children = null });

    try write_tuple(list.writer(), &Ast{ .body = "", .ast_type = AstType.list, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "{1, 2}"));
}

fn write_list(w: anytype, a: *const Ast) error{OutOfMemory}!void {
    _ = try w.write("[");
    var i: usize = 0;
    for (a.children.?.items) |c| {
        try write_ast(w, c);
        if (i + 1 < a.children.?.items.len) {
            _ = try w.write(", ");
        }
        i += 1;
    }
    _ = try w.write("]");
}

test "write list" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "1", .ast_type = AstType.int, .children = null });
    try children.append(&Ast{ .body = "foo", .ast_type = AstType.atom, .children = null });
    try children.append(&Ast{ .body = "42.42", .ast_type = AstType.float, .children = null });
    try children.append(&Ast{ .body = "\"foobar\"", .ast_type = AstType.binary, .children = null });

    try write_list(list.writer(), &Ast{ .body = "", .children = children, .ast_type = AstType.list });

    try std.testing.expect(std.mem.eql(u8, list.items, "[1, foo, 42.42, <<\"foobar\">>]"));
}

fn write_module(w: anytype, name: []const u8) !void {
    _ = try w.write("-module(");
    _ = try w.write(name);
    _ = try w.write(").\n");
}

test "write module" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();
    try write_module(list.writer(), "basic");
    try std.testing.expect(std.mem.eql(u8, list.items, "-module(basic).\n"));
}

fn write_function_signature(w: anytype, name: []const u8, args: []const []const u8) !void {
    _ = try w.write(name);
    _ = try w.write("(");
    var i: usize = 0;
    for (args) |a| {
        _ = try w.write(a);

        if (i + 1 < args.len) {
            _ = try w.write(", ");
        }

        i += 1;
    }
    _ = try w.write(") ->\n");
}

test "write function signature" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();
    const args = [_][]const u8{ "A", "B" };
    try write_function_signature(list.writer(), "basic", &args);
    const expected = "basic(A, B) ->\n";
    try std.testing.expect(std.mem.eql(u8, list.items, expected));
}
