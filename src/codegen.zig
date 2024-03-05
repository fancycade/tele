const std = @import("std");
const ast = @import("ast.zig");
const Ast = ast.Ast;
const AstType = ast.AstType;
const test_allocator = std.testing.allocator;

pub fn write_ast(w: anytype, a: *const Ast) !void {
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
        .variable => {
            try write_value(w, a);
        },
        .record => {
            try write_record(w, a);
        },
        .tuple => {
            try write_tuple(w, a);
        },
        .list => {
            try write_list(w, a);
        },
        .map => {
            try write_map(w, a);
        },
        .attribute => {
            try write_attribute(w, a);
        },
        .function_call => {
            try write_function_call(w, a);
        },
        .function_def => {
            try write_function_def(w, a);
        },
        .op => {
            try write_op(w, a);
        },
        .case => {},
    }
}

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
    list.clearAndFree();

    // TODO: Scientific notation

    try write_value(list.writer(), &Ast{ .body = "foo", .ast_type = AstType.atom, .children = null });
    try std.testing.expect(std.mem.eql(u8, list.items, "foo"));
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

// Children list consist of alternating key value pairs [k1, v1, k2, v2...]

fn write_map(w: anytype, a: *const Ast) error{OutOfMemory}!void {
    _ = try w.write("#{");

    var loop = true;
    var i: usize = 0;
    while (loop) {
        try write_ast(w, a.children.?.items[i]);
        _ = try w.write(" => ");
        try write_ast(w, a.children.?.items[i + 1]);

        const len = a.children.?.items.len;

        if (i + 2 != len) {
            _ = try w.write(", ");
        }

        i += 2;

        if (i >= len) {
            loop = false;
        }
    }

    _ = try w.write("}");
}

test "write map" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "foo", .ast_type = AstType.atom, .children = null });
    try children.append(&Ast{ .body = "bar", .ast_type = AstType.atom, .children = null });
    try children.append(&Ast{ .body = "foo2", .ast_type = AstType.atom, .children = null });
    try children.append(&Ast{ .body = "baz", .ast_type = AstType.atom, .children = null });

    try write_map(list.writer(), &Ast{ .body = "", .ast_type = AstType.map, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "#{foo => bar, foo2 => baz}"));
}

fn write_record(w: anytype, a: *const Ast) error{OutOfMemory}!void {
    _ = try w.write("#");
    _ = try w.write(a.body);
    _ = try w.write("{");

    var i: usize = 0;
    var loop = true;
    while (loop) {
        try write_ast(w, a.children.?.items[i]);
        _ = try w.write("=");
        try write_ast(w, a.children.?.items[i + 1]);

        const len = a.children.?.items.len;

        if (i + 2 != len) {
            _ = try w.write(", ");
        }

        i += 2;

        if (i >= len) {
            loop = false;
        }
    }

    _ = try w.write("}");
}

test "write record" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "name", .ast_type = AstType.atom, .children = null });
    try children.append(&Ast{ .body = "\"Joe\"", .ast_type = AstType.binary, .children = null });
    try children.append(&Ast{ .body = "age", .ast_type = AstType.atom, .children = null });
    try children.append(&Ast{ .body = "68", .ast_type = AstType.int, .children = null });

    try write_record(list.writer(), &Ast{ .body = "person", .ast_type = AstType.record, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "#person{name=<<\"Joe\">>, age=68}"));
}

fn write_op(w: anytype, a: *const Ast) error{OutOfMemory}!void {
    // TODO: Throw error if children is not length 2
    try write_ast(w, a.children.?.items[0]);
    _ = try w.write(" ");
    _ = try w.write(a.body);
    _ = try w.write(" ");
    try write_ast(w, a.children.?.items[1]);
}

test "write op" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "1", .ast_type = AstType.int, .children = null });
    try children.append(&Ast{ .body = "2", .ast_type = AstType.int, .children = null });

    try write_op(list.writer(), &Ast{ .body = "+", .children = children, .ast_type = AstType.op });

    try std.testing.expect(std.mem.eql(u8, list.items, "1 + 2"));
}

fn write_function_call(w: anytype, a: *const Ast) error{OutOfMemory}!void {
    _ = try w.write(a.body);
    _ = try w.write("(");

    if (a.children != null) {
        var i: usize = 0;
        for (a.children.?.items) |c| {
            try write_ast(w, c);

            if (i + 1 != a.children.?.items.len) {
                _ = try w.write(", ");
            }

            i += 1;
        }
    }

    _ = try w.write(")");
}

test "write function call" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "1", .ast_type = AstType.int, .children = null });
    try children.append(&Ast{ .body = "2", .ast_type = AstType.int, .children = null });

    try write_function_call(list.writer(), &Ast{ .body = "erlang:add", .ast_type = AstType.function_call, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "erlang:add(1, 2)"));

    // TODO: Test no arguments
}

fn write_attribute(w: anytype, a: *const Ast) error{OutOfMemory}!void {
    _ = try w.write("-");
    _ = try w.write(a.body);
    _ = try w.write("(");

    var i: usize = 0;
    for (a.children.?.items) |c| {
        try write_ast(w, c);

        if (i + 1 != a.children.?.items.len) {
            _ = try w.write(", ");
        }
        i = i + 1;
    }

    _ = try w.write(").\n");
}

test "write attribute" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "foobar", .ast_type = AstType.atom, .children = null });

    try write_attribute(list.writer(), &Ast{ .body = "module", .ast_type = AstType.attribute, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "-module(foobar).\n"));

    // Test multi arg string
}

fn write_function_def(w: anytype, a: *const Ast) error{OutOfMemory}!void {
    _ = try w.write(a.body);
    _ = try w.write(" ->\n");

    var i: usize = 0;
    for (a.children.?.items) |c| {
        _ = try w.write("    ");
        try write_ast(w, c);

        if (i + 1 == a.children.?.items.len) {
            _ = try w.write(".");
        } else {
            _ = try w.write(",");
        }

        _ = try w.write("\n");

        i = i + 1;
    }
}

test "write function def" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "hello", .ast_type = AstType.function_call, .children = null });
    try children.append(&Ast{ .body = "world", .ast_type = AstType.function_call, .children = null });

    try write_function_def(list.writer(), &Ast{ .body = "hello_world()", .ast_type = AstType.function_def, .children = children });
    try std.testing.expect(std.mem.eql(u8, list.items, "hello_world() ->\n    hello(),\n    world().\n"));
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
