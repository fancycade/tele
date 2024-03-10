const std = @import("std");
const ast = @import("ast.zig");
const Ast = ast.Ast;
const AstType = ast.AstType;
const test_allocator = std.testing.allocator;

const CodegenError = error{WritingFailure};

pub fn write_ast(w: anytype, a: *const Ast, offset: usize) error{WritingFailure}!void {
    switch (a.ast_type) {
        .int => {
            write_value(w, a, offset) catch {
                return CodegenError.WritingFailure;
            };
        },
        .float => {
            write_value(w, a, offset) catch {
                return CodegenError.WritingFailure;
            };
        },
        .binary => {
            write_value(w, a, offset) catch {
                return CodegenError.WritingFailure;
            };
        },
        .atom => {
            write_value(w, a, offset) catch {
                return CodegenError.WritingFailure;
            };
        },
        .variable => {
            write_value(w, a, offset) catch {
                return CodegenError.WritingFailure;
            };
        },
        .tuple => {
            write_tuple(w, a, offset) catch {
                return CodegenError.WritingFailure;
            };
        },
        .list => {
            write_list(w, a, offset) catch {
                return CodegenError.WritingFailure;
            };
        },
        .map => {
            write_map(w, a, offset) catch {
                return CodegenError.WritingFailure;
            };
        },
        .record => {
            write_record(w, a, offset) catch {
                return CodegenError.WritingFailure;
            };
        },
        .op => {
            write_op(w, a, offset) catch {
                return CodegenError.WritingFailure;
            };
        },
        .function_call => {
            write_function_call(w, a, offset) catch {
                return CodegenError.WritingFailure;
            };
        },
        .attribute => {
            write_attribute(w, a, offset) catch {
                return CodegenError.WritingFailure;
            };
        },
        .guard_clause => {
            write_guard_clause(w, a, offset) catch {
                return CodegenError.WritingFailure;
            };
        },
        .function_signature => {
            write_function_signature(w, a, offset) catch {
                return CodegenError.WritingFailure;
            };
        },
        .anonymous_function => {
            write_anonymous_function(w, a, offset) catch {
                return CodegenError.WritingFailure;
            };
        },
        .function_def => {
            write_function_def(w, a, offset) catch {
                return CodegenError.WritingFailure;
            };
        },
        .case_clause => {
            write_case_clause(w, a, offset) catch {
                return CodegenError.WritingFailure;
            };
        },
        .case => {
            write_case(w, a, offset) catch {
                return CodegenError.WritingFailure;
            };
        },
    }
}

fn write_offset(w: anytype, offset: usize) !void {
    var i: usize = 0;
    while (i < offset) {
        _ = try w.write(" ");

        i = i + 1;
    }
}

test "write offset" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    try write_offset(list.writer(), 4);

    try std.testing.expect(std.mem.eql(u8, list.items, "    "));
}

fn write_value(w: anytype, a: *const Ast, offset: usize) !void {
    try write_offset(w, offset);
    _ = try w.write(a.body);
}

test "write value" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    try write_value(list.writer(), &Ast{ .body = "1", .ast_type = AstType.int, .children = null }, 0);
    try std.testing.expect(std.mem.eql(u8, list.items, "1"));
    list.clearAndFree();

    try write_value(list.writer(), &Ast{ .body = "100_000", .ast_type = AstType.int, .children = null }, 0);
    try std.testing.expect(std.mem.eql(u8, list.items, "100_000"));
    list.clearAndFree();

    try write_value(list.writer(), &Ast{ .body = "1.0", .ast_type = AstType.int, .children = null }, 0);
    try std.testing.expect(std.mem.eql(u8, list.items, "1.0"));
    list.clearAndFree();

    // TODO: Scientific notation

    try write_value(list.writer(), &Ast{ .body = "'foo", .ast_type = AstType.atom, .children = null }, 0);
    try std.testing.expect(std.mem.eql(u8, list.items, "'foo"));
    // TODO: Atom with double quotes
}

fn write_tuple(w: anytype, a: *const Ast, offset: usize) !void {
    try write_offset(w, offset);

    _ = try w.write("#(");
    var i: usize = 0;
    for (a.children.?.items) |c| {
        try write_ast(w, c, offset);

        if (i + 1 < a.children.?.items.len) {
            _ = try w.write(", ");
        }

        i = i + 1;
    }
    _ = try w.write(")");
}

test "write tuple" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "1", .ast_type = AstType.int, .children = null });
    try children.append(&Ast{ .body = "2", .ast_type = AstType.int, .children = null });

    try write_tuple(list.writer(), &Ast{ .body = "", .ast_type = AstType.list, .children = children }, 0);

    try std.testing.expect(std.mem.eql(u8, list.items, "#(1, 2)"));
}

fn write_list(w: anytype, a: *const Ast, offset: usize) !void {
    try write_offset(w, offset);

    _ = try w.write("[");
    var i: usize = 0;
    for (a.children.?.items) |c| {
        try write_ast(w, c, offset);

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
    try children.append(&Ast{ .body = "'foo", .ast_type = AstType.atom, .children = null });
    try children.append(&Ast{ .body = "42.42", .ast_type = AstType.float, .children = null });
    try children.append(&Ast{ .body = "\"foobar\"", .ast_type = AstType.binary, .children = null });

    try write_list(list.writer(), &Ast{ .body = "", .children = children, .ast_type = AstType.list }, 0);

    try std.testing.expect(std.mem.eql(u8, list.items, "[1, 'foo, 42.42, \"foobar\"]"));
}

// Children list consist of alternating key value pairs [k1, v1, k2, v2...]

fn write_map(w: anytype, a: *const Ast, offset: usize) !void {
    try write_offset(w, offset);
    _ = try w.write("{");

    var loop = true;
    var i: usize = 0;
    while (loop) {
        try write_ast(w, a.children.?.items[i], offset);

        _ = try w.write(": ");
        try write_ast(w, a.children.?.items[i + 1], offset);

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

    try children.append(&Ast{ .body = "'foo", .ast_type = AstType.atom, .children = null });
    try children.append(&Ast{ .body = "'bar", .ast_type = AstType.atom, .children = null });
    try children.append(&Ast{ .body = "'foo2", .ast_type = AstType.atom, .children = null });
    try children.append(&Ast{ .body = "'baz", .ast_type = AstType.atom, .children = null });

    try write_map(list.writer(), &Ast{ .body = "", .ast_type = AstType.map, .children = children }, 0);

    try std.testing.expect(std.mem.eql(u8, list.items, "{'foo: 'bar, 'foo2: 'baz}"));
}

fn write_record(w: anytype, a: *const Ast, offset: usize) !void {
    try write_offset(w, offset);

    _ = try w.write("#");
    _ = try w.write(a.body);
    _ = try w.write("(");

    var i: usize = 0;
    var loop = true;
    while (loop) {
        try write_ast(w, a.children.?.items[i], offset);
        _ = try w.write("=");
        try write_ast(w, a.children.?.items[i + 1], offset);

        const len = a.children.?.items.len;

        if (i + 2 != len) {
            _ = try w.write(", ");
        }

        i += 2;

        if (i >= len) {
            loop = false;
        }
    }

    _ = try w.write(")");
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

    try write_record(list.writer(), &Ast{ .body = "person", .ast_type = AstType.record, .children = children }, 0);

    try std.testing.expect(std.mem.eql(u8, list.items, "#person(name=\"Joe\", age=68)"));
}

fn write_op(w: anytype, a: *const Ast, offset: usize) !void {
    try write_offset(w, offset);

    // TODO: Throw error if children is not length 2
    try write_ast(w, a.children.?.items[0], 0);

    _ = try w.write(" ");
    _ = try w.write(a.body);
    _ = try w.write(" ");
    try write_ast(w, a.children.?.items[1], 0);
}

test "write op" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "1", .ast_type = AstType.int, .children = null });
    try children.append(&Ast{ .body = "2", .ast_type = AstType.int, .children = null });

    try write_op(list.writer(), &Ast{ .body = "+", .children = children, .ast_type = AstType.op }, 0);

    try std.testing.expect(std.mem.eql(u8, list.items, "1 + 2"));
}

fn write_function_call(w: anytype, a: *const Ast, offset: usize) !void {
    try write_offset(w, offset);

    _ = try w.write(a.body);
    _ = try w.write("(");

    if (a.children != null) {
        var i: usize = 0;
        for (a.children.?.items) |c| {
            try write_ast(w, c, offset);

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

    try write_function_call(list.writer(), &Ast{ .body = "erlang.add", .ast_type = AstType.function_call, .children = children }, 0);

    try std.testing.expect(std.mem.eql(u8, list.items, "erlang.add(1, 2)"));

    // TODO: Test no arguments
}

fn write_attribute(w: anytype, a: *const Ast, offset: usize) !void {
    try write_offset(w, offset);

    _ = try w.write(a.body);
    _ = try w.write(": ");

    try write_ast(w, a.children.?.items[0], offset);
}

test "write attribute" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "int", .ast_type = AstType.atom, .children = null });

    try write_attribute(list.writer(), &Ast{ .body = "type foobar", .ast_type = AstType.attribute, .children = children }, 0);

    try std.testing.expect(std.mem.eql(u8, list.items, "type foobar: int"));
}

fn write_guard_clause(w: anytype, a: *const Ast, offset: usize) !void {
    _ = try w.write("when ");
    try write_ast(w, a.children.?.items[0], offset);
}

test "write guard clause" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    var children2 = std.ArrayList(*const Ast).init(test_allocator);
    defer children2.deinit();

    try children2.append(&Ast{ .body = "x", .ast_type = AstType.variable, .children = null });
    try children.append(&Ast{ .body = "is_number", .ast_type = AstType.function_call, .children = children2 });

    try write_guard_clause(list.writer(), &Ast{ .body = "", .ast_type = AstType.guard_clause, .children = children }, 0);

    try std.testing.expect(std.mem.eql(u8, list.items, "when is_number(x)"));
}

fn write_function_signature(w: anytype, a: *const Ast, offset: usize) !void {
    try write_offset(w, offset);

    if (a.children == null) {
        _ = try w.write("()");
        return;
    }

    _ = try w.write("(");

    // Look for guard clause
    var guard: bool = false;
    for (a.children.?.items) |c| {
        if (c.ast_type == AstType.guard_clause) {
            guard = true;
        }
    }

    var len = a.children.?.items.len;

    if (guard) {
        len = len - 1;
    }

    var i: usize = 0;

    while (true) {
        if (i >= len) {
            break;
        }

        try write_ast(w, a.children.?.items[i], offset);

        if (i + 1 < len) {
            _ = try w.write(", ");
        }

        i += 1;
    }

    _ = try w.write(")");

    if (guard) {
        _ = try w.write(" ");

        try write_guard_clause(w, a.children.?.items[len], offset);
    }
}

test "write function signature" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();
    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "a", .ast_type = AstType.variable, .children = null });
    try children.append(&Ast{ .body = "b", .ast_type = AstType.variable, .children = null });

    try write_function_signature(list.writer(), &Ast{ .body = "", .ast_type = AstType.function_signature, .children = children }, 0);

    const expected = "(a, b)";
    try std.testing.expect(std.mem.eql(u8, list.items, expected));

    list.clearAndFree();

    var children2 = std.ArrayList(*const Ast).init(test_allocator);
    defer children2.deinit();

    try children2.append(&Ast{ .body = "a", .ast_type = AstType.variable, .children = null });
    try children2.append(&Ast{ .body = "b", .ast_type = AstType.variable, .children = null });

    var guard_children = std.ArrayList(*const Ast).init(test_allocator);
    defer guard_children.deinit();

    var function_children = std.ArrayList(*const Ast).init(test_allocator);
    defer function_children.deinit();

    try function_children.append(&Ast{ .body = "a", .ast_type = AstType.variable, .children = null });
    try guard_children.append(&Ast{ .body = "is_integer", .ast_type = AstType.function_call, .children = function_children });

    try children2.append(&Ast{ .body = "", .ast_type = AstType.guard_clause, .children = guard_children });

    try write_function_signature(list.writer(), &Ast{ .body = "", .ast_type = AstType.function_signature, .children = children2 }, 0);

    try std.testing.expect(std.mem.eql(u8, list.items, "(a, b) when is_integer(a)"));
}

fn write_anonymous_function(w: anytype, a: *const Ast, offset: usize) !void {
    try write_offset(w, offset);
    try write_function_signature(w, a.children.?.items[0], offset);
    _ = try w.write(" ->");

    var i: usize = 1;

    while (true) {
        if (i >= a.children.?.items.len) {
            break;
        }

        _ = try w.write("\n    ");

        try write_ast(w, a.children.?.items[i], offset);

        i = i + 1;
    }

    _ = try w.write("\n");
}

test "write anonymous function" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "", .ast_type = AstType.function_signature, .children = null });
    try children.append(&Ast{ .body = "hello", .ast_type = AstType.function_call, .children = null });
    try children.append(&Ast{ .body = "world", .ast_type = AstType.function_call, .children = null });

    try write_anonymous_function(list.writer(), &Ast{ .body = "", .ast_type = AstType.anonymous_function, .children = children }, 0);
    try std.testing.expect(std.mem.eql(u8, list.items, "() ->\n    hello()\n    world()\n"));
}

fn write_function_def(w: anytype, a: *const Ast, offset: usize) !void {
    try write_offset(w, offset);
    _ = try w.write("def ");

    _ = try w.write(a.body);
    try write_function_signature(w, a.children.?.items[0], offset);
    _ = try w.write(":");

    var i: usize = 1;
    while (true) {
        if (i >= a.children.?.items.len) {
            break;
        }

        _ = try w.write("\n    ");
        try write_ast(w, a.children.?.items[i], offset);

        i = i + 1;
    }

    _ = try w.write("\n");
}

test "write function def" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "", .ast_type = AstType.function_signature, .children = null });
    try children.append(&Ast{ .body = "hello", .ast_type = AstType.function_call, .children = null });
    try children.append(&Ast{ .body = "world", .ast_type = AstType.function_call, .children = null });

    try write_function_def(list.writer(), &Ast{ .body = "hello_world", .ast_type = AstType.function_def, .children = children }, 0);
    try std.testing.expect(std.mem.eql(u8, list.items, "def hello_world():\n    hello()\n    world()\n"));
}

fn write_case_clause(w: anytype, a: *const Ast, offset: usize) !void {

    // TODO: Check that children are at least length of 2

    try write_offset(w, offset);

    _ = try w.write("| ");

    try write_ast(w, a.children.?.items[0], offset);

    var i: usize = 1;

    if (a.children.?.items[1].ast_type == AstType.guard_clause) {
        _ = try w.write(" ");
        try write_guard_clause(w, a.children.?.items[1], offset);

        i = i + 1;
    }

    _ = try w.write(": ");

    var loop = true;
    while (loop) {
        try write_ast(w, a.children.?.items[i], offset);

        const len = a.children.?.items.len;

        if (i + 1 != len) {
            _ = try w.write("\n ");
        }

        i += 1;

        if (i >= len) {
            loop = false;
        }
    }
}

test "write case clause" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "x", .ast_type = AstType.variable, .children = null });

    var op1_children = std.ArrayList(*const Ast).init(test_allocator);
    defer op1_children.deinit();

    try op1_children.append(&Ast{ .body = "x", .ast_type = AstType.variable, .children = null });
    try op1_children.append(&Ast{ .body = "2", .ast_type = AstType.int, .children = null });

    var op2_children = std.ArrayList(*const Ast).init(test_allocator);
    defer op2_children.deinit();

    try op2_children.append(&Ast{ .body = "y", .ast_type = AstType.variable, .children = null });
    try op2_children.append(&Ast{ .body = "+", .ast_type = AstType.op, .children = op1_children });

    try children.append(&Ast{ .body = "=", .ast_type = AstType.op, .children = op2_children });
    try children.append(&Ast{ .body = "y", .ast_type = AstType.variable, .children = null });

    try write_case_clause(list.writer(), &Ast{ .body = "", .ast_type = AstType.case_clause, .children = children }, 0);

    try std.testing.expect(std.mem.eql(u8, list.items, "| x: y = x + 2\n y"));

    list.clearAndFree();

    var children2 = std.ArrayList(*const Ast).init(test_allocator);
    defer children2.deinit();

    try children2.append(&Ast{ .body = "x", .ast_type = AstType.variable, .children = null });

    var guard_children = std.ArrayList(*const Ast).init(test_allocator);
    defer guard_children.deinit();

    var function_children = std.ArrayList(*const Ast).init(test_allocator);
    defer function_children.deinit();

    try function_children.append(&Ast{ .body = "x", .ast_type = AstType.variable, .children = null });

    try guard_children.append(&Ast{ .body = "is_integer", .ast_type = AstType.function_call, .children = function_children });

    try children2.append(&Ast{ .body = "", .ast_type = AstType.guard_clause, .children = guard_children });

    try children2.append(&Ast{ .body = "x", .ast_type = AstType.variable, .children = null });

    try write_case_clause(list.writer(), &Ast{ .body = "", .ast_type = AstType.case_clause, .children = children2 }, 0);
    try std.testing.expect(std.mem.eql(u8, list.items, "| x when is_integer(x): x"));
}

fn write_case(w: anytype, a: *const Ast, offset: usize) !void {
    // TODO: Check for children with minimum children length of 2

    try write_offset(w, offset);
    _ = try w.write("match ");
    try write_ast(w, a.children.?.items[0], offset);
    _ = try w.write(":\n");

    var i: usize = 1;
    var loop = true;
    while (loop) {
        try write_ast(w, a.children.?.items[i], offset);
        const len = a.children.?.items.len;

        _ = try w.write("\n");

        i += 1;
        if (i >= len) {
            loop = false;
        }
    }
}

test "write case" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var case_clause_children_1 = std.ArrayList(*const Ast).init(test_allocator);
    defer case_clause_children_1.deinit();

    try case_clause_children_1.append(&Ast{ .body = "'true", .ast_type = AstType.atom, .children = null });
    try case_clause_children_1.append(&Ast{ .body = "'ok", .ast_type = AstType.atom, .children = null });

    var case_clause_children_2 = std.ArrayList(*const Ast).init(test_allocator);
    defer case_clause_children_2.deinit();

    try case_clause_children_2.append(&Ast{ .body = "'false", .ast_type = AstType.atom, .children = null });
    try case_clause_children_2.append(&Ast{ .body = "'error", .ast_type = AstType.atom, .children = null });

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "x", .ast_type = AstType.variable, .children = null });

    try children.append(&Ast{ .body = "", .ast_type = AstType.case_clause, .children = case_clause_children_1 });
    try children.append(&Ast{ .body = "", .ast_type = AstType.case_clause, .children = case_clause_children_2 });

    try write_case(list.writer(), &Ast{ .body = "", .ast_type = AstType.case, .children = children }, 0);

    try std.testing.expect(std.mem.eql(u8, list.items, "match x:\n| 'true: 'ok\n| 'false: 'error\n"));
}
