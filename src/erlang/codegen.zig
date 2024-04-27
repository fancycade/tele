const std = @import("std");
const ast = @import("ast.zig");
const Ast = ast.Ast;
const AstType = ast.AstType;
const test_allocator = std.testing.allocator;

const CodegenError = error{WritingFailure};

pub const Context = struct {
    const Self = @This();

    padding: usize,

    pub fn init() Context {
        return Context{ .padding = 0 };
    }

    pub fn write_value(self: *Self, w: anytype, a: *const Ast) !void {
        try self.write_padding(w);
        _ = try w.write(a.body);
    }

    pub fn write_binary(self: *Self, w: anytype, a: *const Ast) !void {
        try self.write_padding(w);
        _ = try w.write("<<");
        _ = try w.write(a.body);
        _ = try w.write(">>");
    }

    pub fn write_tuple(self: *Self, w: anytype, a: *const Ast) !void {
        try self.write_padding(w);
        _ = try w.write("{");
        var i: usize = 0;
        for (a.children.?.items) |c| {
            try self.write_ast(w, c);

            if (i + 1 < a.children.?.items.len) {
                _ = try w.write(", ");
            }

            i += 1;
        }
        _ = try w.write("}");
    }

    pub fn write_list(self: *Self, w: anytype, a: *const Ast) !void {
        try self.write_padding(w);
        _ = try w.write("[");
        var i: usize = 0;
        for (a.children.?.items) |c| {
            try self.write_ast(w, c);

            if (i + 1 < a.children.?.items.len) {
                _ = try w.write(", ");
            }
            i += 1;
        }
        _ = try w.write("]");
    }

    // Children list consist of alternating key value pairs [k1, v1, k2, v2...]

    fn write_map(self: *Self, w: anytype, a: *const Ast) !void {
        try self.write_padding(w);
        _ = try w.write("#{");

        var loop = true;
        var i: usize = 0;
        while (loop) {
            try self.write_ast(w, a.children.?.items[i]);

            _ = try w.write(" => ");
            try self.write_ast(w, a.children.?.items[i + 1]);

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

    pub fn write_record(self: *Self, w: anytype, a: *const Ast) !void {
        try self.write_padding(w);
        _ = try w.write("#");
        _ = try w.write(a.body);
        _ = try w.write("{");

        var i: usize = 0;
        var loop = true;
        while (loop) {
            try self.write_ast(w, a.children.?.items[i]);
            _ = try w.write("=");
            try self.write_ast(w, a.children.?.items[i + 1]);

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

    pub fn write_op(self: *Self, w: anytype, a: *const Ast) !void {
        try self.write_padding(w);
        // TODO: Throw error if children is not length 2
        try self.write_ast(w, a.children.?.items[0]);

        _ = try w.write(" ");
        _ = try w.write(a.body);
        _ = try w.write(" ");
        try self.write_ast(w, a.children.?.items[1]);
    }

    pub fn write_function_call(self: *Self, w: anytype, a: *const Ast) !void {
        try self.write_padding(w);
        _ = try w.write(a.body);
        _ = try w.write("(");

        if (a.children != null) {
            var i: usize = 0;
            for (a.children.?.items) |c| {
                try self.write_ast(w, c);

                if (i + 1 != a.children.?.items.len) {
                    _ = try w.write(", ");
                }

                i += 1;
            }
        }

        _ = try w.write(")");
    }

    pub fn write_attribute(self: *Self, w: anytype, a: *const Ast) !void {
        try self.write_padding(w);
        _ = try w.write("-");
        _ = try w.write(a.body);
        _ = try w.write("(");

        var i: usize = 0;
        for (a.children.?.items) |c| {
            try self.write_ast(w, c);

            if (i + 1 != a.children.?.items.len) {
                _ = try w.write(", ");
            }
            i = i + 1;
        }

        _ = try w.write(").\n");
    }

    pub fn write_function_def(self: *Self, w: anytype, a: *const Ast) !void {
        try self.write_padding(w);
        _ = try w.write(a.body);
        try self.write_function_signature(w, a.children.?.items[0]);
        _ = try w.write(" ->");

        var i: usize = 1;
        while (true) {
            if (i >= a.children.?.items.len) {
                break;
            }

            _ = try w.write("\n    ");
            try self.write_ast(w, a.children.?.items[i]);

            if (i + 1 < a.children.?.items.len) {
                _ = try w.write(",");
            }

            i = i + 1;
        }

        _ = try w.write(".\n\n");
    }

    pub fn write_anonymous_function(self: *Self, w: anytype, a: *const Ast) !void {
        try self.write_padding(w);
        _ = try w.write("fun");
        try self.write_function_signature(w, a.children.?.items[0]);
        _ = try w.write(" ->");

        var i: usize = 1;

        while (true) {
            if (i >= a.children.?.items.len) {
                break;
            }

            _ = try w.write("\n    ");

            try self.write_ast(w, a.children.?.items[i]);

            if (i + 1 < a.children.?.items.len) {
                _ = try w.write(",");
            }

            i = i + 1;
        }

        _ = try w.write("\nend");
    }

    pub fn write_function_signature(self: *Self, w: anytype, a: *const Ast) !void {
        try self.write_padding(w);
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

            try self.write_ast(w, a.children.?.items[i]);

            if (i + 1 < len) {
                _ = try w.write(", ");
            }

            i += 1;
        }

        _ = try w.write(")");

        if (guard) {
            _ = try w.write(" ");

            try self.write_guard_clause(w, a.children.?.items[len]);
        }
    }

    pub fn write_guard_clause(self: *Self, w: anytype, a: *const Ast) !void {
        try self.write_padding(w);
        _ = try w.write("when ");
        var i: usize = 0;
        for (a.children.?.items) |c| {
            try self.write_ast(w, c);

            if (i + 1 != a.children.?.items.len) {
                _ = try w.write(", ");
            }

            i = i + 1;
        }
    }

    pub fn write_case_clause(self: *Self, w: anytype, a: *const Ast) !void {
        try self.write_padding(w);

        // TODO: Check that children are at least length of 2

        try self.write_ast(w, a.children.?.items[0]);

        var i: usize = 1;

        if (a.children.?.items[1].ast_type == AstType.guard_clause) {
            _ = try w.write(" ");
            try self.write_guard_clause(w, a.children.?.items[1]);

            i = i + 1;
        }

        _ = try w.write(" -> ");

        var loop = true;
        while (loop) {
            try self.write_ast(w, a.children.?.items[i]);

            const len = a.children.?.items.len;

            if (i + 1 != len) {
                _ = try w.write(",\n ");
            }

            i += 1;

            if (i >= len) {
                loop = false;
            }
        }
    }

    pub fn write_case(self: *Self, w: anytype, a: *const Ast) !void {
        try self.write_padding(w);
        // TODO: Check for children with minimum children length of 2

        _ = try w.write("case ");
        try self.write_ast(w, a.children.?.items[0]);
        _ = try w.write(" of\n ");

        var i: usize = 1;
        var loop = true;
        while (loop) {
            try self.write_ast(w, a.children.?.items[i]);
            const len = a.children.?.items.len;

            if (i + 1 != len) {
                _ = try w.write(";");
            }

            _ = try w.write("\n ");

            i += 1;
            if (i >= len) {
                loop = false;
            }
        }

        _ = try w.write("end");
    }

    pub fn write_padding(self: *Self, w: anytype) !void {
        var ctr: usize = 0;
        while (ctr < self.*.padding) {
            _ = try w.write(" ");
            ctr += 1;
        }
    }

    pub fn write_ast(self: *Self, w: anytype, a: *const Ast) error{WritingFailure}!void {
        switch (a.ast_type) {
            .int => {
                self.write_value(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .float => {
                self.write_value(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .binary => {
                self.write_binary(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .atom => {
                self.write_value(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .variable => {
                self.write_value(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .record => {
                self.write_record(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .tuple => {
                self.write_tuple(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .list => {
                self.write_list(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .map => {
                self.write_map(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .attribute => {
                self.write_attribute(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .function_call => {
                self.write_function_call(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .function_def => {
                self.write_function_def(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .anonymous_function => {
                self.write_anonymous_function(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .function_signature => {
                self.write_function_signature(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .op => {
                self.write_op(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .case_clause => {
                self.write_case_clause(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .case => {
                self.write_case(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .guard_clause => {
                self.write_guard_clause(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
        }
    }
};

test "write value" {
    var context = Context.init();

    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    try context.write_value(list.writer(), &Ast{ .body = "1", .ast_type = AstType.int, .children = null });
    try std.testing.expect(std.mem.eql(u8, list.items, "1"));
    list.clearAndFree();

    try context.write_value(list.writer(), &Ast{ .body = "100_000", .ast_type = AstType.int, .children = null });
    try std.testing.expect(std.mem.eql(u8, list.items, "100_000"));
    list.clearAndFree();

    try context.write_value(list.writer(), &Ast{ .body = "1.0", .ast_type = AstType.int, .children = null });
    try std.testing.expect(std.mem.eql(u8, list.items, "1.0"));
    list.clearAndFree();

    // TODO: Scientific notation

    try context.write_value(list.writer(), &Ast{ .body = "foo", .ast_type = AstType.atom, .children = null });
    try std.testing.expect(std.mem.eql(u8, list.items, "foo"));
    // TODO: Normal Atom
    // TODO: Atom with double quotes
}

test "write binary" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var context = Context.init();

    try context.write_binary(list.writer(), &Ast{ .body = "\"foobar\"", .ast_type = AstType.binary, .children = null });

    try std.testing.expect(std.mem.eql(u8, list.items, "<<\"foobar\">>"));
}

test "write tuple" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "1", .ast_type = AstType.int, .children = null });
    try children.append(&Ast{ .body = "2", .ast_type = AstType.int, .children = null });

    var context = Context.init();

    try context.write_tuple(list.writer(), &Ast{ .body = "", .ast_type = AstType.list, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "{1, 2}"));
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

    var context = Context.init();

    try context.write_list(list.writer(), &Ast{ .body = "", .children = children, .ast_type = AstType.list });

    try std.testing.expect(std.mem.eql(u8, list.items, "[1, foo, 42.42, <<\"foobar\">>]"));
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

    var context = Context.init();

    try context.write_map(list.writer(), &Ast{ .body = "", .ast_type = AstType.map, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "#{foo => bar, foo2 => baz}"));
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

    var context = Context.init();

    try context.write_record(list.writer(), &Ast{ .body = "person", .ast_type = AstType.record, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "#person{name=<<\"Joe\">>, age=68}"));
}

test "write op" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "1", .ast_type = AstType.int, .children = null });
    try children.append(&Ast{ .body = "2", .ast_type = AstType.int, .children = null });

    var context = Context.init();
    try context.write_op(list.writer(), &Ast{ .body = "+", .children = children, .ast_type = AstType.op });

    try std.testing.expect(std.mem.eql(u8, list.items, "1 + 2"));
}

test "write function call" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "1", .ast_type = AstType.int, .children = null });
    try children.append(&Ast{ .body = "2", .ast_type = AstType.int, .children = null });

    var context = Context.init();
    try context.write_function_call(list.writer(), &Ast{ .body = "erlang:add", .ast_type = AstType.function_call, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "erlang:add(1, 2)"));

    // TODO: Test no arguments
}

test "write attribute" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "foobar", .ast_type = AstType.atom, .children = null });

    var context = Context.init();
    try context.write_attribute(list.writer(), &Ast{ .body = "module", .ast_type = AstType.attribute, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "-module(foobar).\n"));

    // Test multi arg string
}

test "write function def" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "", .ast_type = AstType.function_signature, .children = null });
    try children.append(&Ast{ .body = "hello", .ast_type = AstType.function_call, .children = null });
    try children.append(&Ast{ .body = "world", .ast_type = AstType.function_call, .children = null });

    var context = Context.init();
    try context.write_function_def(list.writer(), &Ast{ .body = "hello_world", .ast_type = AstType.function_def, .children = children });
    try std.testing.expect(std.mem.eql(u8, list.items, "hello_world() ->\n    hello(),\n    world().\n\n"));
}

test "write anonymous function" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "", .ast_type = AstType.function_signature, .children = null });
    try children.append(&Ast{ .body = "hello", .ast_type = AstType.function_call, .children = null });
    try children.append(&Ast{ .body = "world", .ast_type = AstType.function_call, .children = null });

    var context = Context.init();
    try context.write_anonymous_function(list.writer(), &Ast{ .body = "", .ast_type = AstType.anonymous_function, .children = children });
    try std.testing.expect(std.mem.eql(u8, list.items, "fun() ->\n    hello(),\n    world()\nend"));
}

test "write function signature" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();
    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "A", .ast_type = AstType.variable, .children = null });
    try children.append(&Ast{ .body = "B", .ast_type = AstType.variable, .children = null });

    var context = Context.init();
    try context.write_function_signature(list.writer(), &Ast{ .body = "", .ast_type = AstType.function_signature, .children = children });

    const expected = "(A, B)";
    try std.testing.expect(std.mem.eql(u8, list.items, expected));

    list.clearAndFree();

    var children2 = std.ArrayList(*const Ast).init(test_allocator);
    defer children2.deinit();

    try children2.append(&Ast{ .body = "A", .ast_type = AstType.variable, .children = null });
    try children2.append(&Ast{ .body = "B", .ast_type = AstType.variable, .children = null });

    var guard_children = std.ArrayList(*const Ast).init(test_allocator);
    defer guard_children.deinit();

    var function_children = std.ArrayList(*const Ast).init(test_allocator);
    defer function_children.deinit();

    try function_children.append(&Ast{ .body = "A", .ast_type = AstType.variable, .children = null });
    try guard_children.append(&Ast{ .body = "is_integer", .ast_type = AstType.function_call, .children = function_children });

    try children2.append(&Ast{ .body = "", .ast_type = AstType.guard_clause, .children = guard_children });

    try context.write_function_signature(list.writer(), &Ast{ .body = "", .ast_type = AstType.function_signature, .children = children2 });

    try std.testing.expect(std.mem.eql(u8, list.items, "(A, B) when is_integer(A)"));
}

test "write guard clause" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    var children2 = std.ArrayList(*const Ast).init(test_allocator);
    defer children2.deinit();

    try children2.append(&Ast{ .body = "X", .ast_type = AstType.variable, .children = null });
    try children.append(&Ast{ .body = "is_number", .ast_type = AstType.function_call, .children = children2 });

    var children3 = std.ArrayList(*const Ast).init(test_allocator);
    defer children3.deinit();

    try children3.append(&Ast{ .body = "X", .ast_type = AstType.variable, .children = null });
    try children.append(&Ast{ .body = "is_integer", .ast_type = AstType.function_call, .children = children3 });

    var context = Context.init();
    try context.write_guard_clause(list.writer(), &Ast{ .body = "", .ast_type = AstType.guard_clause, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "when is_number(X), is_integer(X)"));
}

test "write case clause" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "X", .ast_type = AstType.variable, .children = null });

    var op1_children = std.ArrayList(*const Ast).init(test_allocator);
    defer op1_children.deinit();

    try op1_children.append(&Ast{ .body = "X", .ast_type = AstType.variable, .children = null });
    try op1_children.append(&Ast{ .body = "2", .ast_type = AstType.int, .children = null });

    var op2_children = std.ArrayList(*const Ast).init(test_allocator);
    defer op2_children.deinit();

    try op2_children.append(&Ast{ .body = "Y", .ast_type = AstType.variable, .children = null });
    try op2_children.append(&Ast{ .body = "+", .ast_type = AstType.op, .children = op1_children });

    try children.append(&Ast{ .body = "=", .ast_type = AstType.op, .children = op2_children });
    try children.append(&Ast{ .body = "Y", .ast_type = AstType.variable, .children = null });

    var context = Context.init();
    try context.write_case_clause(list.writer(), &Ast{ .body = "", .ast_type = AstType.case_clause, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "X -> Y = X + 2,\n Y"));

    list.clearAndFree();

    var children2 = std.ArrayList(*const Ast).init(test_allocator);
    defer children2.deinit();

    try children2.append(&Ast{ .body = "X", .ast_type = AstType.variable, .children = null });

    var guard_children = std.ArrayList(*const Ast).init(test_allocator);
    defer guard_children.deinit();

    var function_children = std.ArrayList(*const Ast).init(test_allocator);
    defer function_children.deinit();

    try function_children.append(&Ast{ .body = "X", .ast_type = AstType.variable, .children = null });

    try guard_children.append(&Ast{ .body = "is_integer", .ast_type = AstType.function_call, .children = function_children });

    try children2.append(&Ast{ .body = "", .ast_type = AstType.guard_clause, .children = guard_children });

    try children2.append(&Ast{ .body = "X", .ast_type = AstType.variable, .children = null });

    try context.write_case_clause(list.writer(), &Ast{ .body = "", .ast_type = AstType.case_clause, .children = children2 });
    try std.testing.expect(std.mem.eql(u8, list.items, "X when is_integer(X) -> X"));
}

test "write case" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var case_clause_children_1 = std.ArrayList(*const Ast).init(test_allocator);
    defer case_clause_children_1.deinit();

    try case_clause_children_1.append(&Ast{ .body = "true", .ast_type = AstType.atom, .children = null });
    try case_clause_children_1.append(&Ast{ .body = "ok", .ast_type = AstType.atom, .children = null });

    var case_clause_children_2 = std.ArrayList(*const Ast).init(test_allocator);
    defer case_clause_children_2.deinit();

    try case_clause_children_2.append(&Ast{ .body = "false", .ast_type = AstType.atom, .children = null });
    try case_clause_children_2.append(&Ast{ .body = "error", .ast_type = AstType.atom, .children = null });

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "X", .ast_type = AstType.variable, .children = null });

    try children.append(&Ast{ .body = "", .ast_type = AstType.case_clause, .children = case_clause_children_1 });
    try children.append(&Ast{ .body = "", .ast_type = AstType.case_clause, .children = case_clause_children_2 });

    var context = Context.init();
    try context.write_case(list.writer(), &Ast{ .body = "", .ast_type = AstType.case, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "case X of\n true -> ok;\n false -> error\n end"));
}
