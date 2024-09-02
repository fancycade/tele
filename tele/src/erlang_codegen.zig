const std = @import("std");
const ast = @import("erlang_ast.zig");
const Ast = ast.Ast;
const AstType = ast.AstType;
const test_allocator = std.testing.allocator;

const CodegenError = error{WritingFailure};

pub const Context = struct {
    const Self = @This();

    padding_stack: std.ArrayList(usize),
    match_count: usize,
    attribute_mode: bool,

    pub fn init(allocator: std.mem.Allocator) Context {
        return Context{ .padding_stack = std.ArrayList(usize).init(allocator), .match_count = 0, .attribute_mode = false };
    }

    pub fn deinit(self: *Self) void {
        self.padding_stack.deinit();
    }

    pub fn write_value(self: *Self, w: anytype, a: *const Ast) !void {
        try self.write_padding(w);
        _ = try w.write(a.body);
    }

    pub fn write_binary(self: *Self, w: anytype, a: *const Ast) !void {
        try self.write_padding(w);

        if (self.*.attribute_mode) {
            _ = try w.write(a.body);
        } else {
            if (a.body[0] == '<') {
                _ = try w.write(a.body);
            } else {
                _ = try w.write("<<");
                _ = try w.write(a.body);
                _ = try w.write("/utf8>>");
            }
        }
    }

    pub fn write_tuple(self: *Self, w: anytype, a: *const Ast, type_exp: bool) !void {
        try self.write_padding(w);
        _ = try w.write("{");
        var i: usize = 0;
        try self.push_padding(0);
        for (a.children.?.items) |c| {
            try self.write_ast(w, c, type_exp);

            if (i + 1 < a.children.?.items.len) {
                _ = try w.write(", ");
            }

            i += 1;
        }
        try self.pop_padding();
        _ = try w.write("}");
    }

    pub fn write_list(self: *Self, w: anytype, a: *const Ast, type_exp: bool) !void {
        try self.write_padding(w);
        _ = try w.write("[");
        var i: usize = 0;
        try self.push_padding(0);
        for (a.children.?.items) |c| {
            try self.write_ast(w, c, type_exp);

            if (i + 1 < a.children.?.items.len) {
                _ = try w.write(", ");
            }
            i += 1;
        }
        try self.pop_padding();
        _ = try w.write("]");
    }

    // Children list consist of alternating key value pairs [k1, v1, k2, v2...]

    fn write_map(self: *Self, w: anytype, a: *const Ast, type_exp: bool) !void {
        try self.write_padding(w);
        if (!std.mem.eql(u8, a.*.body, "")) {
            _ = try w.write(a.*.body);
        }
        _ = try w.write("#{");

        var loop = true;
        var i: usize = 0;
        try self.push_padding(0);
        if (a.children != null and a.children.?.items.len > 0) {
            while (loop) {
                try self.write_ast(w, a.children.?.items[i], type_exp);

                if (self.match_mode()) {
                    _ = try w.write(" := ");
                } else {
                    _ = try w.write(" => ");
                }
                try self.write_ast(w, a.children.?.items[i + 1], type_exp);

                const len = a.children.?.items.len;

                if (i + 2 != len) {
                    _ = try w.write(", ");
                }

                i += 2;

                if (i >= len) {
                    loop = false;
                }
            }
        }
        try self.pop_padding();

        _ = try w.write("}");
    }

    pub fn write_record(self: *Self, w: anytype, a: *const Ast) !void {
        try self.write_padding(w);

        if (contains_hash(a.body)) {
            _ = try w.write(a.body);
        } else {
            _ = try w.write("#");
            _ = try w.write(a.body);
        }
        _ = try w.write("{");

        try self.push_padding(0);

        var i: usize = 0;
        for (a.children.?.items) |c| {
            try self.write_ast(w, c, false);

            if (i + 1 < a.children.?.items.len) {
                _ = try w.write(", ");
            }
            i += 1;
        }

        try self.pop_padding();
        _ = try w.write("}");
    }

    fn contains_hash(buf: []const u8) bool {
        for (buf) |c| {
            if (c == '#') {
                return true;
            }
        }
        return false;
    }

    pub fn write_fun_val(self: *Self, w: anytype, a: *const Ast) !void {
        try self.write_padding(w);
        if (!self.attribute_mode) {
            _ = try w.write("fun ");
        }
        _ = try w.write(a.body);
    }

    // Matching must be done on left side of operator
    pub fn write_op(self: *Self, w: anytype, a: *const Ast, type_exp: bool) !void {
        try self.write_padding(w);
        try self.push_padding(0);
        // TODO: Throw error if children is not length 2

        if (a.children.?.items.len == 2) {
            self.push_match();
            try self.write_ast(w, a.children.?.items[0], type_exp);
            self.pop_match();

            _ = try w.write(" ");
            _ = try w.write(a.body);
            _ = try w.write(" ");
            try self.write_ast(w, a.children.?.items[1], type_exp);
        } else if (a.children.?.items.len == 1) {
            _ = try w.write(a.body);
            if (std.mem.eql(u8, "not", a.body)) {
                _ = try w.write(" ");
            }
            try self.write_ast(w, a.children.?.items[0], type_exp);
        } else {
            return CodegenError.WritingFailure;
        }

        try self.pop_padding();
    }

    pub fn write_paren_exp(self: *Self, w: anytype, a: *const Ast) !void {
        try self.write_padding(w);
        try self.push_padding(0);
        _ = try w.write("(");
        try self.write_ast(w, a.children.?.items[0], false);
        _ = try w.write(")");
        try self.pop_padding();
    }

    pub fn write_function_call(self: *Self, w: anytype, a: *const Ast, type_exp: bool) !void {
        try self.write_padding(w);
        _ = try w.write(a.body);
        _ = try w.write("(");

        if (a.children != null) {
            try self.push_padding(0);
            var i: usize = 0;
            for (a.children.?.items) |c| {
                try self.write_ast(w, c, type_exp);

                if (i + 1 != a.children.?.items.len) {
                    _ = try w.write(", ");
                }

                i += 1;
            }
            try self.pop_padding();
        }

        _ = try w.write(")");
    }

    pub fn write_import_def(self: *Self, w: anytype, a: *const Ast) !void {
        try self.write_padding(w);
        self.attribute_mode = true;
        _ = try w.write("-import(");
        _ = try w.write(a.body);
        _ = try w.write(", [");

        var i: usize = 0;
        for (a.children.?.items) |c| {
            try self.write_ast(w, c, false);

            if (i + 1 != a.children.?.items.len) {
                _ = try w.write(", ");
            }
            i = i + 1;
        }

        _ = try w.write("]).\n");
        self.attribute_mode = false;
    }

    pub fn write_attribute(self: *Self, w: anytype, a: *const Ast) !void {
        try self.write_padding(w);
        self.attribute_mode = true;
        _ = try w.write("-");
        _ = try w.write(a.body);
        _ = try w.write("(");

        var i: usize = 0;
        for (a.children.?.items) |c| {
            try self.write_ast(w, c, false);

            if (i + 1 != a.children.?.items.len) {
                _ = try w.write(", ");
            }
            i = i + 1;
        }

        _ = try w.write(").\n");
        self.attribute_mode = false;
    }

    pub fn write_custom_attribute(self: *Self, w: anytype, a: *const Ast) !void {
        self.attribute_mode = true;
        try self.write_padding(w);
        _ = try w.write("-");
        try self.write_function_call(w, a, false);
        _ = try w.write(".\n\n");
        self.attribute_mode = false;
    }

    pub fn write_function_def(self: *Self, w: anytype, a: *const Ast) !void {
        var i: usize = 0;

        while (true) {
            if (i >= a.children.?.items.len) {
                break;
            }

            try self.write_padding(w);
            _ = try w.write(a.body);
            self.push_match();
            try self.write_function_signature(w, a.children.?.items[i], false);
            self.pop_match();
            _ = try w.write(" ->");

            i = i + 1;

            try self.push_padding(self.current_padding() + 4);
            while (true) {
                if (i >= a.children.?.items.len or a.children.?.items[i].ast_type == AstType.function_signature) {
                    break;
                }

                _ = try w.write("\n");
                try self.write_ast(w, a.children.?.items[i], false);

                if (i + 1 < a.children.?.items.len and a.children.?.items[i + 1].ast_type != AstType.function_signature) {
                    _ = try w.write(",");
                }

                i = i + 1;
            }

            var can_break = false;
            if (i >= a.children.?.items.len) {
                _ = try w.write(".\n\n");
                can_break = true;
            } else if (a.children.?.items[i].ast_type == AstType.function_signature) {
                _ = try w.write(";\n");
            } else {
                return CodegenError.WritingFailure;
            }

            try self.pop_padding();

            if (can_break) {
                break;
            }
        }
    }

    pub fn write_macro_def(self: *Self, w: anytype, a: *const Ast) !void {
        try self.write_padding(w);
        _ = try w.write("-define(");
        _ = try w.write(a.body);

        if (a.children.?.items.len == 1) {
            _ = try w.write(", ");
            try self.write_ast(w, a.children.?.items[0], false);
        } else {
            try self.write_function_signature(w, a.children.?.items[0], false);
            _ = try w.write(", begin ");
            for (a.children.?.items[1..a.children.?.items.len]) |c| {
                _ = try w.write("\n");
                try self.write_ast(w, c, false);
            }
            _ = try w.write("\nend");
        }
        _ = try w.write(").\n\n");
    }

    pub fn write_spec_def(self: *Self, w: anytype, a: *const Ast) !void {
        _ = try w.write("-spec ");
        _ = try w.write(a.body);
        try self.write_function_signature(w, a.children.?.items[0], true);
        _ = try w.write(" -> ");
        try self.write_ast(w, a.children.?.items[1], true);
        _ = try w.write(".\n");
    }

    pub fn write_callback_def(self: *Self, w: anytype, a: *const Ast) !void {
        _ = try w.write("-callback ");
        _ = try w.write(a.body);
        try self.write_function_signature(w, a.children.?.items[0], true);
        _ = try w.write(" -> ");
        try self.write_ast(w, a.children.?.items[1], true);
        _ = try w.write(".\n");
    }

    pub fn write_opaque_type_def(self: *Self, w: anytype, a: *const Ast) !void {
        _ = try w.write("-opaque ");
        try self.write_ast(w, a.*.children.?.items[0], false);
        _ = try w.write(" :: ");
        try self.write_ast(w, a.*.children.?.items[1], true);
        _ = try w.write(".\n\n");
    }

    pub fn write_type_def(self: *Self, w: anytype, a: *const Ast) !void {
        _ = try w.write("-type ");
        try self.write_ast(w, a.*.children.?.items[0], false);
        _ = try w.write(" :: ");
        try self.write_ast(w, a.*.children.?.items[1], true);
        _ = try w.write(".\n\n");
    }

    pub fn write_record_def(self: *Self, w: anytype, a: *const Ast) !void {
        _ = try w.write("-record(");
        _ = try w.write(a.*.body);
        _ = try w.write(", {");

        try self.push_padding(0);
        var i: usize = 0;
        for (a.children.?.items) |c| {
            try self.write_ast(w, c, true);

            if (i + 1 < a.children.?.items.len) {
                _ = try w.write(", ");
            }
            i += 1;
        }
        try self.pop_padding();

        _ = try w.write("}).\n\n");
    }

    pub fn write_record_field(self: *Self, w: anytype, a: *const Ast) !void {
        _ = try w.write(a.*.body);

        if (a.*.children != null) {
            if (a.*.children.?.items.len == 0) {} else if (a.*.children.?.items.len == 1) {
                const a2 = a.*.children.?.items[0];
                if (a2.ast_type == AstType.record_field_value) {
                    try self.write_record_field_value(w, a2);
                } else if (a2.ast_type == AstType.record_field_type) {
                    try self.write_record_field_type(w, a2);
                } else {
                    return CodegenError.WritingFailure;
                }
            } else if (a.*.children.?.items.len == 2) {
                const a2 = a.*.children.?.items[0];
                if (a2.ast_type == AstType.record_field_value) {
                    try self.write_record_field_value(w, a2);
                } else {
                    return CodegenError.WritingFailure;
                }

                const a3 = a.*.children.?.items[1];
                if (a3.ast_type == AstType.record_field_type) {
                    try self.write_record_field_type(w, a3);
                } else {
                    return CodegenError.WritingFailure;
                }
            } else {
                return CodegenError.WritingFailure;
            }
        }
    }

    pub fn write_record_field_value(self: *Self, w: anytype, a: *const Ast) !void {
        _ = try w.write("=");
        if (a.children.?.items.len < 1) {
            return CodegenError.WritingFailure;
        }
        try self.write_ast(w, a.children.?.items[0], false);
    }

    pub fn write_record_field_type(self: *Self, w: anytype, a: *const Ast) !void {
        _ = try w.write(" :: ");
        if (a.children.?.items.len < 1) {
            return CodegenError.WritingFailure;
        }
        try self.write_ast(w, a.children.?.items[0], true);
    }

    pub fn write_anonymous_function(self: *Self, w: anytype, a: *const Ast, type_exp: bool) !void {
        try self.write_padding(w);
        _ = try w.write("fun");
        if (type_exp and a.children.?.items.len > 1) {
            _ = try w.write("(");
        }
        try self.push_padding(0);
        self.push_match();
        try self.write_function_signature(w, a.children.?.items[0], type_exp);
        self.pop_match();
        try self.pop_padding();

        if (a.children.?.items.len > 1) {
            _ = try w.write(" ->");

            var i: usize = 1;

            while (true) {
                if (i >= a.children.?.items.len) {
                    break;
                }

                if (!type_exp) {
                    _ = try w.write("\n");

                    try self.push_padding(self.current_padding() + 4);
                } else {
                    _ = try w.write(" ");
                }

                try self.write_ast(w, a.children.?.items[i], type_exp);

                if (!type_exp) {
                    if (i + 1 < a.children.?.items.len) {
                        _ = try w.write(",");
                    }

                    try self.pop_padding();
                }

                i = i + 1;
            }

            if (!type_exp) {
                _ = try w.write("\n");
                try self.write_padding(w);
                _ = try w.write("end");
            } else if (a.children.?.items.len > 0) {
                _ = try w.write(")");
            }
        }
    }

    pub fn write_function_signature(self: *Self, w: anytype, a: *const Ast, type_exp: bool) !void {
        try self.write_padding(w);
        if (a.children == null) {
            _ = try w.write("()");
            return;
        }

        _ = try w.write("(");

        // Look for guard clause at end of children
        var guard: bool = false;
        if (a.children.?.items[a.children.?.items.len - 1].ast_type == AstType.guard_clause) {
            guard = true;
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

            try self.write_ast(w, a.children.?.items[i], type_exp);

            if (i + 1 < len) {
                _ = try w.write(", ");
            }

            i += 1;
        }

        _ = try w.write(")");

        if (guard) {
            _ = try w.write(" ");

            try self.write_guard_clause(w, a.children.?.items[len], type_exp);
        }
    }

    pub fn write_guard_clause(self: *Self, w: anytype, a: *const Ast, type_exp: bool) !void {
        try self.write_padding(w);
        _ = try w.write("when ");
        var i: usize = 0;
        for (a.children.?.items) |c| {
            try self.write_ast(w, c, type_exp);

            if (i + 1 != a.children.?.items.len) {
                _ = try w.write(", ");
            }

            i = i + 1;
        }
    }

    pub fn write_case_clause(self: *Self, w: anytype, a: *const Ast) !void {
        // TODO: Check that children are at least length of 2

        self.push_match();
        try self.write_ast(w, a.children.?.items[0], false);
        self.pop_match();

        var i: usize = 1;

        if (a.children.?.items[1].ast_type == AstType.guard_clause) {
            _ = try w.write(" ");
            try self.push_padding(0);
            try self.write_guard_clause(w, a.children.?.items[1], false);
            try self.pop_padding();

            i = i + 1;
        }
        _ = try w.write(" ->");

        try self.push_padding(self.current_padding() + 4);
        var loop = true;
        while (loop) {
            _ = try w.write("\n");

            try self.write_ast(w, a.children.?.items[i], false);

            const len = a.children.?.items.len;

            if (i + 1 != len) {
                _ = try w.write(",");
            }

            i += 1;

            if (i >= len) {
                loop = false;
            }
        }
        try self.pop_padding();
    }

    pub fn write_case(self: *Self, w: anytype, a: *const Ast) !void {
        try self.write_padding(w);
        // TODO: Check for children with minimum children length of 2

        _ = try w.write("case ");
        try self.push_padding(0);
        try self.write_ast(w, a.children.?.items[0], false);
        try self.pop_padding();
        _ = try w.write(" of");

        var i: usize = 1;
        var loop = true;
        try self.push_padding(self.current_padding() + 4);
        while (loop) {
            _ = try w.write("\n");

            try self.write_ast(w, a.children.?.items[i], false);
            const len = a.children.?.items.len;

            if (i + 1 != len) {
                _ = try w.write(";");
            }

            i += 1;
            if (i >= len) {
                loop = false;
            }
        }
        try self.pop_padding();

        _ = try w.write("\n");
        _ = try self.write_padding(w);
        _ = try w.write("end");
    }

    pub fn write_try_catch(self: *Self, w: anytype, a: *const Ast) !void {
        try self.write_try_exp(w, a.*.children.?.items[0]);
        try self.write_catch_exp(w, a.*.children.?.items[1]);
    }

    pub fn write_try_exp(self: *Self, w: anytype, a: *const Ast) !void {
        try self.write_padding(w);
        // Check for children with minimum length of 2

        _ = try w.write("try ");
        try self.push_padding(0);
        try self.write_ast(w, a.children.?.items[0], false);
        try self.pop_padding();
        _ = try w.write(" of");

        var i: usize = 1;
        var loop = true;
        try self.push_padding(self.current_padding() + 4);

        while (loop) {
            _ = try w.write("\n");
            try self.write_ast(w, a.children.?.items[i], false);
            const len = a.children.?.items.len;

            if (i + 1 != len) {
                _ = try w.write(";");
            }

            i += 1;
            if (i >= len) {
                loop = false;
            }
        }
        try self.pop_padding();

        _ = try w.write("\n");
    }

    pub fn write_catch_exp(self: *Self, w: anytype, a: *const Ast) !void {
        try self.write_padding(w);
        // Check for children with minimum length of 1

        _ = try w.write("catch\n");

        try self.push_padding(self.current_padding() + 4);
        for (a.*.children.?.items) |c| {
            try self.write_ast(w, c, false);
        }
        try self.pop_padding();
        _ = try w.write("\n");

        try self.write_padding(w);
        _ = try w.write("end");
    }

    pub fn push_match(self: *Self) void {
        self.*.match_count += 1;
    }

    pub fn pop_match(self: *Self) void {
        if (self.*.match_count == 0) {
            return;
        }
        self.*.match_count -= 1;
    }

    pub fn match_mode(self: *Self) bool {
        return self.*.match_count > 0;
    }

    pub fn push_padding(self: *Self, padding: usize) !void {
        try self.*.padding_stack.append(padding);
    }

    pub fn pop_padding(self: *Self) !void {
        _ = self.*.padding_stack.pop();
    }

    pub fn current_padding(self: *Self) usize {
        if (self.*.padding_stack.items.len == 0) {
            return 0;
        } else {
            return self.*.padding_stack.items[self.*.padding_stack.items.len - 1];
        }
    }

    pub fn write_padding(self: *Self, w: anytype) !void {
        var ctr: usize = 0;
        if (self.*.padding_stack.items.len > 0) {
            while (ctr < self.current_padding()) {
                _ = try w.write(" ");
                ctr += 1;
            }
        }
    }

    pub fn write_ast(self: *Self, w: anytype, a: *const Ast, type_exp: bool) error{WritingFailure}!void {
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
            .fun_val => {
                self.write_fun_val(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .tuple => {
                self.write_tuple(w, a, type_exp) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .list => {
                self.write_list(w, a, type_exp) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .map => {
                self.write_map(w, a, type_exp) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .attribute => {
                self.write_attribute(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .custom_attribute => {
                self.write_custom_attribute(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .function_call => {
                self.write_function_call(w, a, type_exp) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .function_def => {
                self.write_function_def(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .macro_def => {
                self.write_macro_def(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .type_def => {
                self.write_type_def(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .opaque_type_def => {
                self.write_opaque_type_def(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .record_def => {
                self.write_record_def(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .record_field => {
                self.write_record_field(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .record_field_value => {
                self.write_record_field_value(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .record_field_type => {
                self.write_record_field_type(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .spec_def => {
                self.write_spec_def(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .callback_def => {
                self.write_callback_def(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .import_def => {
                self.write_import_def(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .anonymous_function => {
                self.write_anonymous_function(w, a, type_exp) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .function_signature => {
                self.write_function_signature(w, a, type_exp) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .op => {
                self.write_op(w, a, type_exp) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .paren_exp => {
                self.write_paren_exp(w, a) catch {
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
                self.write_guard_clause(w, a, type_exp) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .try_catch => {
                self.write_try_catch(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .try_exp => {
                self.write_try_exp(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .catch_exp => {
                self.write_catch_exp(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
        }
    }
};

test "write value" {
    var context = Context.init(test_allocator);
    defer context.deinit();

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

    var context = Context.init(test_allocator);
    defer context.deinit();

    try context.write_binary(list.writer(), &Ast{ .body = "\"foobar\"", .ast_type = AstType.binary, .children = null });

    try std.testing.expect(std.mem.eql(u8, list.items, "<<\"foobar\"/utf8>>"));
}

test "write tuple" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "1", .ast_type = AstType.int, .children = null });
    try children.append(&Ast{ .body = "2", .ast_type = AstType.int, .children = null });

    var context = Context.init(test_allocator);
    defer context.deinit();

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

    var context = Context.init(test_allocator);
    defer context.deinit();

    try context.write_list(list.writer(), &Ast{ .body = "", .children = children, .ast_type = AstType.list });

    try std.testing.expect(std.mem.eql(u8, list.items, "[1, foo, 42.42, <<\"foobar\"/utf8>>]"));
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

    var context = Context.init(test_allocator);
    defer context.deinit();

    try context.write_map(list.writer(), &Ast{ .body = "", .ast_type = AstType.map, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "#{foo => bar, foo2 => baz}"));
}

test "write record" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    var field1_children = std.ArrayList(*const Ast).init(test_allocator);
    defer field1_children.deinit();

    var fv1_children = std.ArrayList(*const Ast).init(test_allocator);
    defer fv1_children.deinit();
    try fv1_children.append(&Ast{ .body = "\"Joe\"", .ast_type = AstType.binary, .children = null });

    try field1_children.append(&Ast{ .body = "", .ast_type = AstType.record_field_value, .children = fv1_children });

    var field2_children = std.ArrayList(*const Ast).init(test_allocator);
    defer field2_children.deinit();
    var fv2_children = std.ArrayList(*const Ast).init(test_allocator);
    defer fv2_children.deinit();
    try fv2_children.append(&Ast{ .body = "68", .ast_type = AstType.int, .children = null });
    try field2_children.append(&Ast{ .body = "", .ast_type = AstType.record_field_value, .children = fv2_children });

    const field1 = Ast{ .body = "name", .ast_type = AstType.record_field, .children = field1_children };
    const field2 = Ast{ .body = "age", .ast_type = AstType.record_field, .children = field2_children };

    try children.append(&field1);
    try children.append(&field2);

    var context = Context.init(test_allocator);
    defer context.deinit();

    try context.write_record(list.writer(), &Ast{ .body = "person", .ast_type = AstType.record, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "#person{name=<<\"Joe\"/utf8>>, age=68}"));
}

test "write op" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "1", .ast_type = AstType.int, .children = null });
    try children.append(&Ast{ .body = "2", .ast_type = AstType.int, .children = null });

    var context = Context.init(test_allocator);
    defer context.deinit();
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

    var context = Context.init(test_allocator);
    defer context.deinit();
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

    var context = Context.init(test_allocator);
    defer context.deinit();
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

    var context = Context.init(test_allocator);
    defer context.deinit();
    try context.write_function_def(list.writer(), &Ast{ .body = "hello_world", .ast_type = AstType.function_def, .children = children });
    try std.testing.expect(std.mem.eql(u8, list.items, "hello_world() ->\n    hello(),\n    world().\n\n"));
}

test "write function def matching" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    var signature_children = std.ArrayList(*const Ast).init(test_allocator);
    defer signature_children.deinit();

    try signature_children.append(&Ast{ .body = "1", .ast_type = AstType.int, .children = null });

    try children.append(&Ast{ .body = "", .ast_type = AstType.function_signature, .children = signature_children });
    try children.append(&Ast{ .body = "1", .ast_type = AstType.int, .children = null });

    var signature_children2 = std.ArrayList(*const Ast).init(test_allocator);
    defer signature_children2.deinit();

    try signature_children2.append(&Ast{ .body = "2", .ast_type = AstType.int, .children = null });

    try children.append(&Ast{ .body = "", .ast_type = AstType.function_signature, .children = signature_children2 });
    try children.append(&Ast{ .body = "2", .ast_type = AstType.int, .children = null });

    var context = Context.init(test_allocator);
    defer context.deinit();
    try context.write_function_def(list.writer(), &Ast{ .body = "hello", .ast_type = AstType.function_def, .children = children });
    try std.testing.expect(std.mem.eql(u8, list.items, "hello(1) ->\n    1;\nhello(2) ->\n    2.\n\n"));
}

test "write anonymous function" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "", .ast_type = AstType.function_signature, .children = null });
    try children.append(&Ast{ .body = "hello", .ast_type = AstType.function_call, .children = null });
    try children.append(&Ast{ .body = "world", .ast_type = AstType.function_call, .children = null });

    var context = Context.init(test_allocator);
    defer context.deinit();
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

    var context = Context.init(test_allocator);
    defer context.deinit();
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

    var context = Context.init(test_allocator);
    defer context.deinit();
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

    var context = Context.init(test_allocator);
    defer context.deinit();
    try context.write_case_clause(list.writer(), &Ast{ .body = "", .ast_type = AstType.case_clause, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "X ->\n    Y = X + 2,\n    Y"));

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
    try std.testing.expect(std.mem.eql(u8, list.items, "X when is_integer(X) ->\n    X"));
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

    var context = Context.init(test_allocator);
    defer context.deinit();
    try context.write_case(list.writer(), &Ast{ .body = "", .ast_type = AstType.case, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "case X of\n    true ->\n        ok;\n    false ->\n        error\nend"));
}
