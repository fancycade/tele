const std = @import("std");
const ast = @import("tele_ast.zig");
const Ast = ast.Ast;
const AstType = ast.AstType;
const test_allocator = std.testing.allocator;
const util = @import("util.zig");

const CodegenError = error{WritingFailure};

pub const Context = struct {
    const Self = @This();

    padding_stack: std.ArrayList(usize),

    pub fn init(allocator: std.mem.Allocator) Context {
        return Context{ .padding_stack = std.ArrayList(usize).init(allocator) };
    }

    pub fn deinit(self: *Self) void {
        self.padding_stack.deinit();
    }

    pub fn writeValue(self: *Self, w: anytype, a: *const Ast) !void {
        try self.writePadding(w);
        _ = try w.write(a.body);
    }

    fn writeTuple(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.tuple) {
            return CodegenError.WritingFailure;
        }
        try self.writePadding(w);

        _ = try w.write("#(");
        var i: usize = 0;
        try self.pushPadding(0);
        for (a.children.?.items) |c| {
            try self.writeAst(w, c);

            if (i + 1 < a.children.?.items.len) {
                _ = try w.write(", ");
            }

            i = i + 1;
        }
        try self.popPadding();
        _ = try w.write(")");
    }

    pub fn writeList(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.list) {
            return CodegenError.WritingFailure;
        }
        try self.writePadding(w);

        _ = try w.write("[");
        var i: usize = 0;
        try self.pushPadding(0);
        for (a.children.?.items) |c| {
            try self.writeAst(w, c);

            if (i + 1 < a.children.?.items.len) {
                _ = try w.write(", ");
            }
            i += 1;
        }
        try self.popPadding();
        _ = try w.write("]");
    }

    // Children list consist of alternating key value pairs [k1, v1, k2, v2...]

    pub fn writeMap(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.map) {
            return CodegenError.WritingFailure;
        }

        if (a.*.children != null) {
            if (a.*.children.?.items.len % 2 != 0) {
                return CodegenError.WritingFailure;
            }
        }

        try self.writePadding(w);
        _ = try w.write("{");
        if (!std.mem.eql(u8, a.*.body, "")) {
            _ = try w.write(a.*.body);
            _ = try w.write(" | ");
        }

        var loop = true;
        var i: usize = 0;
        try self.pushPadding(0);
        if (a.*.children != null and a.*.children.?.items.len > 0) {
            while (loop) {
                try self.writeAst(w, a.children.?.items[i]);

                _ = try w.write(": ");
                try self.writeAst(w, a.children.?.items[i + 1]);

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
        try self.popPadding();

        _ = try w.write("}");
    }

    pub fn writeRecord(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.record) {
            return CodegenError.WritingFailure;
        }
        try self.writePadding(w);

        if (util.containsHash(a.body)) {
            _ = try w.write(a.body);
        } else {
            _ = try w.write("#");
            _ = try w.write(a.body);
        }
        _ = try w.write("(");

        try self.pushPadding(0);

        if (a.children != null) {
            var i: usize = 0;
            for (a.children.?.items) |c| {
                try self.writeAst(w, c);

                if (i + 1 < a.children.?.items.len) {
                    _ = try w.write(", ");
                }
                i += 1;
            }
        }

        try self.popPadding();
        _ = try w.write(")");
    }

    pub fn writeFunVal(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.fun_val) {
            return CodegenError.WritingFailure;
        }
        try self.writePadding(w);
        _ = try w.write("fun ");
        _ = try w.write(a.body);
    }

    pub fn writeOp(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.op) {
            return CodegenError.WritingFailure;
        }
        try self.writePadding(w);
        try self.pushPadding(0);

        if (a.children.?.items.len == 2) {
            try self.writeAst(w, a.children.?.items[0]);

            _ = try w.write(" ");
            _ = try w.write(a.body);
            _ = try w.write(" ");
            try self.writeAst(w, a.children.?.items[1]);
        } else if (a.children.?.items.len == 1) {
            _ = try w.write(a.body);
            if (std.mem.eql(u8, "not", a.body)) {
                _ = try w.write(" ");
            }
            try self.writeAst(w, a.children.?.items[0]);
        } else {
            return CodegenError.WritingFailure;
        }
        try self.popPadding();
    }

    // Expect paren_exp to only have 1 child element
    pub fn writeParenExp(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.paren_exp) {
            return CodegenError.WritingFailure;
        }
        try self.writePadding(w);
        try self.pushPadding(0);
        _ = try w.write("(");
        if (a.*.children != null) {
            if (a.*.children.?.items.len != 1) {
                return CodegenError.WritingFailure;
            }
            try self.writeAst(w, a.children.?.items[0]);
        }
        _ = try w.write(")");
        try self.popPadding();
    }

    pub fn writeFunctionCall(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.function_call) {
            return CodegenError.WritingFailure;
        }
        try self.writePadding(w);

        _ = try w.write(a.body);
        _ = try w.write("(");

        try self.pushPadding(0);

        if (a.children != null) {
            var i: usize = 0;
            for (a.children.?.items) |c| {
                try self.writeAst(w, c);

                if (i + 1 != a.children.?.items.len) {
                    _ = try w.write(", ");
                }

                i += 1;
            }
        }

        try self.popPadding();

        _ = try w.write(")");
    }

    pub fn writeImportDef(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.import_def) {
            return CodegenError.WritingFailure;
        }
        try self.writePadding(w);
        _ = try w.write("import ");
        try self.writeFunctionCall(w, &Ast{ .body = a.*.body, .children = a.*.children, .ast_type = AstType.function_call, .line = a.*.line, .col = a.*.col });
    }

    pub fn writeAttribute(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.attribute) {
            return CodegenError.WritingFailure;
        }
        try self.writePadding(w);
        try self.writeFunctionCall(w, &Ast{ .body = a.*.body, .children = a.*.children, .ast_type = AstType.function_call, .line = a.*.line, .col = a.*.col });
    }

    pub fn writeCustomAttribute(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.custom_attribute) {
            return CodegenError.WritingFailure;
        }
        try self.writePadding(w);
        _ = try w.write("attr ");
        try self.writeFunctionCall(w, &Ast{ .body = a.*.body, .children = a.*.children, .ast_type = AstType.function_call, .line = a.*.line, .col = a.*.col });
    }

    pub fn writeGuardClause(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.guard_clause) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children == null) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children.?.items.len != 1) {
            return CodegenError.WritingFailure;
        }
        _ = try w.write("when ");
        try self.writeAst(w, a.children.?.items[0]);
    }

    pub fn writeFunctionSignature(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.function_signature) {
            return CodegenError.WritingFailure;
        }
        try self.writePadding(w);

        if (a.children == null) {
            _ = try w.write("()");
            return;
        }

        _ = try w.write("(");

        // Look for guard clause
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

            try self.writeAst(w, a.children.?.items[i]);

            if (i + 1 < len) {
                _ = try w.write(", ");
            }

            i += 1;
        }

        _ = try w.write(")");

        if (guard) {
            _ = try w.write(" ");

            try self.writeGuardClause(w, a.children.?.items[len]);
        }
    }

    pub fn writeAnonymousFunction(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.anonymous_function) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children == null) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children.?.items.len < 2) {
            return CodegenError.WritingFailure;
        }
        try self.writePadding(w);
        try self.pushPadding(0);
        _ = try w.write("fun ");
        try self.writeFunctionSignature(w, a.children.?.items[0]);
        _ = try w.write(":\n");
        try self.popPadding();

        if (a.children.?.items.len > 1) {
            var i: usize = 1;

            try self.pushPadding(self.currentPadding() + 2);
            while (true) {
                if (i >= a.children.?.items.len) {
                    break;
                }

                try self.writeAst(w, a.children.?.items[i]);
                _ = try w.write("\n");

                i = i + 1;
            }
            try self.popPadding();
        }
    }

    pub fn writeCaseClause(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.case_clause) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children == null) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children.?.items.len < 2) {
            return CodegenError.WritingFailure;
        }

        try self.writeAst(w, a.children.?.items[0]);

        var i: usize = 1;

        if (a.children.?.items[1].ast_type == AstType.guard_clause) {
            _ = try w.write(" ");
            try self.pushPadding(0);
            try self.writeGuardClause(w, a.children.?.items[1]);
            try self.popPadding();

            i = i + 1;
        }

        _ = try w.write(":\n");

        try self.pushPadding(self.currentPadding() + 2);

        var loop = true;
        while (loop) {
            try self.writeAst(w, a.children.?.items[i]);

            const len = a.children.?.items.len;

            if (i + 1 != len) {
                _ = try w.write("\n");
            }

            i += 1;

            if (i >= len) {
                loop = false;
            }
        }
        try self.popPadding();
    }

    pub fn writeCase(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.case) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children == null) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children.?.items.len < 2) {
            return CodegenError.WritingFailure;
        }

        try self.writePadding(w);
        _ = try w.write("case ");
        try self.pushPadding(0);
        try self.writeAst(w, a.children.?.items[0]);
        _ = try w.write(":\n");
        try self.popPadding();

        var i: usize = 1;
        var loop = true;
        try self.pushPadding(self.currentPadding() + 2);
        while (loop) {
            try self.writeAst(w, a.children.?.items[i]);
            const len = a.children.?.items.len;

            i += 1;
            if (i >= len) {
                loop = false;
            } else {
                _ = try w.write("\n");
            }
        }
        try self.popPadding();
    }

    pub fn writeReceive(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.receive_exp) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children == null) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children.?.items.len < 1) {
            return CodegenError.WritingFailure;
        }
        try self.writePadding(w);
        _ = try w.write("receive:\n");

        var i: usize = 0;
        var loop = true;
        try self.pushPadding(self.currentPadding() + 2);
        const len = a.children.?.items.len;
        while (loop) {
            try self.writeAst(w, a.children.?.items[i]);
            i = i + 1;
            if (i >= len) {
                loop = false;
            } else {
                _ = try w.write("\n");
            }
        }
        try self.popPadding();
    }

    pub fn writeTryCatch(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.try_catch) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children == null) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children.?.items.len != 2) {
            return CodegenError.WritingFailure;
        }
        try self.writeTryExp(w, a.*.children.?.items[0]);
        try self.writeCatchExp(w, a.*.children.?.items[1]);
    }

    pub fn writeTryExp(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.try_exp) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children == null) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children.?.items.len < 2) {
            return CodegenError.WritingFailure;
        }
        try self.writePadding(w);
        _ = try w.write("try ");
        try self.pushPadding(0);
        try self.writeAst(w, a.children.?.items[0]);
        _ = try w.write(":\n");
        try self.popPadding();

        var i: usize = 1;
        var loop = true;
        try self.pushPadding(self.currentPadding() + 2);
        while (loop) {
            try self.writeAst(w, a.children.?.items[i]);
            const len = a.children.?.items.len;

            i += 1;
            if (i >= len) {
                loop = false;
            } else {
                _ = try w.write("\n");
            }
        }
        try self.popPadding();
        _ = try w.write("\n");
    }

    pub fn writeCatchExp(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.catch_exp) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children == null) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children.?.items.len < 1) {
            return CodegenError.WritingFailure;
        }
        try self.writePadding(w);
        _ = try w.write("catch\n");

        try self.pushPadding(self.currentPadding() + 2);
        for (a.children.?.items) |c| {
            try self.writeAst(w, c);
        }
        try self.popPadding();
    }

    pub fn writeFunctionDef(self: *Self, w: anytype, a: *const Ast, private: bool) !void {
        if (a.*.ast_type != AstType.function_def) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children == null) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children.?.items.len < 2) {
            return CodegenError.WritingFailure;
        }
        try self.writePadding(w);
        if (private) {
            _ = try w.write("funp ");
        } else {
            _ = try w.write("fun ");
        }

        var i: usize = 0;
        while (true) {
            if (i >= a.children.?.items.len) {
                break;
            }

            if (i == 0) {
                _ = try w.write(a.body);

                try self.writeFunctionSignature(w, a.children.?.items[i]);
            } else {
                try self.pushPadding(self.currentPadding() + 2);
                try self.writePadding(w);

                try self.popPadding();
                try self.writeFunctionSignature(w, a.children.?.items[i]);
            }
            _ = try w.write(":");

            i = i + 1;

            try self.pushPadding(self.currentPadding() + 2);
            while (true) {
                if (i >= a.children.?.items.len or a.children.?.items[i].ast_type == AstType.function_signature) {
                    break;
                }

                _ = try w.write("\n");
                try self.writeAst(w, a.children.?.items[i]);

                i = i + 1;
            }
            try self.popPadding();

            _ = try w.write("\n");
        }
        _ = try w.write("\n");
    }

    pub fn writeMacroDef(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.macro_def) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children == null) {
            return CodegenError.WritingFailure;
        }
        _ = try w.write("define ");

        _ = try w.write(a.body);

        if (a.children.?.items.len == 1) {
            _ = try w.write(":");
            try self.writeAst(w, a.children.?.items[0]);
        } else {
            try self.writeFunctionSignature(w, a.children.?.items[0]);
            _ = try w.write(":");
            try self.pushPadding(self.currentPadding() + 2);
            for (a.children.?.items[1..a.children.?.items.len]) |c| {
                _ = try w.write("\n");
                try self.writeAst(w, c);
            }
            try self.popPadding();
        }
    }

    pub fn writeSpecDef(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.spec_def) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children == null) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children.?.items.len < 2) {
            return CodegenError.WritingFailure;
        }
        _ = try w.write("spec ");

        _ = try w.write(a.body);
        try self.writeFunctionSignature(w, a.children.?.items[0]);
        _ = try w.write(": ");

        try self.writeAst(w, a.children.?.items[1]);
        _ = try w.write("\n");
    }

    pub fn writeCallbackDef(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.callback_def) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children == null) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children.?.items.len < 2) {
            return CodegenError.WritingFailure;
        }
        _ = try w.write("callback ");

        _ = try w.write(a.body);
        try self.writeFunctionSignature(w, a.children.?.items[0]);
        _ = try w.write(": ");

        try self.writeAst(w, a.children.?.items[1]);
        _ = try w.write("\n");
    }

    pub fn writeOpaqueTypeDef(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.opaque_type_def) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children == null) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children.?.items.len != 2) {
            return CodegenError.WritingFailure;
        }
        _ = try w.write("opaque ");
        try self.writeAst(w, a.children.?.items[0]);
        _ = try w.write(":\n");

        try self.pushPadding(self.currentPadding() + 2);
        try self.writeAst(w, a.children.?.items[1]);

        _ = try w.write("\n\n");
    }

    pub fn writeTypeDef(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.type_def) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children == null) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children.?.items.len != 2) {
            return CodegenError.WritingFailure;
        }
        _ = try w.write("type ");
        try self.writeAst(w, a.children.?.items[0]);
        _ = try w.write(":\n");

        try self.pushPadding(self.currentPadding() + 2);
        try self.writeAst(w, a.children.?.items[1]);

        _ = try w.write("\n\n");
    }

    pub fn writeRecordDef(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.record_def) {
            return CodegenError.WritingFailure;
        }

        _ = try w.write("record ");
        _ = try w.write(a.*.body);
        _ = try w.write(":\n");

        try self.pushPadding(self.currentPadding() + 2);
        _ = try self.writePadding(w);
        _ = try w.write("#(");
        try self.pushPadding(0);

        var i: usize = 0;
        for (a.children.?.items) |c| {
            try self.writeAst(w, c);

            if (i + 1 < a.children.?.items.len) {
                _ = try w.write(", ");
            }
            i += 1;
        }

        try self.popPadding();
        try self.popPadding();

        _ = try w.write(")\n\n");
    }

    pub fn writeRecordField(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.record_field) {
            return CodegenError.WritingFailure;
        }
        _ = try w.write(a.*.body);

        if (a.*.children != null) {
            if (a.*.children.?.items.len == 1) {
                const a2 = a.*.children.?.items[0];
                if (a2.ast_type == AstType.record_field_value) {
                    try self.writeRecordFieldValue(w, a2);
                } else if (a2.ast_type == AstType.record_field_value) {
                    try self.writeRecordFieldType(w, a2);
                } else {
                    return CodegenError.WritingFailure;
                }
            } else if (a.*.children.?.items.len == 2) {
                const a2 = a.*.children.?.items[0];
                if (a2.ast_type == AstType.record_field_value) {
                    try self.writeRecordFieldValue(w, a2);
                } else {
                    return CodegenError.WritingFailure;
                }

                const a3 = a.*.children.?.items[1];
                if (a3.ast_type == AstType.record_field_type) {
                    try self.writeRecordFieldType(w, a3);
                } else {
                    return CodegenError.WritingFailure;
                }
            } else {
                return CodegenError.WritingFailure;
            }
        }
    }

    pub fn writeRecordFieldValue(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.record_field_value) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children == null) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children.?.items.len != 1) {
            return CodegenError.WritingFailure;
        }

        _ = try w.write("=");
        try self.writeAst(w, a.*.children.?.items[0]);
    }

    pub fn writeRecordFieldType(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.record_field_type) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children == null) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children.?.items.len != 1) {
            return CodegenError.WritingFailure;
        }
        _ = try w.write(": ");

        try self.writeAst(w, a.*.children.?.items[0]);
    }

    pub fn pushPadding(self: *Self, padding: usize) !void {
        try self.*.padding_stack.append(padding);
    }

    pub fn popPadding(self: *Self) !void {
        _ = self.*.padding_stack.pop();
    }

    pub fn currentPadding(self: *Self) usize {
        if (self.*.padding_stack.items.len == 0) {
            return 0;
        } else {
            return self.*.padding_stack.items[self.*.padding_stack.items.len - 1];
        }
    }

    pub fn writePadding(self: *Self, w: anytype) !void {
        var ctr: usize = 0;
        if (self.*.padding_stack.items.len > 0) {
            while (ctr < self.currentPadding()) {
                _ = try w.write(" ");
                ctr += 1;
            }
        }
    }

    pub fn writeAst(self: *Self, w: anytype, a: *const Ast) error{WritingFailure}!void {
        switch (a.ast_type) {
            .int => {
                self.writeValue(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .float => {
                self.writeValue(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .binary => {
                self.writeValue(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .atom => {
                self.writeValue(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .variable => {
                self.writeValue(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .tuple => {
                self.writeTuple(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .list => {
                self.writeList(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .map => {
                self.writeMap(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .record => {
                self.writeRecord(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .fun_val => {
                self.writeFunVal(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .op => {
                self.writeOp(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .paren_exp => {
                self.writeParenExp(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .function_call => {
                self.writeFunctionCall(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .attribute => {
                self.writeAttribute(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .custom_attribute => {
                self.writeCustomAttribute(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .guard_clause => {
                self.writeGuardClause(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .function_signature => {
                self.writeFunctionSignature(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .anonymous_function => {
                self.writeAnonymousFunction(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .function_def => {
                self.writeFunctionDef(w, a, false) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .function_defp => {
                self.writeFunctionDef(w, a, true) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .macro_def => {
                self.writeMacroDef(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .opaque_type_def => {
                self.writeOpaqueTypeDef(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .type_def => {
                self.writeTypeDef(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .record_def => {
                self.writeRecordDef(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .record_field => {
                self.writeRecordField(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .record_field_value => {
                self.writeRecordFieldValue(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .record_field_type => {
                self.writeRecordFieldType(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .spec_def => {
                self.writeSpecDef(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .callback_def => {
                self.writeCallbackDef(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .import_def => {
                self.writeImportDef(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .case_clause => {
                self.writeCaseClause(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .case => {
                self.writeCase(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .try_catch => {
                self.writeTryCatch(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .try_exp => {
                self.writeTryExp(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .catch_exp => {
                self.writeCatchExp(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .receive_exp => {
                self.writeReceive(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .import_element => {
                self.writeValue(w, a) catch {
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

    const a = Ast{ .body = "1", .ast_type = AstType.int, .children = null, .col = 0, .line = 0 };
    try context.writeValue(list.writer(), &a);
    try std.testing.expect(std.mem.eql(u8, list.items, "1"));
    list.clearAndFree();

    try context.writeValue(list.writer(), &Ast{ .body = "100_000", .ast_type = AstType.int, .children = null, .col = 0, .line = 0 });
    try std.testing.expect(std.mem.eql(u8, list.items, "100_000"));
    list.clearAndFree();

    try context.writeValue(list.writer(), &Ast{ .body = "1.0", .ast_type = AstType.int, .children = null, .col = 0, .line = 0 });
    try std.testing.expect(std.mem.eql(u8, list.items, "1.0"));
    list.clearAndFree();

    // TODO: Scientific notation

    try context.writeValue(list.writer(), &Ast{ .body = "'foo", .ast_type = AstType.atom, .children = null, .col = 0, .line = 0 });
    try std.testing.expect(std.mem.eql(u8, list.items, "'foo"));
    // TODO: Atom with double quotes
}

test "write tuple" {
    var context = Context.init(test_allocator);
    defer context.deinit();

    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*Ast).init(test_allocator);
    defer children.deinit();

    var t = Ast{ .body = "1", .ast_type = AstType.int, .children = null, .col = 0, .line = 0 };
    try children.append(&t);
    var t2 = Ast{ .body = "2", .ast_type = AstType.int, .children = null, .col = 0, .line = 0 };
    try children.append(&t2);

    try context.writeTuple(list.writer(), &Ast{ .body = "", .ast_type = AstType.tuple, .children = children, .col = 0, .line = 0 });

    try std.testing.expect(std.mem.eql(u8, list.items, "#(1, 2)"));
}

test "write list" {
    var context = Context.init(test_allocator);
    defer context.deinit();

    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*Ast).init(test_allocator);
    defer children.deinit();

    var t = Ast{ .body = "1", .ast_type = AstType.int, .children = null, .col = 0, .line = 0 };
    try children.append(&t);
    var t2 = Ast{ .body = "'foo", .ast_type = AstType.atom, .children = null, .col = 0, .line = 0 };
    try children.append(&t2);
    var t3 = Ast{ .body = "42.42", .ast_type = AstType.float, .children = null, .col = 0, .line = 0 };
    try children.append(&t3);
    var t4 = Ast{ .body = "\"foobar\"", .ast_type = AstType.binary, .children = null, .col = 0, .line = 0 };
    try children.append(&t4);

    try context.writeList(list.writer(), &Ast{ .body = "", .children = children, .ast_type = AstType.list, .col = 0, .line = 0 });

    try std.testing.expect(std.mem.eql(u8, list.items, "[1, 'foo, 42.42, \"foobar\"]"));
}

test "write map" {
    var context = Context.init(test_allocator);
    defer context.deinit();

    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*Ast).init(test_allocator);
    defer children.deinit();

    var t = Ast{ .body = "'foo", .ast_type = AstType.atom, .children = null, .col = 0, .line = 0 };
    try children.append(&t);
    var t2 = Ast{ .body = "'bar", .ast_type = AstType.atom, .children = null, .col = 0, .line = 0 };
    try children.append(&t2);
    var t3 = Ast{ .body = "'foo2", .ast_type = AstType.atom, .children = null, .col = 0, .line = 0 };
    try children.append(&t3);
    var t4 = Ast{ .body = "'baz", .ast_type = AstType.atom, .children = null, .col = 0, .line = 0 };
    try children.append(&t4);

    try context.writeMap(list.writer(), &Ast{ .body = "", .ast_type = AstType.map, .children = children, .col = 0, .line = 0 });

    try std.testing.expect(std.mem.eql(u8, list.items, "{'foo: 'bar, 'foo2: 'baz}"));
}

test "write record" {
    var context = Context.init(test_allocator);
    defer context.deinit();

    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*Ast).init(test_allocator);
    defer children.deinit();

    var field1_children = std.ArrayList(*Ast).init(test_allocator);
    defer field1_children.deinit();
    var fv1_children = std.ArrayList(*Ast).init(test_allocator);
    defer fv1_children.deinit();
    var fv1 = Ast{ .body = "\"Joe\"", .ast_type = AstType.binary, .children = null, .col = 0, .line = 0 };
    try fv1_children.append(&fv1);
    var fvv1 = Ast{ .body = "", .ast_type = AstType.record_field_value, .children = fv1_children, .col = 0, .line = 0 };
    try field1_children.append(&fvv1);

    var field2_children = std.ArrayList(*Ast).init(test_allocator);
    defer field2_children.deinit();
    var fv2_children = std.ArrayList(*Ast).init(test_allocator);
    defer fv2_children.deinit();
    var fv2 = Ast{ .body = "68", .ast_type = AstType.binary, .children = null, .col = 0, .line = 0 };
    try fv2_children.append(&fv2);
    var fvv2 = Ast{ .body = "", .ast_type = AstType.record_field_value, .children = fv2_children, .col = 0, .line = 0 };
    try field2_children.append(&fvv2);

    var t = Ast{ .body = "name", .ast_type = AstType.record_field, .children = field1_children, .col = 0, .line = 0 };
    try children.append(&t);
    var t2 = Ast{ .body = "age", .ast_type = AstType.record_field, .children = field2_children, .col = 0, .line = 0 };
    try children.append(&t2);

    try context.writeRecord(list.writer(), &Ast{ .body = "person", .ast_type = AstType.record, .children = children, .col = 0, .line = 0 });

    try std.testing.expect(std.mem.eql(u8, list.items, "#person(name=\"Joe\", age=68)"));
}

test "write op" {
    var context = Context.init(test_allocator);
    defer context.deinit();

    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*Ast).init(test_allocator);
    defer children.deinit();

    var a = Ast{ .body = "1", .ast_type = AstType.int, .children = null, .col = 0, .line = 0 };
    try children.append(&a);
    var a2 = Ast{ .body = "2", .ast_type = AstType.int, .children = null, .col = 0, .line = 0 };
    try children.append(&a2);

    try context.writeOp(list.writer(), &Ast{ .body = "+", .children = children, .ast_type = AstType.op, .col = 0, .line = 0 });

    try std.testing.expect(std.mem.eql(u8, list.items, "1 + 2"));
}

test "write function call" {
    var context = Context.init(test_allocator);
    defer context.deinit();

    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*Ast).init(test_allocator);
    defer children.deinit();

    var a = Ast{ .body = "1", .ast_type = AstType.int, .children = null, .col = 0, .line = 0 };
    try children.append(&a);
    var a2 = Ast{ .body = "2", .ast_type = AstType.int, .children = null, .col = 0, .line = 0 };
    try children.append(&a2);

    try context.writeFunctionCall(list.writer(), &Ast{ .body = "erlang.add", .ast_type = AstType.function_call, .children = children, .col = 0, .line = 0 });

    try std.testing.expect(std.mem.eql(u8, list.items, "erlang.add(1, 2)"));

    // TODO: Test no arguments
}

test "write attribute" {
    var context = Context.init(test_allocator);
    defer context.deinit();

    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*Ast).init(test_allocator);
    defer children.deinit();

    var c = Ast{ .body = "\"hello world\"", .ast_type = AstType.binary, .children = null, .col = 0, .line = 0 };
    try children.append(&c);

    var a = Ast{ .body = "doc", .ast_type = AstType.attribute, .children = children, .col = 0, .line = 0 };

    try context.writeAttribute(list.writer(), &a);

    try std.testing.expect(std.mem.eql(u8, list.items, "doc(\"hello world\")"));
}

test "write guard clause" {
    var context = Context.init(test_allocator);
    defer context.deinit();

    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*Ast).init(test_allocator);
    defer children.deinit();

    var children2 = std.ArrayList(*Ast).init(test_allocator);
    defer children2.deinit();

    var a = Ast{ .body = "x", .ast_type = AstType.variable, .children = null, .col = 0, .line = 0 };
    try children2.append(&a);
    var a2 = Ast{ .body = "is_number", .ast_type = AstType.function_call, .children = children2, .col = 0, .line = 0 };
    try children.append(&a2);

    try context.writeGuardClause(list.writer(), &Ast{ .body = "", .ast_type = AstType.guard_clause, .children = children, .col = 0, .line = 0 });

    try std.testing.expect(std.mem.eql(u8, list.items, "when is_number(x)"));
}

test "write function signature" {
    var context = Context.init(test_allocator);
    defer context.deinit();

    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();
    var children = std.ArrayList(*Ast).init(test_allocator);
    defer children.deinit();

    var a = Ast{ .body = "a", .ast_type = AstType.variable, .children = null, .col = 0, .line = 0 };
    try children.append(&a);
    var a2 = Ast{ .body = "b", .ast_type = AstType.variable, .children = null, .col = 0, .line = 0 };
    try children.append(&a2);

    try context.writeFunctionSignature(list.writer(), &Ast{ .body = "", .ast_type = AstType.function_signature, .children = children, .col = 0, .line = 0 });

    const expected = "(a, b)";
    try std.testing.expect(std.mem.eql(u8, list.items, expected));

    list.clearAndFree();

    var children2 = std.ArrayList(*Ast).init(test_allocator);
    defer children2.deinit();

    var a3 = Ast{ .body = "a", .ast_type = AstType.variable, .children = null, .col = 0, .line = 0 };
    try children2.append(&a3);
    var a4 = Ast{ .body = "b", .ast_type = AstType.variable, .children = null, .col = 0, .line = 0 };
    try children2.append(&a4);

    var guard_children = std.ArrayList(*Ast).init(test_allocator);
    defer guard_children.deinit();

    var function_children = std.ArrayList(*Ast).init(test_allocator);
    defer function_children.deinit();

    var a5 = Ast{ .body = "a", .ast_type = AstType.variable, .children = null, .col = 0, .line = 0 };
    try function_children.append(&a5);

    var a6 = Ast{ .body = "is_integer", .ast_type = AstType.function_call, .children = function_children, .col = 0, .line = 0 };
    try guard_children.append(&a6);

    var a7 = Ast{ .body = "", .ast_type = AstType.guard_clause, .children = guard_children, .col = 0, .line = 0 };
    try children2.append(&a7);

    try context.writeFunctionSignature(list.writer(), &Ast{ .body = "", .ast_type = AstType.function_signature, .children = children2, .col = 0, .line = 0 });

    try std.testing.expect(std.mem.eql(u8, list.items, "(a, b) when is_integer(a)"));
}

test "write anonymous function" {
    var context = Context.init(test_allocator);
    defer context.deinit();

    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*Ast).init(test_allocator);
    defer children.deinit();

    var a = Ast{ .body = "", .ast_type = AstType.function_signature, .children = null, .col = 0, .line = 0 };
    try children.append(&a);
    var a2 = Ast{ .body = "hello", .ast_type = AstType.function_call, .children = null, .col = 0, .line = 0 };
    try children.append(&a2);
    var a3 = Ast{ .body = "world", .ast_type = AstType.function_call, .children = null, .col = 0, .line = 0 };
    try children.append(&a3);

    try context.writeAnonymousFunction(list.writer(), &Ast{ .body = "", .ast_type = AstType.anonymous_function, .children = children, .col = 0, .line = 0 });
    try std.testing.expect(std.mem.eql(u8, list.items, "fun ():\n  hello()\n  world()\n"));
}

test "write function def" {
    var context = Context.init(test_allocator);
    defer context.deinit();

    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*Ast).init(test_allocator);
    defer children.deinit();

    var a = Ast{ .body = "", .ast_type = AstType.function_signature, .children = null, .col = 0, .line = 0 };
    try children.append(&a);
    var a2 = Ast{ .body = "hello", .ast_type = AstType.function_call, .children = null, .col = 0, .line = 0 };
    try children.append(&a2);
    var a3 = Ast{ .body = "world", .ast_type = AstType.function_call, .children = null, .col = 0, .line = 0 };
    try children.append(&a3);

    try context.writeFunctionDef(list.writer(), &Ast{ .body = "hello_world", .ast_type = AstType.function_def, .children = children, .col = 0, .line = 0 }, false);
    try std.testing.expect(std.mem.eql(u8, list.items, "fun hello_world():\n  hello()\n  world()\n\n"));
}

test "write function def matching" {
    var context = Context.init(test_allocator);
    defer context.deinit();

    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*Ast).init(test_allocator);
    defer children.deinit();

    var signature_children = std.ArrayList(*Ast).init(test_allocator);
    defer signature_children.deinit();

    var a = Ast{ .body = "1", .ast_type = AstType.int, .children = null, .col = 0, .line = 0 };
    try signature_children.append(&a);

    var a2 = Ast{ .body = "", .ast_type = AstType.function_signature, .children = signature_children, .col = 0, .line = 0 };
    try children.append(&a2);
    var a3 = Ast{ .body = "1", .ast_type = AstType.int, .children = null, .col = 0, .line = 0 };
    try children.append(&a3);

    var signature_children2 = std.ArrayList(*Ast).init(test_allocator);
    defer signature_children2.deinit();

    var a4 = Ast{ .body = "2", .ast_type = AstType.int, .children = null, .col = 0, .line = 0 };
    try signature_children2.append(&a4);

    var a5 = Ast{ .body = "", .ast_type = AstType.function_signature, .children = signature_children2, .col = 0, .line = 0 };
    try children.append(&a5);
    var a6 = Ast{ .body = "2", .ast_type = AstType.int, .children = null, .col = 0, .line = 0 };
    try children.append(&a6);

    try context.writeFunctionDef(list.writer(), &Ast{ .body = "hello", .ast_type = AstType.function_def, .children = children, .col = 0, .line = 0 }, false);
    try std.testing.expect(std.mem.eql(u8, list.items, "fun hello(1):\n  1\n  (2):\n  2\n\n"));
}

test "write case clause" {
    var context = Context.init(test_allocator);
    defer context.deinit();

    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*Ast).init(test_allocator);
    defer children.deinit();

    var a = Ast{ .body = "x", .ast_type = AstType.variable, .children = null, .col = 0, .line = 0 };
    try children.append(&a);

    var op1_children = std.ArrayList(*Ast).init(test_allocator);
    defer op1_children.deinit();

    var a2 = Ast{ .body = "x", .ast_type = AstType.variable, .children = null, .col = 0, .line = 0 };
    try op1_children.append(&a2);
    var a3 = Ast{ .body = "2", .ast_type = AstType.int, .children = null, .col = 0, .line = 0 };
    try op1_children.append(&a3);

    var op2_children = std.ArrayList(*Ast).init(test_allocator);
    defer op2_children.deinit();

    var a4 = Ast{ .body = "y", .ast_type = AstType.variable, .children = null, .col = 0, .line = 0 };
    try op2_children.append(&a4);
    var a5 = Ast{ .body = "+", .ast_type = AstType.op, .children = op1_children, .col = 0, .line = 0 };
    try op2_children.append(&a5);

    var a6 = Ast{ .body = "=", .ast_type = AstType.op, .children = op2_children, .col = 0, .line = 0 };
    try children.append(&a6);
    var a7 = Ast{ .body = "y", .ast_type = AstType.variable, .children = null, .col = 0, .line = 0 };
    try children.append(&a7);

    try context.writeCaseClause(list.writer(), &Ast{ .body = "", .ast_type = AstType.case_clause, .children = children, .col = 0, .line = 0 });

    try std.testing.expect(std.mem.eql(u8, list.items, "x:\n  y = x + 2\n  y"));

    list.clearAndFree();

    var children2 = std.ArrayList(*Ast).init(test_allocator);
    defer children2.deinit();

    var a8 = Ast{ .body = "x", .ast_type = AstType.variable, .children = null, .col = 0, .line = 0 };
    try children2.append(&a8);

    var guard_children = std.ArrayList(*Ast).init(test_allocator);
    defer guard_children.deinit();

    var function_children = std.ArrayList(*Ast).init(test_allocator);
    defer function_children.deinit();

    var a9 = Ast{ .body = "x", .ast_type = AstType.variable, .children = null, .col = 0, .line = 0 };
    try function_children.append(&a9);

    var a10 = Ast{ .body = "is_integer", .ast_type = AstType.function_call, .children = function_children, .col = 0, .line = 0 };
    try guard_children.append(&a10);

    var a11 = Ast{ .body = "", .ast_type = AstType.guard_clause, .children = guard_children, .col = 0, .line = 0 };
    try children2.append(&a11);

    var a12 = Ast{ .body = "x", .ast_type = AstType.variable, .children = null, .col = 0, .line = 0 };
    try children2.append(&a12);

    try context.writeCaseClause(list.writer(), &Ast{ .body = "", .ast_type = AstType.case_clause, .children = children2, .col = 0, .line = 0 });
    try std.testing.expect(std.mem.eql(u8, list.items, "x when is_integer(x):\n  x"));
}

test "write case" {
    var context = Context.init(test_allocator);
    defer context.deinit();

    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var case_clause_children_1 = std.ArrayList(*Ast).init(test_allocator);
    defer case_clause_children_1.deinit();

    var a = Ast{ .body = "'true", .ast_type = AstType.atom, .children = null, .col = 0, .line = 0 };
    try case_clause_children_1.append(&a);
    var a2 = Ast{ .body = "'ok", .ast_type = AstType.atom, .children = null, .col = 0, .line = 0 };
    try case_clause_children_1.append(&a2);

    var case_clause_children_2 = std.ArrayList(*Ast).init(test_allocator);
    defer case_clause_children_2.deinit();

    var a3 = Ast{ .body = "'false", .ast_type = AstType.atom, .children = null, .col = 0, .line = 0 };
    try case_clause_children_2.append(&a3);
    var a4 = Ast{ .body = "'error", .ast_type = AstType.atom, .children = null, .col = 0, .line = 0 };
    try case_clause_children_2.append(&a4);

    var children = std.ArrayList(*Ast).init(test_allocator);
    defer children.deinit();

    var a5 = Ast{ .body = "x", .ast_type = AstType.variable, .children = null, .col = 0, .line = 0 };
    try children.append(&a5);

    var a6 = Ast{ .body = "", .ast_type = AstType.case_clause, .children = case_clause_children_1, .col = 0, .line = 0 };
    try children.append(&a6);
    var a7 = Ast{ .body = "", .ast_type = AstType.case_clause, .children = case_clause_children_2, .col = 0, .line = 0 };
    try children.append(&a7);

    try context.writeCase(list.writer(), &Ast{ .body = "", .ast_type = AstType.case, .children = children, .col = 0, .line = 0 });

    try std.testing.expect(std.mem.eql(u8, list.items, "case x:\n  'true:\n    'ok\n  'false:\n    'error"));
}
