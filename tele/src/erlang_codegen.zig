const std = @import("std");
const ast = @import("erlang_ast.zig");
const Ast = ast.Ast;
const AstType = ast.AstType;
const test_allocator = std.testing.allocator;
const util = @import("util.zig");
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

    pub fn writeValue(self: *Self, w: anytype, a: *const Ast) !void {
        try self.writePadding(w);
        _ = try w.write(a.body);
    }

    pub fn writeString(self: *Self, w: anytype, a: *const Ast, override: bool) !void {
        if (a.*.ast_type != AstType.string) {
            return CodegenError.WritingFailure;
        }

        try self.writePadding(w);

        if (!self.attribute_mode) {
            if (override) {
                _ = try w.write("<<");
            }
        }

        _ = try w.write(a.*.body);

        if (!self.attribute_mode) {
            if (override) {
                _ = try w.write("/utf8>>");
            }
        }
    }

    pub fn writeBinary(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.binary) {
            return CodegenError.WritingFailure;
        }

        try self.writePadding(w);

        _ = try w.write("<<");
        var i: usize = 0;
        try self.pushPadding(0);
        for (a.children.?.items) |c| {
            try self.writeBinaryElement(w, c);

            if (i + 1 < a.children.?.items.len) {
                _ = try w.write(", ");
            }

            i = i + 1;
        }
        try self.popPadding();
        _ = try w.write(">>");
    }

    pub fn writeBinaryElement(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.binary_element) {
            return CodegenError.WritingFailure;
        }

        if (a.*.children == null) {
            return CodegenError.WritingFailure;
        }

        if (a.*.children.?.items.len == 0 or a.*.children.?.items.len > 3) {
            return CodegenError.WritingFailure;
        }

        if (a.*.children.?.items[0].ast_type == AstType.string) {
            try self.writeString(w, a.*.children.?.items[0], false);
        } else {
            try self.writeAst(w, a.*.children.?.items[0], false);
        }

        if (a.*.children.?.items.len == 2) {
            if (a.*.children.?.items[1].ast_type == AstType.binary_element_size) {
                try self.writeBinaryElementSize(w, a.*.children.?.items[1]);
            } else if (a.*.children.?.items[1].ast_type == AstType.binary_element_type) {
                if (a.*.children.?.items.len > 2) {
                    return CodegenError.WritingFailure;
                }
                try self.writeBinaryElementType(w, a.*.children.?.items[1]);
            } else {
                return CodegenError.WritingFailure;
            }
        }

        if (a.*.children.?.items.len == 3) {
            try self.writeBinaryElementType(w, a.*.children.?.items[2]);
        }

        try self.writePadding(w);
    }

    pub fn writeBinaryElementSize(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.binary_element_size) {
            return CodegenError.WritingFailure;
        }

        if (a.*.children == null) {
            return CodegenError.WritingFailure;
        }

        if (a.*.children.?.items.len != 1) {
            return CodegenError.WritingFailure;
        }

        _ = try w.write(":");
        try self.writeAst(w, a.*.children.?.items[0], false);
    }

    pub fn writeBinaryElementType(self: *Self, w: anytype, a: *const Ast) !void {
        _ = self;
        if (a.*.ast_type != AstType.binary_element_type) {
            return CodegenError.WritingFailure;
        }

        _ = try w.write("/");
        _ = try w.write(a.*.body);
    }

    pub fn writeTuple(self: *Self, w: anytype, a: *const Ast, type_exp: bool) !void {
        if (a.*.ast_type != AstType.tuple) {
            return CodegenError.WritingFailure;
        }

        try self.writePadding(w);
        _ = try w.write("{");
        var i: usize = 0;
        try self.pushPadding(0);
        for (a.children.?.items) |c| {
            try self.writeAst(w, c, type_exp);

            if (i + 1 < a.children.?.items.len) {
                _ = try w.write(", ");
            }

            i += 1;
        }
        try self.popPadding();
        _ = try w.write("}");
    }

    pub fn writeList(self: *Self, w: anytype, a: *const Ast, type_exp: bool) !void {
        if (a.*.ast_type != AstType.list) {
            return CodegenError.WritingFailure;
        }

        try self.writePadding(w);
        _ = try w.write("[");
        var i: usize = 0;
        try self.pushPadding(0);
        for (a.children.?.items) |c| {
            try self.writeAst(w, c, type_exp);

            if (i + 1 < a.children.?.items.len) {
                _ = try w.write(", ");
            }
            i += 1;
        }
        try self.popPadding();
        _ = try w.write("]");
    }

    // Children list consist of alternating key value pairs [k1, v1, k2, v2...]

    fn writeMap(self: *Self, w: anytype, a: *const Ast, type_exp: bool) !void {
        if (a.*.ast_type != AstType.map) {
            return CodegenError.WritingFailure;
        }

        try self.writePadding(w);
        if (!std.mem.eql(u8, a.*.body, "")) {
            _ = try w.write(a.*.body);
        }
        _ = try w.write("#{");

        var loop = true;
        var i: usize = 0;
        try self.pushPadding(0);
        if (a.children != null and a.children.?.items.len > 0) {
            while (loop) {
                try self.writeAst(w, a.children.?.items[i], type_exp);

                if (self.matchMode()) {
                    _ = try w.write(" := ");
                } else {
                    _ = try w.write(" => ");
                }
                try self.writeAst(w, a.children.?.items[i + 1], type_exp);

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
        _ = try w.write("{");

        try self.pushPadding(0);

        if (a.children != null) {
            var i: usize = 0;
            for (a.children.?.items) |c| {
                try self.writeAst(w, c, false);

                if (i + 1 < a.children.?.items.len) {
                    _ = try w.write(", ");
                }
                i += 1;
            }
        }

        try self.popPadding();
        _ = try w.write("}");
    }

    pub fn writeFunVal(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.fun_val) {
            return CodegenError.WritingFailure;
        }
        try self.writePadding(w);
        _ = try w.write("fun ");
        _ = try w.write(a.body);
    }

    // Matching must be done on left side of operator
    pub fn writeOp(self: *Self, w: anytype, a: *const Ast, type_exp: bool) !void {
        if (a.*.ast_type != AstType.op) {
            return CodegenError.WritingFailure;
        }
        try self.writePadding(w);
        try self.pushPadding(0);

        if (a.children.?.items.len == 2) {
            self.pushMatch();
            try self.writeAst(w, a.children.?.items[0], type_exp);
            self.popMatch();

            _ = try w.write(" ");

            if (std.mem.eql(u8, a.body, "and")) {
                _ = try w.write("andalso");
            } else if (std.mem.eql(u8, a.body, "or")) {
                _ = try w.write("orelse");
            } else {
                _ = try w.write(a.body);
            }
            _ = try w.write(" ");
            // TODO: handle padding for when second argument is anonymous function
            try self.writeAst(w, a.children.?.items[1], type_exp);
        } else if (a.children.?.items.len == 1) {
            _ = try w.write(a.body);
            if (std.mem.eql(u8, "not", a.body)) {
                _ = try w.write(" ");
            }
            try self.writeAst(w, a.children.?.items[0], type_exp);
        } else {
            return CodegenError.WritingFailure;
        }

        try self.popPadding();
    }

    pub fn writeParenExp(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.paren_exp) {
            return CodegenError.WritingFailure;
        }
        try self.writePadding(w);
        try self.pushPadding(0);
        _ = try w.write("(");
        if (a.children != null) {
            if (a.children.?.items.len == 1) {
                try self.writeAst(w, a.children.?.items[0], false);
            } else {
                return CodegenError.WritingFailure;
            }
        }

        _ = try w.write(")");
        try self.popPadding();
    }

    pub fn writeFunctionCall(self: *Self, w: anytype, a: *const Ast, type_exp: bool) !void {
        if (a.*.ast_type != AstType.function_call) {
            return CodegenError.WritingFailure;
        }
        try self.writePadding(w);
        _ = try w.write(a.body);
        _ = try w.write("(");

        if (a.children != null) {
            try self.pushPadding(0);
            var i: usize = 0;
            for (a.children.?.items) |c| {
                try self.writeAst(w, c, type_exp);

                if (i + 1 != a.children.?.items.len) {
                    _ = try w.write(", ");
                }

                i += 1;
            }
            try self.popPadding();
        }

        _ = try w.write(")");
    }

    pub fn writeImportDef(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.import_def) {
            return CodegenError.WritingFailure;
        }
        try self.writePadding(w);
        _ = try w.write("-import(");
        _ = try w.write(a.body);
        _ = try w.write(", [");

        var i: usize = 0;
        for (a.children.?.items) |c| {
            try self.writeAst(w, c, false);

            if (i + 1 != a.children.?.items.len) {
                _ = try w.write(", ");
            }
            i = i + 1;
        }

        _ = try w.write("]).\n");
    }

    pub fn writeIncludeLib(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.include_lib) {
            return CodegenError.WritingFailure;
        }

        if (!std.mem.eql(u8, "include_lib", a.*.body)) {
            return CodegenError.WritingFailure;
        }

        if (a.*.children == null) {
            return CodegenError.WritingFailure;
        }

        if (a.*.children.?.items.len != 1) {
            return CodegenError.WritingFailure;
        }

        const path = a.*.children.?.items[0];

        if (path.*.ast_type != AstType.string) {
            return CodegenError.WritingFailure;
        }

        // Minimal path is: "a.hrl"
        if (path.*.body.len < 7) {
            return CodegenError.WritingFailure;
        }

        try self.writePadding(w);
        _ = try w.write("-");
        _ = try w.write(a.*.body);
        _ = try w.write("(");

        _ = try w.write(path.*.body);

        _ = try w.write(").\n");
    }

    pub fn writeBehaviour(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.behaviour) {
            return CodegenError.WritingFailure;
        }
        try self.writePadding(w);
        _ = try w.write("-behaviour(");
        _ = try w.write(a.*.body);
        _ = try w.write(").\n");
    }

    pub fn writeAttribute(self: *Self, w: anytype, a: *const Ast) !void {
        try self.writePadding(w);
        self.attribute_mode = true;
        _ = try w.write("-");
        _ = try w.write(a.*.body);
        _ = try w.write("(");

        var i: usize = 0;
        for (a.children.?.items) |c| {
            try self.writeAst(w, c, false);

            if (i + 1 != a.children.?.items.len) {
                _ = try w.write(", ");
            }
            i = i + 1;
        }

        _ = try w.write(").\n");
        self.attribute_mode = false;
    }

    pub fn writeFunctionDef(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.function_def) {
            return CodegenError.WritingFailure;
        }

        if (a.*.children == null) {
            return CodegenError.WritingFailure;
        }

        if (a.*.children.?.items.len < 2) {
            return CodegenError.WritingFailure;
        }
        var i: usize = 0;

        while (true) {
            if (i >= a.children.?.items.len) {
                break;
            }

            try self.writePadding(w);
            _ = try w.write(a.body);
            self.pushMatch();
            try self.writeFunctionSignature(w, a.children.?.items[i], false);
            self.popMatch();
            _ = try w.write(" ->");

            i = i + 1;

            try self.pushPadding(self.currentPadding() + 4);
            while (true) {
                if (i >= a.children.?.items.len or a.children.?.items[i].ast_type == AstType.function_signature) {
                    break;
                }

                _ = try w.write("\n");
                try self.writeAst(w, a.children.?.items[i], false);

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

            try self.popPadding();

            if (can_break) {
                break;
            }
        }
    }

    pub fn writeMacroDef(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.macro_def) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children == null) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children.?.items.len < 1) {
            return CodegenError.WritingFailure;
        }
        try self.writePadding(w);
        _ = try w.write("-define(");
        _ = try w.write(a.body);

        if (a.children.?.items.len == 1) {
            _ = try w.write(", ");
            try self.writeAst(w, a.children.?.items[0], false);
        } else {
            try self.writeFunctionSignature(w, a.children.?.items[0], false);
            _ = try w.write(", begin ");
            for (a.children.?.items[1..a.children.?.items.len]) |c| {
                _ = try w.write("\n");
                try self.writeAst(w, c, false);
            }
            _ = try w.write("\nend");
        }
        _ = try w.write(").\n\n");
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
        _ = try w.write("-spec ");

        var i: usize = 0;

        while (true) {
            if (i >= a.children.?.items.len) {
                break;
            }

            try self.writePadding(w);
            if (i == 0) {
                _ = try w.write(a.body);
            }
            self.pushMatch();
            try self.writeFunctionSignature(w, a.children.?.items[i], false);
            self.popMatch();
            _ = try w.write(" ->");

            i = i + 1;

            try self.pushPadding(self.currentPadding() + 4);
            while (true) {
                if (i >= a.children.?.items.len or a.children.?.items[i].ast_type == AstType.function_signature) {
                    break;
                }

                _ = try w.write("\n");
                try self.writeAst(w, a.children.?.items[i], false);

                if (i + 1 < a.children.?.items.len and a.children.?.items[i + 1].ast_type != AstType.function_signature) {
                    _ = try w.write(",");
                }

                i = i + 1;
            }

            var can_break = false;
            if (i >= a.children.?.items.len) {
                _ = try w.write(".\n");
                can_break = true;
            } else if (a.children.?.items[i].ast_type == AstType.function_signature) {
                _ = try w.write(";\n");
            } else {
                return CodegenError.WritingFailure;
            }

            try self.popPadding();

            if (can_break) {
                break;
            }
        }
    }

    pub fn writeCallbackDef(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.callback_def) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children == null) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children.?.items.len != 2) {
            return CodegenError.WritingFailure;
        }
        _ = try w.write("-callback ");
        _ = try w.write(a.body);
        try self.writeFunctionSignature(w, a.children.?.items[0], true);
        _ = try w.write(" -> ");
        try self.writeAst(w, a.children.?.items[1], true);
        _ = try w.write(".\n");
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
        _ = try w.write("-opaque ");
        try self.writeAst(w, a.*.children.?.items[0], false);
        _ = try w.write(" :: ");
        try self.writeAst(w, a.*.children.?.items[1], true);
        _ = try w.write(".\n\n");
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
        _ = try w.write("-type ");
        try self.writeAst(w, a.*.children.?.items[0], false);
        _ = try w.write(" :: ");
        try self.writeAst(w, a.*.children.?.items[1], true);
        _ = try w.write(".\n\n");
    }

    pub fn writeRecordDef(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.record_def) {
            return CodegenError.WritingFailure;
        }
        _ = try w.write("-record(");
        _ = try w.write(a.*.body);
        _ = try w.write(", {");

        try self.pushPadding(0);
        var i: usize = 0;
        for (a.children.?.items) |c| {
            try self.writeAst(w, c, true);

            if (i + 1 < a.children.?.items.len) {
                _ = try w.write(", ");
            }
            i += 1;
        }
        try self.popPadding();

        _ = try w.write("}).\n\n");
    }

    pub fn writeRecordField(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.record_field) {
            return CodegenError.WritingFailure;
        }
        _ = try w.write(a.*.body);

        if (a.*.children != null) {
            if (a.*.children.?.items.len == 0) {} else if (a.*.children.?.items.len == 1) {
                const a2 = a.*.children.?.items[0];
                if (a2.ast_type == AstType.record_field_value) {
                    try self.writeRecordFieldValue(w, a2);
                } else if (a2.ast_type == AstType.record_field_type) {
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
        _ = try w.write("=");
        if (a.children.?.items.len < 1) {
            return CodegenError.WritingFailure;
        }
        try self.writeAst(w, a.children.?.items[0], false);
    }

    pub fn writeRecordFieldType(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.record_field_type) {
            return CodegenError.WritingFailure;
        }
        _ = try w.write(" :: ");
        if (a.children.?.items.len < 1) {
            return CodegenError.WritingFailure;
        }
        try self.writeAst(w, a.children.?.items[0], true);
    }

    pub fn writeAnonymousFunction(self: *Self, w: anytype, a: *const Ast, type_exp: bool) !void {
        if (a.*.ast_type != AstType.anonymous_function) {
            return CodegenError.WritingFailure;
        }
        try self.writePadding(w);
        _ = try w.write("fun");

        if (type_exp and a.children.?.items.len == 0) {
            _ = try w.write("()");
        } else {
            if (type_exp) {
                _ = try w.write("(");
            }

            var i: usize = 0;

            while (true) {
                if (i >= a.children.?.items.len) {
                    break;
                }

                try self.pushPadding(0);
                self.pushMatch();
                try self.writeFunctionSignature(w, a.children.?.items[i], type_exp);
                self.popMatch();
                try self.popPadding();

                _ = try w.write(" ->");

                i = i + 1;

                while (true) {
                    if (i >= a.children.?.items.len or a.children.?.items[i].ast_type == AstType.function_signature) {
                        if (i < a.children.?.items.len and a.children.?.items[i].ast_type == AstType.function_signature) {
                            _ = try w.write(";\n");
                        }
                        break;
                    }

                    if (!type_exp) {
                        _ = try w.write("\n");

                        try self.pushPadding(self.currentPadding() + 4);
                    } else {
                        _ = try w.write(" ");
                    }

                    try self.writeAst(w, a.children.?.items[i], type_exp);

                    if (!type_exp) {
                        if (i + 1 < a.children.?.items.len and a.children.?.items[i + 1].ast_type != AstType.function_signature) {
                            _ = try w.write(",");
                        }

                        try self.popPadding();
                    }

                    i = i + 1;
                }
            }

            if (!type_exp) {
                _ = try w.write("\n");
                try self.writePadding(w);
                _ = try w.write("end");
            } else {
                _ = try w.write(")");
            }
        }
    }

    pub fn writeFunctionSignature(self: *Self, w: anytype, a: *const Ast, type_exp: bool) !void {
        if (a.*.ast_type != AstType.function_signature) {
            return CodegenError.WritingFailure;
        }
        try self.writePadding(w);
        if (a.children == null) {
            _ = try w.write("()");
            return;
        }

        if (a.children.?.items.len == 0) {
            _ = try w.write("()");
            return;
        }

        _ = try w.write("(");

        const guard_sequence: bool = a.children.?.items[a.children.?.items.len - 1].ast_type == AstType.guard_sequence;
        var len = a.children.?.items.len;

        if (guard_sequence) {
            len = len - 1;
        }

        var i: usize = 0;

        while (true) {
            if (i >= len) {
                break;
            }

            try self.writeAst(w, a.children.?.items[i], type_exp);

            if (i + 1 < len) {
                _ = try w.write(", ");
            }

            i += 1;
        }

        _ = try w.write(")");

        if (guard_sequence) {
            _ = try w.write(" ");
            try self.writePadding(w);
            _ = try w.write("when ");

            try self.writeGuardSequence(w, a.children.?.items[a.children.?.items.len - 1], type_exp);
        }
    }

    pub fn writeGuardSequence(self: *Self, w: anytype, a: *const Ast, type_exp: bool) !void {
        if (a.*.ast_type != AstType.guard_sequence) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children == null) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children.?.items.len == 0) {
            return CodegenError.WritingFailure;
        }

        var i: usize = 0;
        while (i < a.children.?.items.len) {
            try self.writeGuard(w, a.children.?.items[i], type_exp);
            if (i + 1 < a.children.?.items.len) {
                _ = try w.write(";\n");
            }
            i = i + 1;
        }
    }

    pub fn writeGuard(self: *Self, w: anytype, a: *const Ast, type_exp: bool) !void {
        if (a.*.ast_type != AstType.guard) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children == null) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children.?.items.len == 0) {
            return CodegenError.WritingFailure;
        }

        var i: usize = 0;
        while (i < a.children.?.items.len) {
            try self.writeAst(w, a.children.?.items[i], type_exp);
            if (i + 1 < a.children.?.items.len) {
                _ = try w.write(", ");
            }
            i = i + 1;
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

        self.pushMatch();
        try self.writeAst(w, a.children.?.items[0], false);
        self.popMatch();

        var i: usize = 1;

        const guard_sequence: bool = a.children.?.items[i].ast_type == AstType.guard_sequence;
        if (guard_sequence) {
            _ = try w.write(" ");
            try self.pushPadding(0);
            try self.writePadding(w);
            _ = try w.write("when ");

            try self.writeGuardSequence(w, a.children.?.items[i], false);
            try self.popPadding();

            i = i + 1;
        }
        _ = try w.write(" ->");

        try self.pushPadding(self.currentPadding() + 4);
        var loop = true;
        while (loop) {
            _ = try w.write("\n");

            try self.writeAst(w, a.children.?.items[i], false);

            const len = a.children.?.items.len;

            if (i + 1 != len) {
                _ = try w.write(",");
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
        try self.writeAst(w, a.children.?.items[0], false);
        try self.popPadding();
        _ = try w.write(" of");

        var i: usize = 1;
        var loop = true;
        try self.pushPadding(self.currentPadding() + 4);
        while (loop) {
            _ = try w.write("\n");

            try self.writeAst(w, a.children.?.items[i], false);
            const len = a.children.?.items.len;

            if (i + 1 != len) {
                _ = try w.write(";");
            }

            i += 1;
            if (i >= len) {
                loop = false;
            }
        }
        try self.popPadding();

        _ = try w.write("\n");
        _ = try self.writePadding(w);
        _ = try w.write("end");
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

        _ = try w.write("receive");

        var i: usize = 0;
        var loop = true;
        try self.pushPadding(self.currentPadding() + 4);
        const len = a.children.?.items.len;
        while (loop) {
            _ = try w.write("\n");

            try self.writeAst(w, a.children.?.items[i], false);

            if (i + 1 != len) {
                _ = try w.write(";");
            }

            i += 1;
            if (i >= len) {
                loop = false;
            }
        }
        try self.popPadding();

        _ = try w.write("\n");
        _ = try self.writePadding(w);
        _ = try w.write("end");
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
        try self.writeAst(w, a.children.?.items[0], false);
        try self.popPadding();
        _ = try w.write(" of");

        var i: usize = 1;
        var loop = true;
        try self.pushPadding(self.currentPadding() + 4);

        while (loop) {
            _ = try w.write("\n");
            try self.writeAst(w, a.children.?.items[i], false);
            const len = a.children.?.items.len;

            if (i + 1 != len) {
                _ = try w.write(";");
            }

            i += 1;
            if (i >= len) {
                loop = false;
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

        try self.pushPadding(self.currentPadding() + 4);

        var loop = true;
        var i: usize = 0;
        while (loop) {
            _ = try w.write("\n");
            try self.writeAst(w, a.children.?.items[i], false);
            const len = a.children.?.items.len;

            if (i + 1 != len) {
                _ = try w.write(";");
            }

            i += 1;
            if (i >= len) {
                loop = false;
            }
        }

        try self.popPadding();
        _ = try w.write("\n");

        try self.writePadding(w);
        _ = try w.write("end");
    }

    pub fn writeTestBlock(self: *Self, w: anytype, a: *const Ast, type_exp: bool) !void {
        if (a.*.ast_type != AstType.test_block) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children == null) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children.?.items.len == 0) {
            return CodegenError.WritingFailure;
        }
        _ = try w.write("-ifdef(TEST).\n");
        for (a.*.children.?.items) |c| {
            try self.writeAst(w, c, type_exp);
        }
        _ = try w.write("-endif.\n\n");
    }

    pub fn writeTestUnit(self: *Self, w: anytype, a: *const Ast) !void {
        if (a.*.ast_type != AstType.test_unit) {
            return CodegenError.WritingFailure;
        }
        if (std.mem.eql(u8, "", a.*.body)) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children == null) {
            return CodegenError.WritingFailure;
        }
        if (a.*.children.?.items.len == 0) {
            return CodegenError.WritingFailure;
        }
        _ = try w.write("-ifdef(TEST).\n");
        _ = try w.write(a.*.body);
        _ = try w.write("_test() ->\n");
        var i: usize = 0;
        try self.pushPadding(self.currentPadding() + 4);
        while (true) {
            if (i >= a.children.?.items.len) {
                break;
            }

            try self.writeAst(w, a.children.?.items[i], false);

            if (i + 1 < a.children.?.items.len) {
                _ = try w.write(",");
            }

            i = i + 1;
        }
        try self.popPadding();
        _ = try w.write(".\n\n-endif.\n\n");
    }

    pub fn pushMatch(self: *Self) void {
        self.*.match_count += 1;
    }

    pub fn popMatch(self: *Self) void {
        if (self.*.match_count == 0) {
            return;
        }
        self.*.match_count -= 1;
    }

    pub fn matchMode(self: *Self) bool {
        return self.*.match_count > 0;
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

    pub fn writeAst(self: *Self, w: anytype, a: *const Ast, type_exp: bool) error{WritingFailure}!void {
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
                self.writeBinary(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .binary_element => {
                self.writeBinaryElement(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .binary_element_size => {
                self.writeBinaryElementSize(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .binary_element_type => {
                self.writeBinaryElementType(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .string => {
                self.writeString(w, a, true) catch {
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
            .tuple => {
                self.writeTuple(w, a, type_exp) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .list => {
                self.writeList(w, a, type_exp) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .map => {
                self.writeMap(w, a, type_exp) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .include_lib => {
                self.writeIncludeLib(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .on_load => {
                self.writeAttribute(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .nifs => {
                self.writeAttribute(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .doc => {
                self.writeAttribute(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .moduledoc => {
                self.writeAttribute(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .function_call => {
                self.writeFunctionCall(w, a, type_exp) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .function_def => {
                self.writeFunctionDef(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .macro_def => {
                self.writeMacroDef(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .type_def => {
                self.writeTypeDef(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .opaque_type_def => {
                self.writeOpaqueTypeDef(w, a) catch {
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
            .anonymous_function => {
                self.writeAnonymousFunction(w, a, type_exp) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .function_signature => {
                self.writeFunctionSignature(w, a, type_exp) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .op => {
                self.writeOp(w, a, type_exp) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .paren_exp => {
                self.writeParenExp(w, a) catch {
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
            .guard_sequence => {
                self.writeGuardSequence(w, a, type_exp) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .guard => {
                self.writeGuard(w, a, type_exp) catch {
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
            .test_block => {
                self.writeTestBlock(w, a, type_exp) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .test_unit => {
                self.writeTestUnit(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .behaviour => {
                self.writeBehaviour(w, a) catch {
                    return CodegenError.WritingFailure;
                };
            },
            .exception => {
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

    try context.writeValue(list.writer(), &Ast{ .body = "1", .ast_type = AstType.int, .children = null });
    try std.testing.expect(std.mem.eql(u8, list.items, "1"));
    list.clearAndFree();

    try context.writeValue(list.writer(), &Ast{ .body = "100_000", .ast_type = AstType.int, .children = null });
    try std.testing.expect(std.mem.eql(u8, list.items, "100_000"));
    list.clearAndFree();

    try context.writeValue(list.writer(), &Ast{ .body = "1.0", .ast_type = AstType.int, .children = null });
    try std.testing.expect(std.mem.eql(u8, list.items, "1.0"));
    list.clearAndFree();

    // TODO: Scientific notation

    try context.writeValue(list.writer(), &Ast{ .body = "foo", .ast_type = AstType.atom, .children = null });
    try std.testing.expect(std.mem.eql(u8, list.items, "foo"));
    // TODO: Normal Atom
    // TODO: Atom with double quotes
}

test "write string" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var context = Context.init(test_allocator);
    defer context.deinit();

    try context.writeString(list.writer(), &Ast{ .body = "\"foobar\"", .ast_type = AstType.string, .children = null }, true);

    try std.testing.expect(std.mem.eql(u8, list.items, "<<\"foobar\"/utf8>>"));

    list.clearAndFree();

    try context.writeString(list.writer(), &Ast{ .body = "\"foobar\"", .ast_type = AstType.string, .children = null }, false);

    try std.testing.expect(std.mem.eql(u8, list.items, "\"foobar\""));
}

test "write binary" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var context = Context.init(test_allocator);
    defer context.deinit();

    var echildren = std.ArrayList(*const Ast).init(test_allocator);
    defer echildren.deinit();

    try echildren.append(&Ast{ .body = "\"foobar\"", .ast_type = AstType.string, .children = null });

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();
    try children.append(&Ast{ .body = "", .ast_type = AstType.binary_element, .children = echildren });

    try context.writeBinary(list.writer(), &Ast{ .body = "", .ast_type = AstType.binary, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "<<\"foobar\">>"));
}

test "write binary element" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var context = Context.init(test_allocator);
    defer context.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "\"foo\"", .ast_type = AstType.string, .children = null });

    try context.writeBinaryElement(list.writer(), &Ast{ .body = "", .ast_type = AstType.binary_element, .children = children });
    try std.testing.expect(std.mem.eql(u8, list.items, "\"foo\""));
}

test "write binary element size" {
    var context = Context.init(test_allocator);
    defer context.deinit();

    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "1", .ast_type = AstType.int, .children = null });

    try context.writeBinaryElementSize(list.writer(), &Ast{ .body = "", .ast_type = AstType.binary_element_size, .children = children });
    try std.testing.expect(std.mem.eql(u8, list.items, ":1"));
}

test "write binary element type" {
    var context = Context.init(test_allocator);
    defer context.deinit();

    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    try context.writeBinaryElementType(list.writer(), &Ast{ .body = "binary", .ast_type = AstType.binary_element_type, .children = null });
    try std.testing.expect(std.mem.eql(u8, list.items, "/binary"));
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

    try context.writeTuple(list.writer(), &Ast{ .body = "", .ast_type = AstType.tuple, .children = children }, false);

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
    try children.append(&Ast{ .body = "\"foobar\"", .ast_type = AstType.string, .children = null });

    var context = Context.init(test_allocator);
    defer context.deinit();

    try context.writeList(list.writer(), &Ast{ .body = "", .children = children, .ast_type = AstType.list }, false);

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

    try context.writeMap(list.writer(), &Ast{ .body = "", .ast_type = AstType.map, .children = children }, false);

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
    try fv1_children.append(&Ast{ .body = "\"Joe\"", .ast_type = AstType.string, .children = null });

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

    try context.writeRecord(list.writer(), &Ast{ .body = "person", .ast_type = AstType.record, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "#person{name=<<\"Joe\"/utf8>>, age=68}"));
}

test "write fun val" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    const a = Ast{ .body = "foobar/1", .ast_type = AstType.fun_val, .children = null };

    var context = Context.init(test_allocator);
    defer context.deinit();

    try context.writeFunVal(list.writer(), &a);

    try std.testing.expect(std.mem.eql(u8, list.items, "fun foobar/1"));
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
    try context.writeOp(list.writer(), &Ast{ .body = "+", .children = children, .ast_type = AstType.op }, false);

    try std.testing.expect(std.mem.eql(u8, list.items, "1 + 2"));
}

test "write paren exp" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var op_children = std.ArrayList(*const Ast).init(test_allocator);
    defer op_children.deinit();

    try op_children.append(&Ast{ .body = "1", .ast_type = AstType.int, .children = null });
    try op_children.append(&Ast{ .body = "2", .ast_type = AstType.int, .children = null });

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "+", .ast_type = AstType.op, .children = op_children });

    var context = Context.init(test_allocator);
    defer context.deinit();
    try context.writeParenExp(list.writer(), &Ast{ .body = "", .ast_type = AstType.paren_exp, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "(1 + 2)"));
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
    try context.writeFunctionCall(list.writer(), &Ast{ .body = "erlang:add", .ast_type = AstType.function_call, .children = children }, false);

    try std.testing.expect(std.mem.eql(u8, list.items, "erlang:add(1, 2)"));

    list.clearAndFree();

    try context.writeFunctionCall(list.writer(), &Ast{ .body = "timestamp", .ast_type = AstType.function_call, .children = null }, false);
    try std.testing.expect(std.mem.eql(u8, list.items, "timestamp()"));
}

test "write import def" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "foobar/1", .ast_type = AstType.import_element, .children = null });
    try children.append(&Ast{ .body = "barfoo/2", .ast_type = AstType.import_element, .children = null });

    var context = Context.init(test_allocator);
    defer context.deinit();
    try context.writeImportDef(list.writer(), &Ast{ .body = "other", .ast_type = AstType.import_def, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "-import(other, [foobar/1, barfoo/2]).\n"));
}

test "write behaviour" {
    var context = Context.init(test_allocator);
    defer context.deinit();

    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    try context.writeBehaviour(list.writer(), &Ast{ .body = "gen_server", .children = null, .ast_type = AstType.behaviour });

    try std.testing.expect(std.mem.eql(u8, list.items, "-behaviour(gen_server).\n"));
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
    try context.writeFunctionDef(list.writer(), &Ast{ .body = "hello_world", .ast_type = AstType.function_def, .children = children });
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
    try context.writeFunctionDef(list.writer(), &Ast{ .body = "hello", .ast_type = AstType.function_def, .children = children });
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
    try context.writeAnonymousFunction(list.writer(), &Ast{ .body = "", .ast_type = AstType.anonymous_function, .children = children }, false);
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
    try context.writeFunctionSignature(list.writer(), &Ast{ .body = "", .ast_type = AstType.function_signature, .children = children }, false);

    const expected = "(A, B)";
    try std.testing.expect(std.mem.eql(u8, list.items, expected));

    list.clearAndFree();

    //    var children2 = std.ArrayList(*const Ast).init(test_allocator);
    //    defer children2.deinit();

    //    try children2.append(&Ast{ .body = "A", .ast_type = AstType.variable, .children = null });
    //    try children2.append(&Ast{ .body = "B", .ast_type = AstType.variable, .children = null });

    //    var guard_children = std.ArrayList(*const Ast).init(test_allocator);
    //    defer guard_children.deinit();

    //    var function_children = std.ArrayList(*const Ast).init(test_allocator);
    //    defer function_children.deinit();

    //    try function_children.append(&Ast{ .body = "A", .ast_type = AstType.variable, .children = null });
    //    try guard_children.append(&Ast{ .body = "is_integer", .ast_type = AstType.function_call, .children = function_children });

    //    try children2.append(&Ast{ .body = "", .ast_type = AstType.guard_clause, .children = guard_children });

    //    try context.writeFunctionSignature(list.writer(), &Ast{ .body = "", .ast_type = AstType.function_signature, .children = children2 }, false);

    //    try std.testing.expect(std.mem.eql(u8, list.items, "(A, B) when is_integer(A)"));

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
    try context.writeCaseClause(list.writer(), &Ast{ .body = "", .ast_type = AstType.case_clause, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "X ->\n    Y = X + 2,\n    Y"));

    list.clearAndFree();

    //    var children2 = std.ArrayList(*const Ast).init(test_allocator);
    //    defer children2.deinit();

    //    try children2.append(&Ast{ .body = "X", .ast_type = AstType.variable, .children = null });

    //    var guard_children = std.ArrayList(*const Ast).init(test_allocator);
    //    defer guard_children.deinit();

    //    var function_children = std.ArrayList(*const Ast).init(test_allocator);
    //    defer function_children.deinit();

    //    try function_children.append(&Ast{ .body = "X", .ast_type = AstType.variable, .children = null });

    //    try guard_children.append(&Ast{ .body = "is_integer", .ast_type = AstType.function_call, .children = function_children });

    //    try children2.append(&Ast{ .body = "", .ast_type = AstType.guard_clause, .children = guard_children });

    //    try children2.append(&Ast{ .body = "X", .ast_type = AstType.variable, .children = null });

    //    try context.writeCaseClause(list.writer(), &Ast{ .body = "", .ast_type = AstType.case_clause, .children = children2 });
    //    try std.testing.expect(std.mem.eql(u8, list.items, "X when is_integer(X) ->\n    X"));
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
    try context.writeCase(list.writer(), &Ast{ .body = "", .ast_type = AstType.case, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "case X of\n    true ->\n        ok;\n    false ->\n        error\nend"));
}

test "write spec def" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "", .ast_type = AstType.function_signature, .children = null });
    try children.append(&Ast{ .body = "integer", .ast_type = AstType.function_call, .children = null });

    var context = Context.init(test_allocator);
    defer context.deinit();
    try context.writeSpecDef(list.writer(), &Ast{ .body = "foobar", .ast_type = AstType.spec_def, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "-spec foobar() ->\n    integer().\n"));
}

test "write macro def" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "4200", .ast_type = AstType.int, .children = null });

    var context = Context.init(test_allocator);
    defer context.deinit();
    try context.writeMacroDef(list.writer(), &Ast{ .body = "TIMEOUT", .ast_type = AstType.macro_def, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "-define(TIMEOUT, 4200).\n\n"));

    // TODO: Test case multiline expression
}

test "write callback def" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "", .ast_type = AstType.function_signature, .children = null });
    try children.append(&Ast{ .body = "integer", .ast_type = AstType.function_call, .children = null });

    var context = Context.init(test_allocator);
    defer context.deinit();
    try context.writeCallbackDef(list.writer(), &Ast{ .body = "foobar", .ast_type = AstType.callback_def, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "-callback foobar() -> integer().\n"));
}

test "write opaque type def" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "id", .ast_type = AstType.function_call, .children = null });
    try children.append(&Ast{ .body = "integer", .ast_type = AstType.function_call, .children = null });

    var context = Context.init(test_allocator);
    defer context.deinit();
    try context.writeOpaqueTypeDef(list.writer(), &Ast{ .body = "id", .ast_type = AstType.opaque_type_def, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "-opaque id() :: integer().\n\n"));
}

test "write type def" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "id", .ast_type = AstType.function_call, .children = null });
    try children.append(&Ast{ .body = "integer", .ast_type = AstType.function_call, .children = null });

    var context = Context.init(test_allocator);
    defer context.deinit();
    try context.writeTypeDef(list.writer(), &Ast{ .body = "id", .ast_type = AstType.type_def, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "-type id() :: integer().\n\n"));
}

test "write record def" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "x", .ast_type = AstType.record_field, .children = null });
    try children.append(&Ast{ .body = "y", .ast_type = AstType.record_field, .children = null });

    var context = Context.init(test_allocator);
    defer context.deinit();
    try context.writeRecordDef(list.writer(), &Ast{ .body = "point", .ast_type = AstType.record_def, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "-record(point, {x, y}).\n\n"));
}

test "write record field" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var context = Context.init(test_allocator);
    defer context.deinit();
    try context.writeRecordField(list.writer(), &Ast{ .body = "foobar", .ast_type = AstType.record_field, .children = null });

    try std.testing.expect(std.mem.eql(u8, list.items, "foobar"));

    // TODO: Cases with field value, and field types
}

test "write record field value" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "42", .ast_type = AstType.int, .children = null });

    var context = Context.init(test_allocator);
    defer context.deinit();
    try context.writeRecordFieldValue(list.writer(), &Ast{ .body = "", .ast_type = AstType.record_field_value, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "=42"));
}

test "write record field type" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "integer", .ast_type = AstType.function_call, .children = null });

    var context = Context.init(test_allocator);
    defer context.deinit();
    try context.writeRecordFieldType(list.writer(), &Ast{ .body = "", .ast_type = AstType.record_field_type, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, " :: integer()"));
}

test "write receive" {
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

    try children.append(&Ast{ .body = "", .ast_type = AstType.case_clause, .children = case_clause_children_1 });
    try children.append(&Ast{ .body = "", .ast_type = AstType.case_clause, .children = case_clause_children_2 });

    var context = Context.init(test_allocator);
    defer context.deinit();
    try context.writeReceive(list.writer(), &Ast{ .body = "", .ast_type = AstType.receive_exp, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "receive\n    true ->\n        ok;\n    false ->\n        error\nend"));
}

test "write try catch" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var try_children = std.ArrayList(*const Ast).init(test_allocator);
    defer try_children.deinit();

    try try_children.append(&Ast{ .body = "_", .ast_type = AstType.variable, .children = null });
    try try_children.append(&Ast{ .body = "42", .ast_type = AstType.int, .children = null });

    var try_children2 = std.ArrayList(*const Ast).init(test_allocator);
    defer try_children2.deinit();

    try try_children2.append(&Ast{ .body = "foobar", .ast_type = AstType.function_call, .children = null });
    try try_children2.append(&Ast{ .body = "", .ast_type = AstType.case_clause, .children = try_children });

    var catch_children = std.ArrayList(*const Ast).init(test_allocator);
    defer catch_children.deinit();

    try catch_children.append(&Ast{ .body = "_", .ast_type = AstType.variable, .children = null });
    try catch_children.append(&Ast{ .body = "42", .ast_type = AstType.int, .children = null });

    var catch_children2 = std.ArrayList(*const Ast).init(test_allocator);
    defer catch_children2.deinit();

    try catch_children2.append(&Ast{ .body = "", .ast_type = AstType.case_clause, .children = catch_children });

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "", .ast_type = AstType.try_exp, .children = try_children2 });
    try children.append(&Ast{ .body = "", .ast_type = AstType.catch_exp, .children = catch_children2 });

    var context = Context.init(test_allocator);
    defer context.deinit();
    try context.writeTryCatch(list.writer(), &Ast{ .body = "", .ast_type = AstType.try_catch, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "try foobar() of\n    _ ->\n        42\ncatch\n\n    _ ->\n        42\nend"));
}

test "write try exp" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "_", .ast_type = AstType.variable, .children = null });
    try children.append(&Ast{ .body = "42", .ast_type = AstType.int, .children = null });

    var children2 = std.ArrayList(*const Ast).init(test_allocator);
    defer children2.deinit();

    try children2.append(&Ast{ .body = "foobar", .ast_type = AstType.function_call, .children = null });
    try children2.append(&Ast{ .body = "", .ast_type = AstType.case_clause, .children = children });

    var context = Context.init(test_allocator);
    defer context.deinit();
    try context.writeTryExp(list.writer(), &Ast{ .body = "", .ast_type = AstType.try_exp, .children = children2 });

    try std.testing.expect(std.mem.eql(u8, list.items, "try foobar() of\n    _ ->\n        42\n"));
}

test "write catch exp" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "_", .ast_type = AstType.variable, .children = null });
    try children.append(&Ast{ .body = "42", .ast_type = AstType.int, .children = null });

    var children2 = std.ArrayList(*const Ast).init(test_allocator);
    defer children2.deinit();

    try children2.append(&Ast{ .body = "", .ast_type = AstType.case_clause, .children = children });

    var context = Context.init(test_allocator);
    defer context.deinit();
    try context.writeCatchExp(list.writer(), &Ast{ .body = "", .ast_type = AstType.catch_exp, .children = children2 });

    try std.testing.expect(std.mem.eql(u8, list.items, "catch\n\n    _ ->\n        42\nend"));
}

test "write guard" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var op_children = std.ArrayList(*const Ast).init(test_allocator);
    defer op_children.deinit();
    try op_children.append(&Ast{ .body = "A", .ast_type = AstType.variable, .children = null });
    try op_children.append(&Ast{ .body = "2", .ast_type = AstType.int, .children = null });

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = ">=", .ast_type = AstType.op, .children = op_children });

    var context = Context.init(test_allocator);
    defer context.deinit();
    try context.writeGuard(list.writer(), &Ast{ .body = "", .ast_type = AstType.guard, .children = children }, false);

    try std.testing.expect(std.mem.eql(u8, list.items, "A >= 2"));
}

test "write guard sequence" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var op_children = std.ArrayList(*const Ast).init(test_allocator);
    defer op_children.deinit();
    try op_children.append(&Ast{ .body = "A", .ast_type = AstType.variable, .children = null });
    try op_children.append(&Ast{ .body = "2", .ast_type = AstType.int, .children = null });

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = ">=", .ast_type = AstType.op, .children = op_children });

    var gchildren = std.ArrayList(*const Ast).init(test_allocator);
    defer gchildren.deinit();

    try gchildren.append(&Ast{ .body = "", .ast_type = AstType.guard, .children = children });
    try gchildren.append(&Ast{ .body = "", .ast_type = AstType.guard, .children = children });

    var context = Context.init(test_allocator);
    defer context.deinit();

    try context.writeGuardSequence(list.writer(), &Ast{ .body = "", .ast_type = AstType.guard_sequence, .children = gchildren }, false);

    try std.testing.expect(std.mem.eql(u8, list.items, "A >= 2;\nA >= 2"));
}

test "write test block" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var fchildren = std.ArrayList(*const Ast).init(test_allocator);
    defer fchildren.deinit();

    try fchildren.append(&Ast{ .body = "", .ast_type = AstType.function_signature, .children = null });
    try fchildren.append(&Ast{ .body = "hello", .ast_type = AstType.function_call, .children = null });

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "foo_test", .ast_type = AstType.function_def, .children = fchildren });

    var context = Context.init(test_allocator);
    defer context.deinit();

    try context.writeTestBlock(list.writer(), &Ast{ .body = "", .ast_type = AstType.test_block, .children = children }, false);

    try std.testing.expect(std.mem.eql(u8, list.items, "-ifdef(TEST).\nfoo_test() ->\n    hello().\n\n-endif.\n\n"));
}

test "write test unit" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();

    var children = std.ArrayList(*const Ast).init(test_allocator);
    defer children.deinit();

    try children.append(&Ast{ .body = "hello", .ast_type = AstType.function_call, .children = null });

    var context = Context.init(test_allocator);
    defer context.deinit();

    try context.writeTestUnit(list.writer(), &Ast{ .body = "foo", .ast_type = AstType.test_unit, .children = children });

    try std.testing.expect(std.mem.eql(u8, list.items, "-ifdef(TEST).\nfoo_test() ->\n    hello().\n\n-endif.\n\n"));
}
