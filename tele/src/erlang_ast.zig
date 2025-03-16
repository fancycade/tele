const std = @import("std");
const test_allocator = std.testing.allocator;

pub const AstType = enum { int, float, string, binary, binary_element, binary_element_size, binary_element_type, atom, tuple, list, map, record, record_field, record_field_value, record_field_type, attribute, custom_attribute, function_def, function_signature, anonymous_function, function_call, case, case_clause, guard_clause, op, variable, type_def, opaque_type_def, record_def, spec_def, callback_def, paren_exp, fun_val, try_catch, try_exp, catch_exp, macro_def, import_def, receive_exp, import_element, test_block, test_unit };

pub const Ast = struct {
    children: ?std.ArrayList(*const Ast),
    body: []const u8,
    ast_type: AstType,
};

pub fn equal(a: *const Ast, b: *const Ast) bool {
    if (a.ast_type != b.ast_type) {
        return false;
    }

    if (!std.mem.eql(u8, a.body, b.body)) {
        return false;
    }

    if (a.children == null and b.children == null) {
        return true;
    }

    if (a.children.?.items.len != b.children.?.items.len) {
        return false;
    }

    var i: usize = 0;
    while (i < a.children.?.items.len) {
        if (!equal(a.children.?.items[i], b.children.?.items[i])) {
            return false;
        }
        i = i + 1;
    }

    return true;
}

// TODO: Change to freeErlangAst
pub fn destroy(a: *const Ast, allocator: std.mem.Allocator) void {
    if (!std.mem.eql(u8, "", a.*.body)) {
        allocator.free(a.*.body);
    }

    if (a.*.children != null) {
        for (a.*.children.?.items) |c| {
            destroy(c, allocator);
        }
        a.*.children.?.deinit();
    }

    allocator.destroy(a);
}

// TODO: Change to freeErlangAstList
pub fn free_erlang_ast_list(e: std.ArrayList(*const Ast), allocator: std.mem.Allocator) void {
    for (e.items) |c| {
        destroy(c, allocator);
    }
    e.deinit();
}

pub fn makeValue(value: []const u8, ast_type: AstType, allocator: std.mem.Allocator) !*Ast {
    const t = try allocator.create(Ast);
    t.*.body = value;
    t.*.ast_type = ast_type;
    t.*.children = null;
    return t;
}

pub fn makeCollection(children: ?std.ArrayList(*const Ast), ast_type: AstType, allocator: std.mem.Allocator) !*Ast {
    const t = try allocator.create(Ast);
    t.*.body = "";
    t.*.ast_type = ast_type;
    t.*.children = children;
    return t;
}

pub fn makeNamedCollection(body: []const u8, children: ?std.ArrayList(*const Ast), ast_type: AstType, allocator: std.mem.Allocator) !*Ast {
    const t = try makeCollection(children, ast_type, allocator);
    t.*.body = body;
    return t;
}
