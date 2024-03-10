const std = @import("std");
const test_allocator = std.testing.allocator;

pub const AstType = enum { int, float, binary, atom, tuple, list, map, record, attribute, function_def, function_signature, anonymous_function, function_call, case, case_clause, guard_clause, op, variable };

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
