const std = @import("std");
const test_allocator = std.testing.allocator;

pub const AstType = enum { int, float, binary, atom, tuple, list, map, record, attribute, function_def, function_defp, function_signature, anonymous_function, function_call, case, case_clause, guard_clause, op, variable };

pub const Ast = struct {
    children: ?std.ArrayList(*Ast),
    body: []const u8,
    ast_type: AstType,
};

pub fn free_tele_ast(t: *Ast, allocator: std.mem.Allocator) void {
    if (t.*.children != null) {
        free_tele_ast_list(t.*.children.?, allocator);
    }
    if (t.*.body.len > 0) {
        allocator.free(t.*.body);
    }
    allocator.destroy(t);
}

pub fn free_tele_ast_list(ta: std.ArrayList(*Ast), allocator: std.mem.Allocator) void {
    for (ta.items) |c| {
        if (c.*.body.len > 0) {
            allocator.free(c.*.body);
        }
        if (c.*.children != null) {
            free_tele_ast_list(c.*.children.?, allocator);
        }
        allocator.destroy(c);
    }
    ta.deinit();
}
