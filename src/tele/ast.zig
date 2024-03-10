const std = @import("std");
const test_allocator = std.testing.allocator;

pub const AstType = enum { space, newline, int, float, binary, atom, tuple, list, map, record, attribute, function_def, function_signature, anonymous_function, function_call, case, case_clause, guard_clause, op, variable };

pub const Ast = struct {
    children: ?std.ArrayList(*const Ast),
    body: []const u8,
    ast_type: AstType,
};
