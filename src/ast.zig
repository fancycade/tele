const std = @import("std");
const test_allocator = std.testing.allocator;

pub const AstType = enum { int, float, binary, atom, tuple, list, map, record, attribute, function_def, function_call, case, op, variable };

pub const Ast = struct {
    const Self = @This();

    children: ?std.ArrayList(*const Ast),
    body: []const u8,
    ast_type: AstType,

    pub fn init(body: []const u8, ast_type: AstType) Self {
        return Self{
            .body = body,
            .ast_type = ast_type,
            .children = null,
        };
    }

    pub fn appendChild(self: *Self, ast: *const Ast) !void {
        try self.children.?.append(ast);
    }
};

test "make an ast" {
    const a = Ast.init("foobar", AstType.binary);

    try std.testing.expect(std.mem.eql(u8, a.body, "foobar"));
    try std.testing.expect(a.ast_type == AstType.binary);
    try std.testing.expect(a.children == null);
}

test "add a child" {
    var a = Ast.init("", AstType.list);
    a.children = std.ArrayList(*const Ast).init(test_allocator);

    try a.appendChild(&Ast.init("barfoo", AstType.binary));

    try std.testing.expect(a.children.?.items.len == 1);

    a.children.?.deinit();
}
