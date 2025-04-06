const std = @import("std");
const test_allocator = std.testing.allocator;
const util = @import("util.zig");

pub const AstType = enum { int, float, binary, string, binary_element, binary_element_size, binary_element_type, atom, tuple, list, map, record, record_field, record_field_value, record_field_type, attribute, custom_attribute, function_def, function_defp, function_signature, anonymous_function, function_call, case, case_clause, guard_sequence, guard, op, variable, type_def, opaque_type_def, record_def, spec_def, callback_def, paren_exp, fun_val, try_catch, try_exp, catch_exp, macro_def, import_def, receive_exp, import_element, test_block, test_unit, behaviour };

pub const Ast = struct { children: ?std.ArrayList(*Ast), body: []const u8, ast_type: AstType, line: usize, col: usize };

pub fn freeTeleAst(t: *Ast, allocator: std.mem.Allocator) void {
    if (t.*.children != null) {
        freeTeleAstList(t.*.children.?, allocator);
    }
    if (t.*.body.len > 0) {
        allocator.free(t.*.body);
    }
    allocator.destroy(t);
}

pub fn freeTeleAstList(ta: std.ArrayList(*Ast), allocator: std.mem.Allocator) void {
    for (ta.items) |c| {
        if (c.*.body.len > 0) {
            allocator.free(c.*.body);
        }
        if (c.*.children != null) {
            freeTeleAstList(c.*.children.?, allocator);
        }
        allocator.destroy(c);
    }
    ta.deinit();
}

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

pub fn makeValue(value: []const u8, ast_type: AstType, allocator: std.mem.Allocator) !*Ast {
    const t = try allocator.create(Ast);
    t.*.body = value;
    t.*.ast_type = ast_type;
    t.*.children = null;
    t.*.col = 0;
    return t;
}

test "make value" {
    const t = try makeValue(try util.copyString("foo", test_allocator), AstType.atom, test_allocator);
    try std.testing.expect(std.mem.eql(u8, "foo", t.*.body));
    try std.testing.expect(t.*.ast_type == AstType.atom);
    freeTeleAst(t, test_allocator);
}

pub fn makeInt(value: []const u8, allocator: std.mem.Allocator) !*Ast {
    return try makeValue(value, AstType.int, allocator);
}

test "make int" {
    const t = try makeInt(try util.copyString("1", test_allocator), test_allocator);
    try std.testing.expect(std.mem.eql(u8, "1", t.*.body));
    try std.testing.expect(t.*.ast_type == AstType.int);
    freeTeleAst(t, test_allocator);
}

pub fn makeFloat(value: []const u8, allocator: std.mem.Allocator) !*Ast {
    return try makeValue(value, AstType.float, allocator);
}

test "make float" {
    const t = try makeFloat(try util.copyString("1.1", test_allocator), test_allocator);
    try std.testing.expect(std.mem.eql(u8, "1.1", t.*.body));
    try std.testing.expect(t.*.ast_type == AstType.float);
    freeTeleAst(t, test_allocator);
}

pub fn makeAtom(value: []const u8, allocator: std.mem.Allocator) !*Ast {
    return try makeValue(value, AstType.atom, allocator);
}

test "make atom" {
    const t = try makeAtom(try util.copyString("foo", test_allocator), test_allocator);
    try std.testing.expect(std.mem.eql(u8, "foo", t.*.body));
    try std.testing.expect(t.*.ast_type == AstType.atom);
    freeTeleAst(t, test_allocator);
}

pub fn makeVariable(value: []const u8, allocator: std.mem.Allocator) !*Ast {
    return try makeValue(value, AstType.variable, allocator);
}

test "make variable" {
    const t = try makeVariable(try util.copyString("a", test_allocator), test_allocator);
    try std.testing.expect(std.mem.eql(u8, "a", t.*.body));
    try std.testing.expect(t.*.ast_type == AstType.variable);
    freeTeleAst(t, test_allocator);
}

pub fn makeOp(body: []const u8, arg1: *Ast, arg2: *Ast, allocator: std.mem.Allocator) !*Ast {
    var children = std.ArrayList(*Ast).init(allocator);

    try children.append(arg1);
    try children.append(arg2);
    const t = try allocator.create(Ast);
    t.*.body = body;
    t.*.ast_type = AstType.op;
    t.*.children = children;
    t.*.col = 0;
    return t;
}

test "make op" {
    const arg1 = try makeInt(try util.copyString("1", test_allocator), test_allocator);
    const arg2 = try makeInt(try util.copyString("2", test_allocator), test_allocator);
    const t = try makeOp(try util.copyString("+", test_allocator), arg1, arg2, test_allocator);

    try std.testing.expect(std.mem.eql(u8, "+", t.*.body));
    try std.testing.expect(t.*.children.?.items.len == 2);
    try std.testing.expect(t.*.ast_type == AstType.op);
    try std.testing.expect(std.mem.eql(u8, "1", t.*.children.?.items[0].*.body));
    try std.testing.expect(std.mem.eql(u8, "2", t.*.children.?.items[1].*.body));

    freeTeleAst(t, test_allocator);
}

pub fn makeCollection(items: []const *Ast, ast_type: AstType, allocator: std.mem.Allocator) !*Ast {
    var children = std.ArrayList(*Ast).init(allocator);
    try children.appendSlice(items);

    const t = try allocator.create(Ast);
    t.*.body = "";
    t.*.ast_type = ast_type;
    t.*.children = children;
    t.*.col = 0;
    return t;
}

test "make collection" {
    const item1 = try makeInt(try util.copyString("1", test_allocator), test_allocator);
    const item2 = try makeInt(try util.copyString("2", test_allocator), test_allocator);

    const a = [_]*Ast{ item1, item2 };
    const t = try makeCollection(&a, AstType.tuple, test_allocator);
    try std.testing.expect(std.mem.eql(u8, "", t.*.body));
    try std.testing.expect(t.*.children.?.items.len == 2);
    try std.testing.expect(t.*.ast_type == AstType.tuple);
    try std.testing.expect(std.mem.eql(u8, "1", t.*.children.?.items[0].*.body));
    try std.testing.expect(std.mem.eql(u8, "2", t.*.children.?.items[1].*.body));
    freeTeleAst(t, test_allocator);
}

pub fn makeTuple(items: []const *Ast, allocator: std.mem.Allocator) !*Ast {
    return try makeCollection(items, AstType.tuple, allocator);
}

pub fn makeList(items: []const *Ast, allocator: std.mem.Allocator) !*Ast {
    return try makeCollection(items, AstType.list, allocator);
}

pub fn makeMap(items: []const *Ast, allocator: std.mem.Allocator) !*Ast {
    return try makeCollection(items, AstType.map, allocator);
}
