const std = @import("std");
const test_allocator = std.testing.allocator;

const tele_ast = @import("tele/ast.zig");
const TeleAst = tele_ast.Ast;
const TeleAstType = tele_ast.AstType;

const erlang_ast = @import("erlang/ast.zig");
const ErlangAst = erlang_ast.Ast;
const ErlangAstType = erlang_ast.AstType;

fn tele_to_erlang_int(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.ast_type = ErlangAstType.int;
    e.*.children = null;
    const buf = try allocator.alloc(u8, t.body.len);

    std.mem.copyForwards(u8, buf, t.body);

    e.*.body = buf;

    return e;
}

test "tele to erlang int" {
    const e = try tele_to_erlang_int(&TeleAst{ .body = "1", .ast_type = TeleAstType.int, .children = null }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "1", .ast_type = ErlangAstType.int, .children = null }));

    test_allocator.free(e.*.body);
    test_allocator.destroy(e);
}

fn tele_to_erlang_float(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.ast_type = ErlangAstType.float;
    e.*.children = null;
    const buf = try allocator.alloc(u8, t.body.len);

    std.mem.copyForwards(u8, buf, t.body);

    e.*.body = buf;

    return e;
}

test "tele to erlang float" {
    const e = try tele_to_erlang_float(&TeleAst{ .body = "42.42", .ast_type = TeleAstType.float, .children = null }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "42.42", .ast_type = ErlangAstType.float, .children = null }));

    test_allocator.free(e.*.body);
    test_allocator.destroy(e);
}

fn tele_to_erlang_atom(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.ast_type = ErlangAstType.atom;
    e.*.children = null;

    if (t.body[0] == '\'' and t.body[t.body.len - 1] != '\'') {
        const buf = try allocator.alloc(u8, t.body.len - 1);
        std.mem.copyForwards(u8, buf, t.body[1..t.body.len]);
        e.*.body = buf;
    } else {
        const buf = try allocator.alloc(u8, t.body.len);
        std.mem.copyForwards(u8, buf, t.body);
        e.*.body = buf;
    }

    return e;
}

test "tele to erlang atom" {
    const e = try tele_to_erlang_atom(&TeleAst{ .body = "'foo", .ast_type = TeleAstType.atom, .children = null }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "foo", .ast_type = ErlangAstType.atom, .children = null }));

    test_allocator.free(e.*.body);
    test_allocator.destroy(e);

    const e2 = try tele_to_erlang_atom(&TeleAst{ .body = "'Foo'", .ast_type = TeleAstType.atom, .children = null }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e2, &ErlangAst{ .body = "'Foo'", .ast_type = ErlangAstType.atom, .children = null }));

    test_allocator.free(e2.*.body);
    test_allocator.destroy(e2);

    const e3 = try tele_to_erlang_atom(&TeleAst{ .body = "foo", .ast_type = TeleAstType.atom, .children = null }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e3, &ErlangAst{ .body = "foo", .ast_type = ErlangAstType.atom, .children = null }));

    test_allocator.free(e3.*.body);
    test_allocator.destroy(e3);
}

fn tele_to_erlang_binary(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.ast_type = ErlangAstType.binary;
    e.*.children = null;

    if (t.body[0] == '"') {
        const buf = try allocator.alloc(u8, t.*.body.len + 4);
        buf[0] = '<';
        buf[1] = '<';

        var i: usize = 2;
        for (t.*.body) |c| {
            buf[i] = c;
            i = i + 1;
        }

        buf[buf.len - 2] = '>';
        buf[buf.len - 1] = '>';

        e.*.body = buf;
    } else if (t.*.body[0] == '<') {
        const buf = try allocator.alloc(u8, t.*.body.len);
        std.mem.copyForwards(u8, buf, t.*.body);
        e.*.body = buf;
    }

    return e;
}

test "tele to erlang binary" {
    const e = try tele_to_erlang_binary(&TeleAst{ .body = "\"foo\"", .ast_type = TeleAstType.binary, .children = null }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "<<\"foo\">>", .ast_type = ErlangAstType.binary, .children = null }));

    test_allocator.free(e.*.body);
    test_allocator.destroy(e);

    const e2 = try tele_to_erlang_binary(&TeleAst{ .body = "<<\"foo\">>", .ast_type = TeleAstType.binary, .children = null }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e2, &ErlangAst{ .body = "<<\"foo\">>", .ast_type = ErlangAstType.binary, .children = null }));

    test_allocator.free(e2.*.body);
    test_allocator.destroy(e2);
}

fn tele_to_erlang_variable(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.ast_type = ErlangAstType.variable;
    e.*.children = null;

    const buf = try allocator.alloc(u8, t.*.body.len);
    std.mem.copyForwards(u8, buf, t.*.body);

    // TODO: Check if first character is ascii or not
    // If not add V prefix to var (does Erlang even support UTF-8 variables ?)

    if (std.ascii.isLower(buf[0])) {
        buf[0] = std.ascii.toUpper(buf[0]);
    }

    e.*.body = buf;
    return e;
}

test "tele to erlang variable" {
    const e = try tele_to_erlang_variable(&TeleAst{ .body = "foo", .ast_type = TeleAstType.variable, .children = null }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "Foo", .ast_type = ErlangAstType.variable, .children = null }));

    test_allocator.free(e.*.body);
    test_allocator.destroy(e);

    const e2 = try tele_to_erlang_variable(&TeleAst{ .body = "Foo", .ast_type = TeleAstType.variable, .children = null }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e2, &ErlangAst{ .body = "Foo", .ast_type = ErlangAstType.variable, .children = null }));

    test_allocator.free(e2.*.body);
    test_allocator.destroy(e2);
}
