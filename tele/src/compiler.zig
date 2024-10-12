const std = @import("std");
const test_allocator = std.testing.allocator;

const tele_ast = @import("tele_ast.zig");
const TeleAst = tele_ast.Ast;
const TeleAstType = tele_ast.AstType;

const erlang_ast = @import("erlang_ast.zig");
const ErlangAst = erlang_ast.Ast;
const ErlangAstType = erlang_ast.AstType;
const util = @import("util.zig");

const CompilerError = error{CompilingFailure};

pub fn teleToErlang(t: *const TeleAst, allocator: std.mem.Allocator) error{CompilingFailure}!*ErlangAst {
    switch (t.ast_type) {
        .int => {
            return teleToErlangInt(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .float => {
            return teleToErlangFloat(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .atom => {
            return teleToErlangAtom(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .binary => {
            return teleToErlangBinary(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .variable => {
            return teleToErlangVariable(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .tuple => {
            return teleToErlangTuple(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .list => {
            return teleToErlangList(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .map => {
            return teleToErlangMap(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .record => {
            return teleToErlangRecord(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .fun_val => {
            return teleToErlangFunVal(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .function_call => {
            return teleToErlangFunctionCall(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .op => {
            return teleToErlangOp(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .paren_exp => {
            return teleToErlangParenExp(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .guard_clause => {
            return teleToErlangGuardClause(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .function_signature => {
            return teleToErlangFunctionSignature(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .anonymous_function => {
            return teleToErlangAnonymousFunction(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .function_def => {
            return teleToErlangFunctionDef(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .function_defp => {
            return teleToErlangFunctionDef(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .macro_def => {
            return teleToErlangMacroDef(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .opaque_type_def => {
            return teleToErlangOpaqueTypeDef(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .type_def => {
            return teleToErlangTypeDef(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .record_def => {
            return teleToErlangRecordDef(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .record_field => {
            return teleToErlangRecordField(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .record_field_value => {
            return teleToErlangRecordFieldValue(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .record_field_type => {
            return teleToErlangRecordFieldType(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .spec_def => {
            return teleToErlangSpecDef(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .callback_def => {
            return teleToErlangCallbackDef(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .import_def => {
            return teleToErlangImportDef(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .case_clause => {
            return teleToErlangCaseClause(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .case => {
            return teleToErlangCase(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .attribute => {
            return teleToErlangAttribute(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .custom_attribute => {
            return teleToErlangCustomAttribute(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .try_catch => {
            return teleToErlangTryCatch(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .try_exp => {
            return teleToErlangTryExp(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .catch_exp => {
            return teleToErlangCatchExp(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
    }
}

fn teleToErlangInt(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    return try erlang_ast.makeValue(try util.copyString(t.*.body, allocator), ErlangAstType.int, allocator);
}

test "tele to erlang int" {
    const e = try teleToErlangInt(&TeleAst{ .body = "1", .ast_type = TeleAstType.int, .children = null, .col = 0 }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "1", .ast_type = ErlangAstType.int, .children = null }));

    test_allocator.free(e.*.body);
    test_allocator.destroy(e);
}

fn teleToErlangFloat(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    return try erlang_ast.makeValue(try util.copyString(t.*.body, allocator), ErlangAstType.float, allocator);
}

test "tele to erlang float" {
    const e = try teleToErlangFloat(&TeleAst{ .body = "42.42", .ast_type = TeleAstType.float, .children = null, .col = 0 }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "42.42", .ast_type = ErlangAstType.float, .children = null }));

    test_allocator.free(e.*.body);
    test_allocator.destroy(e);
}

fn teleToErlangAtom(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    var buf: []const u8 = undefined;
    if (t.*.body[0] == '#') {
        buf = try util.copyString(t.*.body[1..t.*.body.len], allocator);
    } else if (t.*.body[0] == '\'' and t.*.body[t.*.body.len - 1] != '\'') {
        buf = try util.copyString(t.*.body[1..t.*.body.len], allocator);
    } else {
        buf = try util.copyString(t.*.body, allocator);
    }
    return try erlang_ast.makeValue(buf, ErlangAstType.atom, allocator);
}

test "tele to erlang atom" {
    const e = try teleToErlangAtom(&TeleAst{ .body = "'foo", .ast_type = TeleAstType.atom, .children = null, .col = 0 }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "foo", .ast_type = ErlangAstType.atom, .children = null }));

    test_allocator.free(e.*.body);
    test_allocator.destroy(e);

    const e2 = try teleToErlangAtom(&TeleAst{ .body = "'Foo'", .ast_type = TeleAstType.atom, .children = null, .col = 0 }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e2, &ErlangAst{ .body = "'Foo'", .ast_type = ErlangAstType.atom, .children = null }));

    test_allocator.free(e2.*.body);
    test_allocator.destroy(e2);

    const e3 = try teleToErlangAtom(&TeleAst{ .body = "foo", .ast_type = TeleAstType.atom, .children = null, .col = 0 }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e3, &ErlangAst{ .body = "foo", .ast_type = ErlangAstType.atom, .children = null }));

    test_allocator.free(e3.*.body);
    test_allocator.destroy(e3);

    const e4 = try teleToErlangAtom(&TeleAst{ .body = "#'foo bar'", .ast_type = TeleAstType.atom, .children = null, .col = 0 }, test_allocator);
    try std.testing.expect(erlang_ast.equal(e4, &ErlangAst{ .body = "'foo bar'", .ast_type = ErlangAstType.atom, .children = null }));

    test_allocator.free(e4.*.body);
    test_allocator.destroy(e4);
}

fn teleToErlangBinary(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    return try erlang_ast.makeValue(try util.copyString(t.*.body, allocator), ErlangAstType.binary, allocator);
}

test "tele to erlang binary" {
    const e = try teleToErlangBinary(&TeleAst{ .body = "\"foo\"", .ast_type = TeleAstType.binary, .children = null, .col = 0 }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "\"foo\"", .ast_type = ErlangAstType.binary, .children = null }));

    test_allocator.free(e.*.body);
    test_allocator.destroy(e);

    const e2 = try teleToErlangBinary(&TeleAst{ .body = "<<\"foo\">>", .ast_type = TeleAstType.binary, .children = null, .col = 0 }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e2, &ErlangAst{ .body = "<<\"foo\">>", .ast_type = ErlangAstType.binary, .children = null }));

    test_allocator.free(e2.*.body);
    test_allocator.destroy(e2);
}

fn teleToErlangVariable(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const idx = findDot(t.*.body);

    if (idx > 0 and util.containsHash(t.*.body)) {
        const buf = try allocator.alloc(u8, t.*.body.len);
        std.mem.copyForwards(u8, buf, t.*.body);
        if (std.ascii.isLower(buf[0])) {
            buf[0] = std.ascii.toUpper(buf[0]);
        }

        return try erlang_ast.makeValue(buf, ErlangAstType.variable, allocator);
    } else if (idx > 0) {
        const module_buf = try copyFunctionCallSection(t.*.body[0..idx], allocator);
        const function_buf = try copyFunctionCallSection(t.*.body[idx + 1 ..], allocator);
        var buf = try allocator.alloc(u8, module_buf.len + function_buf.len + 1);
        std.mem.copyForwards(u8, buf, module_buf);
        buf[module_buf.len] = ':';
        std.mem.copyForwards(u8, buf[module_buf.len + 1 ..], function_buf);
        allocator.free(module_buf);
        allocator.free(function_buf);

        return try erlang_ast.makeValue(buf, ErlangAstType.variable, allocator);
    } else {
        if (t.*.body[0] == '@') {
            const buf = try allocator.alloc(u8, t.*.body.len - 1);
            std.mem.copyForwards(u8, buf, t.*.body[1..]);
            if (std.ascii.isLower(buf[0])) {
                buf[0] = std.ascii.toUpper(buf[0]);
            }

            return try erlang_ast.makeValue(buf, ErlangAstType.variable, allocator);
        } else {
            const buf = try allocator.alloc(u8, t.*.body.len);
            std.mem.copyForwards(u8, buf, t.*.body);
            // TODO: Check if first character is ascii or not
            // If not add V prefix to var (does Erlang even support UTF-8 variables ?)

            if (buf[0] == '_' and buf.len > 1) {
                if (std.ascii.isLower(buf[1])) {
                    buf[1] = std.ascii.toUpper(buf[1]);
                }
            } else if (std.ascii.isLower(buf[0])) {
                buf[0] = std.ascii.toUpper(buf[0]);
            }

            return try erlang_ast.makeValue(buf, ErlangAstType.variable, allocator);
        }
    }
}

test "tele to erlang variable" {
    const e = try teleToErlangVariable(&TeleAst{ .body = "foo", .ast_type = TeleAstType.variable, .children = null, .col = 0 }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "Foo", .ast_type = ErlangAstType.variable, .children = null }));

    test_allocator.free(e.*.body);
    test_allocator.destroy(e);

    const e2 = try teleToErlangVariable(&TeleAst{ .body = "Foo", .ast_type = TeleAstType.variable, .children = null, .col = 0 }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e2, &ErlangAst{ .body = "Foo", .ast_type = ErlangAstType.variable, .children = null }));

    test_allocator.free(e2.*.body);
    test_allocator.destroy(e2);

    const e3 = try teleToErlangVariable(&TeleAst{ .body = "a#foo.x", .ast_type = TeleAstType.variable, .children = null, .col = 0 }, test_allocator);
    try std.testing.expect(erlang_ast.equal(e3, &ErlangAst{ .body = "A#foo.x", .ast_type = ErlangAstType.variable, .children = null }));

    test_allocator.free(e3.*.body);
    test_allocator.destroy(e3);

    const e4 = try teleToErlangVariable(&TeleAst{ .body = "foo.bar", .ast_type = TeleAstType.variable, .children = null, .col = 0 }, test_allocator);
    try std.testing.expect(erlang_ast.equal(e4, &ErlangAst{ .body = "foo:bar", .ast_type = ErlangAstType.variable, .children = null }));
    test_allocator.free(e4.*.body);
    test_allocator.destroy(e4);

    const e5 = try teleToErlangVariable(&TeleAst{ .body = "@foo", .ast_type = TeleAstType.variable, .children = null, .col = 0 }, test_allocator);
    try std.testing.expect(erlang_ast.equal(e5, &ErlangAst{ .body = "Foo", .ast_type = ErlangAstType.variable, .children = null }));
    test_allocator.free(e5.*.body);
    test_allocator.destroy(e5);

    const e6 = try teleToErlangVariable(&TeleAst{ .body = "_foo", .ast_type = TeleAstType.variable, .children = null, .col = 0 }, test_allocator);
    try std.testing.expect(erlang_ast.equal(e6, &ErlangAst{ .body = "_Foo", .ast_type = ErlangAstType.variable, .children = null }));
    test_allocator.free(e6.*.body);
    test_allocator.destroy(e6);
}

fn compileChildren(tc: ?std.ArrayList(*TeleAst), allocator: std.mem.Allocator) !?std.ArrayList(*const ErlangAst) {
    if (tc != null) {
        var children = std.ArrayList(*const ErlangAst).init(allocator);
        for (tc.?.items) |c| {
            try children.append(try teleToErlang(c, allocator));
        }
        return children;
    } else {
        return null;
    }
}

fn teleToErlangTuple(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.tuple, allocator);
}

test "tele to erlang tuple" {
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    defer t_children.deinit();

    var t = TeleAst{ .body = "1", .ast_type = TeleAstType.int, .children = null, .col = 0 };
    try t_children.append(&t);

    var t2 = TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null, .col = 0 };
    try t_children.append(&t2);

    const e = try teleToErlangTuple(&TeleAst{ .body = "", .ast_type = TeleAstType.tuple, .children = t_children, .col = 0 }, test_allocator);

    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    defer e_children.deinit();

    try e_children.append(&ErlangAst{ .body = "1", .ast_type = ErlangAstType.int, .children = null });
    try e_children.append(&ErlangAst{ .body = "2", .ast_type = ErlangAstType.int, .children = null });

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "", .ast_type = ErlangAstType.tuple, .children = e_children }));

    erlang_ast.destroy(e, test_allocator);
}

fn teleToErlangList(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.list, allocator);
}

test "tele to erlang list" {
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    defer t_children.deinit();

    var t = TeleAst{ .body = "1", .ast_type = TeleAstType.int, .children = null, .col = 0 };
    try t_children.append(&t);
    var t2 = TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null, .col = 0 };
    try t_children.append(&t2);

    const e = try teleToErlangList(&TeleAst{ .body = "", .ast_type = TeleAstType.list, .children = t_children, .col = 0 }, test_allocator);

    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    defer e_children.deinit();

    try e_children.append(&ErlangAst{ .body = "1", .ast_type = ErlangAstType.int, .children = null });
    try e_children.append(&ErlangAst{ .body = "2", .ast_type = ErlangAstType.int, .children = null });

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "", .ast_type = ErlangAstType.list, .children = e_children }));

    erlang_ast.destroy(e, test_allocator);
}

fn teleToErlangMap(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    if (std.mem.eql(u8, t.*.body, "")) {
        e.*.body = "";
    } else {
        const buf = try allocator.alloc(u8, t.*.body.len);
        std.mem.copyForwards(u8, buf, t.*.body);
        if (std.ascii.isLower(buf[0])) {
            buf[0] = std.ascii.toUpper(buf[0]);
        }
        e.*.body = buf;
    }
    e.*.ast_type = ErlangAstType.map;
    e.*.children = try compileChildren(t.*.children, allocator);

    return e;
}

test "tele to erlang map" {
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    defer t_children.deinit();

    var t = TeleAst{ .body = "1", .ast_type = TeleAstType.int, .children = null, .col = 0 };
    try t_children.append(&t);
    var t2 = TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null, .col = 0 };
    try t_children.append(&t2);

    const e = try teleToErlangMap(&TeleAst{ .body = "", .ast_type = TeleAstType.map, .children = t_children, .col = 0 }, test_allocator);

    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    defer e_children.deinit();

    try e_children.append(&ErlangAst{ .body = "1", .ast_type = ErlangAstType.int, .children = null });
    try e_children.append(&ErlangAst{ .body = "2", .ast_type = ErlangAstType.int, .children = null });

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "", .ast_type = ErlangAstType.map, .children = e_children }));

    erlang_ast.destroy(e, test_allocator);
}

fn teleToErlangRecord(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.ast_type = ErlangAstType.record;

    var buf = try allocator.alloc(u8, t.*.body.len);
    std.mem.copyForwards(u8, buf, t.*.body);
    if (util.containsHash(buf)) {
        buf[0] = std.ascii.toUpper(buf[0]);
    }

    e.*.body = buf;
    e.*.children = try compileChildren(t.*.children, allocator);

    return e;
}

test "tele to erlang record" {
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    defer t_children.deinit();

    var field_children = std.ArrayList(*TeleAst).init(test_allocator);
    defer field_children.deinit();

    var t2 = TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null, .col = 0 };
    try field_children.append(&t2);

    var t_field = TeleAst{ .body = "x", .ast_type = TeleAstType.record_field, .children = field_children, .col = 0 };
    try t_children.append(&t_field);

    const e = try teleToErlangRecord(&TeleAst{ .body = "point", .ast_type = TeleAstType.record, .children = t_children, .col = 0 }, test_allocator);

    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    defer e_children.deinit();

    var e_field_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    defer e_field_children.deinit();
    try e_field_children.append(&ErlangAst{ .body = "2", .ast_type = ErlangAstType.int, .children = null });

    const e_field = ErlangAst{ .body = "x", .ast_type = ErlangAstType.record_field, .children = e_field_children };
    try e_children.append(&e_field);

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "point", .ast_type = ErlangAstType.record, .children = e_children }));

    erlang_ast.destroy(e, test_allocator);
}

fn teleToErlangFunVal(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.ast_type = ErlangAstType.fun_val;
    e.*.children = null;

    var buf = try allocator.alloc(u8, t.*.body.len);
    std.mem.copyForwards(u8, buf, t.*.body);

    var i: usize = 0;
    while (i < buf.len) {
        if (buf[i] == '.') {
            buf[i] = ':';
        }
        i += 1;
    }

    e.*.body = buf;

    return e;
}

fn teleToErlangFunctionCall(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.ast_type = ErlangAstType.function_call;

    const idx = findDot(t.*.body);
    if (idx > 0) {
        const module_buf = try copyFunctionCallSection(t.*.body[0..idx], allocator);
        const function_buf = try copyFunctionCallSection(t.*.body[idx + 1 ..], allocator);
        var buf = try allocator.alloc(u8, module_buf.len + function_buf.len + 1);
        std.mem.copyForwards(u8, buf, module_buf);
        buf[module_buf.len] = ':';
        std.mem.copyForwards(u8, buf[module_buf.len + 1 ..], function_buf);
        allocator.free(module_buf);
        allocator.free(function_buf);

        e.*.body = buf;
    } else {
        const buf = try allocator.alloc(u8, t.*.body.len);
        std.mem.copyForwards(u8, buf, t.*.body);

        e.*.body = buf;
    }

    e.*.children = try compileChildren(t.*.children, allocator);

    return e;
}

fn findDot(buf: []const u8) usize {
    var i: usize = 0;
    while (i < buf.len) {
        if (buf[i] == '.') {
            return i;
        }
        i += 1;
    }
    return 0;
}

fn copyFunctionCallSection(buf: []const u8, allocator: std.mem.Allocator) ![]const u8 {
    if (buf[0] == '@') {
        var buf2 = try allocator.alloc(u8, buf.len - 1);
        std.mem.copyForwards(u8, buf2, buf[1..]);
        if (std.ascii.isLower(buf2[0])) {
            buf2[0] = std.ascii.toUpper(buf2[0]);
        }
        return buf2;
    } else {
        const buf2 = try allocator.alloc(u8, buf.len);
        std.mem.copyForwards(u8, buf2, buf);
        return buf2;
    }
}

test "tele to erlang function call" {
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    defer t_children.deinit();

    var t = TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null, .col = 0 };
    try t_children.append(&t);

    const e = try teleToErlangFunctionCall(&TeleAst{ .body = "add2", .ast_type = TeleAstType.function_call, .children = t_children, .col = 0 }, test_allocator);

    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    defer e_children.deinit();

    try e_children.append(&ErlangAst{ .body = "2", .ast_type = ErlangAstType.int, .children = null });

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "add2", .ast_type = ErlangAstType.function_call, .children = e_children }));

    erlang_ast.destroy(e, test_allocator);
}

fn teleToErlangOp(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try erlang_ast.makeValue(try util.copyString(t.*.body, allocator), ErlangAstType.op, allocator);
    e.*.children = try compileChildren(t.*.children, allocator);
    if (e.*.children == null) {
        erlang_ast.destroy(e, allocator);
        return CompilerError.CompilingFailure;
    }
    return e;
}

test "tele to erlang op" {
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    defer t_children.deinit();

    var t = TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null, .col = 0 };
    try t_children.append(&t);
    var t2 = TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null, .col = 0 };
    try t_children.append(&t2);

    const e = try teleToErlangFunctionCall(&TeleAst{ .body = "+", .ast_type = TeleAstType.op, .children = t_children, .col = 0 }, test_allocator);

    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    defer e_children.deinit();

    try e_children.append(&ErlangAst{ .body = "2", .ast_type = ErlangAstType.int, .children = null });
    try e_children.append(&ErlangAst{ .body = "2", .ast_type = ErlangAstType.int, .children = null });

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "+", .ast_type = ErlangAstType.function_call, .children = e_children }));

    erlang_ast.destroy(e, test_allocator);
}

fn teleToErlangParenExp(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.body = "";
    e.*.ast_type = ErlangAstType.paren_exp;
    e.*.children = std.ArrayList(*const ErlangAst).init(allocator);

    if (!(t.*.children.?.items.len > 0)) {
        erlang_ast.destroy(e, allocator);
        return CompilerError.CompilingFailure;
    }

    try e.children.?.append(try teleToErlang(t.*.children.?.items[0], allocator));

    return e;
}

fn teleToErlangGuardClause(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.guard_clause, allocator);
}

test "tele to erlang guard clause" {
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    defer t_children.deinit();

    var t = TeleAst{ .body = "x", .ast_type = TeleAstType.variable, .children = null, .col = 0 };
    try t_children.append(&t);

    var t_children2 = std.ArrayList(*TeleAst).init(test_allocator);
    defer t_children2.deinit();

    var t2 = TeleAst{ .body = "is_number", .ast_type = TeleAstType.function_call, .children = t_children, .col = 0 };
    try t_children2.append(&t2);

    const e = try teleToErlangGuardClause(&TeleAst{ .body = "", .ast_type = TeleAstType.guard_clause, .children = t_children2, .col = 0 }, test_allocator);

    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    defer e_children.deinit();

    try e_children.append(&ErlangAst{ .body = "X", .ast_type = ErlangAstType.variable, .children = null });

    var e_children2 = std.ArrayList(*const ErlangAst).init(test_allocator);
    defer e_children2.deinit();

    try e_children2.append(&ErlangAst{ .body = "is_number", .ast_type = ErlangAstType.function_call, .children = e_children });

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "", .ast_type = ErlangAstType.guard_clause, .children = e_children2 }));

    erlang_ast.destroy(e, test_allocator);
}

fn teleToErlangFunctionSignature(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.function_signature, allocator);
}

test "tele to erlang function signature" {
    const e = try teleToErlangFunctionSignature(&TeleAst{ .body = "", .ast_type = TeleAstType.function_signature, .children = null, .col = 0 }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "", .ast_type = ErlangAstType.function_signature, .children = null }));

    erlang_ast.destroy(e, test_allocator);
}

fn teleToErlangAnonymousFunction(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.anonymous_function, allocator);
}

test "tele to erlang anonymous function" {}

fn teleToErlangFunctionDef(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeNamedCollection(try util.copyString(t.*.body, allocator), children, ErlangAstType.function_def, allocator);
}

test "tele to erlang function def" {}

fn teleToErlangMacroDef(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeNamedCollection(try util.copyString(t.*.body, allocator), children, ErlangAstType.macro_def, allocator);
}

test "tele to macro def" {}

fn teleToErlangOpaqueTypeDef(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeNamedCollection(try util.copyString(t.*.body, allocator), children, ErlangAstType.type_def, allocator);
}

fn teleToErlangTypeDef(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeNamedCollection(try util.copyString(t.*.body, allocator), children, ErlangAstType.type_def, allocator);
}

fn teleToErlangRecordDef(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeNamedCollection(try util.copyString(t.*.body, allocator), children, ErlangAstType.record_def, allocator);
}

fn teleToErlangRecordField(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeNamedCollection(try util.copyString(t.*.body, allocator), children, ErlangAstType.record_field, allocator);
}

fn teleToErlangRecordFieldValue(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.record_field_value, allocator);
}

fn teleToErlangRecordFieldType(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.record_field_type, allocator);
}

fn teleToErlangSpecDef(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeNamedCollection(try util.copyString(t.*.body, allocator), children, ErlangAstType.spec_def, allocator);
}

fn teleToErlangCallbackDef(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeNamedCollection(try util.copyString(t.*.body, allocator), children, ErlangAstType.callback_def, allocator);
}

fn teleToErlangCaseClause(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.case_clause, allocator);
}

test "tele to erlang case clause" {}

fn teleToErlangCase(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.case, allocator);
}

test "tele to erlang case" {}

fn teleToErlangTryCatch(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.try_catch, allocator);
}

test "tele to erlang try catch" {}

fn teleToErlangTryExp(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.try_exp, allocator);
}

test "tele to erlang try exp" {}

fn teleToErlangCatchExp(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.catch_exp, allocator);
}

test "tele to erlang catch exp" {}

fn teleToErlangImportDef(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeNamedCollection(try util.copyString(t.*.body, allocator), children, ErlangAstType.import_def, allocator);
}

fn teleToErlangAttribute(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeNamedCollection(try util.copyString(t.*.body, allocator), children, ErlangAstType.attribute, allocator);
}

fn teleToErlangCustomAttribute(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.ast_type = ErlangAstType.custom_attribute;

    var buf = try allocator.alloc(u8, t.*.body.len);
    std.mem.copyForwards(u8, buf, t.*.body);

    var i: usize = 0;
    while (i < buf.len) {
        if (buf[i] == '.') {
            buf[i] = ':';
        }
        i += 1;
    }

    e.*.body = buf;
    e.*.children = try compileChildren(t.*.children, allocator);
    return e;
}

test "tele to erlang attribute" {}
