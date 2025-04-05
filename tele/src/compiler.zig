const std = @import("std");
const test_allocator = std.testing.allocator;

const tele_ast = @import("tele_ast.zig");
const TeleAst = tele_ast.Ast;
const TeleAstType = tele_ast.AstType;

const erlang_ast = @import("erlang_ast.zig");
const ErlangAst = erlang_ast.Ast;
const ErlangAstType = erlang_ast.AstType;
const util = @import("util.zig");
const tele_error = @import("error.zig");

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
        .receive_exp => {
            return teleToErlangReceive(t, allocator) catch {
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
        .import_element => {
            return teleToErlangImportElement(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .test_block => {
            return teleToErlangTestBlock(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .test_unit => {
            return teleToErlangTestUnit(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .string => {
            return teleToErlangString(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .binary_element => {
            return teleToErlangBinaryElement(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .binary_element_size => {
            return teleToErlangBinaryElementSize(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .binary_element_type => {
            return teleToErlangBinaryElementType(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .behaviour => {
            return teleToErlangBehaviour(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
    }
}

fn teleToErlangInt(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.int) {
        return CompilerError.CompilingFailure;
    }
    return try erlang_ast.makeValue(try util.copyString(t.*.body, allocator), ErlangAstType.int, allocator);
}

test "tele to erlang int" {
    const e = try teleToErlangInt(&TeleAst{ .body = "1", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "1", .ast_type = ErlangAstType.int, .children = null }));

    test_allocator.free(e.*.body);
    test_allocator.destroy(e);
}

fn teleToErlangFloat(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.float) {
        return CompilerError.CompilingFailure;
    }
    return try erlang_ast.makeValue(try util.copyString(t.*.body, allocator), ErlangAstType.float, allocator);
}

test "tele to erlang float" {
    const e = try teleToErlangFloat(&TeleAst{ .body = "42.42", .ast_type = TeleAstType.float, .children = null, .col = 0, .line = 0 }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "42.42", .ast_type = ErlangAstType.float, .children = null }));

    test_allocator.free(e.*.body);
    test_allocator.destroy(e);
}

fn teleToErlangAtom(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.atom) {
        return CompilerError.CompilingFailure;
    }
    var buf: []const u8 = undefined;
    if (t.*.body[0] == '#') {
        buf = try util.copyString(t.*.body[1..t.*.body.len], allocator);
    } else if (t.*.body[0] == '\'' and t.*.body[t.*.body.len - 1] != '\'' and std.ascii.isUpper(t.*.body[1])) {
        var tmp: []u8 = try allocator.alloc(u8, t.*.body.len + 1);
        std.mem.copyForwards(u8, tmp, t.*.body);
        tmp[tmp.len - 1] = '\'';
        buf = tmp;
    } else if (t.*.body[0] == '\'' and t.*.body[t.*.body.len - 1] != '\'') {
        buf = try util.copyString(t.*.body[1..t.*.body.len], allocator);
    } else if (t.*.body[0] == '\'' and t.*.body[t.*.body.len - 1] == '\'') {
        buf = try util.copyString(t.*.body, allocator);
    } else {
        tele_error.setErrorMessage(t.*.line, t.*.col, tele_error.ErrorType.unexpected_token);
        return CompilerError.CompilingFailure;
    }
    return try erlang_ast.makeValue(buf, ErlangAstType.atom, allocator);
}

test "tele to erlang atom" {
    const e = try teleToErlangAtom(&TeleAst{ .body = "'foo", .ast_type = TeleAstType.atom, .children = null, .col = 0, .line = 0 }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "foo", .ast_type = ErlangAstType.atom, .children = null }));

    test_allocator.free(e.*.body);
    test_allocator.destroy(e);

    const e2 = try teleToErlangAtom(&TeleAst{ .body = "'Foo'", .ast_type = TeleAstType.atom, .children = null, .col = 0, .line = 0 }, test_allocator);
    try std.testing.expect(erlang_ast.equal(e2, &ErlangAst{ .body = "'Foo'", .ast_type = ErlangAstType.atom, .children = null }));

    test_allocator.free(e2.*.body);
    test_allocator.destroy(e2);

    const e4 = try teleToErlangAtom(&TeleAst{ .body = "#'foo bar'", .ast_type = TeleAstType.atom, .children = null, .col = 0, .line = 0 }, test_allocator);
    try std.testing.expect(erlang_ast.equal(e4, &ErlangAst{ .body = "'foo bar'", .ast_type = ErlangAstType.atom, .children = null }));

    test_allocator.free(e4.*.body);
    test_allocator.destroy(e4);

    const e5 = try teleToErlangAtom(&TeleAst{ .body = "'GET", .ast_type = TeleAstType.atom, .children = null, .col = 0, .line = 0 }, test_allocator);
    try std.testing.expect(erlang_ast.equal(e5, &ErlangAst{ .body = "'GET'", .ast_type = ErlangAstType.atom, .children = null }));

    test_allocator.free(e5.*.body);
    test_allocator.destroy(e5);
}

fn teleToErlangBinary(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.binary) {
        return CompilerError.CompilingFailure;
    }
    if (t.*.children == null) {
        return CompilerError.CompilingFailure;
    }
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.binary, allocator);
}

test "tele to erlang binary" {
    const e = try teleToErlangString(&TeleAst{ .body = "\"foo\"", .ast_type = TeleAstType.string, .children = null, .col = 0, .line = 0 }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "\"foo\"", .ast_type = ErlangAstType.string, .children = null }));

    test_allocator.free(e.*.body);
    test_allocator.destroy(e);

    var be_children = std.ArrayList(*TeleAst).init(test_allocator);
    var ts = TeleAst{ .body = "\"foo\"", .ast_type = TeleAstType.string, .children = null, .col = 0, .line = 0 };
    try be_children.append(&ts);

    var be = TeleAst{ .body = "", .ast_type = TeleAstType.binary_element, .children = be_children, .col = 0, .line = 0 };
    var children = std.ArrayList(*TeleAst).init(test_allocator);
    try children.append(&be);

    const e2 = try teleToErlangBinary(&TeleAst{ .body = "<<\"foo\">>", .ast_type = TeleAstType.binary, .children = children, .col = 0, .line = 0 }, test_allocator);

    var ebe_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    const es = ErlangAst{ .body = "\"foo\"", .ast_type = ErlangAstType.string, .children = null };
    try ebe_children.append(&es);

    const ebe = ErlangAst{ .body = "", .ast_type = ErlangAstType.binary_element, .children = ebe_children };
    var echildren = std.ArrayList(*const ErlangAst).init(test_allocator);
    try echildren.append(&ebe);
    try std.testing.expect(erlang_ast.equal(e2, &ErlangAst{ .body = "", .ast_type = ErlangAstType.binary, .children = echildren }));

    erlang_ast.destroy(e2, test_allocator);
    be_children.deinit();
    children.deinit();
    ebe_children.deinit();
    echildren.deinit();
}

fn teleToErlangVariable(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.variable) {
        return CompilerError.CompilingFailure;
    }
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
    const e = try teleToErlangVariable(&TeleAst{ .body = "foo", .ast_type = TeleAstType.variable, .children = null, .col = 0, .line = 0 }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "Foo", .ast_type = ErlangAstType.variable, .children = null }));

    test_allocator.free(e.*.body);
    test_allocator.destroy(e);

    const e2 = try teleToErlangVariable(&TeleAst{ .body = "Foo", .ast_type = TeleAstType.variable, .children = null, .col = 0, .line = 0 }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e2, &ErlangAst{ .body = "Foo", .ast_type = ErlangAstType.variable, .children = null }));

    test_allocator.free(e2.*.body);
    test_allocator.destroy(e2);

    const e3 = try teleToErlangVariable(&TeleAst{ .body = "a#foo.x", .ast_type = TeleAstType.variable, .children = null, .col = 0, .line = 0 }, test_allocator);
    try std.testing.expect(erlang_ast.equal(e3, &ErlangAst{ .body = "A#foo.x", .ast_type = ErlangAstType.variable, .children = null }));

    test_allocator.free(e3.*.body);
    test_allocator.destroy(e3);

    const e4 = try teleToErlangVariable(&TeleAst{ .body = "foo.bar", .ast_type = TeleAstType.variable, .children = null, .col = 0, .line = 0 }, test_allocator);
    try std.testing.expect(erlang_ast.equal(e4, &ErlangAst{ .body = "foo:bar", .ast_type = ErlangAstType.variable, .children = null }));
    test_allocator.free(e4.*.body);
    test_allocator.destroy(e4);

    const e5 = try teleToErlangVariable(&TeleAst{ .body = "@foo", .ast_type = TeleAstType.variable, .children = null, .col = 0, .line = 0 }, test_allocator);
    try std.testing.expect(erlang_ast.equal(e5, &ErlangAst{ .body = "Foo", .ast_type = ErlangAstType.variable, .children = null }));
    test_allocator.free(e5.*.body);
    test_allocator.destroy(e5);

    const e6 = try teleToErlangVariable(&TeleAst{ .body = "_foo", .ast_type = TeleAstType.variable, .children = null, .col = 0, .line = 0 }, test_allocator);
    try std.testing.expect(erlang_ast.equal(e6, &ErlangAst{ .body = "_Foo", .ast_type = ErlangAstType.variable, .children = null }));
    test_allocator.free(e6.*.body);
    test_allocator.destroy(e6);
}

fn compileChildren(tc: ?std.ArrayList(*TeleAst), allocator: std.mem.Allocator) !?std.ArrayList(*const ErlangAst) {
    if (tc != null) {
        var children = std.ArrayList(*const ErlangAst).init(allocator);
        errdefer erlang_ast.free_erlang_ast_list(children, allocator);
        for (tc.?.items) |c| {
            try children.append(try teleToErlang(c, allocator));
        }
        return children;
    } else {
        return null;
    }
}

test "compile children" {
    var l = std.ArrayList(*TeleAst).init(test_allocator);
    var t1 = TeleAst{ .body = "foo", .ast_type = TeleAstType.variable, .children = null, .col = 0, .line = 0 };
    var t2 = TeleAst{ .body = "bar", .ast_type = TeleAstType.variable, .children = null, .col = 0, .line = 0 };
    try l.append(&t1);
    try l.append(&t2);

    const l2 = try compileChildren(l, test_allocator);

    try std.testing.expect(l2 != null);

    var expected = std.ArrayList(*const ErlangAst).init(test_allocator);
    try expected.append(&ErlangAst{ .body = "Foo", .ast_type = ErlangAstType.variable, .children = null });
    try expected.append(&ErlangAst{ .body = "Bar", .ast_type = ErlangAstType.variable, .children = null });

    for (l2.?.items, expected.items) |t, e| {
        try std.testing.expect(erlang_ast.equal(t, e));
    }
    l.deinit();
    expected.deinit();
    erlang_ast.free_erlang_ast_list(l2.?, test_allocator);
}

fn teleToErlangTuple(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.tuple) {
        return CompilerError.CompilingFailure;
    }
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.tuple, allocator);
}

test "tele to erlang tuple" {
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    defer t_children.deinit();

    var t = TeleAst{ .body = "1", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    try t_children.append(&t);

    var t2 = TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    try t_children.append(&t2);

    const e = try teleToErlangTuple(&TeleAst{ .body = "", .ast_type = TeleAstType.tuple, .children = t_children, .col = 0, .line = 0 }, test_allocator);

    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    defer e_children.deinit();

    try e_children.append(&ErlangAst{ .body = "1", .ast_type = ErlangAstType.int, .children = null });
    try e_children.append(&ErlangAst{ .body = "2", .ast_type = ErlangAstType.int, .children = null });

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "", .ast_type = ErlangAstType.tuple, .children = e_children }));

    erlang_ast.destroy(e, test_allocator);
}

fn teleToErlangList(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.list) {
        return CompilerError.CompilingFailure;
    }
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.list, allocator);
}

test "tele to erlang list" {
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    defer t_children.deinit();

    var t = TeleAst{ .body = "1", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    try t_children.append(&t);
    var t2 = TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    try t_children.append(&t2);

    const e = try teleToErlangList(&TeleAst{ .body = "", .ast_type = TeleAstType.list, .children = t_children, .col = 0, .line = 0 }, test_allocator);

    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    defer e_children.deinit();

    try e_children.append(&ErlangAst{ .body = "1", .ast_type = ErlangAstType.int, .children = null });
    try e_children.append(&ErlangAst{ .body = "2", .ast_type = ErlangAstType.int, .children = null });

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "", .ast_type = ErlangAstType.list, .children = e_children }));

    erlang_ast.destroy(e, test_allocator);
}

fn teleToErlangMap(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.map) {
        return CompilerError.CompilingFailure;
    }
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

    var t = TeleAst{ .body = "1", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    try t_children.append(&t);
    var t2 = TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    try t_children.append(&t2);

    const e = try teleToErlangMap(&TeleAst{ .body = "", .ast_type = TeleAstType.map, .children = t_children, .col = 0, .line = 0 }, test_allocator);

    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    defer e_children.deinit();

    try e_children.append(&ErlangAst{ .body = "1", .ast_type = ErlangAstType.int, .children = null });
    try e_children.append(&ErlangAst{ .body = "2", .ast_type = ErlangAstType.int, .children = null });

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "", .ast_type = ErlangAstType.map, .children = e_children }));

    erlang_ast.destroy(e, test_allocator);
}

fn teleToErlangRecord(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.record) {
        return CompilerError.CompilingFailure;
    }
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

    var t2 = TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    try field_children.append(&t2);

    var t_field = TeleAst{ .body = "x", .ast_type = TeleAstType.record_field, .children = field_children, .col = 0, .line = 0 };
    try t_children.append(&t_field);

    const e = try teleToErlangRecord(&TeleAst{ .body = "point", .ast_type = TeleAstType.record, .children = t_children, .col = 0, .line = 0 }, test_allocator);

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
    if (t.*.ast_type != TeleAstType.fun_val) {
        return CompilerError.CompilingFailure;
    }
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

test "tele to erlang function val" {
    var t_fun_val = TeleAst{ .body = "foo/2", .ast_type = TeleAstType.fun_val, .children = null, .col = 0, .line = 0 };
    const e_fun_val = ErlangAst{ .body = "foo/2", .ast_type = ErlangAstType.fun_val, .children = null };
    const result = try teleToErlangFunVal(&t_fun_val, test_allocator);
    try std.testing.expect(erlang_ast.equal(&e_fun_val, result));
    erlang_ast.destroy(result, test_allocator);

    var t_fun_val2 = TeleAst{ .body = "foo.bar/2", .ast_type = TeleAstType.fun_val, .children = null, .col = 0, .line = 0 };
    const e_fun_val2 = ErlangAst{ .body = "foo:bar/2", .ast_type = ErlangAstType.fun_val, .children = null };
    const result2 = try teleToErlangFunVal(&t_fun_val2, test_allocator);
    try std.testing.expect(erlang_ast.equal(&e_fun_val2, result2));
    erlang_ast.destroy(result2, test_allocator);
}

fn teleToErlangFunctionCall(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.function_call) {
        return CompilerError.CompilingFailure;
    }
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
        const buf = try copyFunctionCallSection(t.*.body, allocator);

        e.*.body = buf;
    }

    e.*.children = try compileChildren(t.*.children, allocator);

    return e;
}

test "compile function call" {
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    var t_arg1 = TeleAst{ .body = "1", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    var t_arg2 = TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    try t_children.append(&t_arg1);
    try t_children.append(&t_arg2);

    const t_fcall = TeleAst{ .body = "foo", .ast_type = TeleAstType.function_call, .children = t_children, .col = 0, .line = 0 };

    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    var e_arg1 = ErlangAst{ .body = "1", .ast_type = ErlangAstType.int, .children = null };
    var e_arg2 = ErlangAst{ .body = "2", .ast_type = ErlangAstType.int, .children = null };
    try e_children.append(&e_arg1);
    try e_children.append(&e_arg2);
    const expected = ErlangAst{ .body = "foo", .ast_type = ErlangAstType.function_call, .children = e_children };

    const result = try teleToErlangFunctionCall(&t_fcall, test_allocator);
    try std.testing.expect(erlang_ast.equal(result, &expected));

    const t_fcall2 = TeleAst{ .body = "foo.bar", .ast_type = TeleAstType.function_call, .children = t_children, .col = 0, .line = 0 };
    const expected2 = ErlangAst{ .body = "foo:bar", .ast_type = ErlangAstType.function_call, .children = e_children };

    const result2 = try teleToErlangFunctionCall(&t_fcall2, test_allocator);
    try std.testing.expect(erlang_ast.equal(result2, &expected2));

    const t_fcall3 = TeleAst{ .body = "@foo", .ast_type = TeleAstType.function_call, .children = t_children, .col = 0, .line = 0 };
    const expected3 = ErlangAst{ .body = "Foo", .ast_type = ErlangAstType.function_call, .children = e_children };

    const result3 = try teleToErlangFunctionCall(&t_fcall3, test_allocator);
    try std.testing.expect(erlang_ast.equal(result3, &expected3));

    const t_fcall4 = TeleAst{ .body = "@foo.bar", .ast_type = TeleAstType.function_call, .children = t_children, .col = 0, .line = 0 };
    const expected4 = ErlangAst{ .body = "Foo:bar", .ast_type = ErlangAstType.function_call, .children = e_children };

    const result4 = try teleToErlangFunctionCall(&t_fcall4, test_allocator);
    try std.testing.expect(erlang_ast.equal(result4, &expected4));

    const t_fcall5 = TeleAst{ .body = "@foo.@bar", .ast_type = TeleAstType.function_call, .children = t_children, .col = 0, .line = 0 };
    const expected5 = ErlangAst{ .body = "Foo:Bar", .ast_type = ErlangAstType.function_call, .children = e_children };

    const result5 = try teleToErlangFunctionCall(&t_fcall5, test_allocator);
    try std.testing.expect(erlang_ast.equal(result5, &expected5));

    t_children.deinit();
    e_children.deinit();
    erlang_ast.destroy(result, test_allocator);
    erlang_ast.destroy(result2, test_allocator);
    erlang_ast.destroy(result3, test_allocator);
    erlang_ast.destroy(result4, test_allocator);
    erlang_ast.destroy(result5, test_allocator);
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

test "find dot" {
    const result = findDot("foo.bar");
    try std.testing.expect(result == 3);

    const result2 = findDot("foo");
    try std.testing.expect(result2 == 0);
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

test "copy function call section" {
    const result = try copyFunctionCallSection("@foobar", test_allocator);
    try std.testing.expect(std.mem.eql(u8, result, "Foobar"));
    test_allocator.free(result);

    const result2 = try copyFunctionCallSection("foobar", test_allocator);
    try std.testing.expect(std.mem.eql(u8, result2, "foobar"));
    test_allocator.free(result2);
}

test "tele to erlang function call" {
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    defer t_children.deinit();

    var t = TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    try t_children.append(&t);

    const e = try teleToErlangFunctionCall(&TeleAst{ .body = "add2", .ast_type = TeleAstType.function_call, .children = t_children, .col = 0, .line = 0 }, test_allocator);

    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    defer e_children.deinit();

    try e_children.append(&ErlangAst{ .body = "2", .ast_type = ErlangAstType.int, .children = null });

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "add2", .ast_type = ErlangAstType.function_call, .children = e_children }));

    erlang_ast.destroy(e, test_allocator);
}

fn teleToErlangOp(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.op) {
        return CompilerError.CompilingFailure;
    }
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

    var t = TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    try t_children.append(&t);
    var t2 = TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    try t_children.append(&t2);

    const e = try teleToErlangOp(&TeleAst{ .body = "+", .ast_type = TeleAstType.op, .children = t_children, .col = 0, .line = 0 }, test_allocator);

    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    defer e_children.deinit();

    try e_children.append(&ErlangAst{ .body = "2", .ast_type = ErlangAstType.int, .children = null });
    try e_children.append(&ErlangAst{ .body = "2", .ast_type = ErlangAstType.int, .children = null });

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "+", .ast_type = ErlangAstType.op, .children = e_children }));

    erlang_ast.destroy(e, test_allocator);
}

fn teleToErlangParenExp(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.paren_exp) {
        return CompilerError.CompilingFailure;
    }
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

test "compile paren exp" {
    var t_child1 = TeleAst{ .body = "1", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    var t_child2 = TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };

    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    try t_children.append(&t_child1);
    try t_children.append(&t_child2);

    var t_op = TeleAst{ .body = "+", .ast_type = TeleAstType.op, .children = t_children, .col = 0, .line = 0 };

    var t_children2 = std.ArrayList(*TeleAst).init(test_allocator);
    try t_children2.append(&t_op);

    const t_paren_exp = TeleAst{ .body = "", .ast_type = TeleAstType.paren_exp, .children = t_children2, .col = 0, .line = 0 };

    var e_child1 = ErlangAst{ .body = "1", .ast_type = ErlangAstType.int, .children = null };
    var e_child2 = ErlangAst{ .body = "2", .ast_type = ErlangAstType.int, .children = null };
    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try e_children.append(&e_child1);
    try e_children.append(&e_child2);

    var e_op = ErlangAst{ .body = "+", .ast_type = ErlangAstType.op, .children = e_children };

    var e_children2 = std.ArrayList(*const ErlangAst).init(test_allocator);
    try e_children2.append(&e_op);

    var expected = ErlangAst{ .body = "", .ast_type = ErlangAstType.paren_exp, .children = e_children2 };

    const result = try teleToErlangParenExp(&t_paren_exp, test_allocator);
    try std.testing.expect(erlang_ast.equal(&expected, result));

    t_children.deinit();
    t_children2.deinit();
    e_children.deinit();
    e_children2.deinit();
    erlang_ast.destroy(result, test_allocator);
}

fn teleToErlangGuardClause(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.guard_clause) {
        return CompilerError.CompilingFailure;
    }
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.guard_clause, allocator);
}

test "tele to erlang guard clause" {
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    defer t_children.deinit();

    var t = TeleAst{ .body = "x", .ast_type = TeleAstType.variable, .children = null, .col = 0, .line = 0 };
    try t_children.append(&t);

    var t_children2 = std.ArrayList(*TeleAst).init(test_allocator);
    defer t_children2.deinit();

    var t2 = TeleAst{ .body = "is_number", .ast_type = TeleAstType.function_call, .children = t_children, .col = 0, .line = 0 };
    try t_children2.append(&t2);

    const e = try teleToErlangGuardClause(&TeleAst{ .body = "", .ast_type = TeleAstType.guard_clause, .children = t_children2, .col = 0, .line = 0 }, test_allocator);

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
    if (t.*.ast_type != TeleAstType.function_signature) {
        return CompilerError.CompilingFailure;
    }
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.function_signature, allocator);
}

test "tele to erlang function signature" {
    var t_child1 = TeleAst{ .body = "a", .ast_type = TeleAstType.variable, .children = null, .col = 0, .line = 0 };
    var t_child2 = TeleAst{ .body = "b", .ast_type = TeleAstType.variable, .children = null, .col = 0, .line = 0 };
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    try t_children.append(&t_child1);
    try t_children.append(&t_child2);

    const e_child1 = ErlangAst{ .body = "A", .ast_type = ErlangAstType.variable, .children = null };
    const e_child2 = ErlangAst{ .body = "B", .ast_type = ErlangAstType.variable, .children = null };
    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try e_children.append(&e_child1);
    try e_children.append(&e_child2);
    const e = try teleToErlangFunctionSignature(&TeleAst{ .body = "", .ast_type = TeleAstType.function_signature, .children = t_children, .col = 0, .line = 0 }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "", .ast_type = ErlangAstType.function_signature, .children = e_children }));

    t_children.deinit();
    e_children.deinit();
    erlang_ast.destroy(e, test_allocator);

    // Handle empty children case
    const e2 = try teleToErlangFunctionSignature(&TeleAst{ .body = "", .ast_type = TeleAstType.function_signature, .children = null, .col = 0, .line = 0 }, test_allocator);
    try std.testing.expect(erlang_ast.equal(e2, &ErlangAst{ .body = "", .ast_type = ErlangAstType.function_signature, .children = null }));
    erlang_ast.destroy(e2, test_allocator);
}

fn teleToErlangAnonymousFunction(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.anonymous_function) {
        return CompilerError.CompilingFailure;
    }
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.anonymous_function, allocator);
}

test "tele to erlang anonymous function" {
    var t_child1 = TeleAst{ .body = "", .ast_type = TeleAstType.function_signature, .children = null, .col = 0, .line = 0 };
    var t_child2 = TeleAst{ .body = "42", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    try t_children.append(&t_child1);
    try t_children.append(&t_child2);
    var t = TeleAst{ .body = "", .ast_type = TeleAstType.anonymous_function, .children = t_children, .col = 0, .line = 0 };

    const e_child1 = ErlangAst{ .body = "", .ast_type = ErlangAstType.function_signature, .children = null };
    const e_child2 = ErlangAst{ .body = "42", .ast_type = ErlangAstType.int, .children = null };
    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try e_children.append(&e_child1);
    try e_children.append(&e_child2);
    var e = ErlangAst{ .body = "", .ast_type = ErlangAstType.anonymous_function, .children = e_children };

    const result = try teleToErlangAnonymousFunction(&t, test_allocator);
    try std.testing.expect(erlang_ast.equal(&e, result));

    t_children.deinit();
    e_children.deinit();
    erlang_ast.destroy(result, test_allocator);
}

fn teleToErlangFunctionDef(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.function_def and t.*.ast_type != TeleAstType.function_defp) {
        return CompilerError.CompilingFailure;
    }
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeNamedCollection(try util.copyString(t.*.body, allocator), children, ErlangAstType.function_def, allocator);
}

test "tele to erlang function def" {
    var t_child1 = TeleAst{ .body = "", .ast_type = TeleAstType.function_signature, .children = null, .col = 0, .line = 0 };
    var t_child2 = TeleAst{ .body = "42", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    try t_children.append(&t_child1);
    try t_children.append(&t_child2);
    var t = TeleAst{ .body = "", .ast_type = TeleAstType.function_def, .children = t_children, .col = 0, .line = 0 };

    const e_child1 = ErlangAst{ .body = "", .ast_type = ErlangAstType.function_signature, .children = null };
    const e_child2 = ErlangAst{ .body = "42", .ast_type = ErlangAstType.int, .children = null };
    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try e_children.append(&e_child1);
    try e_children.append(&e_child2);
    var e = ErlangAst{ .body = "", .ast_type = ErlangAstType.function_def, .children = e_children };

    const result = try teleToErlangFunctionDef(&t, test_allocator);
    try std.testing.expect(erlang_ast.equal(&e, result));

    t_children.deinit();
    e_children.deinit();
    erlang_ast.destroy(result, test_allocator);
}

fn teleToErlangMacroDef(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.macro_def) {
        return CompilerError.CompilingFailure;
    }
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeNamedCollection(try util.copyString(t.*.body, allocator), children, ErlangAstType.macro_def, allocator);
}

test "tele to macro def" {
    var t_child1 = TeleAst{ .body = "", .ast_type = TeleAstType.function_signature, .children = null, .col = 0, .line = 0 };
    var t_child2 = TeleAst{ .body = "42", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    try t_children.append(&t_child1);
    try t_children.append(&t_child2);
    var t = TeleAst{ .body = "", .ast_type = TeleAstType.macro_def, .children = t_children, .col = 0, .line = 0 };

    const e_child1 = ErlangAst{ .body = "", .ast_type = ErlangAstType.function_signature, .children = null };
    const e_child2 = ErlangAst{ .body = "42", .ast_type = ErlangAstType.int, .children = null };
    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try e_children.append(&e_child1);
    try e_children.append(&e_child2);
    var e = ErlangAst{ .body = "", .ast_type = ErlangAstType.macro_def, .children = e_children };

    const result = try teleToErlangMacroDef(&t, test_allocator);
    try std.testing.expect(erlang_ast.equal(&e, result));

    t_children.deinit();
    e_children.deinit();
    erlang_ast.destroy(result, test_allocator);
}

fn teleToErlangOpaqueTypeDef(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.opaque_type_def) {
        return CompilerError.CompilingFailure;
    }
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeNamedCollection(try util.copyString(t.*.body, allocator), children, ErlangAstType.opaque_type_def, allocator);
}

test "tele to erlang opaque type def" {
    var t_child1 = TeleAst{ .body = "integer", .ast_type = TeleAstType.function_call, .children = null, .col = 0, .line = 0 };
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    try t_children.append(&t_child1);
    var t = TeleAst{ .body = "", .ast_type = TeleAstType.opaque_type_def, .children = t_children, .col = 0, .line = 0 };

    const e_child1 = ErlangAst{ .body = "integer", .ast_type = ErlangAstType.function_call, .children = null };
    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try e_children.append(&e_child1);
    var e = ErlangAst{ .body = "", .ast_type = ErlangAstType.opaque_type_def, .children = e_children };

    const result = try teleToErlangOpaqueTypeDef(&t, test_allocator);
    try std.testing.expect(erlang_ast.equal(&e, result));

    t_children.deinit();
    e_children.deinit();
    erlang_ast.destroy(result, test_allocator);
}

fn teleToErlangTypeDef(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.type_def) {
        return CompilerError.CompilingFailure;
    }
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeNamedCollection(try util.copyString(t.*.body, allocator), children, ErlangAstType.type_def, allocator);
}

test "tele to erlang type def" {
    var t_child1 = TeleAst{ .body = "integer", .ast_type = TeleAstType.function_call, .children = null, .col = 0, .line = 0 };
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    try t_children.append(&t_child1);
    var t = TeleAst{ .body = "", .ast_type = TeleAstType.type_def, .children = t_children, .col = 0, .line = 0 };

    const e_child1 = ErlangAst{ .body = "integer", .ast_type = ErlangAstType.function_call, .children = null };
    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try e_children.append(&e_child1);
    var e = ErlangAst{ .body = "", .ast_type = ErlangAstType.type_def, .children = e_children };

    const result = try teleToErlangTypeDef(&t, test_allocator);
    try std.testing.expect(erlang_ast.equal(&e, result));

    t_children.deinit();
    e_children.deinit();
    erlang_ast.destroy(result, test_allocator);
}

fn teleToErlangRecordDef(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.record_def) {
        return CompilerError.CompilingFailure;
    }
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeNamedCollection(try util.copyString(t.*.body, allocator), children, ErlangAstType.record_def, allocator);
}

test "tele to erlang record def" {
    var t_child1 = TeleAst{ .body = "foo", .ast_type = TeleAstType.record_field, .children = null, .col = 0, .line = 0 };
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    try t_children.append(&t_child1);
    var t = TeleAst{ .body = "foobar", .ast_type = TeleAstType.record_def, .children = t_children, .col = 0, .line = 0 };

    const e_child1 = ErlangAst{ .body = "foo", .ast_type = ErlangAstType.record_field, .children = null };
    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try e_children.append(&e_child1);
    var e = ErlangAst{ .body = "foobar", .ast_type = ErlangAstType.record_def, .children = e_children };

    const result = try teleToErlangRecordDef(&t, test_allocator);
    try std.testing.expect(erlang_ast.equal(&e, result));

    t_children.deinit();
    e_children.deinit();
    erlang_ast.destroy(result, test_allocator);
}

fn teleToErlangRecordField(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.record_field) {
        return CompilerError.CompilingFailure;
    }
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeNamedCollection(try util.copyString(t.*.body, allocator), children, ErlangAstType.record_field, allocator);
}

test "tele to erlang record field" {
    var t = TeleAst{ .body = "foo", .ast_type = TeleAstType.record_field, .children = null, .col = 0, .line = 0 };
    var e = ErlangAst{ .body = "foo", .ast_type = ErlangAstType.record_field, .children = null };

    const result = try teleToErlangRecordField(&t, test_allocator);
    try std.testing.expect(erlang_ast.equal(&e, result));

    erlang_ast.destroy(result, test_allocator);
}

fn teleToErlangRecordFieldValue(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.record_field_value) {
        return CompilerError.CompilingFailure;
    }
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.record_field_value, allocator);
}

test "tele to erlang record field value" {
    var t = TeleAst{ .body = "42", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    try t_children.append(&t);
    var t_ast = TeleAst{ .body = "", .ast_type = TeleAstType.record_field_value, .children = t_children, .col = 0, .line = 0 };

    const e = ErlangAst{ .body = "42", .ast_type = ErlangAstType.int, .children = null };
    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try e_children.append(&e);
    var e_ast = ErlangAst{ .body = "", .ast_type = ErlangAstType.record_field_value, .children = e_children };

    const result = try teleToErlangRecordFieldValue(&t_ast, test_allocator);
    try std.testing.expect(erlang_ast.equal(&e_ast, result));

    t_children.deinit();
    e_children.deinit();
    erlang_ast.destroy(result, test_allocator);
}

fn teleToErlangRecordFieldType(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.record_field_type) {
        return CompilerError.CompilingFailure;
    }
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.record_field_type, allocator);
}

test "tele to erlang record field type" {
    var t = TeleAst{ .body = "foo", .ast_type = TeleAstType.function_call, .children = null, .col = 0, .line = 0 };
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    try t_children.append(&t);
    const t_ast = TeleAst{ .body = "", .ast_type = TeleAstType.record_field_type, .children = t_children, .col = 0, .line = 0 };

    const e = ErlangAst{ .body = "foo", .ast_type = ErlangAstType.function_call, .children = null };
    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try e_children.append(&e);
    var e_ast = ErlangAst{ .body = "", .ast_type = ErlangAstType.record_field_type, .children = e_children };

    const result = try teleToErlangRecordFieldType(&t_ast, test_allocator);
    try std.testing.expect(erlang_ast.equal(&e_ast, result));

    e_children.deinit();
    t_children.deinit();
    erlang_ast.destroy(result, test_allocator);
}

fn teleToErlangSpecDef(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.spec_def) {
        return CompilerError.CompilingFailure;
    }
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeNamedCollection(try util.copyString(t.*.body, allocator), children, ErlangAstType.spec_def, allocator);
}

test "tele to erlang spec def" {
    var body = TeleAst{ .body = "42", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    var sig = TeleAst{ .body = "", .ast_type = TeleAstType.function_signature, .children = null, .col = 0, .line = 0 };
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    try t_children.append(&sig);
    try t_children.append(&body);
    const t_ast = TeleAst{ .body = "foo", .ast_type = TeleAstType.spec_def, .children = t_children, .col = 0, .line = 0 };

    const e_body = ErlangAst{ .body = "42", .ast_type = ErlangAstType.int, .children = null };
    const e_sig = ErlangAst{ .body = "", .ast_type = ErlangAstType.function_signature, .children = null };
    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try e_children.append(&e_sig);
    try e_children.append(&e_body);
    const e_ast = ErlangAst{ .body = "foo", .ast_type = ErlangAstType.spec_def, .children = e_children };

    const result = try teleToErlangSpecDef(&t_ast, test_allocator);
    try std.testing.expect(erlang_ast.equal(&e_ast, result));

    e_children.deinit();
    t_children.deinit();
    erlang_ast.destroy(result, test_allocator);
}

fn teleToErlangCallbackDef(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.callback_def) {
        return CompilerError.CompilingFailure;
    }
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeNamedCollection(try util.copyString(t.*.body, allocator), children, ErlangAstType.callback_def, allocator);
}

test "tele to erlang callback" {
    var body = TeleAst{ .body = "42", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    var sig = TeleAst{ .body = "", .ast_type = TeleAstType.function_signature, .children = null, .col = 0, .line = 0 };
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    try t_children.append(&sig);
    try t_children.append(&body);
    const t_ast = TeleAst{ .body = "foo", .ast_type = TeleAstType.callback_def, .children = t_children, .col = 0, .line = 0 };

    const e_body = ErlangAst{ .body = "42", .ast_type = ErlangAstType.int, .children = null };
    const e_sig = ErlangAst{ .body = "", .ast_type = ErlangAstType.function_signature, .children = null };
    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try e_children.append(&e_sig);
    try e_children.append(&e_body);
    const e_ast = ErlangAst{ .body = "foo", .ast_type = ErlangAstType.callback_def, .children = e_children };

    const result = try teleToErlangCallbackDef(&t_ast, test_allocator);
    try std.testing.expect(erlang_ast.equal(&e_ast, result));

    e_children.deinit();
    t_children.deinit();
    erlang_ast.destroy(result, test_allocator);
}

fn teleToErlangCaseClause(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.case_clause) {
        return CompilerError.CompilingFailure;
    }
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.case_clause, allocator);
}

test "tele to erlang case clause" {
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    var t_sig = TeleAst{ .body = "42", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    try t_children.append(&t_sig);
    var t_body = TeleAst{ .body = "43", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    try t_children.append(&t_body);
    const t_ast = TeleAst{ .body = "", .ast_type = TeleAstType.case_clause, .children = t_children, .col = 0, .line = 0 };

    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try e_children.append(&ErlangAst{ .body = "42", .ast_type = ErlangAstType.int, .children = null });
    try e_children.append(&ErlangAst{ .body = "43", .ast_type = ErlangAstType.int, .children = null });
    const e_ast = ErlangAst{ .body = "", .ast_type = ErlangAstType.case_clause, .children = e_children };

    const result = try teleToErlangCaseClause(&t_ast, test_allocator);
    try std.testing.expect(erlang_ast.equal(&e_ast, result));

    t_children.deinit();
    e_children.deinit();
    erlang_ast.destroy(result, test_allocator);
}

fn teleToErlangCase(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.case) {
        return CompilerError.CompilingFailure;
    }
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.case, allocator);
}

test "tele to erlang case" {
    var case_sig = TeleAst{ .body = "x", .ast_type = TeleAstType.variable, .children = null, .col = 0, .line = 0 };
    var cc_sig = TeleAst{ .body = "42", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    var cc_body = TeleAst{ .body = "43", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    var cc_children = std.ArrayList(*TeleAst).init(test_allocator);
    try cc_children.append(&cc_sig);
    try cc_children.append(&cc_body);
    var cc = TeleAst{ .body = "", .ast_type = TeleAstType.case_clause, .children = cc_children, .col = 0, .line = 0 };
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    try t_children.append(&case_sig);
    try t_children.append(&cc);
    const t_ast = TeleAst{ .body = "", .ast_type = TeleAstType.case, .children = t_children, .col = 0, .line = 0 };

    const ecase_sig = ErlangAst{ .body = "X", .ast_type = ErlangAstType.variable, .children = null };
    var ecc_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try ecc_children.append(&ErlangAst{ .body = "42", .ast_type = ErlangAstType.int, .children = null });
    try ecc_children.append(&ErlangAst{ .body = "43", .ast_type = ErlangAstType.int, .children = null });
    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try e_children.append(&ecase_sig);
    try e_children.append(&ErlangAst{ .body = "", .ast_type = ErlangAstType.case_clause, .children = ecc_children });
    const e_ast = ErlangAst{ .body = "", .ast_type = ErlangAstType.case, .children = e_children };

    const result = try teleToErlangCase(&t_ast, test_allocator);
    try std.testing.expect(erlang_ast.equal(result, &e_ast));

    cc_children.deinit();
    t_children.deinit();
    ecc_children.deinit();
    e_children.deinit();
    erlang_ast.destroy(result, test_allocator);
}

fn teleToErlangReceive(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.receive_exp) {
        return CompilerError.CompilingFailure;
    }
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.receive_exp, allocator);
}

test "test to erlang receive" {
    var cc_sig = TeleAst{ .body = "42", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    var cc_body = TeleAst{ .body = "43", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    var cc_children = std.ArrayList(*TeleAst).init(test_allocator);
    try cc_children.append(&cc_sig);
    try cc_children.append(&cc_body);
    var cc = TeleAst{ .body = "", .ast_type = TeleAstType.case_clause, .children = cc_children, .col = 0, .line = 0 };
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    try t_children.append(&cc);
    const t_ast = TeleAst{ .body = "", .ast_type = TeleAstType.receive_exp, .children = t_children, .col = 0, .line = 0 };

    var ecc_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try ecc_children.append(&ErlangAst{ .body = "42", .ast_type = ErlangAstType.int, .children = null });
    try ecc_children.append(&ErlangAst{ .body = "43", .ast_type = ErlangAstType.int, .children = null });
    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try e_children.append(&ErlangAst{ .body = "", .ast_type = ErlangAstType.case_clause, .children = ecc_children });
    const e_ast = ErlangAst{ .body = "", .ast_type = ErlangAstType.receive_exp, .children = e_children };

    const result = try teleToErlangReceive(&t_ast, test_allocator);
    try std.testing.expect(erlang_ast.equal(result, &e_ast));

    cc_children.deinit();
    t_children.deinit();
    ecc_children.deinit();
    e_children.deinit();
    erlang_ast.destroy(result, test_allocator);
}

fn teleToErlangTryCatch(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.try_catch) {
        return CompilerError.CompilingFailure;
    }
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.try_catch, allocator);
}

test "tele to erlang try catch" {
    var try_case_sig = TeleAst{ .body = "x", .ast_type = TeleAstType.variable, .children = null, .col = 0, .line = 0 };
    var try_cc_sig = TeleAst{ .body = "42", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    var try_cc_body = TeleAst{ .body = "43", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    var try_cc_children = std.ArrayList(*TeleAst).init(test_allocator);
    try try_cc_children.append(&try_cc_sig);
    try try_cc_children.append(&try_cc_body);
    var try_cc = TeleAst{ .body = "", .ast_type = TeleAstType.case_clause, .children = try_cc_children, .col = 0, .line = 0 };
    var try_t_children = std.ArrayList(*TeleAst).init(test_allocator);
    try try_t_children.append(&try_case_sig);
    try try_t_children.append(&try_cc);
    var try_t_ast = TeleAst{ .body = "", .ast_type = TeleAstType.try_exp, .children = try_t_children, .col = 0, .line = 0 };

    const try_ecase_sig = ErlangAst{ .body = "X", .ast_type = ErlangAstType.variable, .children = null };
    var try_ecc_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try try_ecc_children.append(&ErlangAst{ .body = "42", .ast_type = ErlangAstType.int, .children = null });
    try try_ecc_children.append(&ErlangAst{ .body = "43", .ast_type = ErlangAstType.int, .children = null });
    var try_e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try try_e_children.append(&try_ecase_sig);
    try try_e_children.append(&ErlangAst{ .body = "", .ast_type = ErlangAstType.case_clause, .children = try_ecc_children });
    const try_e_ast = ErlangAst{ .body = "", .ast_type = ErlangAstType.try_exp, .children = try_e_children };

    var catch_cc_sig = TeleAst{ .body = "42", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    var catch_cc_body = TeleAst{ .body = "43", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    var catch_cc_children = std.ArrayList(*TeleAst).init(test_allocator);
    try catch_cc_children.append(&catch_cc_sig);
    try catch_cc_children.append(&catch_cc_body);
    var catch_cc = TeleAst{ .body = "", .ast_type = TeleAstType.case_clause, .children = catch_cc_children, .col = 0, .line = 0 };
    var catch_t_children = std.ArrayList(*TeleAst).init(test_allocator);
    try catch_t_children.append(&catch_cc);
    var catch_t_ast = TeleAst{ .body = "", .ast_type = TeleAstType.catch_exp, .children = catch_t_children, .col = 0, .line = 0 };

    var catch_ecc_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try catch_ecc_children.append(&ErlangAst{ .body = "42", .ast_type = ErlangAstType.int, .children = null });
    try catch_ecc_children.append(&ErlangAst{ .body = "43", .ast_type = ErlangAstType.int, .children = null });
    var catch_e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try catch_e_children.append(&ErlangAst{ .body = "", .ast_type = ErlangAstType.case_clause, .children = catch_ecc_children });
    const catch_e_ast = ErlangAst{ .body = "", .ast_type = ErlangAstType.catch_exp, .children = catch_e_children };

    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    try t_children.append(&try_t_ast);
    try t_children.append(&catch_t_ast);
    var t_ast = TeleAst{ .body = "", .ast_type = TeleAstType.try_catch, .children = t_children, .col = 0, .line = 0 };

    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try e_children.append(&try_e_ast);
    try e_children.append(&catch_e_ast);
    const e_ast = ErlangAst{ .body = "", .ast_type = ErlangAstType.try_catch, .children = e_children };

    const result = try teleToErlangTryCatch(&t_ast, test_allocator);
    try std.testing.expect(erlang_ast.equal(result, &e_ast));

    try_cc_children.deinit();
    try_t_children.deinit();
    try_ecc_children.deinit();
    try_e_children.deinit();
    t_children.deinit();

    catch_cc_children.deinit();
    catch_t_children.deinit();
    catch_ecc_children.deinit();
    catch_e_children.deinit();
    e_children.deinit();

    erlang_ast.destroy(result, test_allocator);
}

fn teleToErlangTryExp(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.try_exp) {
        return CompilerError.CompilingFailure;
    }
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.try_exp, allocator);
}

test "tele to erlang try exp" {
    var case_sig = TeleAst{ .body = "x", .ast_type = TeleAstType.variable, .children = null, .col = 0, .line = 0 };
    var cc_sig = TeleAst{ .body = "42", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    var cc_body = TeleAst{ .body = "43", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    var cc_children = std.ArrayList(*TeleAst).init(test_allocator);
    try cc_children.append(&cc_sig);
    try cc_children.append(&cc_body);
    var cc = TeleAst{ .body = "", .ast_type = TeleAstType.case_clause, .children = cc_children, .col = 0, .line = 0 };
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    try t_children.append(&case_sig);
    try t_children.append(&cc);
    const t_ast = TeleAst{ .body = "", .ast_type = TeleAstType.try_exp, .children = t_children, .col = 0, .line = 0 };

    const ecase_sig = ErlangAst{ .body = "X", .ast_type = ErlangAstType.variable, .children = null };
    var ecc_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try ecc_children.append(&ErlangAst{ .body = "42", .ast_type = ErlangAstType.int, .children = null });
    try ecc_children.append(&ErlangAst{ .body = "43", .ast_type = ErlangAstType.int, .children = null });
    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try e_children.append(&ecase_sig);
    try e_children.append(&ErlangAst{ .body = "", .ast_type = ErlangAstType.case_clause, .children = ecc_children });
    const e_ast = ErlangAst{ .body = "", .ast_type = ErlangAstType.try_exp, .children = e_children };

    const result = try teleToErlangTryExp(&t_ast, test_allocator);
    try std.testing.expect(erlang_ast.equal(result, &e_ast));

    cc_children.deinit();
    t_children.deinit();
    ecc_children.deinit();
    e_children.deinit();
    erlang_ast.destroy(result, test_allocator);
}

fn teleToErlangCatchExp(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.catch_exp) {
        return CompilerError.CompilingFailure;
    }

    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.catch_exp, allocator);
}

test "tele to erlang catch exp" {
    var cc_sig = TeleAst{ .body = "42", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    var cc_body = TeleAst{ .body = "43", .ast_type = TeleAstType.int, .children = null, .col = 0, .line = 0 };
    var cc_children = std.ArrayList(*TeleAst).init(test_allocator);
    try cc_children.append(&cc_sig);
    try cc_children.append(&cc_body);
    var cc = TeleAst{ .body = "", .ast_type = TeleAstType.case_clause, .children = cc_children, .col = 0, .line = 0 };
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    try t_children.append(&cc);
    const t_ast = TeleAst{ .body = "", .ast_type = TeleAstType.catch_exp, .children = t_children, .col = 0, .line = 0 };

    var ecc_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try ecc_children.append(&ErlangAst{ .body = "42", .ast_type = ErlangAstType.int, .children = null });
    try ecc_children.append(&ErlangAst{ .body = "43", .ast_type = ErlangAstType.int, .children = null });
    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try e_children.append(&ErlangAst{ .body = "", .ast_type = ErlangAstType.case_clause, .children = ecc_children });
    const e_ast = ErlangAst{ .body = "", .ast_type = ErlangAstType.catch_exp, .children = e_children };

    const result = try teleToErlangCatchExp(&t_ast, test_allocator);
    try std.testing.expect(erlang_ast.equal(result, &e_ast));

    cc_children.deinit();
    t_children.deinit();
    ecc_children.deinit();
    e_children.deinit();
    erlang_ast.destroy(result, test_allocator);
}

fn teleToErlangBehaviour(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.behaviour) {
        return CompilerError.CompilingFailure;
    }
    return try erlang_ast.makeValue(try util.copyString(t.*.body, allocator), ErlangAstType.behaviour, allocator);
}

fn teleToErlangImportDef(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.import_def) {
        return CompilerError.CompilingFailure;
    }
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeNamedCollection(try util.copyString(t.*.body, allocator), children, ErlangAstType.import_def, allocator);
}

test "tele to erlang import def" {
    var tiel = TeleAst{ .body = "foo/2", .ast_type = TeleAstType.import_element, .children = null, .col = 0, .line = 0 };
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    try t_children.append(&tiel);
    const t_ast = TeleAst{ .body = "barfoo", .ast_type = TeleAstType.import_def, .children = t_children, .col = 0, .line = 0 };

    const eiel = ErlangAst{ .body = "foo/2", .ast_type = ErlangAstType.import_element, .children = null };
    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try e_children.append(&eiel);
    const e_ast = ErlangAst{ .body = "barfoo", .ast_type = ErlangAstType.import_def, .children = e_children };

    const result = try teleToErlangImportDef(&t_ast, test_allocator);
    try std.testing.expect(erlang_ast.equal(result, &e_ast));

    erlang_ast.destroy(result, test_allocator);
    t_children.deinit();
    e_children.deinit();
}

fn teleToErlangImportElement(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.import_element) {
        return CompilerError.CompilingFailure;
    }
    return try erlang_ast.makeValue(try util.copyString(t.*.body, allocator), ErlangAstType.import_element, allocator);
}

test "tele to erlang import element" {
    var t = TeleAst{ .body = "foo/2", .ast_type = TeleAstType.import_element, .children = null, .col = 0, .line = 0 };
    const result = try teleToErlangImportElement(&t, test_allocator);
    try std.testing.expect(erlang_ast.equal(result, &ErlangAst{ .body = "foo/2", .ast_type = ErlangAstType.import_element, .children = null }));

    erlang_ast.destroy(result, test_allocator);
}

fn teleToErlangAttribute(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.attribute) {
        return CompilerError.CompilingFailure;
    }
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeNamedCollection(try util.copyString(t.*.body, allocator), children, ErlangAstType.attribute, allocator);
}

test "tele to erlang attribute" {
    var include_s = TeleAst{ .body = "\"include/foo.hrl\"", .ast_type = TeleAstType.string, .children = null, .col = 0, .line = 0 };
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    try t_children.append(&include_s);
    var include_t = TeleAst{ .body = "include", .ast_type = TeleAstType.attribute, .children = t_children, .col = 0, .line = 0 };

    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try e_children.append(&ErlangAst{ .body = "\"include/foo.hrl\"", .ast_type = ErlangAstType.string, .children = null });
    var include_e = ErlangAst{ .body = "include", .ast_type = ErlangAstType.attribute, .children = e_children };

    const result = try teleToErlangAttribute(&include_t, test_allocator);
    try std.testing.expect(erlang_ast.equal(result, &include_e));

    t_children.deinit();
    e_children.deinit();
    erlang_ast.destroy(result, test_allocator);
}

fn teleToErlangCustomAttribute(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.custom_attribute) {
        return CompilerError.CompilingFailure;
    }
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

test "tele to erlang custom attribute" {
    var attr_s = TeleAst{ .body = "\"stuff\"", .ast_type = TeleAstType.string, .children = null, .col = 0, .line = 0 };
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    try t_children.append(&attr_s);
    var attr_t = TeleAst{ .body = "foo.bar", .ast_type = TeleAstType.custom_attribute, .children = t_children, .col = 0, .line = 0 };

    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    try e_children.append(&ErlangAst{ .body = "\"stuff\"", .ast_type = ErlangAstType.string, .children = null });
    var attr_e = ErlangAst{ .body = "foo:bar", .ast_type = ErlangAstType.custom_attribute, .children = e_children };

    const result = try teleToErlangCustomAttribute(&attr_t, test_allocator);
    try std.testing.expect(erlang_ast.equal(result, &attr_e));

    t_children.deinit();
    e_children.deinit();
    erlang_ast.destroy(result, test_allocator);
}

fn teleToErlangTestBlock(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.test_block) {
        return CompilerError.CompilingFailure;
    }

    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.test_block, allocator);
}

fn teleToErlangTestUnit(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.test_unit) {
        return CompilerError.CompilingFailure;
    }

    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeNamedCollection(try util.copyString(t.*.body, allocator), children, ErlangAstType.test_unit, allocator);
}

fn teleToErlangString(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.string) {
        return CompilerError.CompilingFailure;
    }

    return try erlang_ast.makeValue(try util.copyString(t.*.body, allocator), ErlangAstType.string, allocator);
}

fn teleToErlangBinaryElement(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.binary_element) {
        return CompilerError.CompilingFailure;
    }
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.binary_element, allocator);
}

fn teleToErlangBinaryElementSize(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.binary_element_size) {
        return CompilerError.CompilingFailure;
    }
    const children = try compileChildren(t.*.children, allocator);
    return try erlang_ast.makeCollection(children, ErlangAstType.binary_element_size, allocator);
}

fn teleToErlangBinaryElementType(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    if (t.*.ast_type != TeleAstType.binary_element_type) {
        return CompilerError.CompilingFailure;
    }

    return try erlang_ast.makeValue(try util.copyString(t.*.body, allocator), ErlangAstType.binary_element_type, allocator);
}
