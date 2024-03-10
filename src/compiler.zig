const std = @import("std");
const test_allocator = std.testing.allocator;

const tele_ast = @import("tele/ast.zig");
const TeleAst = tele_ast.Ast;
const TeleAstType = tele_ast.AstType;

const erlang_ast = @import("erlang/ast.zig");
const ErlangAst = erlang_ast.Ast;
const ErlangAstType = erlang_ast.AstType;

const CompilerError = error{CompilingFailure};

fn tele_to_erlang(t: *const TeleAst, allocator: std.mem.Allocator) error{CompilingFailure}!*ErlangAst {
    switch (t.ast_type) {
        .int => {
            return tele_to_erlang_int(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .float => {
            return tele_to_erlang_float(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .atom => {
            return tele_to_erlang_atom(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .binary => {
            return tele_to_erlang_binary(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .variable => {
            return tele_to_erlang_variable(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .tuple => {
            return tele_to_erlang_tuple(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .list => {
            return tele_to_erlang_list(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .map => {
            return tele_to_erlang_map(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .record => {
            return tele_to_erlang_record(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .function_call => {
            return tele_to_erlang_function_call(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .op => {
            return tele_to_erlang_op(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .guard_clause => {
            return tele_to_erlang_guard_clause(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .function_signature => {
            return tele_to_erlang_function_signature(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .anonymous_function => {
            return tele_to_erlang_anonymous_function(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .function_def => {
            return tele_to_erlang_function_def(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .case_clause => {
            return tele_to_erlang_case_clause(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .case => {
            return tele_to_erlang_case(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .attribute => {
            return tele_to_erlang_attribute(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
    }
}

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

fn tele_to_erlang_tuple(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.body = "";
    e.*.ast_type = ErlangAstType.tuple;
    e.*.children = std.ArrayList(*const ErlangAst).init(allocator);

    for (t.children.?.items) |c| {
        try e.children.?.append(try tele_to_erlang(c, allocator));
    }

    return e;
}

test "tele to erlang tuple" {
    var t_children = std.ArrayList(*const TeleAst).init(test_allocator);
    defer t_children.deinit();

    try t_children.append(&TeleAst{ .body = "1", .ast_type = TeleAstType.int, .children = null });
    try t_children.append(&TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null });

    const e = try tele_to_erlang_tuple(&TeleAst{ .body = "", .ast_type = TeleAstType.tuple, .children = t_children }, test_allocator);

    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    defer e_children.deinit();

    try e_children.append(&ErlangAst{ .body = "1", .ast_type = ErlangAstType.int, .children = null });
    try e_children.append(&ErlangAst{ .body = "2", .ast_type = ErlangAstType.int, .children = null });

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "", .ast_type = ErlangAstType.tuple, .children = e_children }));

    erlang_ast.destroy(e, test_allocator);
}

fn tele_to_erlang_list(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.body = "";
    e.*.ast_type = ErlangAstType.list;
    e.*.children = std.ArrayList(*const ErlangAst).init(allocator);

    for (t.children.?.items) |c| {
        try e.children.?.append(try tele_to_erlang(c, allocator));
    }

    return e;
}

test "tele to erlang list" {
    var t_children = std.ArrayList(*const TeleAst).init(test_allocator);
    defer t_children.deinit();

    try t_children.append(&TeleAst{ .body = "1", .ast_type = TeleAstType.int, .children = null });
    try t_children.append(&TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null });

    const e = try tele_to_erlang_list(&TeleAst{ .body = "", .ast_type = TeleAstType.list, .children = t_children }, test_allocator);

    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    defer e_children.deinit();

    try e_children.append(&ErlangAst{ .body = "1", .ast_type = ErlangAstType.int, .children = null });
    try e_children.append(&ErlangAst{ .body = "2", .ast_type = ErlangAstType.int, .children = null });

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "", .ast_type = ErlangAstType.list, .children = e_children }));

    erlang_ast.destroy(e, test_allocator);
}

fn tele_to_erlang_map(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.body = "";
    e.*.ast_type = ErlangAstType.map;
    e.*.children = std.ArrayList(*const ErlangAst).init(allocator);

    for (t.children.?.items) |c| {
        try e.children.?.append(try tele_to_erlang(c, allocator));
    }

    return e;
}

test "tele to erlang map" {
    var t_children = std.ArrayList(*const TeleAst).init(test_allocator);
    defer t_children.deinit();

    try t_children.append(&TeleAst{ .body = "1", .ast_type = TeleAstType.int, .children = null });
    try t_children.append(&TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null });

    const e = try tele_to_erlang_map(&TeleAst{ .body = "", .ast_type = TeleAstType.map, .children = t_children }, test_allocator);

    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    defer e_children.deinit();

    try e_children.append(&ErlangAst{ .body = "1", .ast_type = ErlangAstType.int, .children = null });
    try e_children.append(&ErlangAst{ .body = "2", .ast_type = ErlangAstType.int, .children = null });

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "", .ast_type = ErlangAstType.map, .children = e_children }));

    erlang_ast.destroy(e, test_allocator);
}

fn tele_to_erlang_record(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.ast_type = ErlangAstType.record;
    e.*.children = std.ArrayList(*const ErlangAst).init(allocator);

    const buf = try allocator.alloc(u8, t.*.body.len);
    std.mem.copyForwards(u8, buf, t.*.body);
    e.*.body = buf;

    for (t.children.?.items) |c| {
        try e.children.?.append(try tele_to_erlang(c, allocator));
    }

    return e;
}

test "tele to erlang record" {
    var t_children = std.ArrayList(*const TeleAst).init(test_allocator);
    defer t_children.deinit();

    try t_children.append(&TeleAst{ .body = "x", .ast_type = TeleAstType.atom, .children = null });
    try t_children.append(&TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null });

    const e = try tele_to_erlang_record(&TeleAst{ .body = "point", .ast_type = TeleAstType.record, .children = t_children }, test_allocator);

    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    defer e_children.deinit();

    try e_children.append(&ErlangAst{ .body = "x", .ast_type = ErlangAstType.atom, .children = null });
    try e_children.append(&ErlangAst{ .body = "2", .ast_type = ErlangAstType.int, .children = null });

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "point", .ast_type = ErlangAstType.record, .children = e_children }));

    erlang_ast.destroy(e, test_allocator);
}

fn tele_to_erlang_function_call(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.ast_type = ErlangAstType.function_call;
    e.*.children = std.ArrayList(*const ErlangAst).init(allocator);

    const buf = try allocator.alloc(u8, t.*.body.len);
    std.mem.copyForwards(u8, buf, t.*.body);
    e.*.body = buf;

    for (t.children.?.items) |c| {
        try e.children.?.append(try tele_to_erlang(c, allocator));
    }

    return e;
}

test "tele to erlang function call" {
    var t_children = std.ArrayList(*const TeleAst).init(test_allocator);
    defer t_children.deinit();

    try t_children.append(&TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null });

    const e = try tele_to_erlang_function_call(&TeleAst{ .body = "add2", .ast_type = TeleAstType.function_call, .children = t_children }, test_allocator);

    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    defer e_children.deinit();

    try e_children.append(&ErlangAst{ .body = "2", .ast_type = ErlangAstType.int, .children = null });

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "add2", .ast_type = ErlangAstType.function_call, .children = e_children }));

    erlang_ast.destroy(e, test_allocator);
}

fn tele_to_erlang_op(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.ast_type = ErlangAstType.op;
    e.*.children = std.ArrayList(*const ErlangAst).init(allocator);

    const buf = try allocator.alloc(u8, t.*.body.len);
    std.mem.copyForwards(u8, buf, t.*.body);
    e.*.body = buf;

    for (t.children.?.items) |c| {
        try e.children.?.append(try tele_to_erlang(c, allocator));
    }

    return e;
}

test "tele to erlang op" {
    var t_children = std.ArrayList(*const TeleAst).init(test_allocator);
    defer t_children.deinit();

    try t_children.append(&TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null });
    try t_children.append(&TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null });

    const e = try tele_to_erlang_function_call(&TeleAst{ .body = "+", .ast_type = TeleAstType.op, .children = t_children }, test_allocator);

    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    defer e_children.deinit();

    try e_children.append(&ErlangAst{ .body = "2", .ast_type = ErlangAstType.int, .children = null });
    try e_children.append(&ErlangAst{ .body = "2", .ast_type = ErlangAstType.int, .children = null });

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "+", .ast_type = ErlangAstType.function_call, .children = e_children }));

    erlang_ast.destroy(e, test_allocator);
}

fn tele_to_erlang_guard_clause(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.body = "";
    e.*.ast_type = ErlangAstType.guard_clause;
    e.*.children = std.ArrayList(*const ErlangAst).init(allocator);

    for (t.children.?.items) |c| {
        try e.children.?.append(try tele_to_erlang(c, allocator));
    }

    return e;
}

test "tele to erlang guard clause" {
    var t_children = std.ArrayList(*const TeleAst).init(test_allocator);
    defer t_children.deinit();

    try t_children.append(&TeleAst{ .body = "x", .ast_type = TeleAstType.variable, .children = null });

    var t_children2 = std.ArrayList(*const TeleAst).init(test_allocator);
    defer t_children2.deinit();

    try t_children2.append(&TeleAst{ .body = "is_number", .ast_type = TeleAstType.function_call, .children = t_children });

    const e = try tele_to_erlang_guard_clause(&TeleAst{ .body = "", .ast_type = TeleAstType.guard_clause, .children = t_children2 }, test_allocator);

    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    defer e_children.deinit();

    try e_children.append(&ErlangAst{ .body = "X", .ast_type = ErlangAstType.variable, .children = null });

    var e_children2 = std.ArrayList(*const ErlangAst).init(test_allocator);
    defer e_children2.deinit();

    try e_children2.append(&ErlangAst{ .body = "is_number", .ast_type = ErlangAstType.function_call, .children = e_children });

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "", .ast_type = ErlangAstType.guard_clause, .children = e_children2 }));

    erlang_ast.destroy(e, test_allocator);
}

fn tele_to_erlang_function_signature(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.body = "";
    e.*.ast_type = ErlangAstType.function_signature;

    if (t.children != null) {
        e.*.children = std.ArrayList(*const ErlangAst).init(allocator);

        for (t.children.?.items) |c| {
            try e.children.?.append(try tele_to_erlang(c, allocator));
        }
    } else {
        e.*.children = null;
    }

    return e;
}

test "tele to erlang function signature" {
    const e = try tele_to_erlang_function_signature(&TeleAst{ .body = "", .ast_type = TeleAstType.function_signature, .children = null }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "", .ast_type = ErlangAstType.function_signature, .children = null }));

    erlang_ast.destroy(e, test_allocator);
}

fn tele_to_erlang_anonymous_function(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.body = "";
    e.*.ast_type = ErlangAstType.anonymous_function;
    e.*.children = std.ArrayList(*const ErlangAst).init(allocator);

    for (t.children.?.items) |c| {
        try e.children.?.append(try tele_to_erlang(c, allocator));
    }

    return e;
}

test "tele to erlang anonymous function" {}

fn tele_to_erlang_function_def(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.ast_type = ErlangAstType.function_def;
    e.*.children = std.ArrayList(*const ErlangAst).init(allocator);

    const buf = try allocator.alloc(u8, t.*.body.len);
    std.mem.copyForwards(u8, buf, t.*.body);
    e.*.body = buf;

    for (t.children.?.items) |c| {
        try e.children.?.append(try tele_to_erlang(c, allocator));
    }

    return e;
}

test "tele to erlang function def" {}

fn tele_to_erlang_case_clause(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.body = "";
    e.*.ast_type = ErlangAstType.case_clause;
    e.*.children = std.ArrayList(*const ErlangAst).init(allocator);

    for (t.children.?.items) |c| {
        try e.children.?.append(try tele_to_erlang(c, allocator));
    }

    return e;
}

test "tele to erlang case clause" {}

fn tele_to_erlang_case(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.body = "";
    e.*.ast_type = ErlangAstType.case;
    e.*.children = std.ArrayList(*const ErlangAst).init(allocator);

    for (t.children.?.items) |c| {
        try e.children.?.append(try tele_to_erlang(c, allocator));
    }

    return e;
}

test "tele to erlang case" {}

fn tele_to_erlang_attribute(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.ast_type = ErlangAstType.attribute;
    e.*.children = std.ArrayList(*const ErlangAst).init(allocator);

    const buf = try allocator.alloc(u8, t.*.body.len);
    std.mem.copyForwards(u8, buf, t.*.body);
    e.*.body = buf;

    for (t.children.?.items) |c| {
        try e.children.?.append(try tele_to_erlang(c, allocator));
    }

    return e;
}

test "tele to erlang attribute" {}
