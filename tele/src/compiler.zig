const std = @import("std");
const test_allocator = std.testing.allocator;

const tele_ast = @import("tele/ast.zig");
const TeleAst = tele_ast.Ast;
const TeleAstType = tele_ast.AstType;

const erlang_ast = @import("erlang/ast.zig");
const ErlangAst = erlang_ast.Ast;
const ErlangAstType = erlang_ast.AstType;

const CompilerError = error{CompilingFailure};

pub fn preprocess(ta: *std.ArrayList(*TeleAst), allocator: std.mem.Allocator) !std.ArrayList(*TeleAst) {
    var ta2 = std.ArrayList(*TeleAst).init(allocator);

    // Aggregate function definitions

    for (ta.items) |c| {
        if (c.ast_type == TeleAstType.function_def) {
            const t2 = find_function_definition(c, ta2);
            if (t2 != null) {
                for (c.children.?.items) |c2| {
                    try t2.?.children.?.append(c2);
                }
                c.children.?.deinit();
                allocator.free(c.body);
                allocator.destroy(c);
            } else {
                try ta2.append(c);
            }
        } else {
            try ta2.append(c);
        }
    }

    return ta2;
}

pub fn find_function_definition(fdef: *TeleAst, ta: std.ArrayList(*TeleAst)) ?*TeleAst {
    const name = fdef.*.body;
    const sig1 = fdef.*.children.?.items[0];

    var arg_count: usize = 0;
    if (sig1.*.children != null) {
        arg_count = sig1.*.children.?.items.len;
    }

    var i: usize = 0;
    while (true) {
        if (i >= ta.items.len) {
            break;
        }

        const t = ta.items[i];

        if (t.*.ast_type == TeleAstType.function_def or t.*.ast_type == TeleAstType.function_defp) {
            if (std.mem.eql(u8, name, t.*.body)) {
                const sig2 = t.*.children.?.items[0];
                var arg_count2: usize = 0;
                if (sig2.*.children != null) {
                    arg_count2 = sig2.*.children.?.items.len;
                }

                if (arg_count2 == arg_count) {
                    return t;
                }
            }
        }

        i = i + 1;
    }
    return null;
}

pub fn tele_to_erlang(t: *const TeleAst, allocator: std.mem.Allocator) error{CompilingFailure}!*ErlangAst {
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
            return tele_to_erlang_variable(t, allocator, true) catch {
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
        .fun_val => {
            return tele_to_erlang_fun_val(t, allocator) catch {
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
        .paren_exp => {
            return tele_to_erlang_paren_exp(t, allocator) catch {
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
        .function_defp => {
            return tele_to_erlang_function_def(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .macro_def => {
            return tele_to_erlang_macro_def(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .type_def => {
            return tele_to_erlang_type_def(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .record_def => {
            return tele_to_erlang_record_def(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .record_field => {
            return tele_to_erlang_record_field(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .record_field_value => {
            return tele_to_erlang_record_field_value(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .record_field_type => {
            return tele_to_erlang_record_field_type(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .spec_def => {
            return tele_to_erlang_spec_def(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .callback_def => {
            return tele_to_erlang_callback_def(t, allocator) catch {
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
        .custom_attribute => {
            return tele_to_erlang_custom_attribute(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .try_catch => {
            return tele_to_erlang_try_catch(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .try_exp => {
            return tele_to_erlang_try_exp(t, allocator) catch {
                return CompilerError.CompilingFailure;
            };
        },
        .catch_exp => {
            return tele_to_erlang_catch_exp(t, allocator) catch {
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
    const e = try tele_to_erlang_int(&TeleAst{ .body = "1", .ast_type = TeleAstType.int, .children = null, .col = 0 }, test_allocator);

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
    const e = try tele_to_erlang_float(&TeleAst{ .body = "42.42", .ast_type = TeleAstType.float, .children = null, .col = 0 }, test_allocator);

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
    const e = try tele_to_erlang_atom(&TeleAst{ .body = "'foo", .ast_type = TeleAstType.atom, .children = null, .col = 0 }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "foo", .ast_type = ErlangAstType.atom, .children = null }));

    test_allocator.free(e.*.body);
    test_allocator.destroy(e);

    const e2 = try tele_to_erlang_atom(&TeleAst{ .body = "'Foo'", .ast_type = TeleAstType.atom, .children = null, .col = 0 }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e2, &ErlangAst{ .body = "'Foo'", .ast_type = ErlangAstType.atom, .children = null }));

    test_allocator.free(e2.*.body);
    test_allocator.destroy(e2);

    const e3 = try tele_to_erlang_atom(&TeleAst{ .body = "foo", .ast_type = TeleAstType.atom, .children = null, .col = 0 }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e3, &ErlangAst{ .body = "foo", .ast_type = ErlangAstType.atom, .children = null }));

    test_allocator.free(e3.*.body);
    test_allocator.destroy(e3);
}

fn tele_to_erlang_binary(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.ast_type = ErlangAstType.binary;
    e.*.children = null;
    const buf = try allocator.alloc(u8, t.*.body.len);
    std.mem.copyForwards(u8, buf, t.*.body);
    e.*.body = buf;
    return e;
}

test "tele to erlang binary" {
    const e = try tele_to_erlang_binary(&TeleAst{ .body = "\"foo\"", .ast_type = TeleAstType.binary, .children = null, .col = 0 }, test_allocator);

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "\"foo\"", .ast_type = ErlangAstType.binary, .children = null }));

    test_allocator.free(e.*.body);
    test_allocator.destroy(e);
}

fn tele_to_erlang_variable(t: *const TeleAst, allocator: std.mem.Allocator, capitalize: bool) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.ast_type = ErlangAstType.variable;
    e.*.children = null;

    const buf = try allocator.alloc(u8, t.*.body.len);
    std.mem.copyForwards(u8, buf, t.*.body);

    // TODO: Check if first character is ascii or not
    // If not add V prefix to var (does Erlang even support UTF-8 variables ?)

    if (capitalize) {
        if (buf[0] == '_' and buf.len > 1) {
            if (std.ascii.isLower(buf[1])) {
                buf[1] = std.ascii.toUpper(buf[1]);
            }
        } else if (std.ascii.isLower(buf[0])) {
            buf[0] = std.ascii.toUpper(buf[0]);
        }

        if (contains_dot(buf) and !contains_hash(buf)) {
            var i: usize = 0;
            while (i < buf.len) {
                if (buf[i] == '.') {
                    buf[i] = ':';
                    buf[i + 1] = std.ascii.toUpper(buf[i + 1]);
                    break;
                }
                i = i + 1;
            }
        }
    }

    e.*.body = buf;
    return e;
}

fn contains_dot(buf: []const u8) bool {
    var i: usize = 0;
    while (i < buf.len) {
        if (buf[i] == '.') {
            return true;
        }
        i = i + 1;
    }
    return false;
}

test "tele to erlang variable" {
    const e = try tele_to_erlang_variable(&TeleAst{ .body = "foo", .ast_type = TeleAstType.variable, .children = null, .col = 0 }, test_allocator, true);

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "Foo", .ast_type = ErlangAstType.variable, .children = null }));

    test_allocator.free(e.*.body);
    test_allocator.destroy(e);

    const e2 = try tele_to_erlang_variable(&TeleAst{ .body = "Foo", .ast_type = TeleAstType.variable, .children = null, .col = 0 }, test_allocator, true);

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
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    defer t_children.deinit();

    var t = TeleAst{ .body = "1", .ast_type = TeleAstType.int, .children = null, .col = 0 };
    try t_children.append(&t);

    var t2 = TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null, .col = 0 };
    try t_children.append(&t2);

    const e = try tele_to_erlang_tuple(&TeleAst{ .body = "", .ast_type = TeleAstType.tuple, .children = t_children, .col = 0 }, test_allocator);

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
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    defer t_children.deinit();

    var t = TeleAst{ .body = "1", .ast_type = TeleAstType.int, .children = null, .col = 0 };
    try t_children.append(&t);
    var t2 = TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null, .col = 0 };
    try t_children.append(&t2);

    const e = try tele_to_erlang_list(&TeleAst{ .body = "", .ast_type = TeleAstType.list, .children = t_children, .col = 0 }, test_allocator);

    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    defer e_children.deinit();

    try e_children.append(&ErlangAst{ .body = "1", .ast_type = ErlangAstType.int, .children = null });
    try e_children.append(&ErlangAst{ .body = "2", .ast_type = ErlangAstType.int, .children = null });

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "", .ast_type = ErlangAstType.list, .children = e_children }));

    erlang_ast.destroy(e, test_allocator);
}

fn tele_to_erlang_map(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
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
    e.*.children = std.ArrayList(*const ErlangAst).init(allocator);

    for (t.children.?.items) |c| {
        try e.children.?.append(try tele_to_erlang(c, allocator));
    }

    return e;
}

test "tele to erlang map" {
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    defer t_children.deinit();

    var t = TeleAst{ .body = "1", .ast_type = TeleAstType.int, .children = null, .col = 0 };
    try t_children.append(&t);
    var t2 = TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null, .col = 0 };
    try t_children.append(&t2);

    const e = try tele_to_erlang_map(&TeleAst{ .body = "", .ast_type = TeleAstType.map, .children = t_children, .col = 0 }, test_allocator);

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

    var buf = try allocator.alloc(u8, t.*.body.len);
    std.mem.copyForwards(u8, buf, t.*.body);
    if (contains_hash(buf)) {
        buf[0] = std.ascii.toUpper(buf[0]);
    }

    e.*.body = buf;

    if (t.*.children != null) {
        e.*.children = std.ArrayList(*const ErlangAst).init(allocator);
        for (t.*.children.?.items) |c| {
            try e.*.children.?.append(try tele_to_erlang(c, allocator));
        }
    }

    return e;
}

fn contains_hash(buf: []const u8) bool {
    for (buf) |c| {
        if (c == '#') {
            return true;
        }
    }
    return false;
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

    const e = try tele_to_erlang_record(&TeleAst{ .body = "point", .ast_type = TeleAstType.record, .children = t_children, .col = 0 }, test_allocator);

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

fn tele_to_erlang_fun_val(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
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

fn tele_to_erlang_function_call(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.ast_type = ErlangAstType.function_call;

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

    if (t.children != null) {
        e.*.children = std.ArrayList(*const ErlangAst).init(allocator);
        for (t.children.?.items) |c| {
            try e.children.?.append(try tele_to_erlang(c, allocator));
        }
    } else {
        e.children = null;
    }

    return e;
}

test "tele to erlang function call" {
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    defer t_children.deinit();

    var t = TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null, .col = 0 };
    try t_children.append(&t);

    const e = try tele_to_erlang_function_call(&TeleAst{ .body = "add2", .ast_type = TeleAstType.function_call, .children = t_children, .col = 0 }, test_allocator);

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
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    defer t_children.deinit();

    var t = TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null, .col = 0 };
    try t_children.append(&t);
    var t2 = TeleAst{ .body = "2", .ast_type = TeleAstType.int, .children = null, .col = 0 };
    try t_children.append(&t2);

    const e = try tele_to_erlang_function_call(&TeleAst{ .body = "+", .ast_type = TeleAstType.op, .children = t_children, .col = 0 }, test_allocator);

    var e_children = std.ArrayList(*const ErlangAst).init(test_allocator);
    defer e_children.deinit();

    try e_children.append(&ErlangAst{ .body = "2", .ast_type = ErlangAstType.int, .children = null });
    try e_children.append(&ErlangAst{ .body = "2", .ast_type = ErlangAstType.int, .children = null });

    try std.testing.expect(erlang_ast.equal(e, &ErlangAst{ .body = "+", .ast_type = ErlangAstType.function_call, .children = e_children }));

    erlang_ast.destroy(e, test_allocator);
}

fn tele_to_erlang_paren_exp(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.body = "";
    e.*.ast_type = ErlangAstType.paren_exp;
    e.*.children = std.ArrayList(*const ErlangAst).init(allocator);

    if (!(t.*.children.?.items.len > 0)) {
        e.*.children.?.deinit();
        allocator.destroy(e);
        return CompilerError.CompilingFailure;
    }

    try e.children.?.append(try tele_to_erlang(t.*.children.?.items[0], allocator));

    return e;
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
    var t_children = std.ArrayList(*TeleAst).init(test_allocator);
    defer t_children.deinit();

    var t = TeleAst{ .body = "x", .ast_type = TeleAstType.variable, .children = null, .col = 0 };
    try t_children.append(&t);

    var t_children2 = std.ArrayList(*TeleAst).init(test_allocator);
    defer t_children2.deinit();

    var t2 = TeleAst{ .body = "is_number", .ast_type = TeleAstType.function_call, .children = t_children, .col = 0 };
    try t_children2.append(&t2);

    const e = try tele_to_erlang_guard_clause(&TeleAst{ .body = "", .ast_type = TeleAstType.guard_clause, .children = t_children2, .col = 0 }, test_allocator);

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
    const e = try tele_to_erlang_function_signature(&TeleAst{ .body = "", .ast_type = TeleAstType.function_signature, .children = null, .col = 0 }, test_allocator);

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

fn tele_to_erlang_macro_def(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.ast_type = ErlangAstType.macro_def;
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

fn tele_to_erlang_type_def(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.ast_type = ErlangAstType.type_def;
    e.*.children = std.ArrayList(*const ErlangAst).init(allocator);

    const buf = try allocator.alloc(u8, t.*.body.len);
    std.mem.copyForwards(u8, buf, t.*.body);
    e.*.body = buf;

    for (t.children.?.items) |c| {
        try e.children.?.append(try tele_to_erlang(c, allocator));
    }

    return e;
}

fn tele_to_erlang_record_def(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.ast_type = ErlangAstType.record_def;
    e.*.children = std.ArrayList(*const ErlangAst).init(allocator);

    const buf = try allocator.alloc(u8, t.*.body.len);
    std.mem.copyForwards(u8, buf, t.*.body);
    e.*.body = buf;

    if (t.children != null) {
        for (t.children.?.items) |c| {
            try e.children.?.append(try tele_to_erlang(c, allocator));
        }
    }

    return e;
}

fn tele_to_erlang_record_field(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.ast_type = ErlangAstType.record_field;

    if (t.*.children != null) {
        e.*.children = std.ArrayList(*const ErlangAst).init(allocator);

        for (t.*.children.?.items) |c| {
            try e.*.children.?.append(try tele_to_erlang(c, allocator));
        }
    } else {
        e.*.children = null;
    }

    const buf = try allocator.alloc(u8, t.*.body.len);
    std.mem.copyForwards(u8, buf, t.*.body);
    e.*.body = buf;

    return e;
}

fn tele_to_erlang_record_field_value(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.ast_type = ErlangAstType.record_field_value;
    e.*.children = std.ArrayList(*const ErlangAst).init(allocator);
    e.*.body = "";

    for (t.*.children.?.items) |c| {
        try e.*.children.?.append(try tele_to_erlang(c, allocator));
    }
    return e;
}

fn tele_to_erlang_record_field_type(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.ast_type = ErlangAstType.record_field_type;
    e.*.children = std.ArrayList(*const ErlangAst).init(allocator);
    e.*.body = "";

    for (t.*.children.?.items) |c| {
        try e.*.children.?.append(try tele_to_erlang(c, allocator));
    }
    return e;
}

fn tele_to_erlang_spec_def(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.ast_type = ErlangAstType.spec_def;
    e.*.children = std.ArrayList(*const ErlangAst).init(allocator);

    const buf = try allocator.alloc(u8, t.*.body.len);
    std.mem.copyForwards(u8, buf, t.*.body);
    e.*.body = buf;

    for (t.children.?.items) |c| {
        try e.children.?.append(try tele_to_erlang(c, allocator));
    }

    return e;
}

fn tele_to_erlang_callback_def(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.ast_type = ErlangAstType.callback_def;
    e.*.children = std.ArrayList(*const ErlangAst).init(allocator);

    const buf = try allocator.alloc(u8, t.*.body.len);
    std.mem.copyForwards(u8, buf, t.*.body);
    e.*.body = buf;

    for (t.children.?.items) |c| {
        try e.children.?.append(try tele_to_erlang(c, allocator));
    }

    return e;
}

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

fn tele_to_erlang_try_catch(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.body = "";
    e.*.ast_type = ErlangAstType.try_catch;
    e.*.children = std.ArrayList(*const ErlangAst).init(allocator);

    for (t.children.?.items) |c| {
        try e.children.?.append(try tele_to_erlang(c, allocator));
    }

    return e;
}

test "tele to erlang try catch" {}

fn tele_to_erlang_try_exp(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.body = "";
    e.*.ast_type = ErlangAstType.try_exp;
    e.*.children = std.ArrayList(*const ErlangAst).init(allocator);

    for (t.children.?.items) |c| {
        try e.children.?.append(try tele_to_erlang(c, allocator));
    }

    return e;
}

test "tele to erlang try exp" {}

fn tele_to_erlang_catch_exp(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
    const e = try allocator.create(ErlangAst);
    e.*.body = "";
    e.*.ast_type = ErlangAstType.catch_exp;
    e.*.children = std.ArrayList(*const ErlangAst).init(allocator);

    for (t.children.?.items) |c| {
        try e.children.?.append(try tele_to_erlang(c, allocator));
    }

    return e;
}

test "tele to erlang catch exp" {}

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

fn tele_to_erlang_custom_attribute(t: *const TeleAst, allocator: std.mem.Allocator) !*ErlangAst {
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

    if (t.children != null) {
        e.*.children = std.ArrayList(*const ErlangAst).init(allocator);
        for (t.children.?.items) |c| {
            try e.children.?.append(try tele_to_erlang(c, allocator));
        }
    } else {
        e.children = null;
    }

    return e;
}

test "tele to erlang attribute" {}
