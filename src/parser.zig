const std = @import("std");
const test_allocator = std.testing.allocator;
const tokenizer = @import("tokenizer.zig");
const TokenQueue = tokenizer.TokenQueue;
const tele_ast = @import("tele/ast.zig");
const TeleAst = tele_ast.Ast;
const TeleAstType = tele_ast.AstType;

const ParserError = error{ ParsingFailure, TokenFailure };

const ParserMode = enum { none, op };

pub fn parse_reader(r: anytype, allocator: std.mem.Allocator) !std.ArrayList(*TeleAst) {
    const token_queue = try tokenizer.read_tokens(r, allocator);
    errdefer token_queue.deinit();

    const result = try parse_tokens(token_queue, allocator);
    if (!token_queue.empty()) {
        return ParserError.ParsingFailure;
    }

    token_queue.deinit();

    return result;
}

fn parse_tokens(token_queue: *TokenQueue, allocator: std.mem.Allocator) ParserError!std.ArrayList(*TeleAst) {
    var list = std.ArrayList(*TeleAst).init(allocator);
    errdefer tele_ast.free_tele_ast_list(list, allocator);

    while (!token_queue.empty()) {
        const node = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };
        errdefer allocator.destroy(node);

        if (is_float(node.*.body)) {
            const t = parse_float(node.*.body, allocator) catch {
                return ParserError.ParsingFailure;
            };
            errdefer tele_ast.free_tele_ast(t, allocator);
            list.append(t) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_int(node.*.body)) {
            const t = parse_int(node.*.body, allocator) catch {
                return ParserError.ParsingFailure;
            };
            errdefer tele_ast.free_tele_ast(t, allocator);
            list.append(t) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_atom(node.*.body)) {
            const t = parse_atom(node.*.body, allocator) catch {
                return ParserError.ParsingFailure;
            };
            errdefer tele_ast.free_tele_ast(t, allocator);
            list.append(t) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_binary(node.*.body)) {
            const t = parse_binary(node.*.body, allocator) catch {
                return ParserError.ParsingFailure;
            };
            errdefer tele_ast.free_tele_ast(t, allocator);
            list.append(t) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_tuple_start(node.*.body)) {
            // Body not needed
            allocator.free(node.*.body);
            const t = parse_tuple(token_queue, allocator) catch {
                return ParserError.ParsingFailure;
            };
            errdefer tele_ast.free_tele_ast(t, allocator);
            list.append(t) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_list_start(node.*.body)) {
            allocator.free(node.*.body);
            const t = parse_list(token_queue, allocator) catch {
                return ParserError.ParsingFailure;
            };
            errdefer tele_ast.free_tele_ast(t, allocator);
            list.append(t) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_map_start(node.*.body)) {
            allocator.free(node.*.body);
            const t = parse_map(token_queue, allocator) catch {
                return ParserError.ParsingFailure;
            };
            errdefer tele_ast.free_tele_ast(t, allocator);
            list.append(t) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_operator(node.*.body)) {
            if (list.items.len == 0) {
                // TODO: Error expected value on left side of operator
                return ParserError.ParsingFailure;
            }
            const ast1 = list.pop();
            errdefer tele_ast.free_tele_ast(ast1, allocator);
            const t = parse_operator(node.*.body, token_queue, allocator, ast1) catch {
                return ParserError.ParsingFailure;
            };
            errdefer tele_ast.free_tele_ast(t, allocator);
            list.append(t) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_keyword(node.*.body)) {
            if (is_function_definition(node.*.body)) {
                allocator.free(node.*.body);
                const t = parse_function_definition(token_queue, allocator, node.*.col) catch {
                    return ParserError.ParsingFailure;
                };
                errdefer tele_ast.free_tele_ast(t, allocator);
                list.append(t) catch {
                    return ParserError.ParsingFailure;
                };
            } else {
                return ParserError.ParsingFailure;
            }
        } else {
            // Check if function call
            if (!token_queue.empty() and try check_paren_start_peek(token_queue)) {
                const t = parse_function_call(node.*.body, token_queue, allocator) catch {
                    return ParserError.ParsingFailure;
                };
                errdefer tele_ast.free_tele_ast(t, allocator);

                list.append(t) catch {
                    return ParserError.ParsingFailure;
                };
            } else {
                const t = parse_variable(node.*.body, allocator) catch {
                    return ParserError.ParsingFailure;
                };
                errdefer tele_ast.free_tele_ast(t, allocator);

                list.append(t) catch {
                    return ParserError.ParsingFailure;
                };
            }
        }

        allocator.destroy(node);
    }

    return list;
}

fn parse_operator(body: []const u8, token_queue: *TokenQueue, allocator: std.mem.Allocator, arg: *TeleAst) !*TeleAst {
    var alist = parse_tokens(token_queue, allocator) catch {
        return ParserError.ParsingFailure;
    };
    errdefer tele_ast.free_tele_ast_list(alist, allocator);
    if (alist.items.len != 1) {
        return ParserError.ParsingFailure;
    }

    const ast2 = alist.pop();
    errdefer tele_ast.free_tele_ast(ast2, allocator);
    defer alist.deinit();

    var children = std.ArrayList(*TeleAst).init(allocator);
    errdefer tele_ast.free_tele_ast_list(children, allocator);
    try children.append(arg);
    try children.append(ast2);

    const t = try allocator.create(TeleAst);
    t.*.body = body;
    t.*.ast_type = TeleAstType.op;
    t.*.children = children;

    return t;
}

test "parse operator expression" {
    const file = try std.fs.cwd().openFile(
        "snippets/op.tl",
        .{ .mode = .read_only },
    );
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), test_allocator);
    defer token_queue.deinit();

    var result = try parse_tokens(token_queue, test_allocator);
    defer result.deinit();
    try std.testing.expect(result.items.len == 1);

    const a = result.pop();
    try std.testing.expect(a.*.ast_type == TeleAstType.op);
    try std.testing.expect(std.mem.eql(u8, a.*.body, "+"));
    try std.testing.expect(a.children.?.items.len == 2);

    const c = a.children.?;
    try std.testing.expect(c.items[0].ast_type == TeleAstType.int);
    try std.testing.expect(std.mem.eql(u8, c.items[0].body, "1"));
    try std.testing.expect(c.items[0].children == null);

    try std.testing.expect(c.items[1].ast_type == TeleAstType.int);
    try std.testing.expect(std.mem.eql(u8, c.items[1].body, "2"));
    try std.testing.expect(c.items[1].children == null);

    test_allocator.free(a.*.body);
    tele_ast.free_tele_ast_list(a.children.?, test_allocator);
    test_allocator.destroy(a);
}

test "parse operator expression chained" {
    const file = try std.fs.cwd().openFile(
        "snippets/op2.tl",
        .{ .mode = .read_only },
    );
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), test_allocator);
    defer token_queue.deinit();

    var result = try parse_tokens(token_queue, test_allocator);
    defer result.deinit();
    try std.testing.expect(result.items.len == 1);

    const a = result.pop();
    try std.testing.expect(a.*.ast_type == TeleAstType.op);
    try std.testing.expect(std.mem.eql(u8, a.*.body, "="));
    try std.testing.expect(a.children.?.items.len == 2);

    const c = a.children.?;
    try std.testing.expect(c.items[0].ast_type == TeleAstType.variable);
    try std.testing.expect(std.mem.eql(u8, c.items[0].body, "a"));
    try std.testing.expect(c.items[0].children == null);

    try std.testing.expect(c.items[1].ast_type == TeleAstType.op);
    try std.testing.expect(std.mem.eql(u8, c.items[1].body, "+"));
    try std.testing.expect(c.items[1].children.?.items.len == 2);

    const c2 = c.items[1].children.?;
    try std.testing.expect(c2.items[0].ast_type == TeleAstType.int);
    try std.testing.expect(std.mem.eql(u8, c2.items[0].body, "1"));
    try std.testing.expect(c2.items[0].children == null);

    try std.testing.expect(c2.items[1].ast_type == TeleAstType.int);
    try std.testing.expect(std.mem.eql(u8, c2.items[1].body, "2"));
    try std.testing.expect(c2.items[1].children == null);

    test_allocator.free(a.*.body);
    tele_ast.free_tele_ast_list(a.children.?, test_allocator);
    test_allocator.destroy(a);
}

fn check_paren_start_peek(token_queue: *TokenQueue) !bool {
    const peek_node = token_queue.peek() catch {
        return ParserError.ParsingFailure;
    };
    return is_paren_start(peek_node.*.body);
}

fn parse_float(body: []const u8, allocator: std.mem.Allocator) !*TeleAst {
    return try parse_value(body, allocator, TeleAstType.float);
}

fn parse_int(body: []const u8, allocator: std.mem.Allocator) !*TeleAst {
    return try parse_value(body, allocator, TeleAstType.int);
}

fn parse_atom(body: []const u8, allocator: std.mem.Allocator) !*TeleAst {
    return try parse_value(body, allocator, TeleAstType.atom);
}

fn parse_binary(body: []const u8, allocator: std.mem.Allocator) !*TeleAst {
    return try parse_value(body, allocator, TeleAstType.binary);
}

fn parse_variable(body: []const u8, allocator: std.mem.Allocator) !*TeleAst {
    return try parse_value(body, allocator, TeleAstType.variable);
}

fn parse_value(body: []const u8, allocator: std.mem.Allocator, ast_type: TeleAstType) !*TeleAst {
    const t = try allocator.create(TeleAst);
    t.*.body = body;
    t.*.ast_type = ast_type;
    t.*.children = null;
    return t;
}

fn parse_tuple(token_queue: *TokenQueue, allocator: std.mem.Allocator) !*TeleAst {
    var children = std.ArrayList(*TeleAst).init(allocator);
    errdefer tele_ast.free_tele_ast_list(children, allocator);
    var token_queue2 = try TokenQueue.init(allocator);
    errdefer token_queue2.deinit();

    // Expected to have already parsed start of tuple, so count starts at 1
    var count: usize = 1;
    var end_of_tuple = false;

    while (!token_queue.empty() and !end_of_tuple) {
        while (!token_queue.empty()) {
            const node2 = try token_queue.pop();
            errdefer allocator.destroy(node2);

            if (is_tuple_start(node2.*.body) or is_paren_start(node2.*.body)) {
                count += 1;
            } else if (is_paren_end(node2.*.body)) {
                count -= 1;
                if (count == 0) {
                    // Free paren end body
                    allocator.free(node2.*.body);
                    allocator.destroy(node2);
                    end_of_tuple = true;
                    break;
                }
            }

            if (count == 1 and is_comma(node2.*.body)) {
                allocator.free(node2.*.body);
                allocator.destroy(node2);
                break;
            } else {
                try token_queue2.push(node2.*.body, node2.*.line, node2.*.col);
            }

            allocator.destroy(node2);
        }

        var alist = try parse_tokens(token_queue2, allocator);
        errdefer tele_ast.free_tele_ast_list(alist, allocator);
        if (!token_queue2.empty()) {
            return ParserError.ParsingFailure;
        }

        if (alist.items.len != 1) {
            return ParserError.ParsingFailure;
        }
        if (alist.items.len != 1) {
            return ParserError.ParsingFailure;
        }
        const a = alist.pop();
        errdefer tele_ast.free_tele_ast(a, allocator);
        try children.append(a);
        alist.deinit();
    }

    if (count != 0) {
        return ParserError.ParsingFailure;
    }

    const t = try allocator.create(TeleAst);
    t.*.body = "";
    t.*.ast_type = TeleAstType.tuple;
    t.*.children = children;
    token_queue2.deinit();
    return t;
}

test "parse tuple" {
    const file = try std.fs.cwd().openFile(
        "snippets/tuple.tl",
        .{ .mode = .read_only },
    );
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), test_allocator);
    defer token_queue.deinit();

    const result = try parse_tuple(token_queue, test_allocator);
    try std.testing.expect(result.*.ast_type == TeleAstType.tuple);
    try std.testing.expect(std.mem.eql(u8, result.*.body, ""));
    try std.testing.expect(result.*.children.?.items.len == 3);

    const c = result.*.children.?;
    try std.testing.expect(c.items[0].ast_type == TeleAstType.int);
    try std.testing.expect(std.mem.eql(u8, c.items[0].body, "1"));
    try std.testing.expect(c.items[0].children == null);
    try std.testing.expect(c.items[1].ast_type == TeleAstType.int);
    try std.testing.expect(std.mem.eql(u8, c.items[1].body, "2"));
    try std.testing.expect(c.items[1].children == null);
    try std.testing.expect(c.items[2].ast_type == TeleAstType.int);
    try std.testing.expect(std.mem.eql(u8, c.items[2].body, "3"));
    try std.testing.expect(c.items[2].children == null);

    tele_ast.free_tele_ast_list(result.children.?, test_allocator);
    test_allocator.destroy(result);
}

fn parse_list(token_queue: *TokenQueue, allocator: std.mem.Allocator) !*TeleAst {
    var children = std.ArrayList(*TeleAst).init(allocator);
    errdefer tele_ast.free_tele_ast_list(children, allocator);
    var token_queue2 = try TokenQueue.init(allocator);
    errdefer token_queue2.deinit();

    // Expected to have already parsed start of list so count starts at 1
    var count: usize = 1;
    var end_of_list = false;

    while (!token_queue.empty() and !end_of_list) {
        while (!token_queue.empty()) {
            const node2 = try token_queue.pop();
            errdefer allocator.destroy(node2);

            if (is_list_start(node2.*.body)) {
                count += 1;
            } else if (is_list_end(node2.*.body)) {
                count -= 1;
                if (count == 0) {
                    allocator.free(node2.*.body);
                    allocator.destroy(node2);
                    end_of_list = true;
                    break;
                }
            }

            if (count == 1 and is_comma(node2.*.body)) {
                allocator.free(node2.*.body);
                allocator.destroy(node2);
                break;
            } else {
                try token_queue2.push(node2.*.body, node2.*.line, node2.*.col);
            }

            allocator.destroy(node2);
        }

        var alist = try parse_tokens(token_queue2, allocator);
        errdefer tele_ast.free_tele_ast_list(alist, allocator);
        if (!token_queue2.empty()) {
            return ParserError.ParsingFailure;
        }
        if (alist.items.len != 1) {
            return ParserError.ParsingFailure;
        }
        const a = alist.pop();
        errdefer tele_ast.free_tele_ast(a, allocator);
        try children.append(a);
        alist.deinit();
    }

    if (count != 0) {
        return ParserError.ParsingFailure;
    }

    const t = try allocator.create(TeleAst);
    t.*.body = "";
    t.*.ast_type = TeleAstType.list;
    t.*.children = children;
    token_queue2.deinit();
    return t;
}

test "parse list" {
    const file = try std.fs.cwd().openFile(
        "snippets/list.tl",
        .{ .mode = .read_only },
    );
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), test_allocator);
    defer token_queue.deinit();

    const result = try parse_list(token_queue, test_allocator);
    try std.testing.expect(result.*.ast_type == TeleAstType.list);
    try std.testing.expect(std.mem.eql(u8, result.*.body, ""));
    try std.testing.expect(result.*.children.?.items.len == 3);

    const c = result.*.children.?;
    try std.testing.expect(c.items[0].ast_type == TeleAstType.int);
    try std.testing.expect(std.mem.eql(u8, c.items[0].body, "1"));
    try std.testing.expect(c.items[0].children == null);
    try std.testing.expect(c.items[1].ast_type == TeleAstType.int);
    try std.testing.expect(std.mem.eql(u8, c.items[1].body, "2"));
    try std.testing.expect(c.items[1].children == null);
    try std.testing.expect(c.items[2].ast_type == TeleAstType.int);
    try std.testing.expect(std.mem.eql(u8, c.items[2].body, "3"));
    try std.testing.expect(c.items[2].children == null);

    tele_ast.free_tele_ast_list(result.children.?, test_allocator);
    test_allocator.destroy(result);
}

fn parse_map(token_queue: *TokenQueue, allocator: std.mem.Allocator) !*TeleAst {
    var children = std.ArrayList(*TeleAst).init(allocator);
    errdefer tele_ast.free_tele_ast_list(children, allocator);
    var token_queue2 = try TokenQueue.init(allocator);
    errdefer token_queue2.deinit();
    var count: usize = 1;
    var end_of_map: bool = false;

    while (!token_queue.empty() and !end_of_map) {
        while (!token_queue.empty()) {
            const node2 = token_queue.pop() catch {
                return ParserError.ParsingFailure;
            };
            errdefer allocator.free(node2.*.body);
            errdefer allocator.destroy(node2);

            if (is_map_start(node2.*.body)) {
                count += 1;
            } else if (is_map_end(node2.*.body)) {
                count -= 1;
                if (count == 0) {
                    allocator.free(node2.*.body);
                    allocator.destroy(node2);
                    end_of_map = true;
                    break;
                }
            }

            if (count == 1 and (is_comma(node2.*.body) or is_colon(node2.*.body))) {
                allocator.free(node2.*.body);
                allocator.destroy(node2);
                break;
            } else {
                try token_queue2.push(node2.*.body, node2.*.line, node2.*.col);
            }

            allocator.destroy(node2);
        }

        var alist = parse_tokens(token_queue2, allocator) catch {
            return ParserError.ParsingFailure;
        };
        errdefer tele_ast.free_tele_ast_list(alist, allocator);
        if (!token_queue2.empty()) {
            return ParserError.ParsingFailure;
        }

        if (alist.items.len != 1) {
            return ParserError.ParsingFailure;
        }
        const a = alist.pop();
        errdefer tele_ast.free_tele_ast(a, allocator);
        try children.append(a);
        alist.deinit();
    }

    if (count != 0) {
        return ParserError.ParsingFailure;
    }

    const t = try allocator.create(TeleAst);
    t.*.body = "";
    t.*.ast_type = TeleAstType.map;
    t.*.children = children;

    token_queue2.deinit();
    return t;
}

test "parse map" {
    const file = try std.fs.cwd().openFile(
        "snippets/map.tl",
        .{ .mode = .read_only },
    );
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), test_allocator);
    defer token_queue.deinit();

    const result = try parse_map(token_queue, test_allocator);
    try std.testing.expect(result.*.ast_type == TeleAstType.map);
    try std.testing.expect(std.mem.eql(u8, result.*.body, ""));
    try std.testing.expect(result.*.children.?.items.len == 6);

    const c = result.*.children.?;
    try std.testing.expect(c.items[0].ast_type == TeleAstType.int);
    try std.testing.expect(std.mem.eql(u8, c.items[0].body, "1"));
    try std.testing.expect(c.items[0].children == null);
    try std.testing.expect(c.items[1].ast_type == TeleAstType.variable);
    try std.testing.expect(std.mem.eql(u8, c.items[1].body, "a"));
    try std.testing.expect(c.items[1].children == null);

    try std.testing.expect(c.items[2].ast_type == TeleAstType.int);
    try std.testing.expect(std.mem.eql(u8, c.items[2].body, "2"));
    try std.testing.expect(c.items[2].children == null);
    try std.testing.expect(c.items[3].ast_type == TeleAstType.variable);
    try std.testing.expect(std.mem.eql(u8, c.items[3].body, "b"));
    try std.testing.expect(c.items[3].children == null);

    try std.testing.expect(c.items[4].ast_type == TeleAstType.int);
    try std.testing.expect(std.mem.eql(u8, c.items[4].body, "3"));
    try std.testing.expect(c.items[4].children == null);
    try std.testing.expect(c.items[5].ast_type == TeleAstType.variable);
    try std.testing.expect(std.mem.eql(u8, c.items[5].body, "c"));
    try std.testing.expect(c.items[5].children == null);

    tele_ast.free_tele_ast_list(result.children.?, test_allocator);
    test_allocator.destroy(result);
}

fn parse_function_definition(token_queue: *TokenQueue, allocator: std.mem.Allocator, current_col: usize) !*TeleAst {
    const node3 = token_queue.pop() catch {
        return ParserError.ParsingFailure;
    };

    // Function Definition Name
    const buf = node3.*.body;
    allocator.destroy(node3);

    // Function Definition Signature
    var token_queue2 = TokenQueue.init(allocator) catch {
        allocator.free(node3.*.body);
        allocator.destroy(node3);
        return ParserError.ParsingFailure;
    };
    errdefer token_queue2.deinit();

    var map_count: usize = 0;
    while (!token_queue.empty()) {
        const node2 = try token_queue.pop();
        errdefer allocator.free(node2.*.body);
        errdefer allocator.destroy(node2);

        if (is_map_start(node2.*.body)) {
            map_count += 1;
        } else if (is_map_end(node2.*.body)) {
            map_count -= 1;
        }

        if (map_count == 0 and is_colon(node2.*.body)) {
            allocator.free(node2.*.body);
            allocator.destroy(node2);
            break;
        } else {
            std.debug.print("{s}\n", .{node2.*.body});
            try token_queue2.push(node2.*.body, node2.*.line, node2.col);
        }

        allocator.destroy(node2);
    }

    const func_sig = try parse_function_signature(token_queue2, allocator);
    token_queue2.deinit();

    var children = std.ArrayList(*TeleAst).init(allocator);
    errdefer tele_ast.free_tele_ast_list(children, allocator);
    try children.append(func_sig);

    // Function Definition Body
    var token_queue3 = try TokenQueue.init(allocator);
    errdefer token_queue3.deinit();
    if (token_queue.empty()) {
        return ParserError.ParsingFailure;
    }

    while (!token_queue.empty()) {
        const peek_node2 = try token_queue.peek();
        if (peek_node2.*.col <= current_col) {
            break;
        }

        const node2 = try token_queue.pop();
        errdefer allocator.free(node2.*.body);
        errdefer allocator.destroy(node2);

        try token_queue3.push(node2.*.body, node2.*.line, node2.*.col);
        allocator.destroy(node2);
    }

    if (token_queue3.empty()) {
        return ParserError.ParsingFailure;
    }

    var alist = parse_body(token_queue3, allocator) catch {
        return ParserError.ParsingFailure;
    };
    errdefer tele_ast.free_tele_ast_list(alist, allocator);

    for (alist.items) |a| {
        try children.append(a);
    }
    alist.deinit();

    // Assemble Function Definition Ast
    const t = try allocator.create(TeleAst);
    t.*.body = buf;
    t.*.ast_type = TeleAstType.function_def;
    t.*.children = children;

    token_queue3.deinit();
    return t;
}

fn parse_body(token_queue: *TokenQueue, allocator: std.mem.Allocator) ParserError!std.ArrayList(*TeleAst) {
    var alist = std.ArrayList(*TeleAst).init(allocator);
    errdefer tele_ast.free_tele_ast_list(alist, allocator);

    var i: usize = 0;

    while (!token_queue.empty()) {
        i = i + 1;
        const pnode = token_queue.peek() catch {
            return ParserError.ParsingFailure;
        };
        if (is_body_keyword(pnode.*.body)) {
            if (is_match_keyword(pnode.*.body)) {
                const current_col = pnode.*.col;

                const ast = parse_match_expression(token_queue, allocator, current_col) catch {
                    return ParserError.ParsingFailure;
                };
                errdefer tele_ast.free_tele_ast(ast, allocator);
                alist.append(ast) catch {
                    return ParserError.ParsingFailure;
                };
            } else {
                return ParserError.ParsingFailure;
            }
        } else {
            const current_line = pnode.*.line;
            const ast = parse_body_line(token_queue, allocator, current_line) catch {
                return ParserError.ParsingFailure;
            };
            errdefer tele_ast.free_tele_ast(ast, allocator);
            alist.append(ast) catch {
                return ParserError.ParsingFailure;
            };
        }
    }

    if (alist.items.len == 0) {
        return ParserError.ParsingFailure;
    }
    return alist;
}

test "parse body multi" {
    const file = try std.fs.cwd().openFile(
        "snippets/body_multi.tl",
        .{ .mode = .read_only },
    );
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), test_allocator);
    defer token_queue.deinit();

    const result = try parse_body(token_queue, test_allocator);
    try std.testing.expect(result.items.len == 2);

    const ast1 = result.items[0];
    try std.testing.expect(ast1.*.ast_type == TeleAstType.op);
    try std.testing.expect(std.mem.eql(u8, ast1.*.body, "+"));
    try std.testing.expect(ast1.*.children.?.items.len == 2);

    const ast2 = result.items[1];
    try std.testing.expect(ast2.*.ast_type == TeleAstType.op);
    try std.testing.expect(std.mem.eql(u8, ast2.*.body, "-"));
    try std.testing.expect(ast2.*.children.?.items.len == 2);

    tele_ast.free_tele_ast_list(result, test_allocator);
}

fn parse_body_line(token_queue: *TokenQueue, allocator: std.mem.Allocator, current_line: usize) !*TeleAst {
    var buffer_token_queue = try TokenQueue.init(allocator);
    errdefer buffer_token_queue.deinit();

    // Parse Line
    while (!token_queue.empty()) {
        const pnode2 = try token_queue.peek();

        if (pnode2.*.line > current_line) {
            break;
        } else {
            const node = try token_queue.pop();
            errdefer allocator.free(node.*.body);
            errdefer allocator.destroy(node);
            buffer_token_queue.push(node.*.body, node.*.line, node.*.col) catch {
                return ParserError.ParsingFailure;
            };
            allocator.destroy(node);
        }
    }
    var alist = parse_tokens(buffer_token_queue, allocator) catch {
        return ParserError.ParsingFailure;
    };
    errdefer tele_ast.free_tele_ast_list(alist, allocator);
    defer alist.deinit();

    if (!buffer_token_queue.empty()) {
        return ParserError.ParsingFailure;
    }
    buffer_token_queue.deinit();
    if (alist.items.len != 1) {
        return ParserError.ParsingFailure;
    }

    // Statement should have one AST item that needs to be popped off
    const ast = alist.pop();
    return ast;
}

test "parse body line" {
    const file = try std.fs.cwd().openFile(
        "snippets/body_single.tl",
        .{ .mode = .read_only },
    );
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), test_allocator);
    defer token_queue.deinit();

    const result = try parse_body_line(token_queue, test_allocator, 0);
    try std.testing.expect(result.*.ast_type == TeleAstType.op);
    try std.testing.expect(std.mem.eql(u8, result.*.body, "+"));
    try std.testing.expect(result.*.children.?.items.len == 2);

    const ast1 = result.*.children.?.items[0];
    try std.testing.expect(ast1.*.ast_type == TeleAstType.variable);
    try std.testing.expect(std.mem.eql(u8, ast1.*.body, "a"));
    try std.testing.expect(ast1.*.children == null);

    const ast2 = result.*.children.?.items[1];
    try std.testing.expect(ast2.*.ast_type == TeleAstType.int);
    try std.testing.expect(std.mem.eql(u8, ast2.*.body, "2"));
    try std.testing.expect(ast2.*.children == null);

    tele_ast.free_tele_ast_list(result.*.children.?, test_allocator);
    test_allocator.free(result.*.body);
    test_allocator.destroy(result);
}

fn parse_match_expression(token_queue: *TokenQueue, allocator: std.mem.Allocator, current_col: usize) !*TeleAst {
    // Pop off match keyword
    const n = try token_queue.pop();
    allocator.free(n.*.body);
    allocator.destroy(n);

    var buffer_token_queue = try TokenQueue.init(allocator);
    errdefer buffer_token_queue.deinit();

    while (!token_queue.empty()) {
        const pnode2 = token_queue.peek() catch {
            return ParserError.ParsingFailure;
        };

        if (pnode2.*.col <= current_col) {
            break;
        } else {
            const node = try token_queue.pop();
            errdefer allocator.free(node.*.body);
            errdefer allocator.destroy(node);

            buffer_token_queue.push(node.*.body, node.*.line, node.*.col) catch {
                return ParserError.ParsingFailure;
            };

            allocator.destroy(node);
        }
    }

    const signature_ast = try parse_match_signature(buffer_token_queue, allocator);
    errdefer tele_ast.free_tele_ast(signature_ast, allocator);

    var children = std.ArrayList(*TeleAst).init(allocator);
    errdefer tele_ast.free_tele_ast_list(children, allocator);
    try children.append(signature_ast);

    var body = try parse_match_body(buffer_token_queue, allocator);
    errdefer tele_ast.free_tele_ast_list(body, allocator);
    if (!buffer_token_queue.empty()) {
        return ParserError.ParsingFailure;
    }
    buffer_token_queue.deinit();

    for (body.items) |c| {
        try children.append(c);
    }
    body.deinit();

    const final_ast = try allocator.create(TeleAst);
    final_ast.*.body = "";
    final_ast.*.ast_type = TeleAstType.case;
    final_ast.*.children = children;
    return final_ast;
}

test "parse match expression" {
    const file = try std.fs.cwd().openFile(
        "snippets/match.tl",
        .{ .mode = .read_only },
    );
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), test_allocator);
    defer token_queue.deinit();

    const result = try parse_match_expression(token_queue, test_allocator, 0);
    try std.testing.expect(std.mem.eql(u8, result.*.body, ""));
    try std.testing.expect(result.*.ast_type == TeleAstType.case);
    try std.testing.expect(result.*.children.?.items.len == 3);

    const mc = result.*.children.?;
    try std.testing.expect(mc.items[0].ast_type == TeleAstType.variable);
    try std.testing.expect(std.mem.eql(u8, mc.items[0].body, "x"));
    try std.testing.expect(mc.items[0].children == null);

    try std.testing.expect(mc.items[1].ast_type == TeleAstType.case_clause);
    try std.testing.expect(std.mem.eql(u8, mc.items[1].body, ""));
    try std.testing.expect(mc.items[1].children.?.items.len == 2);

    const c = mc.items[1].children.?;
    try std.testing.expect(c.items[0].ast_type == TeleAstType.int);
    try std.testing.expect(std.mem.eql(u8, c.items[0].body, "1"));
    try std.testing.expect(c.items[0].children == null);
    try std.testing.expect(c.items[1].ast_type == TeleAstType.int);
    try std.testing.expect(std.mem.eql(u8, c.items[1].body, "42"));
    try std.testing.expect(c.items[1].children == null);

    try std.testing.expect(mc.items[2].ast_type == TeleAstType.case_clause);
    try std.testing.expect(std.mem.eql(u8, mc.items[2].body, ""));
    try std.testing.expect(mc.items[2].children.?.items.len == 2);

    const c2 = mc.items[2].children.?;
    try std.testing.expect(c2.items[0].ast_type == TeleAstType.int);
    try std.testing.expect(std.mem.eql(u8, c2.items[0].body, "2"));
    try std.testing.expect(c2.items[0].children == null);
    try std.testing.expect(c2.items[1].ast_type == TeleAstType.int);
    try std.testing.expect(std.mem.eql(u8, c2.items[1].body, "43"));
    try std.testing.expect(c2.items[1].children == null);

    tele_ast.free_tele_ast_list(result.*.children.?, test_allocator);
    test_allocator.destroy(result);
}

fn parse_match_body(token_queue: *TokenQueue, allocator: std.mem.Allocator) !std.ArrayList(*TeleAst) {
    var clause_col: usize = 0;
    var alist = std.ArrayList(*TeleAst).init(allocator);
    errdefer tele_ast.free_tele_ast_list(alist, allocator);

    // Parse Match Clauses
    while (!token_queue.empty()) {
        var children = std.ArrayList(*TeleAst).init(allocator);
        errdefer tele_ast.free_tele_ast_list(children, allocator);

        const case_clause_signature_ast = try parse_case_clause_signature(token_queue, allocator, &clause_col);
        errdefer tele_ast.free_tele_ast(case_clause_signature_ast, allocator);
        try children.append(case_clause_signature_ast);

        var case_clause_body = try parse_case_clause_body(token_queue, clause_col, allocator);
        errdefer tele_ast.free_tele_ast_list(case_clause_body, allocator);
        for (case_clause_body.items) |c| {
            try children.append(c);
        }
        case_clause_body.deinit();

        const t = try allocator.create(TeleAst);
        t.body = "";
        t.ast_type = TeleAstType.case_clause;
        t.children = children;
        errdefer tele_ast.free_tele_ast(t, allocator);

        try alist.append(t);
    }

    return alist;
}

test "parse match body single clause" {
    const file = try std.fs.cwd().openFile(
        "snippets/match_body.tl",
        .{ .mode = .read_only },
    );
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), test_allocator);
    defer token_queue.deinit();

    const result = try parse_match_body(token_queue, test_allocator);
    try std.testing.expect(result.items.len == 1);
    try std.testing.expect(result.items[0].ast_type == TeleAstType.case_clause);
    try std.testing.expect(std.mem.eql(u8, result.items[0].body, ""));

    try std.testing.expect(result.items[0].children.?.items.len == 2);
    const c = result.items[0].children.?;
    try std.testing.expect(c.items[0].ast_type == TeleAstType.int);
    try std.testing.expect(std.mem.eql(u8, c.items[0].body, "1"));
    try std.testing.expect(c.items[0].children == null);
    try std.testing.expect(c.items[1].ast_type == TeleAstType.int);
    try std.testing.expect(std.mem.eql(u8, c.items[1].body, "42"));
    try std.testing.expect(c.items[1].children == null);
    tele_ast.free_tele_ast_list(result, test_allocator);
}

test "parse match body multi clause" {
    const file = try std.fs.cwd().openFile(
        "snippets/match_body_multi.tl",
        .{ .mode = .read_only },
    );
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), test_allocator);
    defer token_queue.deinit();

    const result = try parse_match_body(token_queue, test_allocator);
    try std.testing.expect(result.items.len == 2);
    try std.testing.expect(result.items[0].ast_type == TeleAstType.case_clause);
    try std.testing.expect(std.mem.eql(u8, result.items[0].body, ""));

    try std.testing.expect(result.items[0].children.?.items.len == 2);
    const c = result.items[0].children.?;
    try std.testing.expect(c.items[0].ast_type == TeleAstType.int);
    try std.testing.expect(std.mem.eql(u8, c.items[0].body, "1"));
    try std.testing.expect(c.items[0].children == null);
    try std.testing.expect(c.items[1].ast_type == TeleAstType.int);
    try std.testing.expect(std.mem.eql(u8, c.items[1].body, "42"));
    try std.testing.expect(c.items[1].children == null);

    try std.testing.expect(result.items[1].ast_type == TeleAstType.case_clause);
    try std.testing.expect(std.mem.eql(u8, result.items[1].body, ""));

    try std.testing.expect(result.items[1].children.?.items.len == 2);
    const c2 = result.items[1].children.?;
    try std.testing.expect(c2.items[0].ast_type == TeleAstType.int);
    try std.testing.expect(std.mem.eql(u8, c2.items[0].body, "2"));
    try std.testing.expect(c2.items[0].children == null);
    try std.testing.expect(c2.items[1].ast_type == TeleAstType.int);
    try std.testing.expect(std.mem.eql(u8, c2.items[1].body, "43"));
    try std.testing.expect(c2.items[1].children == null);

    tele_ast.free_tele_ast_list(result, test_allocator);
}

fn parse_match_signature(token_queue: *TokenQueue, allocator: std.mem.Allocator) !*TeleAst {
    var buffer_token_queue = try TokenQueue.init(allocator);
    errdefer buffer_token_queue.deinit();
    // Parse Match Signature
    while (!token_queue.empty()) {
        const node = try token_queue.pop();
        errdefer allocator.free(node.*.body);
        errdefer allocator.destroy(node);

        if (is_colon(node.*.body)) {
            allocator.free(node.*.body);
            allocator.destroy(node);
            break;
        } else {
            try buffer_token_queue.push(node.*.body, node.*.line, node.*.col);
            allocator.destroy(node);
        }
    }

    if (buffer_token_queue.empty()) {
        return ParserError.ParsingFailure;
    }

    var signature_list = try parse_tokens(buffer_token_queue, allocator);
    errdefer tele_ast.free_tele_ast_list(signature_list, allocator);
    if (!buffer_token_queue.empty()) {
        return ParserError.ParsingFailure;
    }
    buffer_token_queue.deinit();

    if (signature_list.items.len != 1) {
        return ParserError.ParsingFailure;
    }

    const signature_ast = signature_list.pop();
    errdefer tele_ast.free_tele_ast(signature_ast, allocator);

    if (signature_list.items.len > 0) {
        return ParserError.ParsingFailure;
    }

    signature_list.deinit();

    return signature_ast;
}

test "parse match signature" {
    const file = try std.fs.cwd().openFile(
        "snippets/match_signature.tl",
        .{ .mode = .read_only },
    );
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), test_allocator);
    defer token_queue.deinit();

    const result = try parse_match_signature(token_queue, test_allocator);
    try std.testing.expect(result.*.ast_type == TeleAstType.int);
    try std.testing.expect(std.mem.eql(u8, result.*.body, "1"));
    try std.testing.expect(result.*.children == null);

    test_allocator.free(result.*.body);
    test_allocator.destroy(result);
}

fn parse_case_clause_signature(token_queue: *TokenQueue, allocator: std.mem.Allocator, clause_col: *usize) !*TeleAst {
    var buffer_token_queue = try TokenQueue.init(allocator);
    errdefer buffer_token_queue.deinit();
    var clause_col_first: bool = true;

    // Case Clause Signature
    while (!token_queue.empty()) {
        const node = try token_queue.pop();
        errdefer allocator.free(node.*.body);
        errdefer allocator.destroy(node);

        if (clause_col_first) {
            clause_col.* = node.*.col;
            clause_col_first = false;
        }

        if (is_colon(node.*.body)) {
            allocator.free(node.*.body);
            allocator.destroy(node);
            break;
        } else {
            try buffer_token_queue.push(node.*.body, node.*.line, node.*.col);
            allocator.destroy(node);
        }
    }

    var case_clause_signature_list = try parse_tokens(buffer_token_queue, allocator);
    errdefer tele_ast.free_tele_ast_list(case_clause_signature_list, allocator);

    if (!buffer_token_queue.empty()) {
        return ParserError.ParsingFailure;
    }
    buffer_token_queue.deinit();

    const case_clause_signature_ast = case_clause_signature_list.pop();
    errdefer tele_ast.free_tele_ast(case_clause_signature_ast, allocator);

    if (case_clause_signature_list.items.len > 0) {
        return ParserError.ParsingFailure;
    }

    case_clause_signature_list.deinit();

    return case_clause_signature_ast;
}

test "parse case clause signature" {
    const file = try std.fs.cwd().openFile(
        "snippets/case_clause_signature.tl",
        .{ .mode = .read_only },
    );
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), test_allocator);
    defer token_queue.deinit();

    var n: usize = 0;
    const result = try parse_case_clause_signature(token_queue, test_allocator, &n);
    try std.testing.expect(result.*.ast_type == TeleAstType.int);
    try std.testing.expect(std.mem.eql(u8, result.*.body, "1"));
    try std.testing.expect(result.*.children == null);

    test_allocator.free(result.*.body);
    test_allocator.destroy(result);
}

fn parse_case_clause_body(token_queue: *TokenQueue, clause_col: usize, allocator: std.mem.Allocator) !std.ArrayList(*TeleAst) {
    var buffer_token_queue = try TokenQueue.init(allocator);
    errdefer buffer_token_queue.deinit();
    // Parse Case Clause Body

    while (!token_queue.empty()) {
        const peek_node = try token_queue.peek();

        if (peek_node.*.col <= clause_col) {
            break;
        } else {
            const node = try token_queue.pop();
            errdefer allocator.free(node.*.body);
            errdefer allocator.destroy(node);

            try buffer_token_queue.push(node.*.body, node.*.line, node.*.col);
            allocator.destroy(node);
        }
    }

    const alist = try parse_body(buffer_token_queue, allocator);
    errdefer tele_ast.free_tele_ast_list(alist, allocator);

    if (!buffer_token_queue.empty()) {
        return ParserError.ParsingFailure;
    }

    buffer_token_queue.deinit();

    return alist;
}

test "parse case clause body single line" {
    const file = try std.fs.cwd().openFile(
        "snippets/case_clause_body_single.tl",
        .{ .mode = .read_only },
    );
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), test_allocator);
    defer token_queue.deinit();

    const result = try parse_case_clause_body(token_queue, 0, test_allocator);
    try std.testing.expect(result.items.len == 1);
    try std.testing.expect(std.mem.eql(u8, result.items[0].body, "42"));
    try std.testing.expect(result.items[0].ast_type == TeleAstType.int);
    try std.testing.expect(result.items[0].children == null);

    tele_ast.free_tele_ast_list(result, test_allocator);
}

test "parse case clause body multi line" {
    const file = try std.fs.cwd().openFile(
        "snippets/case_clause_body_multi.tl",
        .{ .mode = .read_only },
    );
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), test_allocator);
    defer token_queue.deinit();

    const result = try parse_case_clause_body(token_queue, 0, test_allocator);
    try std.testing.expect(result.items.len == 3);
    try std.testing.expect(std.mem.eql(u8, result.items[0].body, "42"));
    try std.testing.expect(result.items[0].ast_type == TeleAstType.int);
    try std.testing.expect(result.items[0].children == null);
    try std.testing.expect(std.mem.eql(u8, result.items[1].body, "43"));
    try std.testing.expect(result.items[1].ast_type == TeleAstType.int);
    try std.testing.expect(result.items[1].children == null);
    try std.testing.expect(std.mem.eql(u8, result.items[2].body, "44"));
    try std.testing.expect(result.items[2].ast_type == TeleAstType.int);
    try std.testing.expect(result.items[2].children == null);

    tele_ast.free_tele_ast_list(result, test_allocator);
}

fn parse_function_signature(token_queue: *TokenQueue, allocator: std.mem.Allocator) !*TeleAst {
    const init = try token_queue.pop();
    if (!is_paren_start(init.*.body)) {
        allocator.free(init.*.body);
        allocator.destroy(init);
        // TODO: Error expected paren start
        return ParserError.ParsingFailure;
    }
    allocator.free(init.*.body);
    allocator.destroy(init);

    var children: ?std.ArrayList(*TeleAst) = null;

    const pn2 = try token_queue.peek();

    if (!is_paren_end(pn2.*.body)) {
        children = std.ArrayList(*TeleAst).init(allocator);
        errdefer tele_ast.free_tele_ast_list(children.?, allocator);

        while (!token_queue.empty()) {
            const ast = try parse_function_signature_param(token_queue, allocator);
            errdefer tele_ast.free_tele_ast(ast, allocator);
            try children.?.append(ast);
        }
    } else {
        // Free Paren End Token
        const tn = try token_queue.pop();
        allocator.free(tn.*.body);
        allocator.destroy(tn);
    }

    const tast = try allocator.create(TeleAst);
    tast.*.body = "";
    tast.*.ast_type = TeleAstType.function_signature;
    tast.*.children = children;

    return tast;
}

fn parse_function_signature_param(token_queue: *TokenQueue, allocator: std.mem.Allocator) !*TeleAst {
    var buffer_token_queue = try TokenQueue.init(allocator);
    //errdefer buffer_token_queue.deinit();

    var n_count: usize = 1;
    while (!token_queue.empty()) {
        const n2 = try token_queue.pop();
        errdefer allocator.free(n2.*.body);
        errdefer allocator.destroy(n2);

        if (n_count == 1 and (is_comma(n2.*.body) or is_paren_end(n2.*.body))) {
            allocator.free(n2.*.body);
            allocator.destroy(n2);
            break;
        }

        if (is_tuple_start(n2.*.body) or is_list_start(n2.*.body) or is_map_start(n2.*.body)) {
            n_count += 1;
        } else if (is_paren_end(n2.*.body) or is_list_end(n2.*.body) or is_map_end(n2.*.body)) {
            n_count -= 1;
        }

        try buffer_token_queue.push(n2.*.body, n2.*.line, n2.*.col);
        allocator.destroy(n2);
    }

    var alist = try parse_tokens(buffer_token_queue, allocator);
    errdefer tele_ast.free_tele_ast_list(alist, allocator);

    if (!buffer_token_queue.empty()) {
        return ParserError.ParsingFailure;
    }
    buffer_token_queue.deinit();
    if (alist.items.len != 1) {
        return ParserError.ParsingFailure;
    }

    const ast = alist.pop();
    alist.deinit();
    return ast;
}

fn parse_function_call(body: []const u8, token_queue: *TokenQueue, allocator: std.mem.Allocator) !*TeleAst {
    // Remove paren start
    const pn = try token_queue.pop();
    allocator.free(pn.*.body);
    allocator.destroy(pn);

    const pn2 = try token_queue.peek();

    var children: ?std.ArrayList(*TeleAst) = null;
    if (!is_paren_end(pn2.*.body)) {
        children = std.ArrayList(*TeleAst).init(allocator);
        errdefer tele_ast.free_tele_ast_list(children.?, allocator);
        while (!token_queue.empty()) {
            const ast = try parse_function_call_arg(token_queue, allocator);
            errdefer tele_ast.free_tele_ast(ast, allocator);
            try children.?.append(ast);
        }
    } else {
        // Free Paren End Token
        const tn = try token_queue.pop();
        allocator.free(tn.*.body);
        allocator.destroy(tn);
    }

    const t = try allocator.create(TeleAst);
    t.*.body = body;
    t.*.ast_type = TeleAstType.function_call;
    t.*.children = children;
    return t;
}

fn parse_function_call_arg(token_queue: *TokenQueue, allocator: std.mem.Allocator) !*TeleAst {
    // Function Call Body Token Queue
    var buffer_token_queue = try TokenQueue.init(allocator);
    errdefer buffer_token_queue.deinit();

    var n_count: usize = 0;
    while (!token_queue.empty()) {
        const n2 = try token_queue.pop();
        errdefer allocator.free(n2.*.body);
        errdefer allocator.destroy(n2);

        if (n_count == 0) {
            if (is_comma(n2.*.body) or is_paren_end(n2.*.body)) {
                allocator.free(n2.*.body);
                allocator.destroy(n2);
                break;
            }
        }

        if (is_paren_start(n2.*.body) or is_tuple_start(n2.*.body) or is_list_start(n2.*.body) or is_map_start(n2.*.body)) {
            n_count += 1;
        } else if (is_paren_end(n2.*.body) or is_list_end(n2.*.body) or is_map_end(n2.*.body)) {
            n_count -= 1;
        }

        try buffer_token_queue.push(n2.*.body, n2.*.line, n2.*.col);
        allocator.destroy(n2);
    }

    var alist = try parse_tokens(buffer_token_queue, allocator);
    errdefer tele_ast.free_tele_ast_list(alist, allocator);
    if (!buffer_token_queue.empty()) {
        return ParserError.ParsingFailure;
    }
    buffer_token_queue.deinit();
    if (alist.items.len != 1) {
        return ParserError.ParsingFailure;
    }

    const ast = alist.pop();
    alist.deinit();
    return ast;
}

fn is_float(buf: []const u8) bool {
    var contains_period = false;
    for (buf) |c| {
        if (!std.ascii.isDigit(c)) {
            if (c == '.') {
                contains_period = true;
            } else {
                return false;
            }
        }
    }

    return contains_period;
}

fn is_int(buf: []const u8) bool {
    return std.ascii.isDigit(buf[0]);
}

fn is_atom(buf: []const u8) bool {
    return buf[0] == '\'';
}

fn is_binary(buf: []const u8) bool {
    if (buf[0] == '"') {
        return true;
    }

    if (buf[0] == '<') {
        if (buf.len > 1) {
            if (buf[1] == '<') {
                return true;
            }
        }
    }

    return false;
}

fn is_tuple_start(buf: []const u8) bool {
    return std.mem.eql(u8, "#(", buf);
}

fn is_list_start(buf: []const u8) bool {
    return buf[0] == '[';
}

fn is_list_end(buf: []const u8) bool {
    return buf[0] == ']';
}

fn is_map_start(buf: []const u8) bool {
    return buf[0] == '{';
}

fn is_map_end(buf: []const u8) bool {
    return buf[0] == '}';
}

fn is_paren_start(buf: []const u8) bool {
    return buf[0] == '(';
}

fn is_paren_end(buf: []const u8) bool {
    return buf[0] == ')';
}

fn is_operator(buf: []const u8) bool {
    if (buf.len == 0) {
        return false;
    }

    switch (buf[0]) {
        '+', '*', '/', '!', '-' => {
            return true;
        },
        '<', '>', '=' => {
            if (buf.len == 1) {
                return true;
            } else if (buf.len == 2) {
                if (buf[1] == '=') {
                    return true;
                } else {
                    return false;
                }
            }
        },
        else => {
            return false;
        },
    }

    return false;
}

fn is_keyword(buf: []const u8) bool {
    if (buf.len == 0) {
        return false;
    }

    if (buf[0] == 'd') {
        if (std.mem.eql(u8, "def", buf)) {
            return true;
        }
    }

    return false;
}

fn is_body_keyword(buf: []const u8) bool {
    if (buf.len == 0) {
        return false;
    }

    if (buf[0] == 'm') {
        if (std.mem.eql(u8, "match", buf)) {
            return true;
        }
    }

    return false;
}

fn is_match_keyword(buf: []const u8) bool {
    if (buf.len == 0) {
        return false;
    }

    if (buf[0] == 'm') {
        if (std.mem.eql(u8, "match", buf)) {
            return true;
        }
    }
    return false;
}

fn is_function_definition(buf: []const u8) bool {
    return std.mem.eql(u8, "def", buf);
}

fn is_colon(buf: []const u8) bool {
    return buf[0] == ':';
}

fn is_comma(buf: []const u8) bool {
    return buf[0] == ',';
}

fn is_space(buf: []const u8) bool {
    return buf[0] == ' ';
}
