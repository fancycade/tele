const std = @import("std");
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

    const result = try parse_tokens(token_queue, allocator, ParserMode.none);

    token_queue.deinit();

    return result;
}

fn parse_tokens(token_queue: *TokenQueue, allocator: std.mem.Allocator, mode: ParserMode) ParserError!std.ArrayList(*TeleAst) {
    var list = std.ArrayList(*TeleAst).init(allocator);
    errdefer list.deinit();

    while (!token_queue.empty()) {
        const node = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };

        if (is_float(node.*.body)) {
            const t = parse_float(node.*.body, allocator) catch {
                return ParserError.ParsingFailure;
            };

            list.append(t) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_int(node.*.body)) {
            const t = parse_int(node.*.body, allocator) catch {
                return ParserError.ParsingFailure;
            };
            list.append(t) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_atom(node.*.body)) {
            const t = parse_atom(node.*.body, allocator) catch {
                return ParserError.ParsingFailure;
            };
            list.append(t) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_binary(node.*.body)) {
            const t = parse_binary(node.*.body, allocator) catch {
                return ParserError.ParsingFailure;
            };
            list.append(t) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_tuple_start(node.*.body)) {
            // Body not needed
            allocator.free(node.*.body);
            const t = parse_tuple(token_queue, allocator) catch {
                return ParserError.ParsingFailure;
            };
            list.append(t) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_list_start(node.*.body)) {
            allocator.free(node.*.body);
            const t = parse_list(token_queue, allocator) catch {
                return ParserError.ParsingFailure;
            };
            list.append(t) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_map_start(node.*.body)) {
            allocator.free(node.*.body);
            const t = parse_map(token_queue, allocator) catch {
                return ParserError.ParsingFailure;
            };
            list.append(t) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_operator(node.*.body)) {
            if (list.items.len == 0) {
                return ParserError.ParsingFailure;
                // TODO: Error expected value on left side of operator
            }
            const ast1 = list.pop();

            var alist = parse_tokens(token_queue, allocator, ParserMode.op) catch {
                return ParserError.ParsingFailure;
            };

            const ast2 = alist.pop();

            var children = std.ArrayList(*TeleAst).init(allocator);
            children.append(ast1) catch {
                return ParserError.ParsingFailure;
            };
            children.append(ast2) catch {
                return ParserError.ParsingFailure;
            };

            const t = allocator.create(TeleAst) catch {
                return ParserError.ParsingFailure;
            };
            t.*.body = node.*.body;
            t.*.ast_type = TeleAstType.op;
            t.*.children = children;
            list.append(t) catch {
                return ParserError.ParsingFailure;
            };
            alist.deinit();
        } else if (is_keyword(node.*.body)) {
            if (is_function_definition(node.*.body)) {
                allocator.free(node.*.body);
                const t = parse_function_definition(token_queue, allocator) catch {
                    return ParserError.ParsingFailure;
                };
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

                list.append(t) catch {
                    return ParserError.ParsingFailure;
                };
            } else {
                const t = parse_variable(node.*.body, allocator) catch {
                    return ParserError.ParsingFailure;
                };

                list.append(t) catch {
                    return ParserError.ParsingFailure;
                };
            }
        }

        //allocator.free(node.*.body);
        allocator.destroy(node);

        if (mode == ParserMode.op) {
            break;
        }
    }

    if (!token_queue.empty()) {
        return ParserError.ParsingFailure;
    }

    return list;
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
    var token_queue2 = TokenQueue.init(allocator) catch {
        return ParserError.ParsingFailure;
    };
    var count: usize = 1;

    while (!token_queue.empty()) {
        const node2 = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };

        if (is_tuple_start(node2.*.body)) {
            count += 1;
        } else if (is_paren_start(node2.*.body)) {
            count += 1;
        } else if (is_paren_end(node2.*.body)) {
            count -= 1;
            if (count == 0) {
                // Free paren end body
                allocator.free(node2.*.body);
                break;
            }
        }

        token_queue2.push(node2.*.body, node2.*.line, node2.*.col) catch {
            return ParserError.ParsingFailure;
        };

        allocator.destroy(node2);
    }

    if (count != 0) {
        return ParserError.ParsingFailure;
    }

    const children = parse_tokens(token_queue2, allocator, ParserMode.none) catch {
        return ParserError.ParsingFailure;
    };

    const t = allocator.create(TeleAst) catch {
        return ParserError.ParsingFailure;
    };
    t.*.body = "";
    t.*.ast_type = TeleAstType.tuple;
    t.*.children = children;
    token_queue2.deinit();
    return t;
}

fn parse_list(token_queue: *TokenQueue, allocator: std.mem.Allocator) !*TeleAst {
    var token_queue2 = TokenQueue.init(allocator) catch {
        return ParserError.ParsingFailure;
    };
    var count: usize = 1;

    while (!token_queue.empty()) {
        const node2 = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };

        if (is_list_start(node2.*.body)) {
            count += 1;
        } else if (is_list_end(node2.*.body)) {
            count -= 1;
            if (count == 0) {
                allocator.free(node2.*.body);
                break;
            }
        }

        token_queue2.push(node2.*.body, node2.*.line, node2.*.col) catch {
            return ParserError.ParsingFailure;
        };

        allocator.destroy(node2);
    }

    if (count != 0) {
        return ParserError.ParsingFailure;
    }

    const children = parse_tokens(token_queue2, allocator, ParserMode.none) catch {
        return ParserError.ParsingFailure;
    };

    const t = allocator.create(TeleAst) catch {
        return ParserError.ParsingFailure;
    };
    t.*.body = "";
    t.*.ast_type = TeleAstType.list;
    t.*.children = children;
    token_queue2.deinit();
    return t;
}

fn parse_map(token_queue: *TokenQueue, allocator: std.mem.Allocator) !*TeleAst {
    var token_queue2 = TokenQueue.init(allocator) catch {
        return ParserError.ParsingFailure;
    };
    var count: usize = 1;

    while (!token_queue.empty()) {
        const node2 = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };

        if (is_map_start(node2.*.body)) {
            count += 1;
        } else if (is_map_end(node2.*.body)) {
            count -= 1;
            if (count == 0) {
                allocator.free(node2.*.body);
                break;
            }
        }

        token_queue2.push(node2.*.body, node2.*.line, node2.*.col) catch {
            return ParserError.ParsingFailure;
        };

        allocator.destroy(node2);
    }

    if (count != 0) {
        return ParserError.ParsingFailure;
    }

    const children = parse_tokens(token_queue2, allocator, ParserMode.none) catch {
        return ParserError.ParsingFailure;
    };

    const t = allocator.create(TeleAst) catch {
        return ParserError.ParsingFailure;
    };
    t.*.body = "";
    t.*.ast_type = TeleAstType.map;
    t.*.children = children;

    token_queue2.deinit();
    return t;
}

fn parse_function_definition(token_queue: *TokenQueue, allocator: std.mem.Allocator) !*TeleAst {
    const node3 = token_queue.pop() catch {
        return ParserError.ParsingFailure;
    };

    // Function Definition Name
    const buf = node3.*.body;
    allocator.destroy(node3);

    // Function Definition Signature
    var token_queue2 = TokenQueue.init(allocator) catch {
        return ParserError.ParsingFailure;
    };
    while (!token_queue.empty()) {
        const node2 = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };

        if (is_colon(node2.*.body)) {
            allocator.free(node2.*.body);
            allocator.destroy(node2);
            break;
        } else {
            token_queue2.push(node2.*.body, node2.*.line, node2.col) catch {
                return ParserError.ParsingFailure;
            };
        }

        allocator.destroy(node2);
    }

    const func_sig = parse_function_signature(token_queue2, allocator) catch {
        token_queue2.deinit();
        return ParserError.ParsingFailure;
    };
    token_queue2.deinit();

    var children = std.ArrayList(*TeleAst).init(allocator);
    children.append(func_sig) catch {
        return ParserError.ParsingFailure;
    };

    // Function Definition Body
    var token_queue3 = TokenQueue.init(allocator) catch {
        return ParserError.ParsingFailure;
    };
    var first_body_child_col: usize = 0;
    var first_node: bool = true;
    while (!token_queue.empty()) {
        const peek_node2 = token_queue.peek() catch {
            return ParserError.ParsingFailure;
        };

        if (first_node) {
            first_body_child_col = peek_node2.col;
            first_node = false;
        } else {
            if (peek_node2.*.col < first_body_child_col) {
                break;
            }
        }

        const node2 = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };

        token_queue3.push(node2.*.body, node2.*.line, node2.*.col) catch {
            return ParserError.ParsingFailure;
        };

        allocator.destroy(node2);
    }

    var alist = parse_body(token_queue3, allocator) catch {
        return ParserError.ParsingFailure;
    };

    while (alist.items.len > 0) {
        const a = alist.pop();
        children.append(a) catch {
            return ParserError.ParsingFailure;
        };
    }
    alist.deinit();

    // Assemble Function Definition Ast
    const t = allocator.create(TeleAst) catch {
        return ParserError.ParsingFailure;
    };
    t.*.body = buf;
    t.*.ast_type = TeleAstType.function_def;
    t.*.children = children;

    token_queue3.deinit();
    return t;
}

fn parse_body(token_queue: *TokenQueue, allocator: std.mem.Allocator) ParserError!std.ArrayList(*TeleAst) {
    var statement_token_queue = TokenQueue.init(allocator) catch {
        return ParserError.ParsingFailure;
    };
    defer statement_token_queue.deinit();
    var alist = std.ArrayList(*TeleAst).init(allocator);

    var ast: *TeleAst = undefined;
    while (!token_queue.empty()) {
        const pnode = token_queue.peek() catch {
            return ParserError.ParsingFailure;
        };

        if (is_body_keyword(pnode.*.body)) {
            if (is_match_keyword(pnode.*.body)) {
                const current_col = pnode.*.col;

                ast = parse_match_expression(current_col, token_queue, statement_token_queue, allocator) catch {
                    return ParserError.ParsingFailure;
                };
            } else {
                return ParserError.ParsingFailure;
            }
        } else {
            const current_line = pnode.*.line;

            // Parse Line
            while (!token_queue.empty()) {
                const pnode2 = token_queue.peek() catch {
                    return ParserError.ParsingFailure;
                };

                if (pnode2.*.line > current_line) {
                    break;
                } else {
                    const node = token_queue.pop() catch {
                        return ParserError.ParsingFailure;
                    };

                    statement_token_queue.push(node.*.body, node.*.line, node.*.col) catch {
                        return ParserError.ParsingFailure;
                    };
                    allocator.destroy(node);
                }
            }

            var statement_list = parse_tokens(statement_token_queue, allocator, ParserMode.none) catch {
                // TODO: Do memory cleanup here
                return ParserError.ParsingFailure;
            };

            // Statement Token Queue should be empty because all tokens were consumed in parse_tokens
            if (!statement_token_queue.empty()) {
                statement_list.deinit();
                alist.deinit();
                return ParserError.ParsingFailure;
            }

            // Statement should have one AST item that needs to be popped off
            ast = statement_list.pop();

            // Throw error if statement list is not empty
            if (statement_list.items.len > 0) {
                statement_list.deinit();
                return ParserError.ParsingFailure;
            } else {
                statement_list.deinit();
            }
        }

        alist.append(ast) catch {
            return ParserError.ParsingFailure;
        };
    }

    var final_list = std.ArrayList(*TeleAst).init(allocator);

    while (alist.items.len > 0) {
        const a = alist.pop();
        final_list.append(a) catch {
            return ParserError.ParsingFailure;
        };
    }
    alist.deinit();

    return final_list;
}

fn parse_match_expression(current_col: usize, token_queue: *TokenQueue, statement_token_queue: *TokenQueue, allocator: std.mem.Allocator) !*TeleAst {
    // Pop off match keyword
    const n = token_queue.pop() catch {
        return ParserError.ParsingFailure;
    };
    allocator.free(n.*.body);
    allocator.destroy(n);

    while (!token_queue.empty()) {
        const pnode2 = token_queue.peek() catch {
            return ParserError.ParsingFailure;
        };

        if (pnode2.*.col <= current_col) {
            break;
        } else {
            const node = token_queue.pop() catch {
                return ParserError.ParsingFailure;
            };

            statement_token_queue.push(node.*.body, node.*.line, node.*.col) catch {
                return ParserError.ParsingFailure;
            };

            allocator.destroy(node);
        }
    }

    var buffer_token_queue = TokenQueue.init(allocator) catch {
        return ParserError.ParsingFailure;
    };

    const signature_ast = try parse_match_signature(token_queue, buffer_token_queue, allocator);

    if (!buffer_token_queue.empty()) {
        return ParserError.ParsingFailure;
    }

    var children = std.ArrayList(*TeleAst).init(allocator);
    children.append(signature_ast) catch {
        return ParserError.ParsingFailure;
    };
    var clause_col: usize = 0;

    // Parse Match Clauses
    while (!token_queue.empty()) {
        const case_clause_signature_ast = try parse_case_clause_signature(token_queue, buffer_token_queue, allocator, &clause_col);
        if (!buffer_token_queue.empty()) {
            return ParserError.ParsingFailure;
        }

        const case_clause_ast = try parse_case_clause_body(token_queue, buffer_token_queue, clause_col, allocator, case_clause_signature_ast);

        children.append(case_clause_ast) catch {
            return ParserError.ParsingFailure;
        };
    }

    buffer_token_queue.deinit();

    const final_ast = allocator.create(TeleAst) catch {
        return ParserError.ParsingFailure;
    };
    final_ast.*.body = "";
    final_ast.*.ast_type = TeleAstType.case;
    final_ast.*.children = children;
    return final_ast;
}

fn parse_match_signature(token_queue: *TokenQueue, buffer_token_queue: *TokenQueue, allocator: std.mem.Allocator) !*TeleAst {
    // Parse Match Signature
    while (!token_queue.empty()) {
        const node = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };

        if (is_colon(node.*.body)) {
            allocator.free(node.*.body);
            allocator.destroy(node);
            break;
        } else {
            buffer_token_queue.push(node.*.body, node.*.line, node.*.col) catch {
                return ParserError.ParsingFailure;
            };
            allocator.destroy(node);
        }
    }

    var signature_list = parse_tokens(buffer_token_queue, allocator, ParserMode.none) catch {
        return ParserError.ParsingFailure;
    };

    const signature_ast = signature_list.pop();

    if (signature_list.items.len > 0) {
        return ParserError.ParsingFailure;
    }

    signature_list.deinit();

    return signature_ast;
}

fn parse_case_clause_signature(token_queue: *TokenQueue, buffer_token_queue: *TokenQueue, allocator: std.mem.Allocator, clause_col: *usize) !*TeleAst {
    var clause_col_first: bool = true;

    // Case Clause Signature
    while (!token_queue.empty()) {
        const node = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };

        if (clause_col_first) {
            clause_col.* = node.*.col;
            clause_col_first = false;
        }

        if (is_colon(node.*.body)) {
            allocator.free(node.*.body);
            allocator.destroy(node);
            break;
        } else {
            buffer_token_queue.push(node.*.body, node.*.line, node.*.col) catch {
                return ParserError.ParsingFailure;
            };
            allocator.destroy(node);
        }
    }

    var case_clause_signature_list = parse_tokens(buffer_token_queue, allocator, ParserMode.none) catch {
        return ParserError.ParsingFailure;
    };

    const case_clause_signature_ast = case_clause_signature_list.pop();

    if (case_clause_signature_list.items.len > 0) {
        return ParserError.ParsingFailure;
    }

    case_clause_signature_list.deinit();

    return case_clause_signature_ast;
}

fn parse_case_clause_body(token_queue: *TokenQueue, buffer_token_queue: *TokenQueue, clause_col: usize, allocator: std.mem.Allocator, case_signature_ast: *TeleAst) !*TeleAst {
    var clause_children = std.ArrayList(*TeleAst).init(allocator);
    try clause_children.append(case_signature_ast);
    // Parse Case Clause Body

    while (!token_queue.empty()) {
        const peek_node = token_queue.peek() catch {
            return ParserError.ParsingFailure;
        };

        if (peek_node.*.col <= clause_col) {
            break;
        } else {
            const node = token_queue.pop() catch {
                return ParserError.ParsingFailure;
            };

            buffer_token_queue.push(node.*.body, node.*.line, node.*.col) catch {
                return ParserError.ParsingFailure;
            };
            allocator.destroy(node);
        }
    }

    var alist = parse_body(buffer_token_queue, allocator) catch {
        return ParserError.ParsingFailure;
    };

    if (!buffer_token_queue.empty()) {
        return ParserError.ParsingFailure;
    }

    while (alist.items.len > 0) {
        const a = alist.pop();
        try clause_children.append(a);
    }
    alist.deinit();

    const case_clause_ast = allocator.create(TeleAst) catch {
        return ParserError.ParsingFailure;
    };
    case_clause_ast.*.body = "";
    case_clause_ast.*.children = clause_children;
    case_clause_ast.*.ast_type = TeleAstType.case_clause;
    return case_clause_ast;
}

fn parse_function_signature(token_queue: *TokenQueue, allocator: std.mem.Allocator) !*TeleAst {
    const init = try token_queue.pop();
    if (!is_paren_start(init.*.body)) {
        // TODO: Error expected paren start
        return ParserError.ParsingFailure;
    }
    allocator.free(init.*.body);
    allocator.destroy(init);

    var children = std.ArrayList(*TeleAst).init(allocator);

    var success: bool = false;
    while (!token_queue.empty()) {
        const n = try token_queue.pop();

        if (is_comma(n.*.body) or is_space(n.*.body)) {
            allocator.free(n.*.body);
            // Skip
        } else if (is_paren_end(n.*.body)) {
            success = true;
            allocator.free(n.*.body);
            allocator.destroy(n);
            break;
        } else {
            const inner_tast = try allocator.create(TeleAst);
            inner_tast.*.body = n.*.body;
            inner_tast.*.ast_type = TeleAstType.variable;
            inner_tast.*.children = null;
            try children.append(inner_tast);
        }

        allocator.destroy(n);
    }

    if (!success) {
        return ParserError.ParsingFailure;
    }

    const tast = try allocator.create(TeleAst);
    tast.*.body = "";
    tast.*.ast_type = TeleAstType.function_signature;
    tast.*.children = children;

    return tast;
}

fn parse_function_call(body: []const u8, token_queue: *TokenQueue, allocator: std.mem.Allocator) !*TeleAst {
    // Remove paren start
    const pn = token_queue.pop() catch {
        return ParserError.ParsingFailure;
    };

    allocator.free(pn.*.body);
    allocator.destroy(pn);

    const pn2 = token_queue.peek() catch {
        return ParserError.ParsingFailure;
    };

    var children: ?std.ArrayList(*TeleAst) = null;
    if (!is_paren_end(pn2.*.body)) {

        // Function Call Body Token Queue
        var token_queue2 = TokenQueue.init(allocator) catch {
            return ParserError.ParsingFailure;
        };

        // TODO: Handle multiple arguments and nested commas

        while (!token_queue.empty()) {
            const n2 = token_queue.pop() catch {
                return ParserError.ParsingFailure;
            };

            if (is_paren_end(n2.*.body)) {
                allocator.free(n2.*.body);
                allocator.destroy(n2);
                break;
            }

            token_queue2.push(n2.*.body, n2.*.line, n2.*.col) catch {
                return ParserError.ParsingFailure;
            };
            allocator.destroy(n2);
        }

        var alist = parse_tokens(token_queue2, allocator, ParserMode.none) catch {
            return ParserError.ParsingFailure;
        };

        if (alist.items.len > 0) {
            if (alist.items.len != 1) {
                return ParserError.ParsingFailure;
            }

            // Only supports one argument right now
            const a = alist.pop();

            // Append Argument List Children
            var c = std.ArrayList(*TeleAst).init(allocator);
            c.append(a) catch {
                return ParserError.ParsingFailure;
            };
            children = c;
        }

        alist.deinit();
        if (!token_queue2.empty()) {
            return ParserError.ParsingFailure;
        }

        token_queue2.deinit();
    } else {
        // Free Paren End Token
        const tn = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };
        allocator.free(tn.*.body);
        allocator.destroy(tn);
    }

    const t = allocator.create(TeleAst) catch {
        return ParserError.ParsingFailure;
    };
    t.*.body = body;
    t.*.ast_type = TeleAstType.function_call;
    t.*.children = children;
    return t;
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
