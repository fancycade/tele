const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const TokenQueue = tokenizer.TokenQueue;
const tele_ast = @import("tele/ast.zig");
const TeleAst = tele_ast.Ast;
const TeleAstType = tele_ast.AstType;

const ParserError = error{ParsingFailure};

const ParserMode = enum { none, op };

pub fn parse_reader(r: anytype, allocator: std.mem.Allocator) !std.ArrayList(*const TeleAst) {
    const token_queue = try tokenizer.read_tokens(r, allocator);
    defer token_queue.deinit();

    return try parse_tokens(token_queue, allocator, ParserMode.none);
}

fn parse_tokens(token_queue: *TokenQueue, allocator: std.mem.Allocator, mode: ParserMode) ParserError!std.ArrayList(*const TeleAst) {
    var list = std.ArrayList(*const TeleAst).init(allocator);

    while (true) {
        if (token_queue.empty()) {
            break;
        }

        const node = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };

        if (is_float(node.*.body)) {
            const buf = allocator.alloc(u8, node.*.body.len) catch {
                return ParserError.ParsingFailure;
            };
            std.mem.copyForwards(u8, buf, node.*.body);
            list.append(&TeleAst{ .body = buf, .ast_type = TeleAstType.float, .children = null }) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_int(node.*.body)) {
            const buf = allocator.alloc(u8, node.*.body.len) catch {
                return ParserError.ParsingFailure;
            };
            std.mem.copyForwards(u8, buf, node.*.body);
            list.append(&TeleAst{ .body = buf, .ast_type = TeleAstType.float, .children = null }) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_atom(node.*.body)) {
            const buf = allocator.alloc(u8, node.*.body.len) catch {
                return ParserError.ParsingFailure;
            };
            std.mem.copyForwards(u8, buf, node.*.body);
            list.append(&TeleAst{ .body = buf, .ast_type = TeleAstType.float, .children = null }) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_binary(node.*.body)) {
            const buf = allocator.alloc(u8, node.*.body.len) catch {
                return ParserError.ParsingFailure;
            };
            std.mem.copyForwards(u8, buf, node.*.body);
            list.append(&TeleAst{ .body = buf, .ast_type = TeleAstType.float, .children = null }) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_tuple_start(node.*.body)) {
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
                        break;
                    }
                }

                token_queue2.push(node2.*.body) catch {
                    return ParserError.ParsingFailure;
                };
            }

            if (count != 0) {
                return ParserError.ParsingFailure;
            }

            const children = parse_tokens(token_queue2, allocator, ParserMode.none) catch {
                return ParserError.ParsingFailure;
            };

            list.append(&TeleAst{ .body = "", .ast_type = TeleAstType.tuple, .children = children }) catch {
                return ParserError.ParsingFailure;
            };

            token_queue2.deinit();
        } else if (is_list_start(node.*.body)) {
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
                        break;
                    }
                }

                token_queue2.push(node2.*.body) catch {
                    return ParserError.ParsingFailure;
                };
            }

            if (count != 0) {
                return ParserError.ParsingFailure;
            }

            const children = parse_tokens(token_queue2, allocator, ParserMode.none) catch {
                return ParserError.ParsingFailure;
            };

            list.append(&TeleAst{ .body = "", .ast_type = TeleAstType.list, .children = children }) catch {
                return ParserError.ParsingFailure;
            };

            token_queue2.deinit();
        } else if (is_map_start(node.*.body)) {
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
                        break;
                    }
                }

                token_queue2.push(node2.*.body) catch {
                    return ParserError.ParsingFailure;
                };
            }

            if (count != 0) {
                return ParserError.ParsingFailure;
            }

            const children = parse_tokens(token_queue2, allocator, ParserMode.none) catch {
                return ParserError.ParsingFailure;
            };

            list.append(&TeleAst{ .body = "", .ast_type = TeleAstType.map, .children = children }) catch {
                return ParserError.ParsingFailure;
            };

            token_queue2.deinit();
        } else if (is_operator(node.*.body)) {
            if (list.items.len == 0) {
                return ParserError.ParsingFailure;
                // TODO: Error expected value on left side of operator
            }
            const ast1 = list.pop();

            var alist = parse_tokens(token_queue, allocator, ParserMode.op) catch {
                return ParserError.ParsingFailure;
            };
            defer alist.deinit();

            const ast2 = alist.pop();

            var children = std.ArrayList(*const TeleAst).init(allocator);
            children.append(ast1) catch {
                return ParserError.ParsingFailure;
            };
            children.append(ast2) catch {
                return ParserError.ParsingFailure;
            };

            const buf = allocator.alloc(u8, node.*.body.len) catch {
                return ParserError.ParsingFailure;
            };
            std.mem.copyForwards(u8, buf, node.*.body);

            list.append(&TeleAst{ .body = buf, .ast_type = TeleAstType.op, .children = children }) catch {
                return ParserError.ParsingFailure;
            };
            alist.deinit();
        } else if (is_keyword(node.*.body)) {
            const node3 = token_queue.pop() catch {
                return ParserError.ParsingFailure;
            };
            const buf = allocator.alloc(u8, node3.*.body.len) catch {
                return ParserError.ParsingFailure;
            };
            std.mem.copyForwards(u8, buf, node3.*.body);

            var token_queue2 = TokenQueue.init(allocator) catch {
                return ParserError.ParsingFailure;
            };
            while (!token_queue.empty()) {
                const node2 = token_queue.pop() catch {
                    return ParserError.ParsingFailure;
                };

                if (is_colon(node2.*.body)) {
                    break;
                } else {
                    token_queue2.push(node2.*.body) catch {
                        return ParserError.ParsingFailure;
                    };
                }
            }

            const func_sig = parse_function_signature(token_queue2, allocator) catch {
                return ParserError.ParsingFailure;
            };
            var children = std.ArrayList(*const TeleAst).init(allocator);
            children.append(func_sig) catch {
                return ParserError.ParsingFailure;
            };

            token_queue2.deinit();

            var token_queue3 = TokenQueue.init(allocator) catch {
                return ParserError.ParsingFailure;
            };
            while (!token_queue.empty()) {
                const node2 = token_queue.pop() catch {
                    return ParserError.ParsingFailure;
                };

                if (is_keyword(node2.*.body)) {
                    break;
                } else {
                    token_queue3.push(node2.*.body) catch {
                        return ParserError.ParsingFailure;
                    };
                }
            }

            var alist = parse_tokens(token_queue3, allocator, ParserMode.none) catch {
                return ParserError.ParsingFailure;
            };
            var temp = std.ArrayList(*const TeleAst).init(allocator);

            // Do this to get the order correct for the children list
            while (alist.items.len > 0) {
                const a = alist.pop();
                temp.append(a) catch {
                    return ParserError.ParsingFailure;
                };
            }
            alist.deinit();

            while (temp.items.len > 0) {
                const a = temp.pop();
                children.append(a) catch {
                    return ParserError.ParsingFailure;
                };
            }
            temp.deinit();

            list.append(&TeleAst{ .body = buf, .ast_type = TeleAstType.function_def, .children = children }) catch {
                return ParserError.ParsingFailure;
            };

            token_queue3.deinit();
        } else {
            const buf = allocator.alloc(u8, node.*.body.len) catch {
                return ParserError.ParsingFailure;
            };
            std.mem.copyForwards(u8, buf, node.*.body);
            list.append(&TeleAst{ .body = buf, .ast_type = TeleAstType.variable, .children = null }) catch {
                return ParserError.ParsingFailure;
            };
        }

        allocator.free(node.*.body);
        allocator.destroy(node);

        if (mode == ParserMode.op) {
            break;
        }
    }

    return list;
}

fn parse_function_signature(token_queue: *TokenQueue, allocator: std.mem.Allocator) !*TeleAst {
    const init = try token_queue.pop();
    if (!is_paren_start(init.*.body)) {
        // TODO: Error expected paren start
        return ParserError.ParsingFailure;
    }

    var children = std.ArrayList(*const TeleAst).init(allocator);

    var success: bool = false;
    while (!token_queue.empty()) {
        const n = try token_queue.pop();

        if (is_comma(n.*.body) or is_space(n.*.body)) {
            // Skip
        } else if (is_paren_end(n.*.body)) {
            success = true;
        } else {
            const buf = try allocator.alloc(u8, n.*.body.len);
            std.mem.copyForwards(u8, buf, n.*.body);

            const inner_tast = try allocator.create(TeleAst);
            inner_tast.*.body = buf;
            inner_tast.*.ast_type = TeleAstType.variable;
            inner_tast.*.children = null;
            try children.append(inner_tast);
        }
    }

    const tast = try allocator.create(TeleAst);
    tast.*.body = "";
    tast.*.ast_type = TeleAstType.function_signature;
    tast.*.children = children;

    return tast;
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
        '+', '*', '/', '!' => {
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

fn is_colon(buf: []const u8) bool {
    return buf[0] == ':';
}

fn is_comma(buf: []const u8) bool {
    return buf[0] == ',';
}

fn is_space(buf: []const u8) bool {
    return buf[0] == ' ';
}
