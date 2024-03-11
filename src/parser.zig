const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const TokenQueue = tokenizer.TokenQueue;
const tele_ast = @import("tele/ast.zig");
const TeleAst = tele_ast.Ast;
const TeleAstType = tele_ast.AstType;

const ParserError = error{ParsingFailure};

fn parse_reader(r: anytype, allocator: std.mem.Allocator) !std.ArrayList(*const TeleAst) {
    const token_queue = tokenizer.read_tokens(r, allocator);
    defer token_queue.deinit();

    return try parse_tokens(token_queue, allocator);
}

fn parse_tokens(token_queue: *TokenQueue, allocator: std.mem.Allocator) ParserError!std.ArrayList(*const TeleAst) {
    var list = std.ArrayList(*const TeleAst).init(allocator);

    while (true) {
        if (token_queue.empty()) {
            break;
        }

        const node = try token_queue.pop();

        if (is_float(node.*.body)) {
            const buf = allocator.alloc(u8, node.*.body.len);
            std.mem.copyForwards(u8, buf, node.*.body);
            try list.append(&TeleAst{ .body = buf, .ast_type = TeleAstType.float, .children = null });
        } else if (is_int(node.*.body)) {
            const buf = allocator.alloc(u8, node.*.body.len);
            std.mem.copyForwards(u8, buf, node.*.body);
            try list.append(&TeleAst{ .body = buf, .ast_type = TeleAstType.float, .children = null });
        } else if (is_atom(node.*.body)) {
            const buf = allocator.alloc(u8, node.*.body.len);
            std.mem.copyForwards(u8, buf, node.*.body);
            try list.append(&TeleAst{ .body = buf, .ast_type = TeleAstType.float, .children = null });
        } else if (is_binary(node.*.body)) {
            const buf = allocator.alloc(u8, node.*.body.len);
            std.mem.copyForwards(u8, buf, node.*.body);
            try list.append(&TeleAst{ .body = buf, .ast_type = TeleAstType.float, .children = null });
        } else if (is_tuple_start(node.*.body)) {
            var token_queue2 = TokenQueue.init(allocator);
            var count: usize = 1;

            while (!token_queue.empty()) {
                const node2 = try token_queue.pop();

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

                try token_queue2.push(node2);
            }

            if (count != 0) {
                return ParserError.ParsingFailure;
            }

            const children = try parse_tokens(token_queue2, allocator);

            try list.append(&TeleAst{ .body = "", .ast_type = TeleAstType.tuple, .children = children });

            token_queue2.deinit();
        } else if (is_list_start(node.*.body)) {
            var token_queue2 = TokenQueue.init(allocator);
            var count: usize = 1;

            while (!token_queue.empty()) {
                const node2 = try token_queue.pop();

                if (is_list_start(node2.*.body)) {
                    count += 1;
                } else if (is_list_end(node2.*.body)) {
                    count -= 1;
                    if (count == 0) {
                        break;
                    }
                }

                try token_queue2.push(node2);
            }

            if (count != 0) {
                return ParserError.ParsingFailure;
            }

            const children = try parse_tokens(token_queue2, allocator);

            try list.append(&TeleAst{ .body = "", .ast_type = TeleAstType.list, .children = children });

            token_queue2.deinit();
        } else if (is_map_start(node.*.body)) {
            var token_queue2 = TokenQueue.init(allocator);
            var count: usize = 1;

            while (!token_queue.empty()) {
                const node2 = try token_queue.pop();

                if (is_map_start(node2.*.body)) {
                    count += 1;
                } else if (is_map_end(node2.*.body)) {
                    count -= 1;
                    if (count == 0) {
                        break;
                    }
                }

                try token_queue2.push(node2);
            }

            if (count != 0) {
                return ParserError.ParsingFailure;
            }

            const children = try parse_tokens(token_queue2, allocator);

            try list.append(&TeleAst{ .body = "", .ast_type = TeleAstType.map, .children = children });

            token_queue2.deinit();
        } else {
            const buf = allocator.alloc(u8, node.*.body.len);
            std.mem.copyForwards(u8, buf, node.*.body);
            try list.append(&TeleAst{ .body = buf, .ast_type = TeleAstType.variable, .children = null });
        }

        allocator.free(node.*.body);
        allocator.destroy(node);
    }

    return list;
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
