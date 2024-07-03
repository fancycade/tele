const std = @import("std");
const test_allocator = std.testing.allocator;
const tokenizer = @import("tokenizer.zig");
const TokenQueue = tokenizer.TokenQueue;
const tele_ast = @import("tele/ast.zig");
const TeleAst = tele_ast.Ast;
const TeleAstType = tele_ast.AstType;

const ParserError = error{ ParsingFailure, TokenFailure, ExpectedStatement, InvalidStatement };

const ParserMode = enum { none, op };

pub fn parse_reader(r: anytype, allocator: std.mem.Allocator) !std.ArrayList(*TeleAst) {
    const token_queue = try tokenizer.read_tokens(r, allocator);

    const parser = try Parser.init(token_queue, allocator);
    defer parser.deinit(false);
    const result = try parser.parse();

    if (!token_queue.empty()) {
        return ParserError.ParsingFailure;
    }

    return result;
}

pub const Parser = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    token_queue: *TokenQueue,
    ast_stack: std.ArrayList(*TeleAst),

    pub fn init(token_queue: *TokenQueue, allocator: std.mem.Allocator) !*Self {
        const parser = try allocator.create(Self);
        parser.*.allocator = allocator;
        parser.*.token_queue = token_queue;
        parser.*.ast_stack = std.ArrayList(*TeleAst).init(allocator);
        return parser;
    }

    pub fn deinit(self: *Self, ast: bool) void {
        if (ast) {
            tele_ast.free_tele_ast_list(self.ast_stack, self.allocator);
        }
        self.token_queue.deinit();
        self.allocator.destroy(self);
    }

    pub fn parse(self: *Self) !std.ArrayList(*TeleAst) {
        try self.parse_statements(false);

        if (!self.token_queue.empty()) {
            return ParserError.ParsingFailure;
        }

        return self.ast_stack;
    }

    pub fn parse_statements(self: *Self, allow_exps: bool) !void {
        while (!self.token_queue.empty()) {
            const pn = self.token_queue.peek() catch {
                return ParserError.ParsingFailure;
            };

            if (is_statement_keyword(pn.*.body)) {
                if (is_type_keyword(pn.*.body)) {
                    try self.parse_type_definition();
                } else if (is_spec_keyword(pn.*.body)) {
                    try self.parse_spec_definition();
                } else if (is_fun_keyword(pn.*.body)) {
                    try self.parse_function_definition(false);
                } else if (is_funp_keyword(pn.*.body)) {
                    try self.parse_function_definition(true);
                } else if (is_record_keyword(pn.*.body)) {
                    try self.parse_record_definition();
                } else {
                    return ParserError.InvalidStatement;
                }
            } else {
                if (allow_exps) {
                    try self.parse_exp(self.token_queue);
                    if (!self.token_queue.empty()) {
                        return ParserError.ParsingFailure;
                    }
                } else {
                    // TODO: Expected Statement Error
                    return ParserError.ExpectedStatement;
                }
            }
        }
    }

    fn parse_function_definition(self: *Self, private: bool) !void {

        // Pop off def/defp
        const n = self.token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };
        const current_col = n.*.col;
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        const node3 = self.token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };

        // Function Definition Name
        const buf = node3.*.body;
        self.allocator.destroy(node3);
        errdefer self.allocator.free(buf);

        // Function Definition Signature
        var token_queue2 = TokenQueue.init(self.allocator) catch {
            return ParserError.ParsingFailure;
        };
        errdefer token_queue2.deinit();

        // Gather tokens for function signature
        var map_count: usize = 0;
        while (!self.token_queue.empty()) {
            const node2 = try self.token_queue.pop();
            errdefer self.allocator.free(node2.*.body);
            errdefer self.allocator.destroy(node2);

            if (is_map_start(node2.*.body)) {
                map_count += 1;
            } else if (is_map_end(node2.*.body)) {
                map_count -= 1;
            }

            if (map_count == 0 and is_colon(node2.*.body)) {
                self.allocator.free(node2.*.body);
                self.allocator.destroy(node2);
                break;
            } else {
                try token_queue2.push(node2.*.body, node2.*.line, node2.col);
            }

            self.allocator.destroy(node2);
        }

        try self.parse_function_signature(token_queue2, false);

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.free_tele_ast_list(children, self.allocator);
        //TODO: Check if ast stack has at least one item
        try children.append(self.ast_stack.pop());

        // Function Definition Body
        var token_queue3 = try TokenQueue.init(self.allocator);
        errdefer token_queue3.deinit();
        if (self.token_queue.empty()) {
            return ParserError.ParsingFailure;
        }

        // Gather tokens for function body
        while (!self.token_queue.empty()) {
            const peek_node2 = try self.token_queue.peek();
            if (peek_node2.*.col <= current_col) {
                break;
            }

            const node2 = try self.token_queue.pop();
            errdefer self.allocator.free(node2.*.body);
            errdefer self.allocator.destroy(node2);

            try token_queue3.push(node2.*.body, node2.*.line, node2.*.col);
            self.allocator.destroy(node2);
        }

        if (token_queue3.empty()) {
            return ParserError.ParsingFailure;
        }

        var alist = self.parse_body(token_queue3) catch {
            return ParserError.ParsingFailure;
        };
        errdefer tele_ast.free_tele_ast_list(alist, self.allocator);

        for (alist.items) |a| {
            try children.append(a);
        }
        alist.deinit();

        // Assemble Function Definition Ast
        const t = try self.allocator.create(TeleAst);
        t.*.body = buf;
        if (private) {
            t.*.ast_type = TeleAstType.function_defp;
        } else {
            t.*.ast_type = TeleAstType.function_def;
        }
        t.*.children = children;
        t.*.col = 0;

        token_queue3.deinit();
        token_queue2.deinit();

        try self.ast_stack.append(t);
    }

    fn parse_function_signature(self: *Self, token_queue: *TokenQueue, type_exp: bool) !void {
        const ini = try token_queue.pop();
        if (!is_paren_start(ini.*.body)) {
            self.allocator.free(ini.*.body);
            self.allocator.destroy(ini);
            // TODO: Error expected paren start
            return ParserError.ParsingFailure;
        }
        self.allocator.free(ini.*.body);
        self.allocator.destroy(ini);

        var children: ?std.ArrayList(*TeleAst) = null;

        const pn2 = try token_queue.peek();

        if (!is_paren_end(pn2.*.body)) {
            children = std.ArrayList(*TeleAst).init(self.allocator);
            errdefer tele_ast.free_tele_ast_list(children.?, self.allocator);

            while (!token_queue.empty()) {
                try self.parse_function_signature_param(token_queue, type_exp);
                if (self.ast_stack.items.len < 1) {
                    return ParserError.ParsingFailure;
                }
                try children.?.append(self.ast_stack.pop());
            }
        } else {
            // Free Paren End Token
            const tn = try token_queue.pop();
            self.allocator.free(tn.*.body);
            self.allocator.destroy(tn);
        }

        // TODO: Support guard clauses for function signatures

        const tast = try self.allocator.create(TeleAst);
        tast.*.body = "";
        tast.*.ast_type = TeleAstType.function_signature;
        tast.*.children = children;
        tast.*.col = 0;

        try self.ast_stack.append(tast);
    }

    fn parse_function_signature_param(self: *Self, token_queue: *TokenQueue, type_exp: bool) !void {
        var buffer_token_queue = try TokenQueue.init(self.allocator);
        errdefer buffer_token_queue.deinit();
        defer buffer_token_queue.deinit();

        var n_count: usize = 1;
        while (!token_queue.empty()) {
            const n2 = try token_queue.pop();
            errdefer self.allocator.free(n2.*.body);
            errdefer self.allocator.destroy(n2);

            if (n_count == 1 and (is_comma(n2.*.body) or is_paren_end(n2.*.body))) {
                self.allocator.free(n2.*.body);
                self.allocator.destroy(n2);
                break;
            }

            if (is_tuple_start(n2.*.body) or is_list_start(n2.*.body) or is_map_start(n2.*.body) or is_record_start(n2.*.body) or is_paren_start(n2.*.body)) {
                n_count += 1;
            } else if (is_paren_end(n2.*.body) or is_list_end(n2.*.body) or is_map_end(n2.*.body)) {
                n_count -= 1;
            }

            try buffer_token_queue.push(n2.*.body, n2.*.line, n2.*.col);
            self.allocator.destroy(n2);
        }

        if (type_exp) {
            try self.parse_type_exp(buffer_token_queue);
        } else {
            try self.parse_exp(buffer_token_queue);
        }
    }

    fn parse_spec_definition(self: *Self) !void {
        // Pop off spec keyword
        const n = try self.token_queue.pop();
        const current_col = n.*.col;
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        // Spec Definition Name
        const node3 = self.token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };

        const buf = node3.*.body;
        self.allocator.destroy(node3);
        errdefer self.allocator.free(buf);

        // Spec Definition Signature
        var token_queue2 = TokenQueue.init(self.allocator) catch {
            self.allocator.free(node3.*.body);
            self.allocator.destroy(node3);
            return ParserError.ParsingFailure;
        };
        errdefer token_queue2.deinit();

        var map_count: usize = 0;
        while (!self.token_queue.empty()) {
            const node2 = try self.token_queue.pop();
            errdefer self.allocator.free(node2.*.body);
            errdefer self.allocator.destroy(node2);

            if (is_map_start(node2.*.body)) {
                map_count += 1;
            } else if (is_map_end(node2.*.body)) {
                map_count -= 1;
            }

            if (map_count == 0 and is_colon(node2.*.body)) {
                self.allocator.free(node2.*.body);
                self.allocator.destroy(node2);
                break;
            } else {
                try token_queue2.push(node2.*.body, node2.*.line, node2.col);
            }

            self.allocator.destroy(node2);
        }

        try self.parse_function_signature(token_queue2, true);

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.free_tele_ast_list(children, self.allocator);
        if (self.ast_stack.items.len < 1) {
            return ParserError.ParsingFailure;
        }
        try children.append(self.ast_stack.pop());

        // Spec Definition Body
        var token_queue3 = try TokenQueue.init(self.allocator);
        errdefer token_queue3.deinit();
        if (self.token_queue.empty()) {
            return ParserError.ParsingFailure;
        }

        while (!self.token_queue.empty()) {
            const peek_node2 = try self.token_queue.peek();
            if (peek_node2.*.col <= current_col) {
                break;
            }

            const node2 = try self.token_queue.pop();
            errdefer self.allocator.free(node2.*.body);
            errdefer self.allocator.destroy(node2);

            try token_queue3.push(node2.*.body, node2.*.line, node2.*.col);
            self.allocator.destroy(node2);
        }

        if (token_queue3.empty()) {
            return ParserError.ParsingFailure;
        }

        self.parse_type_exp(token_queue3) catch {
            return ParserError.ParsingFailure;
        };
        if (!token_queue3.empty()) {
            return ParserError.ParsingFailure;
        }
        if (self.ast_stack.items.len < 1) {
            return ParserError.ParsingFailure;
        }
        try children.append(self.ast_stack.pop());

        // Assemble Spec Definition Ast
        const t = try self.allocator.create(TeleAst);
        t.*.body = buf;
        t.*.ast_type = TeleAstType.spec_def;
        t.*.children = children;
        t.*.col = 0;

        token_queue2.deinit();
        token_queue3.deinit();

        try self.ast_stack.append(t);
    }

    fn parse_type_definition(self: *Self) !void {
        // Free type keyword
        const n = self.token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };
        const current_col = n.*.col;
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);
        const node3 = self.token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };

        // type Definition Name
        const buf = node3.*.body;
        self.allocator.destroy(node3);

        // Skip colon
        const cn = self.token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };
        self.allocator.free(cn.*.body);
        self.allocator.destroy(cn);

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.free_tele_ast_list(children, self.allocator);

        // type Definition Body
        var token_queue3 = try TokenQueue.init(self.allocator);
        errdefer token_queue3.deinit();
        if (self.token_queue.empty()) {
            return ParserError.ParsingFailure;
        }

        while (!self.token_queue.empty()) {
            const peek_node2 = try self.token_queue.peek();
            if (peek_node2.*.col <= current_col) {
                break;
            }

            const node2 = try self.token_queue.pop();
            errdefer self.allocator.free(node2.*.body);
            errdefer self.allocator.destroy(node2);

            try token_queue3.push(node2.*.body, node2.*.line, node2.*.col);
            self.allocator.destroy(node2);
        }

        if (token_queue3.empty()) {
            return ParserError.ParsingFailure;
        }

        try self.parse_type_exp(token_queue3);
        if (!token_queue3.empty()) {
            return ParserError.ParsingFailure;
        }
        if (self.ast_stack.items.len < 1) {
            return ParserError.ParsingFailure;
        }
        try children.append(self.ast_stack.pop());

        // Assemble Type Definition Ast
        const t = try self.allocator.create(TeleAst);
        t.*.body = buf;
        t.*.ast_type = TeleAstType.type_def;
        t.*.children = children;
        t.*.col = 0;

        token_queue3.deinit();

        try self.ast_stack.append(t);
    }

    fn parse_record_definition(self: *Self) !void {
        const n = self.token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };
        const current_col = n.*.col;
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);
        const node3 = self.token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };

        // type Definition Name
        const buf = node3.*.body;
        self.allocator.destroy(node3);

        // Skip colon
        const cn = self.token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };
        self.allocator.free(cn.*.body);
        self.allocator.destroy(cn);

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.free_tele_ast_list(children, self.allocator);

        // type Definition Body
        var token_queue3 = try TokenQueue.init(self.allocator);
        errdefer token_queue3.deinit();
        if (self.token_queue.empty()) {
            return ParserError.ParsingFailure;
        }

        while (!self.token_queue.empty()) {
            const peek_node2 = try self.token_queue.peek();
            if (peek_node2.*.col <= current_col) {
                break;
            }

            const node2 = try self.token_queue.pop();
            errdefer self.allocator.free(node2.*.body);
            errdefer self.allocator.destroy(node2);

            try token_queue3.push(node2.*.body, node2.*.line, node2.*.col);
            self.allocator.destroy(node2);
        }

        if (token_queue3.empty()) {
            return ParserError.ParsingFailure;
        }

        self.parse_tuple(token_queue3, false) catch {
            return ParserError.ParsingFailure;
        };
        if (!token_queue3.empty()) {
            return ParserError.ParsingFailure;
        }
        if (self.ast_stack.items.len < 1) {
            return ParserError.ParsingFailure;
        }

        try children.append(self.ast_stack.pop());

        // Assemble Record Definition Ast
        const t = try self.allocator.create(TeleAst);
        t.*.body = buf;
        t.*.ast_type = TeleAstType.record_def;
        t.*.children = children;
        t.*.col = 0;

        token_queue3.deinit();

        try self.ast_stack.append(t);
    }

    fn parse_exp(self: *Self, token_queue: *TokenQueue) ParserError!void {
        const pn = token_queue.peek() catch {
            return ParserError.ParsingFailure;
        };

        if (is_float(pn.*.body)) {
            self.parse_float(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_int(pn.*.body)) {
            self.parse_int(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_atom(pn.*.body)) {
            self.parse_atom(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_binary(pn.*.body)) {
            self.parse_binary(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_fun_val(pn.*.body)) {
            self.parse_fun_val(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_arrow_operator(pn.*.body)) {
            self.parse_arrow_operator(token_queue, false) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_operator(pn.*.body)) {
            self.parse_operator(token_queue, false) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_paren_start(pn.*.body)) {
            self.parse_paren_exp(token_queue, false) catch {
                return ParserError.ParsingFailure;
            };

            if (!token_queue.empty()) {
                const res = check_arrow_op_peek(token_queue) catch {
                    return ParserError.ParsingFailure;
                };
                if (res) {
                    try self.parse_exp(token_queue);
                }
            }
        } else if (is_tuple_start(pn.*.body)) {
            self.parse_tuple(token_queue, false) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_record_start(pn.*.body)) {
            self.parse_record(token_queue, false, "") catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_list_start(pn.*.body)) {
            self.parse_list(token_queue, false) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_map_start(pn.*.body)) {
            self.parse_map(token_queue, false) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_match_keyword(pn.*.body)) {
            self.parse_match_expression(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_try_keyword(pn.*.body)) {
            self.parse_try_catch(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else {
            const n = token_queue.pop() catch {
                return ParserError.ParsingFailure;
            };
            defer self.allocator.destroy(n);

            if (!token_queue.empty()) {
                // Check if next token is a paren_start
                const res = check_paren_start_peek(token_queue) catch {
                    return ParserError.ParsingFailure;
                };
                if (res) {
                    if (contains_hash(n.*.body)) {
                        self.parse_record(token_queue, true, n.*.body) catch { // Is a record variable
                            return ParserError.ParsingFailure;
                        };
                    } else { // Is a function call
                        self.parse_function_call(token_queue, n.*.body, false) catch {
                            return ParserError.ParsingFailure;
                        };
                    }
                } else {
                    self.parse_variable(n.*.body, false) catch {
                        return ParserError.ParsingFailure;
                    };
                }
            } else {
                self.parse_variable(n.*.body, false) catch {
                    return ParserError.ParsingFailure;
                };
            }
        }

        if (!token_queue.empty()) {
            const res2 = check_operator_peek(token_queue) catch {
                return ParserError.ParsingFailure;
            };
            if (res2) {
                try self.parse_exp(token_queue);
            }
        }
    }

    fn parse_type_exp(self: *Self, token_queue: *TokenQueue) ParserError!void {
        const pn = token_queue.peek() catch {
            return ParserError.ParsingFailure;
        };

        if (is_float(pn.*.body)) {
            self.parse_float(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_int(pn.*.body)) {
            self.parse_int(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_atom(pn.*.body)) {
            self.parse_atom(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_binary(pn.*.body)) {
            self.parse_binary(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_fun_val(pn.*.body)) {
            return ParserError.ParsingFailure;
        } else if (is_arrow_operator(pn.*.body)) {
            self.parse_arrow_operator(token_queue, true) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_operator(pn.*.body)) {
            self.parse_operator(token_queue, true) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_paren_start(pn.*.body)) {
            self.parse_paren_exp(token_queue, true) catch {
                return ParserError.ParsingFailure;
            };

            if (!token_queue.empty()) {
                const res = check_arrow_op_peek(token_queue) catch {
                    return ParserError.ParsingFailure;
                };
                if (res) {
                    try self.parse_exp(token_queue);
                }
            } else {
                // Expected arrow operator, paren_exp by itself not allowed in type expressions
                return ParserError.ParsingFailure;
            }
        } else if (is_tuple_start(pn.*.body)) {
            self.parse_tuple(token_queue, true) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_record_start(pn.*.body)) {
            return ParserError.ParsingFailure;
        } else if (is_list_start(pn.*.body)) {
            self.parse_list(token_queue, true) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_map_start(pn.*.body)) {
            self.parse_map(token_queue, true) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_match_keyword(pn.*.body)) {
            return ParserError.ParsingFailure;
        } else if (is_try_keyword(pn.*.body)) {
            return ParserError.ParsingFailure;
        } else {
            const n = token_queue.pop() catch {
                return ParserError.ParsingFailure;
            };
            defer self.allocator.destroy(n);

            if (!token_queue.empty()) {
                // Check if next token is a paren_start
                const res = check_paren_start_peek(token_queue) catch {
                    return ParserError.ParsingFailure;
                };
                if (res) {
                    if (contains_hash(n.*.body)) {
                        // Record literal syntax not allowed in type expression
                        return ParserError.ParsingFailure;
                    } else { // Is a function call
                        self.parse_function_call(token_queue, n.*.body, true) catch {
                            return ParserError.ParsingFailure;
                        };
                    }
                } else {
                    self.parse_variable(n.*.body, true) catch {
                        return ParserError.ParsingFailure;
                    };
                }
            } else {
                self.parse_variable(n.*.body, true) catch {
                    return ParserError.ParsingFailure;
                };
            }
        }

        if (!token_queue.empty()) {
            const res2 = check_operator_peek(token_queue) catch {
                return ParserError.ParsingFailure;
            };
            if (res2) {
                try self.parse_type_exp(token_queue);
            }
        }
    }

    fn parse_float(self: *Self, token_queue: *TokenQueue) !void {
        try self.ast_stack.append(try self.parse_value(token_queue, TeleAstType.float));
    }

    fn parse_int(self: *Self, token_queue: *TokenQueue) !void {
        try self.ast_stack.append(try self.parse_value(token_queue, TeleAstType.int));
    }

    fn parse_atom(self: *Self, token_queue: *TokenQueue) !void {
        try self.ast_stack.append(try self.parse_value(token_queue, TeleAstType.atom));
    }

    fn parse_binary(self: *Self, token_queue: *TokenQueue) !void {
        try self.ast_stack.append(try self.parse_value(token_queue, TeleAstType.binary));
    }

    fn parse_variable(self: *Self, buf: []const u8, type_exp: bool) !void {
        const t = try self.allocator.create(TeleAst);
        t.*.body = buf;
        if (type_exp) {
            t.*.ast_type = TeleAstType.function_call;
        } else {
            t.*.ast_type = TeleAstType.variable;
        }
        t.*.children = null;
        t.*.col = 0;
        try self.ast_stack.append(t);
    }

    fn parse_value(self: *Self, token_queue: *TokenQueue, ast_type: TeleAstType) !*TeleAst {
        const n = try token_queue.pop();
        const t = try self.allocator.create(TeleAst);
        t.*.body = n.*.body;
        t.*.ast_type = ast_type;
        t.*.children = null;
        t.*.col = 0;
        self.allocator.destroy(n);
        return t;
    }

    fn parse_fun_val(self: *Self, token_queue: *TokenQueue) !void {
        const n = try token_queue.pop();
        const buf = try self.allocator.alloc(u8, n.*.body.len - 1);
        var i: usize = 1;
        while (i < n.*.body.len) {
            buf[i - 1] = n.*.body[i];
            i = i + 1;
        }
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        const t = try self.allocator.create(TeleAst);
        t.*.body = buf;
        t.*.ast_type = TeleAstType.fun_val;
        t.*.children = null;
        t.*.col = 0;

        try self.ast_stack.append(t);
    }

    fn parse_operator(self: *Self, token_queue: *TokenQueue, type_exp: bool) !void {
        // Pop off operator
        const node = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };
        errdefer self.allocator.destroy(node);

        if (type_exp and !is_pipe_operator(node.*.body)) {
            self.allocator.free(node.*.body);
            return ParserError.ParsingFailure;
        }

        if (self.ast_stack.items.len < 1) {
            self.allocator.free(node.*.body);
            return ParserError.ParsingFailure;
        }
        const arg = self.ast_stack.pop();
        errdefer tele_ast.free_tele_ast(arg, self.allocator);

        self.parse_exp(token_queue) catch {
            self.allocator.free(node.*.body);
            return ParserError.ParsingFailure;
        };
        if (self.ast_stack.items.len < 1) {
            self.allocator.free(node.*.body);
            return ParserError.ParsingFailure;
        }
        const ast2 = self.ast_stack.pop();
        errdefer tele_ast.free_tele_ast(ast2, self.allocator);

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.free_tele_ast_list(children, self.allocator);
        try children.append(arg);
        try children.append(ast2);

        const t = try self.allocator.create(TeleAst);
        t.*.body = node.*.body;
        t.*.ast_type = TeleAstType.op;
        t.*.children = children;
        t.*.col = 0;

        self.ast_stack.append(t) catch {
            return ParserError.ParsingFailure;
        };

        self.allocator.destroy(node);
    }

    fn parse_paren_exp(self: *Self, token_queue: *TokenQueue, type_exp: bool) !void {
        // Pop off paren start
        const n = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };
        const col = n.*.col;
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);
        var children: ?std.ArrayList(*TeleAst) = null;

        const pn2 = try token_queue.peek();

        if (!is_paren_end(pn2.*.body)) {
            children = std.ArrayList(*TeleAst).init(self.allocator);
            errdefer tele_ast.free_tele_ast_list(children.?, self.allocator);

            // Gather paren exp tokens
            var buffer_token_queue = try TokenQueue.init(self.allocator);
            errdefer buffer_token_queue.deinit();

            var n_count: usize = 0;
            while (!token_queue.empty()) {
                const n2 = try token_queue.pop();
                errdefer self.allocator.free(n2.*.body);
                errdefer self.allocator.destroy(n2);

                if (n_count == 0) {
                    if (is_paren_end(n2.*.body)) {
                        self.allocator.free(n2.*.body);
                        self.allocator.destroy(n2);
                        break;
                    }
                }

                if (is_paren_start(n2.*.body)) {
                    n_count += 1;
                } else if (is_paren_end(n2.*.body)) {
                    n_count -= 1;
                }

                try buffer_token_queue.push(n2.*.body, n2.*.line, n2.*.col);
                self.allocator.destroy(n2);
            }

            while (!buffer_token_queue.empty()) {
                try self.parse_paren_exp_element(buffer_token_queue, type_exp);
                if (self.ast_stack.items.len < 1) {
                    return ParserError.ParsingFailure;
                }
                const ast = self.ast_stack.pop();
                errdefer tele_ast.free_tele_ast(ast, self.allocator);
                try children.?.append(ast);
            }
            buffer_token_queue.deinit();
        } else {
            // Free Paren End Token
            const tn = try token_queue.pop();
            self.allocator.free(tn.*.body);
            self.allocator.destroy(tn);
        }

        const tast = try self.allocator.create(TeleAst);
        tast.*.body = "";
        tast.*.ast_type = TeleAstType.paren_exp;
        tast.*.children = children;
        tast.*.col = col;

        try self.ast_stack.append(tast);
    }

    fn parse_paren_exp_element(self: *Self, token_queue: *TokenQueue, type_exp: bool) !void {
        // Function Call Body Token Queue
        var buffer_token_queue = try TokenQueue.init(self.allocator);
        errdefer buffer_token_queue.deinit();
        defer buffer_token_queue.deinit();

        var n_count: usize = 0;
        while (!token_queue.empty()) {
            const n2 = try token_queue.pop();
            errdefer self.allocator.free(n2.*.body);
            errdefer self.allocator.destroy(n2);

            if (n_count == 0) {
                if (is_comma(n2.*.body) or is_paren_end(n2.*.body)) {
                    self.allocator.free(n2.*.body);
                    self.allocator.destroy(n2);
                    break;
                }
            }

            if (is_paren_start(n2.*.body) or is_tuple_start(n2.*.body) or is_list_start(n2.*.body) or is_map_start(n2.*.body) or is_record_start(n2.*.body)) {
                n_count += 1;
            } else if (is_paren_end(n2.*.body) or is_list_end(n2.*.body) or is_map_end(n2.*.body)) {
                n_count -= 1;
            }

            try buffer_token_queue.push(n2.*.body, n2.*.line, n2.*.col);
            self.allocator.destroy(n2);
        }

        if (type_exp) {
            try self.parse_type_exp(buffer_token_queue);
        } else {
            try self.parse_exp(buffer_token_queue);
        }
        if (!buffer_token_queue.empty()) {
            return ParserError.ParsingFailure;
        }
    }

    fn parse_tuple(self: *Self, token_queue: *TokenQueue, type_exp: bool) !void {
        //Pop off #(

        const n = try token_queue.pop();
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.free_tele_ast_list(children, self.allocator);
        var token_queue2 = try TokenQueue.init(self.allocator);
        errdefer token_queue2.deinit();

        // Expected to have already parsed start of tuple, so count starts at 1
        var count: usize = 1;
        var end_of_tuple = false;

        while (!token_queue.empty() and !end_of_tuple) {
            while (!token_queue.empty()) {
                const node2 = try token_queue.pop();
                errdefer self.allocator.destroy(node2);

                if (is_tuple_start(node2.*.body) or is_paren_start(node2.*.body) or is_record_start(node2.*.body)) {
                    count += 1;
                } else if (is_paren_end(node2.*.body)) {
                    count -= 1;
                    if (count == 0) {
                        // Free paren end body
                        self.allocator.free(node2.*.body);
                        self.allocator.destroy(node2);
                        end_of_tuple = true;
                        break;
                    }
                }

                if (count == 1 and is_comma(node2.*.body)) {
                    self.allocator.free(node2.*.body);
                    self.allocator.destroy(node2);
                    break;
                } else {
                    try token_queue2.push(node2.*.body, node2.*.line, node2.*.col);
                }

                self.allocator.destroy(node2);
            }

            if (type_exp) {
                try self.parse_type_exp(token_queue2);
            } else {
                try self.parse_exp(token_queue2);
            }
            if (!token_queue2.empty()) {
                return ParserError.ParsingFailure;
            }
            if (self.ast_stack.items.len < 1) {
                return ParserError.ParsingFailure;
            }
            const a = self.ast_stack.pop();
            errdefer tele_ast.free_tele_ast(a, self.allocator);
            try children.append(a);
        }

        if (count != 0) {
            return ParserError.ParsingFailure;
        }

        const t = try self.allocator.create(TeleAst);
        t.*.body = "";
        t.*.ast_type = TeleAstType.tuple;
        t.*.children = children;
        t.*.col = 0;
        token_queue2.deinit();

        try self.ast_stack.append(t);
    }

    fn parse_list(self: *Self, token_queue: *TokenQueue, type_exp: bool) !void {
        // Pop off [
        const n = try token_queue.pop();
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.free_tele_ast_list(children, self.allocator);
        var token_queue2 = try TokenQueue.init(self.allocator);
        errdefer token_queue2.deinit();

        // Expected to have already parsed start of list so count starts at 1
        var count: usize = 1;
        var end_of_list = false;

        while (!token_queue.empty() and !end_of_list) {
            while (!token_queue.empty()) {
                const node2 = try token_queue.pop();
                errdefer self.allocator.destroy(node2);

                if (is_list_start(node2.*.body)) {
                    count += 1;
                } else if (is_list_end(node2.*.body)) {
                    count -= 1;
                    if (count == 0) {
                        self.allocator.free(node2.*.body);
                        self.allocator.destroy(node2);
                        end_of_list = true;
                        break;
                    }
                }

                if (count == 1 and is_comma(node2.*.body)) {
                    self.allocator.free(node2.*.body);
                    self.allocator.destroy(node2);
                    break;
                } else {
                    try token_queue2.push(node2.*.body, node2.*.line, node2.*.col);
                }

                self.allocator.destroy(node2);
            }

            if (type_exp) {
                try self.parse_type_exp(token_queue2);
            } else {
                try self.parse_exp(token_queue2);
            }

            if (!token_queue2.empty()) {
                return ParserError.ParsingFailure;
            }
            if (self.ast_stack.items.len < 1) {
                return ParserError.ParsingFailure;
            }
            const a = self.ast_stack.pop();
            errdefer tele_ast.free_tele_ast(a, self.allocator);
            try children.append(a);
        }

        if (count != 0) {
            return ParserError.ParsingFailure;
        }

        const t = try self.allocator.create(TeleAst);
        t.*.body = "";
        t.*.ast_type = TeleAstType.list;
        t.*.children = children;
        t.*.col = 0;
        token_queue2.deinit();

        try self.ast_stack.append(t);
    }

    fn parse_map(self: *Self, token_queue: *TokenQueue, type_exp: bool) !void {
        // Pop off {
        const n = try token_queue.pop();
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.free_tele_ast_list(children, self.allocator);
        var token_queue2 = try TokenQueue.init(self.allocator);
        errdefer token_queue2.deinit();
        var count: usize = 1;
        var end_of_map: bool = false;

        while (!token_queue.empty() and !end_of_map) {
            while (!token_queue.empty()) {
                const node2 = token_queue.pop() catch {
                    return ParserError.ParsingFailure;
                };
                errdefer self.allocator.free(node2.*.body);
                errdefer self.allocator.destroy(node2);

                if (is_map_start(node2.*.body)) {
                    count += 1;
                } else if (is_map_end(node2.*.body)) {
                    count -= 1;
                    if (count == 0) {
                        self.allocator.free(node2.*.body);
                        self.allocator.destroy(node2);
                        end_of_map = true;
                        break;
                    }
                }

                if (count == 1 and (is_comma(node2.*.body) or is_colon(node2.*.body))) {
                    self.allocator.free(node2.*.body);
                    self.allocator.destroy(node2);
                    break;
                } else {
                    try token_queue2.push(node2.*.body, node2.*.line, node2.*.col);
                }

                self.allocator.destroy(node2);
            }

            if (type_exp) {
                try self.parse_type_exp(token_queue2);
            } else {
                try self.parse_exp(token_queue2);
            }
            if (!token_queue2.empty()) {
                return ParserError.ParsingFailure;
            }
            if (self.ast_stack.items.len < 1) {
                return ParserError.ParsingFailure;
            }
            const a = self.ast_stack.pop();
            errdefer tele_ast.free_tele_ast(a, self.allocator);
            try children.append(a);
        }

        if (count != 0) {
            return ParserError.ParsingFailure;
        }

        const t = try self.allocator.create(TeleAst);
        t.*.body = "";
        t.*.ast_type = TeleAstType.map;
        t.*.children = children;
        t.*.col = 0;

        token_queue2.deinit();
        try self.ast_stack.append(t);
    }

    fn parse_record(self: *Self, token_queue: *TokenQueue, variable: bool, buf: []const u8) !void {
        var buf2: []const u8 = undefined;
        if (!variable) {
            const node = try token_queue.pop();
            buf2 = node.*.body;
            defer self.allocator.destroy(node);
        } else {
            buf2 = buf;

            // Pop off paren start
            const pn = try token_queue.pop();
            self.allocator.free(pn.*.body);
            self.allocator.destroy(pn);
        }

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.free_tele_ast_list(children, self.allocator);
        var buffer_token_queue = try TokenQueue.init(self.allocator);
        errdefer buffer_token_queue.deinit();

        var count: usize = 1;
        var end_of_record: bool = false;

        while (!token_queue.empty() and !end_of_record) {
            while (!token_queue.empty()) {
                const node2 = try token_queue.pop();
                errdefer self.allocator.free(node2.*.body);
                errdefer self.allocator.destroy(node2);

                if (is_tuple_start(node2.*.body) or is_paren_start(node2.*.body) or is_record_start(node2.*.body) or is_list_start(node2.*.body) or is_map_start(node2.*.body)) {
                    count += 1;
                } else if (is_paren_end(node2.*.body) or is_list_end(node2.*.body) or is_map_end(node2.*.body)) {
                    count -= 1;
                    if (count == 0) {
                        // Free paren end body
                        self.allocator.free(node2.*.body);
                        self.allocator.destroy(node2);
                        end_of_record = true;
                        break;
                    }
                }

                if (count == 1 and (is_comma(node2.*.body) or is_equal(node2.*.body))) {
                    self.allocator.free(node2.*.body);
                    self.allocator.destroy(node2);
                    break;
                } else {
                    try buffer_token_queue.push(node2.*.body, node2.*.line, node2.*.col);
                }

                self.allocator.destroy(node2);
            }

            try self.parse_exp(buffer_token_queue);
            if (!buffer_token_queue.empty()) {
                return ParserError.ParsingFailure;
            }
            if (self.ast_stack.items.len < 1) {
                return ParserError.ParsingFailure;
            }
            const a = self.ast_stack.pop();

            errdefer tele_ast.free_tele_ast(a, self.allocator);
            try children.append(a);
        }

        if (count != 0) {
            return ParserError.ParsingFailure;
        }

        buffer_token_queue.deinit();
        const t = try self.allocator.create(TeleAst);
        if (variable) {
            t.*.body = buf2;
        } else {
            t.*.body = try extract_record_name(buf2, self.allocator);
            self.allocator.free(buf2);
        }
        t.*.ast_type = TeleAstType.record;
        t.*.children = children;
        t.*.col = 0;
        try self.ast_stack.append(t);
    }

    fn parse_body(self: *Self, token_queue: *TokenQueue) !std.ArrayList(*TeleAst) {
        var alist = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.free_tele_ast_list(alist, self.allocator);

        while (!token_queue.empty()) {
            try self.parse_exp(token_queue);
            if (self.ast_stack.items.len < 1) {
                return ParserError.ParsingFailure;
            }
            try alist.append(self.ast_stack.pop());
        }

        if (alist.items.len == 0) {
            return ParserError.ParsingFailure;
        }
        return alist;
    }

    fn parse_arrow_operator(self: *Self, token_queue: *TokenQueue, type_exp: bool) !void {
        if (self.ast_stack.items.len < 1) {
            return ParserError.ParsingFailure;
        }
        const paren_exp = self.ast_stack.pop();
        if (paren_exp.*.ast_type != TeleAstType.paren_exp) {
            tele_ast.free_tele_ast(paren_exp, self.allocator);
            return ParserError.ParsingFailure;
        }

        // Free arrow operator
        const n = token_queue.pop() catch {
            tele_ast.free_tele_ast(paren_exp, self.allocator);
            return ParserError.ParsingFailure;
        };
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        var buffer_token_queue = try TokenQueue.init(self.allocator);
        errdefer buffer_token_queue.deinit();
        if (token_queue.empty()) {
            tele_ast.free_tele_ast(paren_exp, self.allocator);
            return ParserError.ParsingFailure;
        }

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.free_tele_ast_list(children, self.allocator);

        const func_sig = try paren_exp_to_function_signature(paren_exp, self.allocator);

        // Free paren exp memory, copied children to func_sig
        if (paren_exp.*.children != null) {
            paren_exp.*.children.?.deinit();
        }
        try children.append(func_sig);

        while (!token_queue.empty()) {
            const peek_node = try token_queue.peek();
            if (paren_exp.*.col >= peek_node.*.col) {
                break;
            }

            const node2 = try token_queue.pop();
            errdefer self.allocator.free(node2.*.body);
            errdefer self.allocator.destroy(node2);

            try buffer_token_queue.push(node2.*.body, node2.*.line, node2.*.col);
            self.allocator.destroy(node2);
        }
        self.allocator.destroy(paren_exp);

        if (buffer_token_queue.empty()) {
            return ParserError.ParsingFailure;
        }

        if (type_exp) {
            self.parse_type_exp(buffer_token_queue) catch {
                return ParserError.ParsingFailure;
            };
            if (self.ast_stack.items.len < 1) {
                return ParserError.ParsingFailure;
            }
            try children.append(self.ast_stack.pop());
        } else {
            var alist = self.parse_body(buffer_token_queue) catch {
                return ParserError.ParsingFailure;
            };
            errdefer tele_ast.free_tele_ast_list(alist, self.allocator);

            for (alist.items) |a| {
                try children.append(a);
            }
            alist.deinit();
        }

        // Assemble Anonymous Function Definition Ast
        const t = try self.allocator.create(TeleAst);
        t.*.body = "";
        t.*.ast_type = TeleAstType.anonymous_function;
        t.*.children = children;
        t.*.col = 0;

        buffer_token_queue.deinit();

        try self.ast_stack.append(t);
    }

    fn parse_match_expression(self: *Self, token_queue: *TokenQueue) !void {
        // Pop off match keyword
        const n = try token_queue.pop();
        const current_col = n.*.col;
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        var buffer_token_queue = try TokenQueue.init(self.allocator);
        errdefer buffer_token_queue.deinit();

        while (!token_queue.empty()) {
            const pnode2 = token_queue.peek() catch {
                return ParserError.ParsingFailure;
            };

            if (pnode2.*.col <= current_col) {
                break;
            } else {
                const node = try token_queue.pop();
                errdefer self.allocator.free(node.*.body);
                errdefer self.allocator.destroy(node);

                buffer_token_queue.push(node.*.body, node.*.line, node.*.col) catch {
                    return ParserError.ParsingFailure;
                };

                self.allocator.destroy(node);
            }
        }

        try self.parse_match_signature(buffer_token_queue);
        if (self.ast_stack.items.len < 1) {
            return ParserError.ParsingFailure;
        }
        const signature_ast = self.ast_stack.pop();
        errdefer tele_ast.free_tele_ast(signature_ast, self.allocator);

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.free_tele_ast_list(children, self.allocator);
        try children.append(signature_ast);

        var body = try self.parse_match_body(buffer_token_queue);
        errdefer tele_ast.free_tele_ast_list(body, self.allocator);
        if (!buffer_token_queue.empty()) {
            return ParserError.ParsingFailure;
        }
        buffer_token_queue.deinit();

        for (body.items) |c| {
            try children.append(c);
        }
        body.deinit();

        const final_ast = try self.allocator.create(TeleAst);
        final_ast.*.body = "";
        final_ast.*.ast_type = TeleAstType.case;
        final_ast.*.children = children;
        final_ast.*.col = 0;

        try self.ast_stack.append(final_ast);
    }

    fn parse_match_signature(self: *Self, token_queue: *TokenQueue) !void {
        var buffer_token_queue = try TokenQueue.init(self.allocator);
        errdefer buffer_token_queue.deinit();
        // Parse Match Signature
        while (!token_queue.empty()) {
            const node = try token_queue.pop();
            errdefer self.allocator.free(node.*.body);
            errdefer self.allocator.destroy(node);

            if (is_colon(node.*.body)) {
                self.allocator.free(node.*.body);
                self.allocator.destroy(node);
                break;
            } else {
                try buffer_token_queue.push(node.*.body, node.*.line, node.*.col);
                self.allocator.destroy(node);
            }
        }

        if (buffer_token_queue.empty()) {
            return ParserError.ParsingFailure;
        }

        try self.parse_exp(buffer_token_queue);
        if (!buffer_token_queue.empty()) {
            return ParserError.ParsingFailure;
        }
        buffer_token_queue.deinit();
    }

    fn parse_match_body(self: *Self, token_queue: *TokenQueue) !std.ArrayList(*TeleAst) {
        var clause_col: usize = 0;
        var alist = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.free_tele_ast_list(alist, self.allocator);

        // Parse Match Clauses
        while (!token_queue.empty()) {
            var children = std.ArrayList(*TeleAst).init(self.allocator);
            errdefer tele_ast.free_tele_ast_list(children, self.allocator);

            try self.parse_case_clause_signature(token_queue, &clause_col);
            if (self.ast_stack.items.len < 1) {
                return ParserError.ParsingFailure;
            }
            try children.append(self.ast_stack.pop());

            var case_clause_body = try self.parse_case_clause_body(token_queue, clause_col);
            errdefer tele_ast.free_tele_ast_list(case_clause_body, self.allocator);
            for (case_clause_body.items) |c| {
                try children.append(c);
            }
            case_clause_body.deinit();

            const t = try self.allocator.create(TeleAst);
            t.body = "";
            t.ast_type = TeleAstType.case_clause;
            t.children = children;
            t.col = 0;
            errdefer tele_ast.free_tele_ast(t, self.allocator);

            try alist.append(t);
        }

        return alist;
    }

    fn parse_try_catch(self: *Self, token_queue: *TokenQueue) !void {
        // Pop off try keyword
        const n = try token_queue.pop();
        const current_col = n.*.col;
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        var buffer_token_queue = try TokenQueue.init(self.allocator);
        errdefer buffer_token_queue.deinit();

        // Parse Try Expression
        while (!token_queue.empty()) {
            const pnode2 = token_queue.peek() catch {
                return ParserError.ParsingFailure;
            };

            if (pnode2.*.col <= current_col) {
                break;
            } else {
                const node = try token_queue.pop();
                errdefer self.allocator.free(node.*.body);
                errdefer self.allocator.destroy(node);

                buffer_token_queue.push(node.*.body, node.*.line, node.*.col) catch {
                    return ParserError.ParsingFailure;
                };
                self.allocator.destroy(node);
            }
        }

        try self.parse_match_signature(buffer_token_queue);
        if (self.ast_stack.items.len < 1) {
            return ParserError.ParsingFailure;
        }
        const signature_ast = self.ast_stack.pop();
        errdefer tele_ast.free_tele_ast(signature_ast, self.allocator);

        var try_children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.free_tele_ast_list(try_children, self.allocator);
        try try_children.append(signature_ast);

        var body = try self.parse_match_body(buffer_token_queue);
        errdefer tele_ast.free_tele_ast_list(body, self.allocator);
        if (!buffer_token_queue.empty()) {
            return ParserError.ParsingFailure;
        }

        for (body.items) |c| {
            try try_children.append(c);
        }
        body.deinit();

        const try_ast = try self.allocator.create(TeleAst);
        try_ast.*.body = "";
        try_ast.*.ast_type = TeleAstType.try_exp;
        try_ast.*.children = try_children;
        try_ast.*.col = 0;

        // Pop off catch keyword
        const n_catch = try token_queue.pop();
        errdefer self.allocator.free(n_catch.*.body);
        errdefer self.allocator.destroy(n_catch);
        if (!is_catch_keyword(n_catch.*.body)) {
            return ParserError.ParsingFailure;
        }
        self.allocator.free(n_catch.*.body);
        self.allocator.destroy(n_catch);

        // Pop off colon
        const n_colon = try token_queue.pop();
        errdefer self.allocator.free(n_colon.*.body);
        errdefer self.allocator.destroy(n_colon);
        if (!is_colon(n_colon.*.body)) {
            return ParserError.ParsingFailure;
        }
        self.allocator.free(n_colon.*.body);
        self.allocator.destroy(n_colon);

        // Parse Catch Expression
        while (!token_queue.empty()) {
            const pnode2 = token_queue.peek() catch {
                return ParserError.ParsingFailure;
            };

            if (pnode2.*.col <= current_col) {
                break;
            } else {
                const node = try token_queue.pop();
                errdefer self.allocator.free(node.*.body);
                errdefer self.allocator.destroy(node);

                buffer_token_queue.push(node.*.body, node.*.line, node.*.col) catch {
                    return ParserError.ParsingFailure;
                };
                self.allocator.destroy(node);
            }
        }

        var catch_children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.free_tele_ast_list(catch_children, self.allocator);
        try try_children.append(signature_ast);

        var body2 = try self.parse_match_body(buffer_token_queue);
        errdefer tele_ast.free_tele_ast_list(body2, self.allocator);
        if (!buffer_token_queue.empty()) {
            return ParserError.ParsingFailure;
        }

        for (body2.items) |c| {
            try catch_children.append(c);
        }
        body2.deinit();

        const catch_ast = try self.allocator.create(TeleAst);
        catch_ast.*.body = "";
        catch_ast.*.ast_type = TeleAstType.catch_exp;
        catch_ast.*.children = catch_children;
        catch_ast.*.col = 0;

        const final_ast = try self.allocator.create(TeleAst);
        final_ast.*.body = "";
        final_ast.*.ast_type = TeleAstType.try_catch;

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.free_tele_ast_list(children, self.allocator);
        try children.append(try_ast);
        try children.append(catch_ast);
        final_ast.*.children = children;
        final_ast.*.col = 0;

        buffer_token_queue.deinit();

        try self.ast_stack.append(final_ast);
    }

    fn parse_case_clause_signature(self: *Self, token_queue: *TokenQueue, clause_col: *usize) !void {
        var buffer_token_queue = try TokenQueue.init(self.allocator);
        errdefer buffer_token_queue.deinit();
        var clause_col_first: bool = true;

        // Case Clause Signature
        while (!token_queue.empty()) {
            const node = try token_queue.pop();
            errdefer self.allocator.free(node.*.body);
            errdefer self.allocator.destroy(node);

            if (clause_col_first) {
                clause_col.* = node.*.col;
                clause_col_first = false;
            }

            if (is_colon(node.*.body)) {
                self.allocator.free(node.*.body);
                self.allocator.destroy(node);
                break;
            } else {
                try buffer_token_queue.push(node.*.body, node.*.line, node.*.col);
                self.allocator.destroy(node);
            }
        }

        try self.parse_exp(buffer_token_queue);
        if (!buffer_token_queue.empty()) {
            return ParserError.ParsingFailure;
        }
        buffer_token_queue.deinit();
    }

    fn parse_case_clause_body(self: *Self, token_queue: *TokenQueue, clause_col: usize) !std.ArrayList(*TeleAst) {
        var buffer_token_queue = try TokenQueue.init(self.allocator);
        errdefer buffer_token_queue.deinit();
        // Parse Case Clause Body

        while (!token_queue.empty()) {
            const peek_node = try token_queue.peek();

            if (peek_node.*.col <= clause_col) {
                break;
            } else {
                const node = try token_queue.pop();
                errdefer self.allocator.free(node.*.body);
                errdefer self.allocator.destroy(node);

                try buffer_token_queue.push(node.*.body, node.*.line, node.*.col);
                self.allocator.destroy(node);
            }
        }

        const alist = try self.parse_body(buffer_token_queue);
        errdefer tele_ast.free_tele_ast_list(alist, self.allocator);

        if (!buffer_token_queue.empty()) {
            return ParserError.ParsingFailure;
        }

        buffer_token_queue.deinit();

        return alist;
    }

    fn parse_function_call(self: *Self, token_queue: *TokenQueue, buf: []const u8, type_exp: bool) !void {

        // Remove paren start
        const pn = try token_queue.pop();
        self.allocator.free(pn.*.body);
        self.allocator.destroy(pn);

        const pn2 = try token_queue.peek();

        var children: ?std.ArrayList(*TeleAst) = null;
        if (!is_paren_end(pn2.*.body)) {
            children = std.ArrayList(*TeleAst).init(self.allocator);
            errdefer tele_ast.free_tele_ast_list(children.?, self.allocator);
            while (!token_queue.empty()) {
                try self.parse_function_call_arg(token_queue, type_exp);
                if (self.ast_stack.items.len < 1) {
                    return ParserError.ParsingFailure;
                }
                try children.?.append(self.ast_stack.pop());
            }
        } else {
            // Free Paren End Token
            const tn = try token_queue.pop();
            self.allocator.free(tn.*.body);
            self.allocator.destroy(tn);
        }

        const t = try self.allocator.create(TeleAst);
        t.*.body = buf;
        t.*.ast_type = TeleAstType.function_call;
        t.*.children = children;
        t.*.col = 0;

        try self.ast_stack.append(t);
    }

    fn parse_function_call_arg(self: *Self, token_queue: *TokenQueue, type_exp: bool) !void {
        // Function Call Body Token Queue
        var buffer_token_queue = try TokenQueue.init(self.allocator);
        errdefer buffer_token_queue.deinit();

        var n_count: usize = 0;
        while (!token_queue.empty()) {
            const n2 = try token_queue.pop();
            errdefer self.allocator.free(n2.*.body);
            errdefer self.allocator.destroy(n2);

            if (n_count == 0) {
                if (is_comma(n2.*.body) or is_paren_end(n2.*.body)) {
                    self.allocator.free(n2.*.body);
                    self.allocator.destroy(n2);
                    break;
                }
            }

            if (is_paren_start(n2.*.body) or is_tuple_start(n2.*.body) or is_list_start(n2.*.body) or is_map_start(n2.*.body) or is_record_start(n2.*.body)) {
                n_count += 1;
            } else if (is_paren_end(n2.*.body) or is_list_end(n2.*.body) or is_map_end(n2.*.body)) {
                n_count -= 1;
            }

            try buffer_token_queue.push(n2.*.body, n2.*.line, n2.*.col);
            self.allocator.destroy(n2);
        }

        if (type_exp) {
            try self.parse_type_exp(buffer_token_queue);
        } else {
            try self.parse_exp(buffer_token_queue);
        }
        if (!buffer_token_queue.empty()) {
            return ParserError.ParsingFailure;
        }
        buffer_token_queue.deinit();
    }
};

fn is_statement_keyword(buf: []const u8) bool {
    if (buf[0] == 's') {
        return is_spec_keyword(buf);
    }

    if (buf[0] == 'f') {
        return is_fun_keyword(buf) or is_funp_keyword(buf);
    }

    if (buf[0] == 't') {
        return is_type_keyword(buf);
    }

    if (buf[0] == 'r') {
        return is_record_keyword(buf);
    }

    return false;
}

test "is statement keyword" {
    try std.testing.expect(is_statement_keyword("fun"));
    try std.testing.expect(is_statement_keyword("funp"));
    try std.testing.expect(is_statement_keyword("spec"));
    try std.testing.expect(is_statement_keyword("type"));
    try std.testing.expect(is_statement_keyword("record"));

    try std.testing.expect(!is_statement_keyword("["));
    try std.testing.expect(!is_statement_keyword("]"));
    try std.testing.expect(!is_statement_keyword("try"));
    try std.testing.expect(!is_statement_keyword("match"));
    try std.testing.expect(!is_statement_keyword("catch"));
    try std.testing.expect(!is_statement_keyword("("));
    try std.testing.expect(!is_statement_keyword(")"));
}

fn is_type_keyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "type");
}

fn is_spec_keyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "spec");
}

fn is_fun_keyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "fun");
}

fn is_funp_keyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "funp");
}

fn is_record_keyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "record");
}

test "is keywords" {
    try std.testing.expect(is_type_keyword("type"));
    try std.testing.expect(is_spec_keyword("spec"));
    try std.testing.expect(is_fun_keyword("fun"));
    try std.testing.expect(is_funp_keyword("funp"));
    try std.testing.expect(is_record_keyword("record"));
}

test "parse operator expression" {
    const file = try std.fs.cwd().openFile(
        "snippets/op.tl",
        .{ .mode = .read_only },
    );
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), test_allocator);
    defer token_queue.deinit();

    //var result = try parse_tokens(token_queue, test_allocator);
    //defer result.deinit();
    //try std.testing.expect(result.items.len == 1);

    //const a = result.pop();
    //try std.testing.expect(a.*.ast_type == TeleAstType.op);
    //try std.testing.expect(std.mem.eql(u8, a.*.body, "+"));
    //try std.testing.expect(a.children.?.items.len == 2);

    //const c = a.children.?;
    //try std.testing.expect(c.items[0].ast_type == TeleAstType.int);
    //try std.testing.expect(std.mem.eql(u8, c.items[0].body, "1"));
    //try std.testing.expect(c.items[0].children == null);

    //try std.testing.expect(c.items[1].ast_type == TeleAstType.int);
    //try std.testing.expect(std.mem.eql(u8, c.items[1].body, "2"));
    //try std.testing.expect(c.items[1].children == null);

    //test_allocator.free(a.*.body);
    //tele_ast.free_tele_ast_list(a.children.?, test_allocator);
    //test_allocator.destroy(a);
}

test "parse operator expression chained" {
    const file = try std.fs.cwd().openFile(
        "snippets/op2.tl",
        .{ .mode = .read_only },
    );
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), test_allocator);
    defer token_queue.deinit();

    //var result = try parse_tokens(token_queue, test_allocator);
    //defer result.deinit();
    //try std.testing.expect(result.items.len == 1);

    //const a = result.pop();
    //try std.testing.expect(a.*.ast_type == TeleAstType.op);
    //try std.testing.expect(std.mem.eql(u8, a.*.body, "="));
    //try std.testing.expect(a.children.?.items.len == 2);

    //const c = a.children.?;
    //try std.testing.expect(c.items[0].ast_type == TeleAstType.variable);
    //try std.testing.expect(std.mem.eql(u8, c.items[0].body, "a"));
    //try std.testing.expect(c.items[0].children == null);

    //try std.testing.expect(c.items[1].ast_type == TeleAstType.op);
    //try std.testing.expect(std.mem.eql(u8, c.items[1].body, "+"));
    //try std.testing.expect(c.items[1].children.?.items.len == 2);

    //const c2 = c.items[1].children.?;
    //try std.testing.expect(c2.items[0].ast_type == TeleAstType.int);
    //try std.testing.expect(std.mem.eql(u8, c2.items[0].body, "1"));
    //try std.testing.expect(c2.items[0].children == null);

    //try std.testing.expect(c2.items[1].ast_type == TeleAstType.int);
    //try std.testing.expect(std.mem.eql(u8, c2.items[1].body, "2"));
    //try std.testing.expect(c2.items[1].children == null);

    //test_allocator.free(a.*.body);
    //tele_ast.free_tele_ast_list(a.children.?, test_allocator);
    //test_allocator.destroy(a);
}

fn check_paren_start_peek(token_queue: *TokenQueue) !bool {
    const peek_node = try token_queue.peek();
    return is_paren_start(peek_node.*.body);
}

fn check_operator_peek(token_queue: *TokenQueue) !bool {
    const peek_node = try token_queue.peek();
    return is_operator(peek_node.*.body);
}

fn check_arrow_op_peek(token_queue: *TokenQueue) !bool {
    const peek_node = try token_queue.peek();
    return is_arrow_operator(peek_node.*.body);
}

fn paren_exp_to_function_signature(paren_exp: *TeleAst, allocator: std.mem.Allocator) !*TeleAst {
    const t = try allocator.create(TeleAst);
    t.*.body = "";
    t.*.ast_type = TeleAstType.function_signature;
    t.*.children = null;
    t.*.col = paren_exp.*.col;
    errdefer tele_ast.free_tele_ast(t, allocator);

    if (paren_exp.*.children != null) {
        var children = std.ArrayList(*TeleAst).init(allocator);
        errdefer tele_ast.free_tele_ast_list(children, allocator);

        for (paren_exp.*.children.?.items) |c| {
            try children.append(c);
        }
        t.*.children = children;
    }
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

    //const result = try parse_tuple(token_queue, test_allocator);
    //try std.testing.expect(result.*.ast_type == TeleAstType.tuple);
    //try std.testing.expect(std.mem.eql(u8, result.*.body, ""));
    //try std.testing.expect(result.*.children.?.items.len == 3);

    //const c = result.*.children.?;
    //try std.testing.expect(c.items[0].ast_type == TeleAstType.int);
    //try std.testing.expect(std.mem.eql(u8, c.items[0].body, "1"));
    //try std.testing.expect(c.items[0].children == null);
    //try std.testing.expect(c.items[1].ast_type == TeleAstType.int);
    //try std.testing.expect(std.mem.eql(u8, c.items[1].body, "2"));
    //try std.testing.expect(c.items[1].children == null);
    //try std.testing.expect(c.items[2].ast_type == TeleAstType.int);
    //try std.testing.expect(std.mem.eql(u8, c.items[2].body, "3"));
    //try std.testing.expect(c.items[2].children == null);

    //tele_ast.free_tele_ast_list(result.children.?, test_allocator);
    //test_allocator.destroy(result);
}

test "parse list" {
    const file = try std.fs.cwd().openFile(
        "snippets/list.tl",
        .{ .mode = .read_only },
    );
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), test_allocator);
    defer token_queue.deinit();

    //const result = try parse_list(token_queue, test_allocator);
    //try std.testing.expect(result.*.ast_type == TeleAstType.list);
    //try std.testing.expect(std.mem.eql(u8, result.*.body, ""));
    //try std.testing.expect(result.*.children.?.items.len == 3);

    //const c = result.*.children.?;
    //try std.testing.expect(c.items[0].ast_type == TeleAstType.int);
    //try std.testing.expect(std.mem.eql(u8, c.items[0].body, "1"));
    //try std.testing.expect(c.items[0].children == null);
    //try std.testing.expect(c.items[1].ast_type == TeleAstType.int);
    //try std.testing.expect(std.mem.eql(u8, c.items[1].body, "2"));
    //try std.testing.expect(c.items[1].children == null);
    //try std.testing.expect(c.items[2].ast_type == TeleAstType.int);
    //try std.testing.expect(std.mem.eql(u8, c.items[2].body, "3"));
    //try std.testing.expect(c.items[2].children == null);

    //tele_ast.free_tele_ast_list(result.children.?, test_allocator);
    //test_allocator.destroy(result);
}

test "parse map" {
    const file = try std.fs.cwd().openFile(
        "snippets/map.tl",
        .{ .mode = .read_only },
    );
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), test_allocator);
    defer token_queue.deinit();

    //const result = try parse_map(token_queue, test_allocator);
    //try std.testing.expect(result.*.ast_type == TeleAstType.map);
    //try std.testing.expect(std.mem.eql(u8, result.*.body, ""));
    //try std.testing.expect(result.*.children.?.items.len == 6);

    //const c = result.*.children.?;
    //try std.testing.expect(c.items[0].ast_type == TeleAstType.int);
    //try std.testing.expect(std.mem.eql(u8, c.items[0].body, "1"));
    //try std.testing.expect(c.items[0].children == null);
    //try std.testing.expect(c.items[1].ast_type == TeleAstType.variable);
    //try std.testing.expect(std.mem.eql(u8, c.items[1].body, "a"));
    //try std.testing.expect(c.items[1].children == null);

    //try std.testing.expect(c.items[2].ast_type == TeleAstType.int);
    //try std.testing.expect(std.mem.eql(u8, c.items[2].body, "2"));
    //try std.testing.expect(c.items[2].children == null);
    //try std.testing.expect(c.items[3].ast_type == TeleAstType.variable);
    //try std.testing.expect(std.mem.eql(u8, c.items[3].body, "b"));
    //try std.testing.expect(c.items[3].children == null);

    //try std.testing.expect(c.items[4].ast_type == TeleAstType.int);
    //try std.testing.expect(std.mem.eql(u8, c.items[4].body, "3"));
    //try std.testing.expect(c.items[4].children == null);
    //try std.testing.expect(c.items[5].ast_type == TeleAstType.variable);
    //try std.testing.expect(std.mem.eql(u8, c.items[5].body, "c"));
    //try std.testing.expect(c.items[5].children == null);

    //tele_ast.free_tele_ast_list(result.children.?, test_allocator);
    //test_allocator.destroy(result);
}

fn extract_record_name(name: []const u8, allocator: std.mem.Allocator) ![]const u8 {
    const buf = try allocator.alloc(u8, name.len - 2);
    var i: usize = 1;
    while (i < name.len - 1) {
        buf[i - 1] = name[i];
        i += 1;
    }

    return buf;
}

test "parse body multi" {
    const file = try std.fs.cwd().openFile(
        "snippets/body_multi.tl",
        .{ .mode = .read_only },
    );
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), test_allocator);
    defer token_queue.deinit();

    //const result = try parse_body(token_queue, test_allocator);
    //try std.testing.expect(result.items.len == 2);

    //const ast1 = result.items[0];
    //try std.testing.expect(ast1.*.ast_type == TeleAstType.op);
    //try std.testing.expect(std.mem.eql(u8, ast1.*.body, "+"));
    //try std.testing.expect(ast1.*.children.?.items.len == 2);

    //const ast2 = result.items[1];
    //try std.testing.expect(ast2.*.ast_type == TeleAstType.op);
    //try std.testing.expect(std.mem.eql(u8, ast2.*.body, "-"));
    //try std.testing.expect(ast2.*.children.?.items.len == 2);

    //tele_ast.free_tele_ast_list(result, test_allocator);
}

test "parse body line" {
    const file = try std.fs.cwd().openFile(
        "snippets/body_single.tl",
        .{ .mode = .read_only },
    );
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), test_allocator);
    defer token_queue.deinit();

    //const result = try parse_body_line(token_queue, test_allocator, 0);
    //try std.testing.expect(result.*.ast_type == TeleAstType.op);
    //try std.testing.expect(std.mem.eql(u8, result.*.body, "+"));
    //try std.testing.expect(result.*.children.?.items.len == 2);

    //const ast1 = result.*.children.?.items[0];
    //try std.testing.expect(ast1.*.ast_type == TeleAstType.variable);
    //try std.testing.expect(std.mem.eql(u8, ast1.*.body, "a"));
    //try std.testing.expect(ast1.*.children == null);

    //const ast2 = result.*.children.?.items[1];
    //try std.testing.expect(ast2.*.ast_type == TeleAstType.int);
    //try std.testing.expect(std.mem.eql(u8, ast2.*.body, "2"));
    //try std.testing.expect(ast2.*.children == null);

    //tele_ast.free_tele_ast_list(result.*.children.?, test_allocator);
    //test_allocator.free(result.*.body);
    //test_allocator.destroy(result);
}

test "parse match expression" {
    const file = try std.fs.cwd().openFile(
        "snippets/match.tl",
        .{ .mode = .read_only },
    );
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), test_allocator);
    defer token_queue.deinit();

    //const result = try parse_match_expression(token_queue, test_allocator, 0);
    //try std.testing.expect(std.mem.eql(u8, result.*.body, ""));
    //try std.testing.expect(result.*.ast_type == TeleAstType.case);
    //try std.testing.expect(result.*.children.?.items.len == 3);

    //const mc = result.*.children.?;
    //try std.testing.expect(mc.items[0].ast_type == TeleAstType.variable);
    //try std.testing.expect(std.mem.eql(u8, mc.items[0].body, "x"));
    //try std.testing.expect(mc.items[0].children == null);

    //try std.testing.expect(mc.items[1].ast_type == TeleAstType.case_clause);
    //try std.testing.expect(std.mem.eql(u8, mc.items[1].body, ""));
    //try std.testing.expect(mc.items[1].children.?.items.len == 2);

    //const c = mc.items[1].children.?;
    //try std.testing.expect(c.items[0].ast_type == TeleAstType.int);
    //try std.testing.expect(std.mem.eql(u8, c.items[0].body, "1"));
    //try std.testing.expect(c.items[0].children == null);
    //try std.testing.expect(c.items[1].ast_type == TeleAstType.int);
    //try std.testing.expect(std.mem.eql(u8, c.items[1].body, "42"));
    //try std.testing.expect(c.items[1].children == null);

    //try std.testing.expect(mc.items[2].ast_type == TeleAstType.case_clause);
    //try std.testing.expect(std.mem.eql(u8, mc.items[2].body, ""));
    //try std.testing.expect(mc.items[2].children.?.items.len == 2);

    //const c2 = mc.items[2].children.?;
    //try std.testing.expect(c2.items[0].ast_type == TeleAstType.int);
    //try std.testing.expect(std.mem.eql(u8, c2.items[0].body, "2"));
    //try std.testing.expect(c2.items[0].children == null);
    //try std.testing.expect(c2.items[1].ast_type == TeleAstType.int);
    //try std.testing.expect(std.mem.eql(u8, c2.items[1].body, "43"));
    //try std.testing.expect(c2.items[1].children == null);

    //tele_ast.free_tele_ast_list(result.*.children.?, test_allocator);
    //test_allocator.destroy(result);
}

test "parse match body single clause" {
    const file = try std.fs.cwd().openFile(
        "snippets/match_body.tl",
        .{ .mode = .read_only },
    );
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), test_allocator);
    defer token_queue.deinit();

    //const result = try parse_match_body(token_queue, test_allocator);
    //try std.testing.expect(result.items.len == 1);
    //try std.testing.expect(result.items[0].ast_type == TeleAstType.case_clause);
    //try std.testing.expect(std.mem.eql(u8, result.items[0].body, ""));

    //try std.testing.expect(result.items[0].children.?.items.len == 2);
    //const c = result.items[0].children.?;
    //try std.testing.expect(c.items[0].ast_type == TeleAstType.int);
    //try std.testing.expect(std.mem.eql(u8, c.items[0].body, "1"));
    //try std.testing.expect(c.items[0].children == null);
    //try std.testing.expect(c.items[1].ast_type == TeleAstType.int);
    //try std.testing.expect(std.mem.eql(u8, c.items[1].body, "42"));
    //try std.testing.expect(c.items[1].children == null);
    //tele_ast.free_tele_ast_list(result, test_allocator);
}

test "parse match body multi clause" {
    const file = try std.fs.cwd().openFile(
        "snippets/match_body_multi.tl",
        .{ .mode = .read_only },
    );
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), test_allocator);
    defer token_queue.deinit();

    //const result = try parse_match_body(token_queue, test_allocator);
    //try std.testing.expect(result.items.len == 2);
    //try std.testing.expect(result.items[0].ast_type == TeleAstType.case_clause);
    //try std.testing.expect(std.mem.eql(u8, result.items[0].body, ""));

    //try std.testing.expect(result.items[0].children.?.items.len == 2);
    //const c = result.items[0].children.?;
    //try std.testing.expect(c.items[0].ast_type == TeleAstType.int);
    //try std.testing.expect(std.mem.eql(u8, c.items[0].body, "1"));
    //try std.testing.expect(c.items[0].children == null);
    //try std.testing.expect(c.items[1].ast_type == TeleAstType.int);
    //try std.testing.expect(std.mem.eql(u8, c.items[1].body, "42"));
    //try std.testing.expect(c.items[1].children == null);

    //try std.testing.expect(result.items[1].ast_type == TeleAstType.case_clause);
    //try std.testing.expect(std.mem.eql(u8, result.items[1].body, ""));

    //try std.testing.expect(result.items[1].children.?.items.len == 2);
    //const c2 = result.items[1].children.?;
    //try std.testing.expect(c2.items[0].ast_type == TeleAstType.int);
    //try std.testing.expect(std.mem.eql(u8, c2.items[0].body, "2"));
    //try std.testing.expect(c2.items[0].children == null);
    //try std.testing.expect(c2.items[1].ast_type == TeleAstType.int);
    //try std.testing.expect(std.mem.eql(u8, c2.items[1].body, "43"));
    //try std.testing.expect(c2.items[1].children == null);

    //tele_ast.free_tele_ast_list(result, test_allocator);
}

test "parse match signature" {
    const file = try std.fs.cwd().openFile(
        "snippets/match_signature.tl",
        .{ .mode = .read_only },
    );
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), test_allocator);
    defer token_queue.deinit();

    //const result = try parse_match_signature(token_queue, test_allocator);
    //try std.testing.expect(result.*.ast_type == TeleAstType.int);
    //try std.testing.expect(std.mem.eql(u8, result.*.body, "1"));
    //try std.testing.expect(result.*.children == null);

    //test_allocator.free(result.*.body);
    //test_allocator.destroy(result);
}

test "parse case clause signature" {
    const file = try std.fs.cwd().openFile(
        "snippets/case_clause_signature.tl",
        .{ .mode = .read_only },
    );
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), test_allocator);
    defer token_queue.deinit();

    //var n: usize = 0;
    //const result = try parse_case_clause_signature(token_queue, test_allocator, &n);
    //try std.testing.expect(result.*.ast_type == TeleAstType.int);
    //try std.testing.expect(std.mem.eql(u8, result.*.body, "1"));
    //try std.testing.expect(result.*.children == null);

    //test_allocator.free(result.*.body);
    //test_allocator.destroy(result);
}

test "parse case clause body single line" {
    const file = try std.fs.cwd().openFile(
        "snippets/case_clause_body_single.tl",
        .{ .mode = .read_only },
    );
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), test_allocator);
    defer token_queue.deinit();

    //const result = try parse_case_clause_body(token_queue, 0, test_allocator);
    //try std.testing.expect(result.items.len == 1);
    //try std.testing.expect(std.mem.eql(u8, result.items[0].body, "42"));
    //try std.testing.expect(result.items[0].ast_type == TeleAstType.int);
    //try std.testing.expect(result.items[0].children == null);

    //tele_ast.free_tele_ast_list(result, test_allocator);
}

test "parse case clause body multi line" {
    const file = try std.fs.cwd().openFile(
        "snippets/case_clause_body_multi.tl",
        .{ .mode = .read_only },
    );
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), test_allocator);
    defer token_queue.deinit();

    //const result = try parse_case_clause_body(token_queue, 0, test_allocator);
    //try std.testing.expect(result.items.len == 3);
    //try std.testing.expect(std.mem.eql(u8, result.items[0].body, "42"));
    //try std.testing.expect(result.items[0].ast_type == TeleAstType.int);
    //try std.testing.expect(result.items[0].children == null);
    //try std.testing.expect(std.mem.eql(u8, result.items[1].body, "43"));
    //try std.testing.expect(result.items[1].ast_type == TeleAstType.int);
    //try std.testing.expect(result.items[1].children == null);
    //try std.testing.expect(std.mem.eql(u8, result.items[2].body, "44"));
    //try std.testing.expect(result.items[2].ast_type == TeleAstType.int);
    //try std.testing.expect(result.items[2].children == null);

    //tele_ast.free_tele_ast_list(result, test_allocator);
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

fn is_fun_val(buf: []const u8) bool {
    if (buf[0] != '#') {
        return false;
    }

    // Minimum number of characters for a fun val
    if (buf.len < 4) {
        return false;
    }

    // TODO: More strict validation
    var i: usize = 1;
    while (i < buf.len) {
        if (buf[i] == '/') {
            return true;
        }
        i = i + 1;
    }

    return false;
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

fn is_record_start(buf: []const u8) bool {
    if (buf[0] == '#') {
        if (buf[buf.len - 1] == '(') {
            return true;
        }
    }
    return false;
}

test "is record start" {
    try std.testing.expect(is_record_start("#foobar("));
}

fn is_arrow_operator(buf: []const u8) bool {
    if (buf.len != 2) {
        return false;
    }

    return std.mem.eql(u8, buf, "=>");
}

fn is_operator(buf: []const u8) bool {
    if (buf.len == 0) {
        return false;
    }

    switch (buf[0]) {
        '+', '*', '/', '!', '-', '|' => {
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

fn is_pipe_operator(buf: []const u8) bool {
    if (buf.len == 0) {
        return false;
    }

    if (buf[0] == '|') {
        return true;
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

fn is_try_keyword(buf: []const u8) bool {
    if (buf.len != 3) {
        return false;
    }

    if (buf[0] == 't') {
        if (std.mem.eql(u8, "try", buf)) {
            return true;
        }
    }
    return false;
}

fn is_catch_keyword(buf: []const u8) bool {
    if (buf.len != 5) {
        return false;
    }

    if (buf[0] == 'c') {
        if (std.mem.eql(u8, "catch", buf)) {
            return true;
        }
    }
    return false;
}

fn is_function_definition(buf: []const u8) bool {
    return std.mem.eql(u8, "fun", buf);
}

fn is_priv_function_definition(buf: []const u8) bool {
    return std.mem.eql(u8, "funp", buf);
}

fn is_type_def(buf: []const u8) bool {
    return std.mem.eql(u8, "type", buf);
}

fn is_record_def(buf: []const u8) bool {
    return std.mem.eql(u8, "record", buf);
}

fn is_spec_def(buf: []const u8) bool {
    return std.mem.eql(u8, "spec", buf);
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

fn is_equal(buf: []const u8) bool {
    return buf[0] == '=';
}

fn contains_hash(buf: []const u8) bool {
    for (buf) |c| {
        if (c == '#') {
            return true;
        }
    }
    return false;
}
