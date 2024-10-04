const std = @import("std");
const test_allocator = std.testing.allocator;
const talloc = test_allocator;
const tokenizer = @import("tokenizer.zig");
const TokenQueue = tokenizer.TokenQueue;
const TokenQueueNode = tokenizer.TokenQueueNode;
const tele_ast = @import("tele_ast.zig");
const TeleAst = tele_ast.Ast;
const TeleAstType = tele_ast.AstType;
const util = @import("util.zig");

const ParserError = error{ ParsingFailure, TokenFailure, ExpectedStatement, InvalidStatement };

const ParserMode = enum { none, op };

pub fn parse_reader(r: anytype, allocator: std.mem.Allocator) !std.ArrayList(*TeleAst) {
    const token_queue = try tokenizer.readTokens(r, allocator);

    const parser = try Parser.init(token_queue, allocator);

    errdefer allocator.destroy(parser);
    errdefer parser.token_queue.deinit();
    const result = try parser.parse();

    if (!token_queue.empty()) {
        return ParserError.ParsingFailure;
    }
    parser.token_queue.deinit();
    allocator.destroy(parser);

    return result;
}

pub fn fileToParser(path: []const u8, allocator: std.mem.Allocator) !*Parser {
    const file = try std.fs.cwd().openFile(path, .{ .mode = .read_only });
    defer file.close();
    const token_queue = try tokenizer.read_tokens(file.reader(), allocator);

    const parser = try Parser.init(token_queue, allocator);

    return parser;
}

pub const Parser = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    token_queue: *TokenQueue,

    pub fn init(token_queue: *TokenQueue, allocator: std.mem.Allocator) !*Self {
        const parser = try allocator.create(Self);
        parser.*.allocator = allocator;
        parser.*.token_queue = token_queue;
        return parser;
    }

    pub fn deinit(self: *Self) void {
        self.token_queue.deinit();
        self.allocator.destroy(self);
    }

    pub fn parse(self: *Self) !std.ArrayList(*TeleAst) {
        const ast_stack = try self.parse_statements(false);

        if (!self.token_queue.empty()) {
            return ParserError.ParsingFailure;
        }

        return ast_stack;
    }

    pub fn parse_statements(self: *Self, allow_exps: bool) !std.ArrayList(*TeleAst) {
        var statements = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(statements, self.allocator);

        while (!self.token_queue.empty()) {
            const pn = self.token_queue.peek() catch {
                return ParserError.ParsingFailure;
            };

            if (is_statement_keyword(pn.*.body)) {
                if (is_type_keyword(pn.*.body)) {
                    const ast = try self.parse_type_definition(self.token_queue, false);
                    try statements.append(ast);
                } else if (is_opaque_keyword(pn.*.body)) {
                    const ast = try self.parse_type_definition(self.token_queue, true);
                    try statements.append(ast);
                } else if (is_spec_keyword(pn.*.body)) {
                    const ast = try self.parse_spec_definition(self.token_queue);
                    try statements.append(ast);
                } else if (is_fun_keyword(pn.*.body)) {
                    const ast = try self.parse_function_definition(self.token_queue, false);
                    try statements.append(ast);
                } else if (is_funp_keyword(pn.*.body)) {
                    const ast = try self.parse_function_definition(self.token_queue, true);
                    try statements.append(ast);
                } else if (is_record_keyword(pn.*.body)) {
                    const ast = try self.parse_record_definition(self.token_queue);
                    try statements.append(ast);
                } else if (is_behaviour_keyword(pn.*.body)) {
                    const ast = try self.parse_behaviour(self.token_queue);
                    try statements.append(ast);
                } else if (is_include_keyword(pn.*.body)) {
                    const ast = try self.parse_attribute(self.token_queue);
                    try statements.append(ast);
                } else if (is_include_lib_keyword(pn.*.body)) {
                    const ast = try self.parse_attribute(self.token_queue);
                    try statements.append(ast);
                } else if (is_nifs_keyword(pn.*.body)) {
                    const ast = try self.parse_attribute(self.token_queue);
                    try statements.append(ast);
                } else if (is_export_type_keyword(pn.*.body)) {
                    const ast = try self.parse_attribute(self.token_queue);
                    try statements.append(ast);
                } else if (is_doc_keyword(pn.*.body)) {
                    const ast = try self.parse_attribute(self.token_queue);
                    try statements.append(ast);
                } else if (is_moduledoc_keyword(pn.*.body)) {
                    const ast = try self.parse_attribute(self.token_queue);
                    try statements.append(ast);
                } else if (is_on_load_keyword(pn.*.body)) {
                    const ast = try self.parse_attribute(self.token_queue);
                    try statements.append(ast);
                } else if (is_callback_keyword(pn.*.body)) {
                    const ast = try self.parse_callback_definition(self.token_queue);
                    try statements.append(ast);
                } else if (is_import_keyword(pn.*.body)) {
                    const ast = try self.parseImport(self.token_queue);
                    try statements.append(ast);
                } else if (is_define_keyword(pn.*.body)) {
                    const ast = try self.parse_macro_definition(self.token_queue);
                    try statements.append(ast);
                } else if (is_attr_keyword(pn.*.body)) {
                    const ast = try self.parse_custom_attribute(self.token_queue);
                    try statements.append(ast);
                } else {
                    return ParserError.InvalidStatement;
                }
            } else {
                if (allow_exps) {
                    const ast = try self.parse_exp(self.token_queue);
                    try statements.append(ast);
                } else {
                    // TODO: Expected Statement Error
                    return ParserError.ExpectedStatement;
                }
            }
        }

        return statements;
    }

    fn parse_function_definition(self: *Self, token_queue: *TokenQueue, private: bool) !*TeleAst {

        // Pop off fun/funp
        const n = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };
        const current_col = n.*.col;
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        const node3 = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };

        // Function Definition Name
        const buf = node3.*.body;
        self.allocator.destroy(node3);
        errdefer self.allocator.free(buf);

        const children = try self.parseFunctionSigAndBody(token_queue, false, current_col);

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

        return t;
    }

    fn parse_macro_definition(self: *Self, token_queue: *TokenQueue) !*TeleAst {

        // Pop off define
        const n = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };
        const current_col = n.*.col;
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        // Function Definition Name
        const node3 = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };
        const buf = node3.*.body;
        self.allocator.destroy(node3);
        errdefer self.allocator.free(buf);

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(children, self.allocator);

        const pn = try token_queue.peek();
        if (is_colon(pn.*.body)) {
            // Pop off colon
            const t = try token_queue.pop();
            self.allocator.free(t.*.body);
            self.allocator.destroy(t);

            const ast = try self.parse_exp(token_queue);
            try children.append(ast);
        } else if (is_paren_start(pn.*.body)) {
            // Function Definition Signature
            var token_queue2 = TokenQueue.init(self.allocator) catch {
                return ParserError.ParsingFailure;
            };
            errdefer token_queue2.deinit();

            // Gather tokens for function signature
            var map_count: usize = 0;
            while (!token_queue.empty()) {
                const node2 = try token_queue.pop();
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

            const ast = try self.parse_function_signature(token_queue2, false);
            try children.append(ast);

            // Function Definition Body
            var token_queue3 = try TokenQueue.init(self.allocator);
            errdefer token_queue3.deinit();
            if (token_queue.empty()) {
                return ParserError.ParsingFailure;
            }

            // Gather tokens for function body
            while (!token_queue.empty()) {
                const peek_node2 = try token_queue.peek();
                if (peek_node2.*.col <= current_col) {
                    break;
                }

                const node2 = try token_queue.pop();
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
            errdefer tele_ast.freeTeleAstList(alist, self.allocator);

            for (alist.items) |a| {
                try children.append(a);
            }
            alist.deinit();
            token_queue3.deinit();
            token_queue2.deinit();
        } else {
            return ParserError.ParsingFailure;
        }

        // Assemble Function Definition Ast
        const t = try self.allocator.create(TeleAst);
        t.*.body = buf;
        t.*.ast_type = TeleAstType.macro_def;
        t.*.children = children;
        t.*.col = 0;

        return t;
    }

    fn parse_function_signature(self: *Self, token_queue: *TokenQueue, type_exp: bool) !*TeleAst {
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
        errdefer free_null_children(children, self.allocator);

        const pn2 = try token_queue.peek();

        if (!is_paren_end(pn2.*.body)) {
            children = std.ArrayList(*TeleAst).init(self.allocator);

            while (!token_queue.empty()) {
                var found_end: bool = false;
                const ast = try self.parse_function_signature_param(token_queue, type_exp, &found_end);
                try children.?.append(ast);
                if (found_end) {
                    break;
                }
            }
        } else {
            // Free Paren End Token
            const tn = try token_queue.pop();
            self.allocator.free(tn.*.body);
            self.allocator.destroy(tn);
        }

        if (!token_queue.empty()) {
            if (children == null) {
                return ParserError.ParsingFailure;
            }
            const pn = try token_queue.peek();
            if (std.mem.eql(u8, "when", pn.*.body)) {
                const al = try self.parseGuardClauses(token_queue);
                for (al.items) |a| {
                    try children.?.append(a);
                }
                al.deinit();
            } else {
                return ParserError.ParsingFailure;
            }
        }

        const tast = try self.allocator.create(TeleAst);
        tast.*.body = "";
        tast.*.ast_type = TeleAstType.function_signature;
        tast.*.children = children;
        tast.*.col = 0;

        return tast;
    }

    fn free_null_children(children: ?std.ArrayList(*TeleAst), allocator: std.mem.Allocator) void {
        if (children != null) {
            tele_ast.freeTeleAstList(children.?, allocator);
        }
    }

    fn parse_function_signature_param(self: *Self, token_queue: *TokenQueue, type_exp: bool, found_end: *bool) !*TeleAst {
        var buffer_token_queue = try TokenQueue.init(self.allocator);
        errdefer buffer_token_queue.deinit();
        defer buffer_token_queue.deinit();

        var n_count: usize = 1;
        while (!token_queue.empty()) {
            const n2 = try token_queue.pop();
            errdefer self.allocator.free(n2.*.body);
            errdefer self.allocator.destroy(n2);

            if (n_count == 1 and (isComma(n2.*.body) or is_paren_end(n2.*.body))) {
                if (is_paren_end(n2.*.body)) {
                    found_end.* = true;
                }
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
            return try self.parse_type_exp(buffer_token_queue);
        } else {
            return try self.parse_exp(buffer_token_queue);
        }
    }

    fn parseGuardClauses(self: *Self, token_queue: *TokenQueue) !std.ArrayList(*TeleAst) {
        var clauses = std.ArrayList(*TeleAst).init(self.allocator);

        // Pop off when keyword
        const n = try token_queue.pop();
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        var buffer_token_queue = try TokenQueue.init(self.allocator);
        defer buffer_token_queue.deinit();

        while (!token_queue.empty()) {
            while (!token_queue.empty()) {
                const n2 = try token_queue.pop();
                if (isComma(n2.*.body)) {
                    self.allocator.free(n2.*.body);
                    self.allocator.destroy(n2);
                    break;
                }
                try buffer_token_queue.push(n2.*.body, n2.*.line, n2.*.col);
                self.allocator.destroy(n2);
            }

            const ast = try self.parseGuardClause(buffer_token_queue);
            try clauses.append(ast);
        }

        return clauses;
    }

    fn parseGuardClause(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        const alist = try self.parse_body(token_queue);

        const t = try self.allocator.create(TeleAst);
        t.*.body = "";
        t.*.ast_type = TeleAstType.guard_clause;
        t.*.children = alist;
        t.*.col = 0;

        return t;
    }

    fn parse_spec_definition(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        // Pop off spec keyword
        const n = try token_queue.pop();
        const current_col = n.*.col;
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        // Spec Definition Name
        const node3 = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };

        const buf = node3.*.body;
        self.allocator.destroy(node3);
        errdefer self.allocator.free(buf);

        const children = try self.parseFunctionSigAndBody(token_queue, true, current_col);

        // Assemble Spec Definition Ast
        const t = try self.allocator.create(TeleAst);
        t.*.body = buf;
        t.*.ast_type = TeleAstType.spec_def;
        t.*.children = children;
        t.*.col = 0;

        return t;
    }

    fn parse_callback_definition(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        // Pop off callback keyword
        const n = try token_queue.pop();
        const current_col = n.*.col;
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        // Spec Definition Name
        const node3 = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };

        const buf = node3.*.body;
        self.allocator.destroy(node3);
        errdefer self.allocator.free(buf);

        // Callback Definition Signature
        var token_queue2 = TokenQueue.init(self.allocator) catch {
            self.allocator.free(node3.*.body);
            self.allocator.destroy(node3);
            return ParserError.ParsingFailure;
        };
        errdefer token_queue2.deinit();

        var map_count: usize = 0;
        while (!token_queue.empty()) {
            const node2 = try token_queue.pop();
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

        const sig_ast = try self.parse_function_signature(token_queue2, true);

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(children, self.allocator);
        try children.append(sig_ast);

        // Callback Definition Body
        var token_queue3 = try TokenQueue.init(self.allocator);
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
            errdefer self.allocator.free(node2.*.body);
            errdefer self.allocator.destroy(node2);

            try token_queue3.push(node2.*.body, node2.*.line, node2.*.col);
            self.allocator.destroy(node2);
        }

        if (token_queue3.empty()) {
            return ParserError.ParsingFailure;
        }

        const ast = self.parse_type_exp(token_queue3) catch {
            return ParserError.ParsingFailure;
        };
        try children.append(ast);
        if (!token_queue3.empty()) {
            return ParserError.ParsingFailure;
        }

        // Assemble Callback Definition Ast
        const t = try self.allocator.create(TeleAst);
        t.*.body = buf;
        t.*.ast_type = TeleAstType.callback_def;
        t.*.children = children;
        t.*.col = 0;

        token_queue2.deinit();
        token_queue3.deinit();

        return t;
    }

    fn parse_type_definition(self: *Self, token_queue: *TokenQueue, opaque_type: bool) !*TeleAst {
        // Free type keyword
        const n = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };
        const current_col = n.*.col;
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);
        const node3 = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };

        // type Definition Name
        const buf = node3.*.body;
        self.allocator.destroy(node3);

        const pn = token_queue.peek() catch {
            return ParserError.ParsingFailure;
        };

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(children, self.allocator);

        if (is_colon(pn.*.body)) {
            const t = try self.allocator.create(TeleAst);
            t.*.body = buf;
            t.*.ast_type = TeleAstType.function_call;
            t.*.children = null;
            t.*.col = 0;
            try children.append(t);
        } else {

            // TODO: Change to parse function signature
            const tfcall = self.parse_function_call(token_queue, buf, true) catch {
                return ParserError.ParsingFailure;
            };

            try children.append(tfcall);
        }

        // Skip colon
        const cn = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };
        self.allocator.free(cn.*.body);
        self.allocator.destroy(cn);

        // type Definition Body
        var token_queue3 = try TokenQueue.init(self.allocator);
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
            errdefer self.allocator.free(node2.*.body);
            errdefer self.allocator.destroy(node2);

            try token_queue3.push(node2.*.body, node2.*.line, node2.*.col);
            self.allocator.destroy(node2);
        }

        if (token_queue3.empty()) {
            return ParserError.ParsingFailure;
        }

        const ast = try self.parse_type_exp(token_queue3);
        try children.append(ast);
        if (!token_queue3.empty()) {
            return ParserError.ParsingFailure;
        }

        // Assemble Type Definition Ast
        const t = try self.allocator.create(TeleAst);
        t.*.body = "";
        if (opaque_type) {
            t.*.ast_type = TeleAstType.opaque_type_def;
        } else {
            t.*.ast_type = TeleAstType.type_def;
        }
        t.*.children = children;
        t.*.col = 0;

        token_queue3.deinit();

        return t;
    }

    fn parse_record_definition(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        const n = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };
        const current_col = n.*.col;
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        // Record Definition Name
        const node3 = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };

        const buf = node3.*.body;
        self.allocator.destroy(node3);

        // Skip colon
        const cn = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };
        self.allocator.free(cn.*.body);
        self.allocator.destroy(cn);

        // Record Definition Body
        var token_queue3 = try TokenQueue.init(self.allocator);
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
            errdefer self.allocator.free(node2.*.body);
            errdefer self.allocator.destroy(node2);

            try token_queue3.push(node2.*.body, node2.*.line, node2.*.col);
            self.allocator.destroy(node2);
        }

        if (token_queue3.empty()) {
            return ParserError.ParsingFailure;
        }

        // Skip tuple start
        const cn2 = token_queue3.pop() catch {
            return ParserError.ParsingFailure;
        };
        self.allocator.free(cn2.*.body);
        self.allocator.destroy(cn2);

        var children: ?std.ArrayList(*TeleAst) = null;

        const pn = try token_queue3.peek();
        if (!is_paren_end(pn.*.body)) {
            children = std.ArrayList(*TeleAst).init(self.allocator);

            while (!token_queue3.empty()) {
                var found_end: bool = false;
                const ast = try self.parse_record_field(token_queue3, true, &found_end);
                try children.?.append(ast);
                if (found_end) {
                    break;
                }
            }
        } else {
            const temp = try token_queue3.pop();
            self.allocator.free(temp.*.body);
            self.allocator.destroy(temp);
        }

        if (!token_queue3.empty()) {
            return ParserError.ParsingFailure;
        }

        // Assemble Record Definition Ast
        const t = try self.allocator.create(TeleAst);
        t.*.body = buf;
        t.*.ast_type = TeleAstType.record_def;
        t.*.children = children;
        t.*.col = 0;

        token_queue3.deinit();

        return t;
    }

    fn parse_record_field(self: *Self, token_queue: *TokenQueue, type_exp: bool, found_end: *bool) !*TeleAst {
        var buffer_token_queue = try TokenQueue.init(self.allocator);
        defer buffer_token_queue.deinit();

        var n_count: usize = 0;
        while (!token_queue.empty()) {
            const n = try token_queue.pop();

            if (n_count == 0 and is_paren_end(n.*.body)) {
                self.allocator.free(n.*.body);
                self.allocator.destroy(n);
                found_end.* = true;
                break;
            } else if (n_count == 0 and isComma(n.*.body)) {
                self.allocator.free(n.*.body);
                self.allocator.destroy(n);
                break;
            }

            if (is_paren_start(n.*.body) or is_record_start(n.*.body) or is_map_start(n.*.body) or is_tuple_start(n.*.body) or is_list_start(n.*.body)) {
                n_count += 1;
            } else if (is_paren_end(n.*.body) or is_map_end(n.*.body) or is_list_end(n.*.body)) {
                n_count -= 1;
            }

            try buffer_token_queue.push(n.*.body, n.*.col, n.*.line);
            self.allocator.destroy(n);
        }

        if (buffer_token_queue.empty()) {
            return ParserError.ParsingFailure;
        }

        // Parse Field Key
        const key = try buffer_token_queue.pop();
        const name = key.*.body;
        errdefer self.allocator.free(name);
        self.allocator.destroy(key);

        var children: ?std.ArrayList(*TeleAst) = std.ArrayList(*TeleAst).init(self.allocator);
        if (!buffer_token_queue.empty()) {
            const op = try buffer_token_queue.pop();
            //errdefer self.allocator.free(op.*.body);
            //errdefer self.allocator.destroy(op);

            if (is_equal(op.*.body)) {
                const ast = try self.parse_record_field_value(buffer_token_queue);
                try children.?.append(ast);
            } else if (is_colon(op.*.body) and type_exp) {
                const ast = try self.parse_record_field_type(buffer_token_queue);
                try children.?.append(ast);
            } else {
                return ParserError.ParsingFailure;
            }
            self.allocator.free(op.*.body);
            self.allocator.destroy(op);
        }

        if (!buffer_token_queue.empty() and type_exp) {
            if (children == null) {
                return ParserError.ParsingFailure;
            }

            const op = try buffer_token_queue.pop();
            //errdefer self.allocator.free(op.*.body);
            //errdefer self.allocator.destroy(op);

            if (is_colon(op.*.body) and type_exp) {
                const ast = try self.parse_record_field_type(buffer_token_queue);
                try children.?.append(ast);
            } else {
                return ParserError.ParsingFailure;
            }
            self.allocator.free(op.*.body);
            self.allocator.destroy(op);
        }

        const t = try self.allocator.create(TeleAst);
        t.*.body = name;
        t.*.children = children;
        t.*.ast_type = TeleAstType.record_field;
        t.*.col = 0;

        return t;
    }

    fn parse_record_field_value(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        const ast = try self.parse_exp(token_queue);
        const t = try self.allocator.create(TeleAst);
        t.*.body = "";
        t.*.children = std.ArrayList(*TeleAst).init(self.allocator);
        try t.*.children.?.append(ast);
        t.*.ast_type = TeleAstType.record_field_value;
        t.*.col = 0;

        return t;
    }

    fn parse_record_field_type(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        const ast = try self.parse_type_exp(token_queue);
        const t = try self.allocator.create(TeleAst);
        t.*.body = "";
        t.*.children = std.ArrayList(*TeleAst).init(self.allocator);
        try t.*.children.?.append(ast);
        t.*.ast_type = TeleAstType.record_field_type;
        t.*.col = 0;
        return t;
    }

    fn parse_exp(self: *Self, token_queue: *TokenQueue) ParserError!*TeleAst {
        const pn = token_queue.peek() catch {
            return ParserError.ParsingFailure;
        };

        var ast: ?*TeleAst = null;

        if (is_float(pn.*.body)) {
            ast = self.parse_float(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_int(pn.*.body)) {
            ast = self.parse_int(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_atom(pn.*.body)) {
            ast = self.parse_atom(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_binary(pn.*.body)) {
            ast = self.parse_binary(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isFun(pn.*.body)) {
            ast = self.parseAnonymousFunction(token_queue, false) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_operator(pn.*.body)) {
            ast = self.parse_operator(token_queue, false, null) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_paren_start(pn.*.body)) {
            ast = self.parse_paren_exp(token_queue, false) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_tuple_start(pn.*.body)) {
            ast = self.parse_tuple(token_queue, false) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_record_start(pn.*.body)) {
            ast = self.parse_record(token_queue, false, "", false) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_list_start(pn.*.body)) {
            ast = self.parse_list(token_queue, false) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_map_start(pn.*.body)) {
            ast = self.parse_map(token_queue, false) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isCaseKeyword(pn.*.body)) {
            ast = self.parseCaseExpression(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isTryKeyword(pn.*.body)) {
            ast = self.parse_try_catch(token_queue) catch {
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
                        ast = self.parse_record(token_queue, true, n.*.body, false) catch { // Is a record variable
                            return ParserError.ParsingFailure;
                        };
                    } else { // Is a function call
                        ast = self.parse_function_call(token_queue, n.*.body, false) catch {
                            return ParserError.ParsingFailure;
                        };
                    }
                } else {
                    ast = self.parse_variable(n.*.body, false) catch {
                        return ParserError.ParsingFailure;
                    };
                }
            } else {
                ast = self.parse_variable(n.*.body, false) catch {
                    return ParserError.ParsingFailure;
                };
            }
        }

        if (!token_queue.empty()) {
            const res2 = check_operator_peek(token_queue) catch {
                return ParserError.ParsingFailure;
            };
            if (res2) {
                ast = self.parse_operator(token_queue, false, ast) catch {
                    return ParserError.ParsingFailure;
                };
            }
        }

        return ast.?;
    }

    fn parse_type_exp(self: *Self, token_queue: *TokenQueue) ParserError!*TeleAst {
        const pn = token_queue.peek() catch {
            return ParserError.ParsingFailure;
        };

        var ast: ?*TeleAst = null;

        if (is_float(pn.*.body)) {
            ast = self.parse_float(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_int(pn.*.body)) {
            ast = self.parse_int(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_atom(pn.*.body)) {
            ast = self.parse_atom(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_binary(pn.*.body)) {
            ast = self.parse_binary(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isFun(pn.*.body)) {
            ast = self.parseAnonymousFunction(token_queue, true) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_operator(pn.*.body)) {
            ast = self.parse_operator(token_queue, true, null) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_paren_start(pn.*.body)) {
            ast = self.parse_paren_exp(token_queue, true) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_tuple_start(pn.*.body)) {
            ast = self.parse_tuple(token_queue, true) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_record_start(pn.*.body)) {
            ast = self.parse_record(token_queue, false, "", true) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_list_start(pn.*.body)) {
            ast = self.parse_list(token_queue, true) catch {
                return ParserError.ParsingFailure;
            };
        } else if (is_map_start(pn.*.body)) {
            ast = self.parse_map(token_queue, true) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isCaseKeyword(pn.*.body)) {
            return ParserError.ParsingFailure;
        } else if (isTryKeyword(pn.*.body)) {
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
                        ast = self.parse_function_call(token_queue, n.*.body, true) catch {
                            return ParserError.ParsingFailure;
                        };
                    }
                } else {
                    ast = self.parse_variable(n.*.body, true) catch {
                        return ParserError.ParsingFailure;
                    };
                }
            } else {
                ast = self.parse_variable(n.*.body, true) catch {
                    return ParserError.ParsingFailure;
                };
            }
        }

        if (!token_queue.empty()) {
            const res2 = check_operator_peek(token_queue) catch {
                return ParserError.ParsingFailure;
            };
            if (res2) {
                ast = self.parse_operator(token_queue, true, ast) catch {
                    return ParserError.ParsingFailure;
                };
            }
        }

        return ast.?;
    }

    fn parse_float(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        return try self.parse_value(token_queue, TeleAstType.float);
    }

    fn parse_int(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        return try self.parse_value(token_queue, TeleAstType.int);
    }

    fn parse_atom(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        return try self.parse_value(token_queue, TeleAstType.atom);
    }

    fn parse_binary(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        return try self.parse_value(token_queue, TeleAstType.binary);
    }

    fn parse_variable(self: *Self, buf: []const u8, type_exp: bool) !*TeleAst {
        if (type_exp and buf[0] != '@') {
            const t = try self.allocator.create(TeleAst);
            t.*.body = buf;
            t.*.ast_type = TeleAstType.function_call;
            t.*.children = std.ArrayList(*TeleAst).init(self.allocator);
            t.*.col = 0;
            return t;
        } else {
            const t = try self.allocator.create(TeleAst);
            t.*.body = buf;
            t.*.ast_type = TeleAstType.variable;
            t.*.children = null;
            t.*.col = 0;
            return t;
        }
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

    fn parseFunVal(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        // TODO: Check that name is not numbers
        const name = try token_queue.pop();
        const div = try token_queue.pop();
        if (div.*.body[0] != '/') {
            self.allocator.free(name.*.body);
            self.allocator.destroy(name);
            self.allocator.free(div.*.body);
            self.allocator.destroy(div);
            return ParserError.ParsingFailure;
        }

        const num = try token_queue.pop();

        const buf = try self.allocator.alloc(u8, name.*.body.len + 1 + num.*.body.len);
        std.mem.copyForwards(u8, buf, name.*.body);
        buf[name.*.body.len] = '/';
        std.mem.copyForwards(u8, buf[name.*.body.len + 1 ..], num.*.body);

        self.allocator.free(name.*.body);
        self.allocator.destroy(name);
        self.allocator.free(div.*.body);
        self.allocator.destroy(div);
        self.allocator.free(num.*.body);
        self.allocator.destroy(num);

        const t = try self.allocator.create(TeleAst);
        t.*.body = buf;
        t.*.ast_type = TeleAstType.fun_val;
        t.*.children = null;
        t.*.col = 0;
        return t;
    }

    fn parse_operator(self: *Self, token_queue: *TokenQueue, type_exp: bool, arg: ?*TeleAst) !*TeleAst {
        // Pop off operator
        const node = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };
        errdefer self.allocator.destroy(node);

        // TODO: Allow for negative numbers
        if (type_exp and !is_pipe_operator(node.*.body)) {
            self.allocator.free(node.*.body);
            return ParserError.ParsingFailure;
        }

        var ast: ?*TeleAst = null;
        if (type_exp) {
            ast = self.parse_type_exp(token_queue) catch {
                self.allocator.free(node.*.body);
                return ParserError.ParsingFailure;
            };
        } else {
            ast = self.parse_exp(token_queue) catch {
                self.allocator.free(node.*.body);
                return ParserError.ParsingFailure;
            };
        }
        errdefer tele_ast.freeTeleAst(ast.?, self.allocator);

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(children, self.allocator);
        if (arg != null) {
            try children.append(arg.?);
        }
        try children.append(ast.?);

        const t = try self.allocator.create(TeleAst);
        t.*.body = node.*.body;
        t.*.ast_type = TeleAstType.op;
        t.*.children = children;
        t.*.col = 0;

        self.allocator.destroy(node);

        return t;
    }

    fn parse_paren_exp(self: *Self, token_queue: *TokenQueue, type_exp: bool) !*TeleAst {
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
            errdefer tele_ast.freeTeleAstList(children.?, self.allocator);

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
                const ast = try self.parse_paren_exp_element(buffer_token_queue, type_exp);
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

        return tast;
    }

    fn parse_paren_exp_element(self: *Self, token_queue: *TokenQueue, type_exp: bool) !*TeleAst {
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
                if (isComma(n2.*.body) or is_paren_end(n2.*.body)) {
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
            return try self.parse_type_exp(buffer_token_queue);
        } else {
            return try self.parse_exp(buffer_token_queue);
        }
    }

    fn parse_tuple(self: *Self, token_queue: *TokenQueue, type_exp: bool) !*TeleAst {
        //Pop off #(

        const n = try token_queue.pop();
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(children, self.allocator);
        var token_queue2 = try TokenQueue.init(self.allocator);
        errdefer token_queue2.deinit();

        // Expected to have already parsed start of tuple, so count starts at 1
        var count: usize = 1;
        var end_of_tuple = false;

        while (!token_queue.empty() and !end_of_tuple) {
            while (!token_queue.empty()) {
                const node2 = try token_queue.pop();
                errdefer self.allocator.destroy(node2);

                if (is_list_start(node2.*.body) or is_tuple_start(node2.*.body) or is_map_start(node2.*.body) or is_record_start(node2.*.body) or is_paren_start(node2.*.body)) {
                    count += 1;
                } else if (is_paren_end(node2.*.body) or is_list_end(node2.*.body) or is_map_end(node2.*.body)) {
                    count -= 1;
                    if (count == 0) {
                        // Free paren end body
                        self.allocator.free(node2.*.body);
                        self.allocator.destroy(node2);
                        end_of_tuple = true;
                        break;
                    }
                }

                if (count == 1 and isComma(node2.*.body)) {
                    self.allocator.free(node2.*.body);
                    self.allocator.destroy(node2);
                    break;
                } else {
                    try token_queue2.push(node2.*.body, node2.*.line, node2.*.col);
                }

                self.allocator.destroy(node2);
            }

            var ast: *TeleAst = undefined;
            if (type_exp) {
                ast = try self.parse_type_exp(token_queue2);
            } else {
                ast = try self.parse_exp(token_queue2);
            }
            if (!token_queue2.empty()) {
                return ParserError.ParsingFailure;
            }
            try children.append(ast);
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

        return t;
    }

    fn parse_list(self: *Self, token_queue: *TokenQueue, type_exp: bool) !*TeleAst {
        // Pop off [
        const n = try token_queue.pop();
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(children, self.allocator);

        const pn_end = try token_queue.peek();
        if (is_list_end(pn_end.*.body)) {
            const n2 = try token_queue.pop();
            self.allocator.free(n2.*.body);
            self.allocator.destroy(n2);
        } else {
            var token_queue2 = try TokenQueue.init(self.allocator);
            errdefer token_queue2.deinit();

            // Expected to have already parsed start of list so count starts at 1
            var count: usize = 1;
            var end_of_list = false;

            while (!token_queue.empty() and !end_of_list) {
                while (!token_queue.empty()) {
                    const node2 = try token_queue.pop();
                    errdefer self.allocator.destroy(node2);

                    if (is_list_start(node2.*.body) or is_tuple_start(node2.*.body) or is_map_start(node2.*.body) or is_record_start(node2.*.body) or is_paren_start(node2.*.body)) {
                        count += 1;
                    } else if (is_list_end(node2.*.body) or is_paren_end(node2.*.body) or is_map_end(node2.*.body)) {
                        count -= 1;
                        if (count == 0) {
                            self.allocator.free(node2.*.body);
                            self.allocator.destroy(node2);
                            end_of_list = true;
                            break;
                        }
                    }

                    if (count == 1 and isComma(node2.*.body)) {
                        self.allocator.free(node2.*.body);
                        self.allocator.destroy(node2);
                        break;
                    } else {
                        try token_queue2.push(node2.*.body, node2.*.line, node2.*.col);
                    }

                    self.allocator.destroy(node2);
                }

                var ast: *TeleAst = undefined;
                if (type_exp) {
                    ast = try self.parse_type_exp(token_queue2);
                } else {
                    ast = try self.parse_exp(token_queue2);
                }

                if (!token_queue2.empty()) {
                    return ParserError.ParsingFailure;
                }
                try children.append(ast);
            }

            if (count != 0) {
                return ParserError.ParsingFailure;
            }

            token_queue2.deinit();
        }

        const t = try self.allocator.create(TeleAst);
        t.*.body = "";
        t.*.ast_type = TeleAstType.list;
        t.*.children = children;
        t.*.col = 0;

        return t;
    }

    // TODO: Handle empty maps properly
    fn parse_map(self: *Self, token_queue: *TokenQueue, type_exp: bool) !*TeleAst {
        // Pop off {
        const n = try token_queue.pop();
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        const t = try self.allocator.create(TeleAst);
        errdefer tele_ast.freeTeleAst(t, self.allocator);
        t.*.body = "";
        t.*.children = null;

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(children, self.allocator);

        const pn_end = try token_queue.peek();
        if (is_map_end(pn_end.*.body)) {
            const n2 = try token_queue.pop();
            self.allocator.free(n2.*.body);
            self.allocator.destroy(n2);
        } else {
            var token_queue2 = try TokenQueue.init(self.allocator);
            errdefer token_queue2.deinit();
            var count: usize = 1;
            var end_of_map: bool = false;
            var map_update: bool = false;

            while (!token_queue.empty() and !end_of_map) {
                while (!token_queue.empty()) {
                    const node2 = token_queue.pop() catch {
                        return ParserError.ParsingFailure;
                    };
                    errdefer self.allocator.free(node2.*.body);
                    errdefer self.allocator.destroy(node2);

                    if (is_map_start(node2.*.body) or is_list_start(node2.*.body) or is_tuple_start(node2.*.body) or is_record_start(node2.*.body) or is_paren_start(node2.*.body)) {
                        count += 1;
                    } else if (is_map_end(node2.*.body) or is_paren_end(node2.*.body) or is_list_end(node2.*.body)) {
                        count -= 1;
                        if (count == 0) {
                            self.allocator.free(node2.*.body);
                            self.allocator.destroy(node2);
                            end_of_map = true;
                            break;
                        }
                    }

                    if (count == 1 and (isComma(node2.*.body) or is_colon(node2.*.body))) {
                        self.allocator.free(node2.*.body);
                        self.allocator.destroy(node2);
                        break;
                    } else if (count == 1 and is_pipe_operator(node2.*.body)) {
                        self.allocator.free(node2.*.body);
                        self.allocator.destroy(node2);
                        map_update = true;
                        break;
                    } else {
                        try token_queue2.push(node2.*.body, node2.*.line, node2.*.col);
                    }

                    self.allocator.destroy(node2);
                }

                var ast: *TeleAst = undefined;
                if (type_exp) {
                    ast = try self.parse_type_exp(token_queue2);
                } else {
                    if (map_update) {
                        const name_node = try token_queue2.pop();
                        t.*.body = name_node.*.body;
                        self.allocator.destroy(name_node);
                    } else {
                        ast = try self.parse_exp(token_queue2);
                    }
                }
                if (!token_queue2.empty()) {
                    return ParserError.ParsingFailure;
                }
                if (map_update) {
                    map_update = false;
                } else {
                    try children.append(ast);
                }
            }

            if (count != 0) {
                return ParserError.ParsingFailure;
            }

            token_queue2.deinit();
        }

        t.*.ast_type = TeleAstType.map;
        t.*.children = children;
        t.*.col = 0;

        return t;
    }

    fn parse_record(self: *Self, token_queue: *TokenQueue, variable: bool, buf: []const u8, type_exp: bool) !*TeleAst {
        var buf2: []const u8 = undefined;
        if (!variable) {
            const node = try token_queue.pop();
            buf2 = node.*.body;
            self.allocator.destroy(node);
        } else {
            buf2 = buf;
            const node = try token_queue.pop();
            self.allocator.free(node.*.body);
            self.allocator.destroy(node);
        }

        var buffer_token_queue = try TokenQueue.init(self.allocator);
        errdefer buffer_token_queue.deinit();

        var count: usize = 0;

        while (!token_queue.empty()) {
            const node2 = try token_queue.pop();
            errdefer self.allocator.free(node2.*.body);
            errdefer self.allocator.destroy(node2);

            if (count == 0 and is_paren_end(node2.*.body)) {
                try buffer_token_queue.push(node2.*.body, node2.*.line, node2.*.col);
                self.allocator.destroy(node2);
                break;
            }

            if (is_tuple_start(node2.*.body) or is_paren_start(node2.*.body) or is_record_start(node2.*.body) or is_list_start(node2.*.body) or is_map_start(node2.*.body)) {
                count += 1;
            } else if (is_paren_end(node2.*.body) or is_list_end(node2.*.body) or is_map_end(node2.*.body)) {
                count -= 1;
            }
            try buffer_token_queue.push(node2.*.body, node2.*.line, node2.*.col);

            self.allocator.destroy(node2);
        }

        if (buffer_token_queue.empty()) {
            return ParserError.ParsingFailure;
        }

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(children, self.allocator);

        const pn = try buffer_token_queue.peek();
        if (!is_paren_end(pn.*.body)) {
            while (!buffer_token_queue.empty()) {
                var found_end: bool = false;
                const ast = try self.parse_record_field(buffer_token_queue, type_exp, &found_end);
                try children.append(ast);
                if (found_end) {
                    break;
                }
            }
        } else {
            // Pop off paren end
            const tmp2 = try buffer_token_queue.pop();
            self.allocator.free(tmp2.*.body);
            self.allocator.destroy(tmp2);
        }

        if (!buffer_token_queue.empty()) {
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
        return t;
    }

    fn parse_body(self: *Self, token_queue: *TokenQueue) !std.ArrayList(*TeleAst) {
        var alist = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(alist, self.allocator);

        while (!token_queue.empty()) {
            const ast = try self.parse_exp(token_queue);
            try alist.append(ast);
        }

        if (alist.items.len == 0) {
            return ParserError.ParsingFailure;
        }
        return alist;
    }

    fn parseAnonymousFunction(self: *Self, token_queue: *TokenQueue, type_exp: bool) !*TeleAst {
        // Pop off fun
        const temp = try token_queue.pop();
        const current_col = temp.*.col;
        self.allocator.free(temp.*.body);
        self.allocator.destroy(temp);

        const pn = token_queue.peek() catch {
            return ParserError.ParsingFailure;
        };

        if (!is_paren_start(pn.*.body)) {
            const t = try self.parseFunVal(token_queue);
            return t;
        } else {
            const children = try self.parseFunctionSigAndBody(token_queue, type_exp, current_col);

            // Assemble Anonymous Function Ast
            const t = try self.allocator.create(TeleAst);
            t.*.body = "";
            t.*.ast_type = TeleAstType.anonymous_function;
            t.*.children = children;
            t.*.col = current_col;

            return t;
        }
    }

    fn parseFunctionSigAndBody(self: *Self, token_queue: *TokenQueue, type_exp: bool, current_col: usize) !std.ArrayList(*TeleAst) {
        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(children, self.allocator);

        var function_end: bool = false;
        while (!token_queue.empty()) {
            var token_queue_section = try self.collectFunctionSection(token_queue, current_col, &function_end);

            // Function Signature
            var token_queue2 = TokenQueue.init(self.allocator) catch {
                return ParserError.ParsingFailure;
            };
            errdefer token_queue2.deinit();

            // Gather tokens for function signature

            var map_count: usize = 0;
            while (!token_queue_section.empty()) {
                const node2 = try token_queue_section.pop();
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

            const ast = try self.parse_function_signature(token_queue2, type_exp);
            errdefer tele_ast.freeTeleAst(ast, self.allocator);
            token_queue2.deinit();

            try children.append(ast);

            if (!token_queue_section.empty()) {

                // Function Definition Body

                var token_queue3 = try TokenQueue.init(self.allocator);
                errdefer token_queue3.deinit();

                // Gather tokens for function body

                while (!token_queue_section.empty()) {
                    const peek_node2 = try token_queue_section.peek();
                    if (peek_node2.*.col <= current_col) {
                        break;
                    }

                    const node2 = try token_queue_section.pop();
                    errdefer self.allocator.free(node2.*.body);
                    errdefer self.allocator.destroy(node2);

                    try token_queue3.push(node2.*.body, node2.*.line, node2.*.col);
                    self.allocator.destroy(node2);
                }

                if (token_queue3.empty()) {
                    return ParserError.ParsingFailure;
                }

                if (type_exp) {
                    const a = self.parse_type_exp(token_queue3) catch {
                        return ParserError.ParsingFailure;
                    };
                    try children.append(a);
                } else {
                    var alist = self.parse_body(token_queue3) catch {
                        return ParserError.ParsingFailure;
                    };
                    errdefer tele_ast.freeTeleAstList(alist, self.allocator);

                    for (alist.items) |a| {
                        try children.append(a);
                    }
                    alist.deinit();
                }
                token_queue3.deinit();
            }

            token_queue_section.deinit();

            if (function_end) {
                break;
            }
        }

        return children;
    }

    // Function Section = signature and body
    // Stop at end of function body or new signature
    fn collectFunctionSection(self: *Self, token_queue: *TokenQueue, current_col: usize, function_end: *bool) !*TokenQueue {
        var token_queue2 = TokenQueue.init(self.allocator) catch {
            return ParserError.ParsingFailure;
        };
        errdefer token_queue2.deinit();

        var buffer_token_queue = TokenQueue.init(self.allocator) catch {
            return ParserError.ParsingFailure;
        };
        errdefer buffer_token_queue.deinit();

        var body: bool = false;
        var paren_count: usize = 0;
        var signature_ready: bool = false;
        var nested_body: bool = false;
        var nested_body_col: usize = 0;

        while (!token_queue.empty()) {
            const peek_node = try token_queue.peek();
            if (peek_node.*.col <= current_col) {
                function_end.* = true;
                break;
            }

            if (!body) {
                if (is_paren_start(peek_node.*.body) or is_tuple_start(peek_node.*.body) or is_record_start(peek_node.*.body) or containsParenStart(peek_node.*.body)) {
                    paren_count += 1;
                } else if (is_paren_end(peek_node.*.body)) {
                    paren_count -= 1;
                    if (paren_count == 0) {
                        signature_ready = true;
                    }
                } else if (signature_ready and is_colon(peek_node.*.body)) {
                    body = true;
                    signature_ready = false;
                } else if (signature_ready) {
                    signature_ready = false;
                }

                const node = try token_queue.pop();
                errdefer self.allocator.free(node.*.body);
                errdefer self.allocator.destroy(node);

                try token_queue2.push(node.*.body, node.*.line, node.*.col);
                self.allocator.destroy(node);
            } else {
                if (!nested_body) {
                    if (is_paren_start(peek_node.*.body) or is_tuple_start(peek_node.*.body) or is_record_start(peek_node.*.body) or containsParenStart(peek_node.*.body)) {
                        paren_count += 1;
                    } else if (is_paren_end(peek_node.*.body)) {
                        paren_count -= 1;
                        if (paren_count == 0) {
                            signature_ready = true;
                        }
                    } else if (signature_ready and is_colon(peek_node.*.body)) {
                        // TODO: Memory management for buffer list
                        var buffer_list = std.ArrayList(*TokenQueueNode).init(self.allocator);

                        while (!buffer_token_queue.empty()) {
                            const n = try buffer_token_queue.pop();
                            try buffer_list.append(n);
                        }

                        while (buffer_list.items.len > 0) {
                            const n = buffer_list.pop();
                            try token_queue.pushHead(n.*.body, n.*.line, n.*.col);
                            self.allocator.destroy(n);
                        }
                        buffer_list.deinit();
                        break;
                    } else if (signature_ready) {
                        signature_ready = false;
                        while (!buffer_token_queue.empty()) {
                            const n = try buffer_token_queue.pop();
                            try token_queue2.push(n.*.body, n.*.line, n.*.col);
                            self.allocator.destroy(n);
                        }
                    } else if (isFun(peek_node.*.body) and paren_count == 0) {
                        nested_body = true;
                        nested_body_col = peek_node.*.col;
                    } else if (isCaseKeyword(peek_node.*.body)) {
                        nested_body = true;
                        nested_body_col = peek_node.*.col;
                    } else if (isTryKeyword(peek_node.*.body)) {
                        nested_body = true;
                        nested_body_col = peek_node.*.col;
                    } else if (isCatchKeyword(peek_node.*.body)) {
                        nested_body = true;
                        nested_body_col = peek_node.*.col;
                    }
                } else {
                    if (peek_node.*.col <= nested_body_col) {
                        nested_body = false;
                        nested_body_col = 0;
                    }
                }

                const node = try token_queue.pop();
                errdefer self.allocator.free(node.*.body);
                errdefer self.allocator.destroy(node);

                if (paren_count > 0 or signature_ready) {
                    try buffer_token_queue.push(node.*.body, node.*.line, node.*.col);
                    self.allocator.destroy(node);
                } else {
                    try token_queue2.push(node.*.body, node.*.line, node.*.col);
                    self.allocator.destroy(node);
                }
            }
        }

        // Make sure to clear buffer_token_queue
        while (!buffer_token_queue.empty()) {
            const n = try buffer_token_queue.pop();
            try token_queue2.push(n.*.body, n.*.line, n.*.col);
            self.allocator.destroy(n);
        }

        if (token_queue2.empty()) {
            return ParserError.ParsingFailure;
        }

        if (token_queue.empty()) {
            function_end.* = true;
        }

        buffer_token_queue.deinit();

        return token_queue2;
    }

    fn parseCaseExpression(self: *Self, token_queue: *TokenQueue) !*TeleAst {
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

        const signature_ast = try self.parseCaseSignature(buffer_token_queue);

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(children, self.allocator);
        try children.append(signature_ast);

        var body = try self.parseCaseBody(buffer_token_queue);
        errdefer tele_ast.freeTeleAstList(body, self.allocator);
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

        return final_ast;
    }

    fn parseCaseSignature(self: *Self, token_queue: *TokenQueue) !*TeleAst {
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

        const ast = try self.parse_exp(buffer_token_queue);
        if (!buffer_token_queue.empty()) {
            return ParserError.ParsingFailure;
        }
        buffer_token_queue.deinit();
        return ast;
    }

    fn parseCaseBody(self: *Self, token_queue: *TokenQueue) !std.ArrayList(*TeleAst) {
        var clause_col: usize = 0;
        var alist = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(alist, self.allocator);

        // Parse Case Clauses
        while (!token_queue.empty()) {
            var children = std.ArrayList(*TeleAst).init(self.allocator);
            errdefer tele_ast.freeTeleAstList(children, self.allocator);

            var alist2 = try self.parse_case_clause_signature(token_queue, &clause_col);
            if (alist2.items.len == 2) {
                const ast = alist.pop();
                if (ast.ast_type != TeleAstType.guard_clause) {
                    tele_ast.freeTeleAst(ast, self.allocator);
                    return ParserError.ParsingFailure;
                }
                const ast2 = alist2.pop();
                try children.append(ast2);
                try children.append(ast);
            } else if (alist2.items.len == 1) {
                try children.append(alist2.pop());
            } else {
                tele_ast.freeTeleAstList(alist2, self.allocator);
                return ParserError.ParsingFailure;
            }
            alist2.deinit();

            var case_clause_body = try self.parse_case_clause_body(token_queue, clause_col);
            errdefer tele_ast.freeTeleAstList(case_clause_body, self.allocator);
            for (case_clause_body.items) |c| {
                try children.append(c);
            }
            case_clause_body.deinit();

            const t = try self.allocator.create(TeleAst);
            t.body = "";
            t.ast_type = TeleAstType.case_clause;
            t.children = children;
            t.col = 0;
            errdefer tele_ast.freeTeleAst(t, self.allocator);

            try alist.append(t);
        }

        return alist;
    }

    fn parse_try_catch(self: *Self, token_queue: *TokenQueue) !*TeleAst {
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

        const signature_ast = try self.parseCaseSignature(buffer_token_queue);
        errdefer tele_ast.freeTeleAst(signature_ast, self.allocator);

        var try_children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(try_children, self.allocator);
        try try_children.append(signature_ast);

        var body = try self.parseCaseBody(buffer_token_queue);
        errdefer tele_ast.freeTeleAstList(body, self.allocator);
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
        if (!isCatchKeyword(n_catch.*.body)) {
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
        errdefer tele_ast.freeTeleAstList(catch_children, self.allocator);
        try try_children.append(signature_ast);

        var body2 = try self.parseCaseBody(buffer_token_queue);
        errdefer tele_ast.freeTeleAstList(body2, self.allocator);
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
        errdefer tele_ast.freeTeleAstList(children, self.allocator);
        try children.append(try_ast);
        try children.append(catch_ast);
        final_ast.*.children = children;
        final_ast.*.col = 0;

        buffer_token_queue.deinit();

        return final_ast;
    }

    fn parse_case_clause_signature(self: *Self, token_queue: *TokenQueue, clause_col: *usize) !std.ArrayList(*TeleAst) {
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

        var alist = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(alist, self.allocator);

        const ast = try self.parse_exp(buffer_token_queue);
        try alist.append(ast);
        if (!buffer_token_queue.empty()) {
            const pn = try buffer_token_queue.peek();
            if (std.mem.eql(u8, "when", pn.*.body)) {
                const gc_ast_list = try self.parseGuardClauses(buffer_token_queue);
                for (gc_ast_list.items) |a| {
                    try alist.append(a);
                }
                gc_ast_list.deinit();
            } else {
                return ParserError.ParsingFailure;
            }
        }
        buffer_token_queue.deinit();
        return alist;
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
        errdefer tele_ast.freeTeleAstList(alist, self.allocator);

        if (!buffer_token_queue.empty()) {
            return ParserError.ParsingFailure;
        }

        buffer_token_queue.deinit();

        return alist;
    }

    fn parse_function_call(self: *Self, token_queue: *TokenQueue, buf: []const u8, type_exp: bool) !*TeleAst {
        // Remove paren start
        const pn = try token_queue.pop();
        self.allocator.free(pn.*.body);
        self.allocator.destroy(pn);

        const pn2 = try token_queue.peek();

        var children: ?std.ArrayList(*TeleAst) = null;
        if (!is_paren_end(pn2.*.body)) {
            children = std.ArrayList(*TeleAst).init(self.allocator);
            errdefer tele_ast.freeTeleAstList(children.?, self.allocator);
            while (!token_queue.empty()) {
                var found_end: bool = false;
                const ast = try self.parse_function_call_arg(token_queue, type_exp, &found_end);
                try children.?.append(ast);
                if (found_end) {
                    break;
                }
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

        return t;
    }

    fn parse_function_call_arg(self: *Self, token_queue: *TokenQueue, type_exp: bool, found_end: *bool) !*TeleAst {
        // Function Call Body Token Queue
        var buffer_token_queue = try TokenQueue.init(self.allocator);
        errdefer buffer_token_queue.deinit();

        var n_count: usize = 0;
        while (!token_queue.empty()) {
            const n2 = try token_queue.pop();
            errdefer self.allocator.free(n2.*.body);
            errdefer self.allocator.destroy(n2);

            if (n_count == 0) {
                if (isComma(n2.*.body) or is_paren_end(n2.*.body)) {
                    if (is_paren_end(n2.*.body)) {
                        found_end.* = true;
                    }
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

        var ast: *TeleAst = undefined;
        if (type_exp) {
            ast = try self.parse_type_exp(buffer_token_queue);
        } else {
            ast = try self.parse_exp(buffer_token_queue);
        }
        if (!buffer_token_queue.empty()) {
            tele_ast.freeTeleAst(ast, self.allocator);
            return ParserError.ParsingFailure;
        }
        buffer_token_queue.deinit();
        return ast;
    }

    fn parse_attribute(self: *Self, token_queue: *TokenQueue) !*TeleAst {

        // Pop off attribute name
        const name_node = try token_queue.pop();
        const name = name_node.*.body;
        errdefer self.allocator.free(name);
        self.allocator.destroy(name_node);

        const bast = self.parse_function_call(token_queue, name, false) catch {
            return ParserError.ParsingFailure;
        };

        bast.*.ast_type = TeleAstType.attribute;

        return bast;
    }

    fn parse_custom_attribute(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        // Pop off attr
        const a_node = try token_queue.pop();
        self.allocator.free(a_node.*.body);
        self.allocator.destroy(a_node);

        const bast = try self.parse_attribute(token_queue);

        bast.*.ast_type = TeleAstType.custom_attribute;

        return bast;
    }

    fn parse_behaviour(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        const bast = try self.parse_attribute(token_queue);

        bast.*.children.?.items[0].ast_type = TeleAstType.atom;
        bast.*.ast_type = TeleAstType.attribute;

        return bast;
    }

    fn parseImport(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        // Pop off import
        const n = try token_queue.pop();
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        const bast = try self.parse_attribute(token_queue);
        bast.*.ast_type = TeleAstType.import_def;

        return bast;
    }
};

test "parse operator expression" {
    const parser = try fileToParser("snippets/op.tl", talloc);
    defer parser.deinit();

    const init = try tele_ast.makeInt(try util.copyString("1", talloc), talloc);
    const result = try parser.parse_operator(parser.*.token_queue, false, init);

    const arg1 = try tele_ast.makeInt(try util.copyString("1", talloc), talloc);
    const arg2 = try tele_ast.makeInt(try util.copyString("2", talloc), talloc);
    const expected = try tele_ast.makeOp(try util.copyString("+", talloc), arg1, arg2, talloc);
    try std.testing.expect(tele_ast.equal(result, expected));

    tele_ast.free_tele_ast(result, talloc);
    tele_ast.free_tele_ast(expected, talloc);
}

test "parse operator expression chained" {
    const parser = try fileToParser("snippets/op2.tl", talloc);
    defer parser.deinit();

    const init = try tele_ast.makeVariable(try util.copyString("a", talloc), talloc);
    const result = try parser.parse_operator(parser.*.token_queue, false, init);

    const arg1 = try tele_ast.makeInt(try util.copyString("1", talloc), talloc);
    const arg2 = try tele_ast.makeInt(try util.copyString("2", talloc), talloc);
    const op = try tele_ast.makeOp(try util.copyString("+", talloc), arg1, arg2, talloc);

    const v = try tele_ast.makeVariable(try util.copyString("a", talloc), talloc);
    const expected = try tele_ast.makeOp(try util.copyString("=", talloc), v, op, talloc);

    try std.testing.expect(tele_ast.equal(result, expected));

    tele_ast.free_tele_ast(expected, talloc);
    tele_ast.free_tele_ast(result, talloc);
}

test "parse tuple" {
    const parser = try fileToParser("snippets/tuple.tl", talloc);
    defer parser.deinit();

    const result = try parser.parse_tuple(parser.*.token_queue, false);

    const e1 = try tele_ast.makeInt(try util.copyString("1", talloc), talloc);
    const e2 = try tele_ast.makeInt(try util.copyString("2", talloc), talloc);
    const e3 = try tele_ast.makeInt(try util.copyString("3", talloc), talloc);
    const a = [_]*TeleAst{ e1, e2, e3 };
    const expected = try tele_ast.makeTuple(&a, talloc);

    try std.testing.expect(tele_ast.equal(result, expected));

    tele_ast.free_tele_ast(expected, talloc);
    tele_ast.free_tele_ast(result, talloc);
}

test "parse list" {
    const parser = try fileToParser("snippets/list.tl", talloc);
    defer parser.deinit();

    const result = try parser.parse_list(parser.*.token_queue, false);

    const e1 = try tele_ast.makeInt(try util.copyString("1", talloc), talloc);
    const e2 = try tele_ast.makeInt(try util.copyString("2", talloc), talloc);
    const e3 = try tele_ast.makeInt(try util.copyString("3", talloc), talloc);
    const a = [_]*TeleAst{ e1, e2, e3 };
    const expected = try tele_ast.makeList(&a, talloc);

    try std.testing.expect(tele_ast.equal(result, expected));

    tele_ast.free_tele_ast(expected, talloc);
    tele_ast.free_tele_ast(result, talloc);
}

test "parse map" {
    const parser = try fileToParser("snippets/map.tl", talloc);
    defer parser.deinit();

    const result = try parser.parse_map(parser.*.token_queue, false);

    const e1 = try tele_ast.makeInt(try util.copyString("1", talloc), talloc);
    const e2 = try tele_ast.makeVariable(try util.copyString("a", talloc), talloc);
    const e3 = try tele_ast.makeInt(try util.copyString("2", talloc), talloc);
    const e4 = try tele_ast.makeVariable(try util.copyString("b", talloc), talloc);
    const e5 = try tele_ast.makeInt(try util.copyString("3", talloc), talloc);
    const e6 = try tele_ast.makeVariable(try util.copyString("c", talloc), talloc);
    const a = [_]*TeleAst{ e1, e2, e3, e4, e5, e6 };
    const expected = try tele_ast.makeMap(&a, talloc);

    tele_ast.free_tele_ast(expected, talloc);
    tele_ast.free_tele_ast(result, talloc);
}

test "parse body multi" {
    const parser = try fileToParser("snippets/body_multi.tl", talloc);
    defer parser.deinit();

    const result = try parser.parse_body(parser.*.token_queue);
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

test "parse case expression" {
    const parser = try fileToParser("snippets/match.tl", talloc);
    defer parser.deinit();

    const result = try parser.parseCaseExpression(parser.*.token_queue);
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

test "parse case body single clause" {
    const parser = try fileToParser("snippets/match_body.tl", talloc);
    defer parser.deinit();

    const result = try parser.parseCaseBody(parser.*.token_queue);
    try std.testing.expect(result.items.len == 1);
    try std.testing.expect(result.items[0].ast_type == TeleAstType.case_clause);
    try std.testing.expect(std.mem.eql(u8, result.items[0].body, ""));

    try std.testing.expect(result.items[0].children.?.items.len == 2);
    const c = result.items[0].children.?;

    const expected1 = try tele_ast.makeInt(try util.copyString("1", talloc), talloc);
    const expected2 = try tele_ast.makeInt(try util.copyString("42", talloc), talloc);
    try std.testing.expect(tele_ast.equal(c.items[0], expected1));
    try std.testing.expect(tele_ast.equal(c.items[1], expected2));

    tele_ast.free_tele_ast(expected1, talloc);
    tele_ast.free_tele_ast(expected2, talloc);
    tele_ast.free_tele_ast_list(result, talloc);
}

test "parse case body multi clause" {
    const parser = try fileToParser("snippets/match_body_multi.tl", talloc);
    defer parser.deinit();

    const result = try parser.parseCaseBody(parser.*.token_queue);
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

test "parse case signature" {
    const parser = try fileToParser("snippets/match_signature.tl", talloc);
    defer parser.deinit();

    const result = try parser.parseCaseSignature(parser.*.token_queue);

    const expected = try tele_ast.makeInt(try util.copyString("1", talloc), talloc);
    try std.testing.expect(tele_ast.equal(result, expected));

    tele_ast.free_tele_ast(expected, talloc);
    tele_ast.free_tele_ast(result, talloc);
}

test "parse case clause signature" {
    const parser = try fileToParser("snippets/case_clause_signature.tl", talloc);
    defer parser.deinit();

    var n: usize = 0;
    const result = try parser.parse_case_clause_signature(parser.*.token_queue, &n);

    const expected = try tele_ast.makeInt(try util.copyString("1", talloc), talloc);
    try std.testing.expect(tele_ast.equal(result.items[0], expected));

    tele_ast.free_tele_ast(expected, talloc);
    tele_ast.free_tele_ast_list(result, talloc);
}

test "parse case clause body single line" {
    const parser = try fileToParser("snippets/case_clause_body_single.tl", talloc);
    defer parser.deinit();

    const result = try parser.parse_case_clause_body(parser.token_queue, 0);
    try std.testing.expect(result.items.len == 1);

    const expected = try tele_ast.makeInt(try util.copyString("42", talloc), talloc);
    try std.testing.expect(tele_ast.equal(result.items[0], expected));

    tele_ast.free_tele_ast(expected, talloc);
    tele_ast.free_tele_ast_list(result, talloc);
}

test "parse case clause body multi line" {
    const parser = try fileToParser("snippets/case_clause_body_multi.tl", talloc);
    defer parser.deinit();

    const result = try parser.parse_case_clause_body(parser.*.token_queue, 0);
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

fn extract_record_name(name: []const u8, allocator: std.mem.Allocator) ![]const u8 {
    const buf = try allocator.alloc(u8, name.len - 2);
    var i: usize = 1;
    while (i < name.len - 1) {
        buf[i - 1] = name[i];
        i += 1;
    }

    return buf;
}

fn check_paren_start_peek(token_queue: *TokenQueue) !bool {
    const peek_node = try token_queue.peek();
    return is_paren_start(peek_node.*.body);
}

fn check_operator_peek(token_queue: *TokenQueue) !bool {
    const peek_node = try token_queue.peek();
    return is_operator(peek_node.*.body);
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

fn is_statement_keyword(buf: []const u8) bool {
    if (buf[0] == 'a') {
        return is_attr_keyword(buf);
    }

    if (buf[0] == 'e') {
        return is_export_type_keyword(buf);
    }

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

    if (buf[0] == 'b') {
        return is_behaviour_keyword(buf);
    }

    if (buf[0] == 'i') {
        return is_include_keyword(buf) or is_include_lib_keyword(buf) or is_import_keyword(buf);
    }

    if (buf[0] == 'm') {
        return is_moduledoc_keyword(buf);
    }

    if (buf[0] == 'o') {
        return is_on_load_keyword(buf) or is_opaque_keyword(buf);
    }

    if (buf[0] == 'n') {
        return is_nifs_keyword(buf);
    }

    if (buf[0] == 'd') {
        return is_doc_keyword(buf) or is_define_keyword(buf);
    }

    if (buf[0] == 'c') {
        return is_callback_keyword(buf);
    }

    return false;
}

test "is statement keyword" {
    try std.testing.expect(is_statement_keyword("fun"));
    try std.testing.expect(is_statement_keyword("funp"));
    try std.testing.expect(is_statement_keyword("spec"));
    try std.testing.expect(is_statement_keyword("type"));
    try std.testing.expect(is_statement_keyword("record"));
    try std.testing.expect(is_statement_keyword("behaviour"));
    try std.testing.expect(is_statement_keyword("import"));
    try std.testing.expect(is_statement_keyword("nifs"));
    try std.testing.expect(is_statement_keyword("callback"));
    try std.testing.expect(is_statement_keyword("include"));
    try std.testing.expect(is_statement_keyword("include_lib"));
    try std.testing.expect(is_statement_keyword("doc"));
    try std.testing.expect(is_statement_keyword("moduledoc"));
    try std.testing.expect(is_statement_keyword("define"));
    try std.testing.expect(is_statement_keyword("opaque"));
    try std.testing.expect(is_statement_keyword("export_type"));

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

fn is_opaque_keyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "opaque");
}

fn is_export_type_keyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "export_type");
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

fn is_behaviour_keyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "behaviour");
}

fn is_attr_keyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "attr");
}

fn is_doc_keyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "doc");
}

fn is_moduledoc_keyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "moduledoc");
}

fn is_callback_keyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "callback");
}

fn is_nifs_keyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "nifs");
}

fn is_include_keyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "include");
}

fn is_include_lib_keyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "include_lib");
}

fn is_define_keyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "define");
}

fn is_import_keyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "import");
}

fn is_on_load_keyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "on_load");
}

test "is keywords" {
    try std.testing.expect(is_type_keyword("type"));
    try std.testing.expect(is_spec_keyword("spec"));
    try std.testing.expect(is_fun_keyword("fun"));
    try std.testing.expect(is_funp_keyword("funp"));
    try std.testing.expect(is_record_keyword("record"));
    try std.testing.expect(is_behaviour_keyword("behaviour"));
    try std.testing.expect(is_attr_keyword("attr"));
    try std.testing.expect(is_doc_keyword("doc"));
    try std.testing.expect(is_moduledoc_keyword("moduledoc"));
    try std.testing.expect(is_callback_keyword("callback"));
    try std.testing.expect(is_on_load_keyword("on_load"));
    try std.testing.expect(is_nifs_keyword("nifs"));
    try std.testing.expect(is_include_keyword("include"));
    try std.testing.expect(is_include_lib_keyword("include_lib"));
    try std.testing.expect(is_define_keyword("define"));
    try std.testing.expect(is_import_keyword("import"));
    try std.testing.expect(is_opaque_keyword("opaque"));
    try std.testing.expect(is_export_type_keyword("export_type"));
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

test "is float" {
    try std.testing.expect(is_float("123.0"));
    try std.testing.expect(!is_float("123"));
    try std.testing.expect(!is_float("abc.def"));
}

// Make sure to check for float before because int check is not exhaustive
fn is_int(buf: []const u8) bool {
    return std.ascii.isDigit(buf[0]);
}

test "is int" {
    try std.testing.expect(is_int("123"));

    // TODO
    // try std.testing.expect(!is_int("123.0"));
    // try std.testing.expect(is_int("$f"));
}

fn is_atom(buf: []const u8) bool {
    return buf[0] == '\'';
}

test "is atom" {
    try std.testing.expect(is_atom("'foo"));
    try std.testing.expect(!is_atom("foo"));
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

test "is binary" {
    try std.testing.expect(is_binary("\"foo\""));
    try std.testing.expect(is_binary("<<\"foo\">>"));
    try std.testing.expect(!is_binary("foo"));

    // TODO: More test cases
}

fn is_tuple_start(buf: []const u8) bool {
    return std.mem.eql(u8, "#(", buf);
}

test "is tuple start" {
    try std.testing.expect(is_tuple_start("#("));
    try std.testing.expect(!is_tuple_start(")"));
    try std.testing.expect(!is_tuple_start("{"));
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

test "is fun val" {
    try std.testing.expect(is_fun_val("#foo/2"));
    try std.testing.expect(!is_fun_val("foo/2"));
    try std.testing.expect(!is_fun_val("#foo2"));

    // TODO
    //try std.testing.expect(!is_fun_val("#foo/o2"));
}

fn is_list_start(buf: []const u8) bool {
    return buf[0] == '[';
}

test "is list start" {
    try std.testing.expect(is_list_start("["));
    try std.testing.expect(!is_list_start("]"));
}

fn is_list_end(buf: []const u8) bool {
    return buf[0] == ']';
}

test "is list end" {
    try std.testing.expect(is_list_end("]"));
    try std.testing.expect(!is_list_end("["));
}

fn is_map_start(buf: []const u8) bool {
    return buf[0] == '{';
}

test "is map start" {
    try std.testing.expect(is_map_start("{"));
    try std.testing.expect(!is_map_start("}"));
}

fn is_map_end(buf: []const u8) bool {
    return buf[0] == '}';
}

test "is map end" {
    try std.testing.expect(is_map_end("}"));
    try std.testing.expect(!is_map_end("{"));
}

fn is_paren_start(buf: []const u8) bool {
    return std.mem.eql(u8, "(", buf);
}

test "is paren start" {
    try std.testing.expect(is_paren_start("("));
    try std.testing.expect(!is_paren_start(")"));
}

fn is_paren_end(buf: []const u8) bool {
    return std.mem.eql(u8, ")", buf);
}

test "is paren end" {
    try std.testing.expect(is_paren_end(")"));
    try std.testing.expect(!is_paren_end("("));
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
    try std.testing.expect(!is_record_start("foobar("));
}

fn isFun(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "fun");
}

test "is fun" {
    try std.testing.expect(isFun("fun"));
    try std.testing.expect(!isFun("foo"));
}

fn is_operator(buf: []const u8) bool {
    if (buf.len == 0) {
        return false;
    }

    switch (buf[0]) {
        '+' => {
            if (buf.len == 1) {
                return true;
            } else if (buf.len == 2) {
                return buf[1] == '+';
            } else {
                return false;
            }
        },
        '*', '/', '!', '-', '|' => {
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
        'a' => {
            return std.mem.eql(u8, "and", buf) or std.mem.eql(u8, "andalso", buf);
        },
        'o' => {
            return std.mem.eql(u8, "or", buf) or std.mem.eql(u8, "orelse", buf);
        },
        'n' => {
            return std.mem.eql(u8, "not", buf);
        },
        ':' => {
            return std.mem.eql(u8, "::", buf);
        },
        else => {
            return false;
        },
    }

    return false;
}

test "is operator" {
    try std.testing.expect(is_operator("+"));
    try std.testing.expect(is_operator("*"));
    try std.testing.expect(is_operator("/"));
    try std.testing.expect(is_operator("!"));
    try std.testing.expect(is_operator("-"));
    try std.testing.expect(is_operator("|"));
    try std.testing.expect(is_operator(">"));
    try std.testing.expect(is_operator("<"));
    try std.testing.expect(is_operator("="));
    try std.testing.expect(is_operator("=="));
    try std.testing.expect(is_operator(">="));
    try std.testing.expect(is_operator("<="));
    try std.testing.expect(is_operator("++"));
    try std.testing.expect(is_operator("and"));
    try std.testing.expect(is_operator("or"));
    try std.testing.expect(is_operator("andalso"));
    try std.testing.expect(is_operator("orelse"));
    try std.testing.expect(is_operator("not"));
    try std.testing.expect(is_operator("::"));

    try std.testing.expect(!is_operator("==="));
    try std.testing.expect(!is_operator("+++"));
    try std.testing.expect(!is_operator("+="));
    try std.testing.expect(!is_operator("+-"));
    try std.testing.expect(!is_operator("foobar"));
}

fn is_pipe_operator(buf: []const u8) bool {
    return std.mem.eql(u8, "|", buf);
}

test "is pipe operator" {
    try std.testing.expect(is_pipe_operator("|"));
    try std.testing.expect(!is_pipe_operator("foobar"));
}

fn isCaseKeyword(buf: []const u8) bool {
    return std.mem.eql(u8, "case", buf);
}

test "is case keyword" {
    try std.testing.expect(isCaseKeyword("case"));
    try std.testing.expect(!isCaseKeyword("foobar"));
}

fn isTryKeyword(buf: []const u8) bool {
    return std.mem.eql(u8, "try", buf);
}

test "is try keyword" {
    try std.testing.expect(isTryKeyword("try"));
    try std.testing.expect(!isTryKeyword("foobar"));
}

fn isCatchKeyword(buf: []const u8) bool {
    return std.mem.eql(u8, "catch", buf);
}

test "is catch keyword" {
    try std.testing.expect(isCatchKeyword("catch"));
    try std.testing.expect(!isCatchKeyword("foobar"));
}

fn is_function_definition(buf: []const u8) bool {
    return std.mem.eql(u8, "fun", buf);
}

test "is function definition" {
    try std.testing.expect(is_function_definition("fun"));
    try std.testing.expect(!is_function_definition("funp"));
    try std.testing.expect(!is_function_definition("def"));
}

fn is_priv_function_definition(buf: []const u8) bool {
    return std.mem.eql(u8, "funp", buf);
}

test "is priv function definition" {
    try std.testing.expect(is_priv_function_definition("funp"));
    try std.testing.expect(!is_priv_function_definition("fun"));
    try std.testing.expect(!is_priv_function_definition("defp"));
}

fn is_type_def(buf: []const u8) bool {
    return std.mem.eql(u8, "type", buf);
}

test "is type def" {
    try std.testing.expect(is_type_def("type"));
    try std.testing.expect(!is_type_def("foobar"));
}

fn is_record_def(buf: []const u8) bool {
    return std.mem.eql(u8, "record", buf);
}

test "is record def" {
    try std.testing.expect(is_record_def("record"));
    try std.testing.expect(!is_record_def("foobar"));
}

fn is_spec_def(buf: []const u8) bool {
    return std.mem.eql(u8, "spec", buf);
}

test "is spec def" {
    try std.testing.expect(is_spec_def("spec"));
    try std.testing.expect(!is_spec_def("foobar"));
}

fn is_colon(buf: []const u8) bool {
    return buf[0] == ':';
}

test "is colon" {
    try std.testing.expect(is_colon(":"));
    try std.testing.expect(!is_colon("!"));
}

fn isComma(buf: []const u8) bool {
    return buf[0] == ',';
}

test "is comma" {
    try std.testing.expect(isComma(","));
    try std.testing.expect(!isComma("!"));
}

fn is_equal(buf: []const u8) bool {
    return buf[0] == '=';
}

test "is equal" {
    try std.testing.expect(is_equal("="));
    try std.testing.expect(!is_equal("-"));
}

fn contains_hash(buf: []const u8) bool {
    for (buf) |c| {
        if (c == '#') {
            return true;
        }
    }
    return false;
}

fn containsParenStart(buf: []const u8) bool {
    for (buf) |c| {
        if (c == '(') {
            return true;
        }
    }
    return false;
}

test "contains hash" {
    try std.testing.expect(contains_hash("foo#bar.a"));
    try std.testing.expect(!contains_hash("foobar"));
}
