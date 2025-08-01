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
const tele_error = @import("error.zig");

const ParserError = error{ ParsingFailure, TokenFailure, ExpectedStatement, InvalidStatement };

const ParserMode = enum { none, op };

pub fn parseReader(r: anytype, allocator: std.mem.Allocator) !std.ArrayList(*TeleAst) {
    const token_queue = try tokenizer.readTokens(r, allocator);

    const parser = try Parser.init(token_queue, allocator);

    errdefer allocator.destroy(parser);
    errdefer parser.token_queue.deinit();
    const result = try parser.parse();

    if (!token_queue.empty()) {
        return ParserError.ExpectedStatement;
    }
    parser.token_queue.deinit();
    allocator.destroy(parser);

    return result;
}

pub fn fileToParser(path: []const u8, allocator: std.mem.Allocator) !*Parser {
    const file = try std.fs.cwd().openFile(path, .{ .mode = .read_only });
    defer file.close();
    const token_queue = try tokenizer.readTokens(file.reader(), allocator);

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
        const ast_stack = try self.parseStatements(self.token_queue, false);

        if (!self.token_queue.empty()) {
            return ParserError.ParsingFailure;
        }

        return ast_stack;
    }

    pub fn parseStatements(self: *Self, token_queue: *TokenQueue, allow_exps: bool) !std.ArrayList(*TeleAst) {
        var statements = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(statements, self.allocator);

        while (!token_queue.empty()) {
            const pn = token_queue.peek() catch {
                return ParserError.ParsingFailure;
            };

            if (isStatementKeyword(pn.*.body)) {
                if (isTypeKeyword(pn.*.body)) {
                    const ast = try self.parseTypeDefinition(token_queue, false, true, false);
                    try statements.append(ast);
                } else if (isTypepKeyword(pn.*.body)) {
                    const ast = try self.parseTypeDefinition(token_queue, false, false, false);
                    try statements.append(ast);
                } else if (isOpaqueKeyword(pn.*.body)) {
                    const ast = try self.parseTypeDefinition(token_queue, true, true, false);
                    try statements.append(ast);
                } else if (isNominalKeyword(pn.*.body)) {
                    const ast = try self.parseTypeDefinition(token_queue, false, false, true);
                    try statements.append(ast);
                } else if (isSpecKeyword(pn.*.body)) {
                    const ast = try self.parseSpecDefinition(token_queue);
                    try statements.append(ast);
                } else if (isFunKeyword(pn.*.body)) {
                    const ast = try self.parseFunctionDefinition(token_queue, false);
                    try statements.append(ast);
                } else if (isFunpKeyword(pn.*.body)) {
                    const ast = try self.parseFunctionDefinition(token_queue, true);
                    try statements.append(ast);
                } else if (isRecordKeyword(pn.*.body)) {
                    const ast = try self.parseRecordDefinition(token_queue);
                    try statements.append(ast);
                } else if (isBehaviourKeyword(pn.*.body)) {
                    const ast = try self.parseBehaviour(token_queue);
                    try statements.append(ast);
                } else if (isIncludeLibKeyword(pn.*.body)) {
                    const ast = try self.parseAttribute(token_queue, TeleAstType.include_lib);
                    try statements.append(ast);
                } else if (isNifsKeyword(pn.*.body)) {
                    const ast = try self.parseNifs(token_queue);
                    try statements.append(ast);
                } else if (isDocKeyword(pn.*.body)) {
                    const ast = try self.parseAttribute(token_queue, TeleAstType.doc);
                    try statements.append(ast);
                } else if (isModuledocKeyword(pn.*.body)) {
                    const ast = try self.parseAttribute(token_queue, TeleAstType.moduledoc);
                    try statements.append(ast);
                } else if (isOnLoadKeyword(pn.*.body)) {
                    const ast = try self.parseOnLoad(token_queue);
                    try statements.append(ast);
                } else if (isCallbackKeyword(pn.*.body)) {
                    const ast = try self.parseCallbackDefinition(token_queue);
                    try statements.append(ast);
                } else if (isImportKeyword(pn.*.body)) {
                    const ast = try self.parseImport(token_queue);
                    try statements.append(ast);
                } else if (isDefineKeyword(pn.*.body)) {
                    const ast = try self.parseMacroDefinition(token_queue);
                    try statements.append(ast);
                } else if (isTestKeyword(pn.*.body)) {
                    const ast = try self.parseTest(token_queue);
                    try statements.append(ast);
                } else {
                    tele_error.setErrorMessage(pn.*.line, pn.*.col, tele_error.ErrorType.invalid_statement);
                    return ParserError.InvalidStatement;
                }
            } else {
                if (allow_exps) {
                    const ast = try self.parseExp(token_queue);
                    try statements.append(ast);
                } else {
                    tele_error.setErrorMessage(pn.*.line, pn.*.col, tele_error.ErrorType.invalid_statement);
                    return ParserError.ExpectedStatement;
                }
            }
        }

        return statements;
    }

    fn parseFunctionDefinition(self: *Self, token_queue: *TokenQueue, private: bool) !*TeleAst {

        // Pop off fun/funp
        const n = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };
        if (!isFunKeyword(n.*.body) and !isFunpKeyword(n.*.body)) {
            tele_error.setErrorMessage(n.*.line, n.*.col, tele_error.ErrorType.invalid_statement);
            self.allocator.free(n.*.body);
            self.allocator.destroy(n);
            return ParserError.ParsingFailure;
        }
        const current_col = n.*.col;
        const ast_line = n.*.line;
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        const node = token_queue.pop() catch {
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.missing_name);
            return ParserError.ParsingFailure;
        };

        // Function Definition Name
        const buf = node.*.body;

        if (!util.validateName(buf)) {
            tele_error.setErrorMessage(node.*.line, node.*.col, tele_error.ErrorType.invalid_name);
            self.allocator.free(node.*.body);
            self.allocator.destroy(node);
            return ParserError.ParsingFailure;
        }
        self.allocator.destroy(node);
        errdefer self.allocator.free(buf);

        const children = try self.parseFunctionSigAndBody(token_queue, false, current_col);
        if (children.items.len < 2) {
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.invalid_statement);
            tele_ast.freeTeleAstList(children, self.allocator);
            return ParserError.ParsingFailure;
        }

        // Assemble Function Definition Ast
        const t = try self.allocator.create(TeleAst);
        t.*.body = buf;
        if (private) {
            t.*.ast_type = TeleAstType.function_defp;
        } else {
            t.*.ast_type = TeleAstType.function_def;
        }
        t.*.children = children;
        t.*.col = current_col;
        t.*.line = ast_line;

        return t;
    }

    fn parseMacroDefinition(self: *Self, token_queue: *TokenQueue) !*TeleAst {

        // Pop off define
        const n = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };
        if (!isDefineKeyword(n.*.body)) {
            tele_error.setErrorMessage(n.*.line, n.*.col, tele_error.ErrorType.invalid_statement);
            self.allocator.free(n.*.body);
            self.allocator.destroy(n);
            return ParserError.ParsingFailure;
        }
        const current_col = n.*.col;
        const ast_line = n.*.line;
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        // Function Definition Name
        const node3 = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };
        const buf = node3.*.body;
        const name_l = node3.*.line;
        const name_c = node3.*.col;
        self.allocator.destroy(node3);
        errdefer self.allocator.free(buf);
        if (!util.validateName(buf)) {
            tele_error.setErrorMessage(name_l, name_c, tele_error.ErrorType.invalid_name);
            return ParserError.ParsingFailure;
        }

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(children, self.allocator);

        const pn = try token_queue.peek();
        const pn_l = pn.*.line;
        const pn_c = pn.*.col;
        if (isColon(pn.*.body)) {
            // Pop off colon
            const t = try token_queue.pop();
            self.allocator.free(t.*.body);
            self.allocator.destroy(t);

            const ast = try self.parseExp(token_queue);
            try children.append(ast);
        } else if (isParenStart(pn.*.body)) {
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

                if (isMapStart(node2.*.body) or isBitStringStart(node2.*.body)) {
                    map_count += 1;
                } else if (isMapEnd(node2.*.body) or isBitStringEnd(node2.*.body)) {
                    map_count -= 1;
                }

                if (map_count == 0 and isColon(node2.*.body)) {
                    self.allocator.free(node2.*.body);
                    self.allocator.destroy(node2);
                    break;
                } else {
                    try token_queue2.push(node2.*.body, node2.*.line, node2.col);
                }

                self.allocator.destroy(node2);
            }
            if (token_queue2.empty()) {
                tele_error.setErrorMessage(pn_l, pn_c, tele_error.ErrorType.missing_signature);
                return ParserError.ParsingFailure;
            }

            const ast = try self.parseFunctionSignature(token_queue2, false);
            try children.append(ast);

            // Function Definition Body
            var token_queue3 = try TokenQueue.init(self.allocator);
            errdefer token_queue3.deinit();
            if (token_queue.empty()) {
                tele_error.setErrorMessage(pn_l, pn_c, tele_error.ErrorType.missing_body);
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
                tele_error.setErrorMessage(pn_l, pn_c, tele_error.ErrorType.missing_body);
                return ParserError.ParsingFailure;
            }

            var alist = self.parseBody(token_queue3, ast_line, current_col) catch {
                return ParserError.ParsingFailure;
            };
            errdefer tele_ast.freeTeleAstList(alist, self.allocator);
            if (alist.items.len == 0) {
                tele_error.setErrorMessage(node3.*.line, node3.*.col, tele_error.ErrorType.missing_body);
                return ParserError.ParsingFailure;
            }

            for (alist.items) |a| {
                try children.append(a);
            }
            alist.deinit();
            token_queue3.deinit();
            token_queue2.deinit();
        } else {
            tele_error.setErrorMessage(pn_l, pn_c, tele_error.ErrorType.unexpected_token);
            return ParserError.ParsingFailure;
        }
        if (children.items.len == 0) {
            tele_error.setErrorMessage(pn_l, pn_c, tele_error.ErrorType.missing_body);
            return ParserError.ParsingFailure;
        }

        // Assemble Function Definition Ast
        const t = try self.allocator.create(TeleAst);
        t.*.body = buf;
        t.*.ast_type = TeleAstType.macro_def;
        t.*.children = children;
        t.*.col = current_col;
        t.*.line = ast_line;

        return t;
    }

    fn parseFunctionSignature(self: *Self, token_queue: *TokenQueue, type_exp: bool) !*TeleAst {
        const ini = try token_queue.pop();
        if (!isParenStart(ini.*.body)) {
            tele_error.setErrorMessage(ini.*.line, ini.*.col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(ini.*.body);
            self.allocator.destroy(ini);
            return ParserError.ParsingFailure;
        }
        const current_col = ini.*.col;
        const ast_line = ini.*.line;
        self.allocator.free(ini.*.body);
        self.allocator.destroy(ini);

        var children: ?std.ArrayList(*TeleAst) = null;
        errdefer freeNullChildren(children, self.allocator);

        const pn2 = try token_queue.peek();
        const pn_l = pn2.*.line;
        const pn_c = pn2.*.col;
        if (!isParenEnd(pn2.*.body)) {
            children = std.ArrayList(*TeleAst).init(self.allocator);

            while (!token_queue.empty()) {
                var found_end: bool = false;
                const ast = try self.parseFunctionSignatureParam(token_queue, type_exp, &found_end);
                try children.?.append(ast);
                if (found_end) {
                    break;
                }
            }
            if (children.?.items.len == 0) {
                tele_error.setErrorMessage(pn_l, pn_c, tele_error.ErrorType.invalid_signature_param);
                return ParserError.ParsingFailure;
            }
        } else {
            // Free Paren End Token
            const tn = try token_queue.pop();
            self.allocator.free(tn.*.body);
            self.allocator.destroy(tn);
        }

        // Parse guard clauses
        if (!token_queue.empty()) {
            // Can't have guard clauses if no params
            if (children == null) {
                tele_error.setErrorMessage(pn_l, pn_c, tele_error.ErrorType.invalid_guard_clause);
                return ParserError.ParsingFailure;
            }
            const pn = try token_queue.peek();
            if (isWhenKeyword(pn.*.body)) {
                const gseq = try self.parseGuardSequence(token_queue);
                try children.?.append(gseq);
            } else {
                tele_error.setErrorMessage(pn.*.line, pn.*.col, tele_error.ErrorType.unexpected_token);
                return ParserError.ParsingFailure;
            }
        }

        const tast = try self.allocator.create(TeleAst);
        tast.*.body = "";
        tast.*.ast_type = TeleAstType.function_signature;
        tast.*.children = children;
        tast.*.col = current_col;
        tast.*.line = ast_line;

        return tast;
    }

    fn freeNullChildren(children: ?std.ArrayList(*TeleAst), allocator: std.mem.Allocator) void {
        if (children != null) {
            tele_ast.freeTeleAstList(children.?, allocator);
        }
    }

    fn parseFunctionSignatureParam(self: *Self, token_queue: *TokenQueue, type_exp: bool, found_end: *bool) !*TeleAst {
        var buffer_token_queue = try TokenQueue.init(self.allocator);
        errdefer buffer_token_queue.deinit();

        var n_count: usize = 1;
        while (!token_queue.empty()) {
            const n2 = try token_queue.pop();
            errdefer self.allocator.free(n2.*.body);
            errdefer self.allocator.destroy(n2);

            if (n_count == 1 and (isComma(n2.*.body) or isParenEnd(n2.*.body))) {
                if (isParenEnd(n2.*.body)) {
                    found_end.* = true;
                }
                self.allocator.free(n2.*.body);
                self.allocator.destroy(n2);
                break;
            }

            if (isTupleStart(n2.*.body) or isListStart(n2.*.body) or isMapStart(n2.*.body) or isRecordStart(n2.*.body) or isParenStart(n2.*.body) or isBitStringStart(n2.*.body)) {
                n_count += 1;
            } else if (isParenEnd(n2.*.body) or isListEnd(n2.*.body) or isMapEnd(n2.*.body) or isBitStringEnd(n2.*.body)) {
                n_count -= 1;
            }

            try buffer_token_queue.push(n2.*.body, n2.*.line, n2.*.col);
            self.allocator.destroy(n2);
        }

        if (type_exp) {
            const result = try self.parseTypeExp(buffer_token_queue);
            buffer_token_queue.deinit();
            return result;
        } else {
            const result = try self.parseExp(buffer_token_queue);
            buffer_token_queue.deinit();
            return result;
        }
    }

    fn parseGuardSequence(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        var guards = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(guards, self.allocator);

        // Pop off when keyword
        const n = try token_queue.pop();
        const ast_line = n.*.line;
        const current_col = n.*.col;
        if (!isWhenKeyword(n.*.body)) {
            tele_error.setErrorMessage(n.*.line, n.*.col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(n.*.body);
            self.allocator.destroy(n);
            return ParserError.ParsingFailure;
        }
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        var buffer_token_queue = try TokenQueue.init(self.allocator);
        errdefer buffer_token_queue.deinit();
        defer buffer_token_queue.deinit();

        while (!token_queue.empty()) {
            while (!token_queue.empty()) {
                const n2 = try token_queue.pop();
                if (isWhenKeyword(n2.*.body)) {
                    self.allocator.free(n2.*.body);
                    self.allocator.destroy(n2);
                    break;
                }
                try buffer_token_queue.push(n2.*.body, n2.*.line, n2.*.col);
                self.allocator.destroy(n2);
            }

            const alist = try self.parseBody(buffer_token_queue, ast_line, current_col);
            const g = try self.allocator.create(TeleAst);
            g.*.body = "";
            g.*.ast_type = TeleAstType.guard;
            g.*.children = alist;
            g.*.col = current_col;
            g.*.line = ast_line;
            try guards.append(g);
        }

        if (guards.items.len == 0) {
            tele_error.setErrorMessage(n.*.line, n.*.col, tele_error.ErrorType.invalid_guard_clause);
            return ParserError.ParsingFailure;
        }

        const t = try self.allocator.create(TeleAst);
        t.*.body = "";
        t.*.ast_type = TeleAstType.guard_sequence;
        t.*.children = guards;
        t.*.col = current_col;
        t.*.line = ast_line;
        return t;
    }

    fn parseSpecDefinition(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        // Pop off spec keyword
        const n = try token_queue.pop();
        if (!isSpecKeyword(n.*.body)) {
            tele_error.setErrorMessage(n.*.line, n.*.col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(n.*.body);
            self.allocator.destroy(n);
            return ParserError.ParsingFailure;
        }
        const current_col = n.*.col;
        const ast_line = n.*.line;
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        // Spec Definition Name
        const node3 = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };

        if (!util.validateName(node3.*.body)) {
            tele_error.setErrorMessage(node3.*.line, node3.*.col, tele_error.ErrorType.invalid_name);
            self.allocator.free(node3.*.body);
            self.allocator.destroy(node3);
            return ParserError.ParsingFailure;
        }

        const buf = node3.*.body;
        self.allocator.destroy(node3);
        errdefer self.allocator.free(buf);

        const children = try self.parseFunctionSigAndBody(token_queue, true, current_col);
        if (children.items.len < 2) {
            tele_ast.freeTeleAstList(children, self.allocator);
            tele_error.setErrorMessage(current_col, ast_line, tele_error.ErrorType.invalid_definition);
            return ParserError.ParsingFailure;
        }

        // Assemble Spec Definition Ast
        const t = try self.allocator.create(TeleAst);
        t.*.body = buf;
        t.*.ast_type = TeleAstType.spec_def;
        t.*.children = children;
        t.*.col = current_col;
        t.*.line = ast_line;

        return t;
    }

    fn parseTest(self: *Self, token_queue: *TokenQueue) ParserError!*TeleAst {
        // Pop off test keyword
        const n = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };
        if (!isTestKeyword(n.*.body)) {
            tele_error.setErrorMessage(n.*.line, n.*.col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(n.*.body);
            self.allocator.destroy(n);
            return ParserError.ParsingFailure;
        }
        const current_col = n.*.col;
        const ast_line = n.*.line;
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        var buf: []const u8 = undefined;
        var block: bool = false;

        // Pop off colon or name
        const cn = token_queue.pop() catch {
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.invalid_expression);
            return ParserError.ParsingFailure;
        };

        if (isColon(cn.*.body)) {
            block = true;
            self.allocator.free(cn.*.body);
            self.allocator.destroy(cn);
            buf = util.copyString("", self.allocator) catch {
                return ParserError.ParsingFailure;
            };
        } else {
            buf = cn.*.body;
            if (!util.validateName(buf)) {
                tele_error.setErrorMessage(cn.*.line, cn.*.col, tele_error.ErrorType.invalid_name);
                self.allocator.free(cn.*.body);
                self.allocator.destroy(cn);
                return ParserError.ParsingFailure;
            }
        }
        errdefer self.allocator.free(buf);

        if (!block) {
            // Pop off colon
            const cn2 = token_queue.pop() catch {
                tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.invalid_expression);
                return ParserError.ParsingFailure;
            };

            if (!isColon(cn2.*.body)) {
                tele_error.setErrorMessage(cn2.*.line, cn2.*.col, tele_error.ErrorType.unexpected_token);
                self.allocator.free(cn2.*.body);
                self.allocator.destroy(cn2);
                return ParserError.ParsingFailure;
            }
            self.allocator.free(cn2.*.body);
            self.allocator.destroy(cn2);
            self.allocator.destroy(cn);
        }

        var buffer_token_queue = TokenQueue.init(self.allocator) catch {
            return ParserError.ParsingFailure;
        };
        errdefer buffer_token_queue.deinit();

        while (!token_queue.empty()) {
            const pnode = token_queue.peek() catch {
                return ParserError.InvalidStatement;
            };

            if (pnode.*.col <= current_col) {
                break;
            } else {
                const node = token_queue.pop() catch {
                    return ParserError.ParsingFailure;
                };
                errdefer self.allocator.free(node.*.body);
                errdefer self.allocator.destroy(node);

                buffer_token_queue.push(node.*.body, node.*.line, node.*.col) catch {
                    return ParserError.ParsingFailure;
                };

                self.allocator.destroy(node);
            }
        }

        var children: ?std.ArrayList(*TeleAst) = null;

        if (block) {
            children = self.parseStatements(buffer_token_queue, false) catch {
                return ParserError.ParsingFailure;
            };
        } else {
            children = self.parseBody(buffer_token_queue, ast_line, current_col) catch {
                return ParserError.ParsingFailure;
            };
        }

        buffer_token_queue.deinit();

        const final_ast = self.allocator.create(TeleAst) catch {
            return ParserError.ParsingFailure;
        };
        if (block) {
            final_ast.*.body = "";
            final_ast.*.ast_type = TeleAstType.test_block;
        } else {
            final_ast.*.body = buf;
            final_ast.*.ast_type = TeleAstType.test_unit;
        }
        final_ast.*.children = children;
        final_ast.*.col = current_col;
        final_ast.*.line = ast_line;

        return final_ast;
    }

    fn parseCallbackDefinition(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        // Pop off callback keyword
        const n = try token_queue.pop();
        if (!isCallbackKeyword(n.*.body)) {
            tele_error.setErrorMessage(n.*.line, n.*.col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(n.*.body);
            self.allocator.destroy(n);
            return ParserError.ParsingFailure;
        }
        const current_col = n.*.col;
        const ast_line = n.*.line;
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        // Callback Definition Name
        const node3 = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };

        if (!util.validateName(node3.*.body)) {
            tele_error.setErrorMessage(node3.*.line, node3.*.col, tele_error.ErrorType.invalid_name);
            self.allocator.free(node3.*.body);
            self.allocator.destroy(node3);
            return ParserError.ParsingFailure;
        }

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

            if (isMapStart(node2.*.body) or isBitStringStart(node2.*.body)) {
                map_count += 1;
            } else if (isMapEnd(node2.*.body) or isBitStringEnd(node2.*.body)) {
                map_count -= 1;
            }

            if (map_count == 0 and isColon(node2.*.body)) {
                self.allocator.free(node2.*.body);
                self.allocator.destroy(node2);
                break;
            } else {
                try token_queue2.push(node2.*.body, node2.*.line, node2.col);
            }

            self.allocator.destroy(node2);
        }

        const sig_ast = try self.parseFunctionSignature(token_queue2, true);

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(children, self.allocator);
        try children.append(sig_ast);

        // Callback Definition Body
        var token_queue3 = try TokenQueue.init(self.allocator);
        errdefer token_queue3.deinit();
        if (token_queue.empty()) {
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.missing_body);
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
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.missing_body);
            return ParserError.ParsingFailure;
        }

        const ast = self.parseTypeExp(token_queue3) catch {
            return ParserError.ParsingFailure;
        };
        try children.append(ast);
        if (!token_queue3.empty()) {
            const peek_n = try token_queue3.peek();
            tele_error.setErrorMessage(peek_n.*.line, peek_n.*.col, tele_error.ErrorType.unexpected_token);
            return ParserError.ParsingFailure;
        }

        // Assemble Callback Definition Ast
        const t = try self.allocator.create(TeleAst);
        t.*.body = buf;
        t.*.ast_type = TeleAstType.callback_def;
        t.*.children = children;
        t.*.col = current_col;
        t.*.line = ast_line;

        token_queue2.deinit();
        token_queue3.deinit();

        return t;
    }

    // TODO: change bools to an enum
    fn parseTypeDefinition(self: *Self, token_queue: *TokenQueue, opaque_type: bool, public: bool, nominal: bool) !*TeleAst {
        // Free type/opaque keyword
        const n = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };
        if (!(isTypeKeyword(n.*.body) or isOpaqueKeyword(n.*.body) or isTypepKeyword(n.*.body) or isNominalKeyword(n.*.body))) {
            tele_error.setErrorMessage(n.*.line, n.*.col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(n.*.body);
            self.allocator.destroy(n);
            return ParserError.ParsingFailure;
        }
        const current_col = n.*.col;
        const ast_line = n.*.line;
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        const node3 = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };

        if (!util.validateName(node3.*.body)) {
            tele_error.setErrorMessage(node3.*.line, node3.*.col, tele_error.ErrorType.invalid_name);
            self.allocator.free(node3.*.body);
            self.allocator.destroy(node3);
            return ParserError.ParsingFailure;
        }

        // type Definition Name
        const buf = node3.*.body;
        const name_line = node3.*.line;
        const name_col = node3.*.col;
        self.allocator.destroy(node3);

        const pn = token_queue.peek() catch {
            self.allocator.free(buf);
            return ParserError.ParsingFailure;
        };

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(children, self.allocator);

        if (isColon(pn.*.body)) {
            const t = try self.allocator.create(TeleAst);
            t.*.body = buf;
            t.*.ast_type = TeleAstType.function_call;
            t.*.children = null;
            t.*.col = pn.*.col;
            t.*.line = pn.*.line;
            try children.append(t);
        } else {

            // TODO: Make separate parser for this instead of function call
            const tfcall = self.parseFunctionCall(token_queue, buf, true, name_line, name_col) catch {
                return ParserError.ParsingFailure;
            };

            try children.append(tfcall);
        }

        // Skip colon
        const cn = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };

        if (!isColon(cn.*.body)) {
            tele_error.setErrorMessage(cn.*.line, cn.*.col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(cn.*.body);
            self.allocator.destroy(cn);
            return ParserError.ParsingFailure;
        }
        self.allocator.free(cn.*.body);
        self.allocator.destroy(cn);

        // type Definition Body
        var token_queue3 = try TokenQueue.init(self.allocator);
        errdefer token_queue3.deinit();
        if (token_queue.empty()) {
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.missing_body);
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
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.missing_body);
            return ParserError.ParsingFailure;
        }

        const ast = try self.parseTypeExp(token_queue3);
        try children.append(ast);
        if (!token_queue3.empty()) {
            const peek_n = try token_queue3.peek();
            tele_error.setErrorMessage(peek_n.*.line, peek_n.*.col, tele_error.ErrorType.unexpected_token);
            return ParserError.ParsingFailure;
        }

        // Assemble Type Definition Ast
        const t = try self.allocator.create(TeleAst);
        t.*.body = "";
        if (opaque_type) {
            t.*.ast_type = TeleAstType.opaque_type_def;
        } else if (public) {
            t.*.ast_type = TeleAstType.type_def;
        } else if (nominal) {
            t.*.ast_type = TeleAstType.nominal_type_def;
        } else {
            t.*.ast_type = TeleAstType.typep_def;
        }
        t.*.children = children;
        t.*.col = current_col;
        t.*.line = ast_line;

        token_queue3.deinit();

        return t;
    }

    fn parseRecordDefinition(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        const n = try token_queue.pop();
        if (!isRecordKeyword(n.*.body)) {
            tele_error.setErrorMessage(n.*.line, n.*.col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(n.*.body);
            self.allocator.destroy(n);
            return ParserError.ParsingFailure;
        }
        const current_col = n.*.col;
        const ast_line = n.*.line;
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        // Record Definition Name
        const node3 = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };

        if (!util.validateName(node3.*.body)) {
            tele_error.setErrorMessage(node3.*.line, node3.*.col, tele_error.ErrorType.invalid_name);
            self.allocator.free(node3.*.body);
            self.allocator.destroy(node3);
            return ParserError.ParsingFailure;
        }

        const buf = node3.*.body;
        self.allocator.destroy(node3);
        errdefer self.allocator.free(buf);

        // Skip colon
        const cn = try token_queue.pop();
        if (!isColon(cn.*.body)) {
            tele_error.setErrorMessage(cn.*.line, cn.*.col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(cn.*.body);
            self.allocator.destroy(cn);
            return ParserError.ParsingFailure;
        }
        self.allocator.free(cn.*.body);
        self.allocator.destroy(cn);

        // Record Definition Body
        var token_queue3 = try TokenQueue.init(self.allocator);
        errdefer token_queue3.deinit();
        if (token_queue.empty()) {
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.missing_body);
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
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.missing_body);
            return ParserError.ParsingFailure;
        }

        // Skip tuple start
        const cn2 = try token_queue3.pop();
        if (!isTupleStart(cn2.*.body)) {
            tele_error.setErrorMessage(cn2.*.line, cn2.*.col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(cn2.*.body);
            self.allocator.destroy(cn2);
            return ParserError.ParsingFailure;
        }
        self.allocator.free(cn2.*.body);
        self.allocator.destroy(cn2);

        var children: ?std.ArrayList(*TeleAst) = null;

        const pn = try token_queue3.peek();
        if (!isParenEnd(pn.*.body)) {
            children = std.ArrayList(*TeleAst).init(self.allocator);

            while (!token_queue3.empty()) {
                var found_end: bool = false;
                const ast = try self.parseRecordField(token_queue3, true, &found_end, ast_line, current_col);
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
            const peek_n = try token_queue3.peek();
            tele_error.setErrorMessage(peek_n.*.line, peek_n.*.col, tele_error.ErrorType.unexpected_token);
            return ParserError.ParsingFailure;
        }

        // Assemble Record Definition Ast
        const t = try self.allocator.create(TeleAst);
        t.*.body = buf;
        t.*.ast_type = TeleAstType.record_def;
        t.*.children = children;
        t.*.col = current_col;
        t.*.line = ast_line;

        token_queue3.deinit();

        return t;
    }

    fn parseRecordField(self: *Self, token_queue: *TokenQueue, type_exp: bool, found_end: *bool, line: usize, column: usize) !*TeleAst {
        var buffer_token_queue = try TokenQueue.init(self.allocator);
        defer buffer_token_queue.deinit();

        var n_count: usize = 0;
        while (!token_queue.empty()) {
            const n = try token_queue.pop();

            if (n_count == 0 and isParenEnd(n.*.body)) {
                self.allocator.free(n.*.body);
                self.allocator.destroy(n);
                found_end.* = true;
                break;
            } else if (n_count == 0 and isComma(n.*.body)) {
                self.allocator.free(n.*.body);
                self.allocator.destroy(n);
                break;
            }

            if (isParenStart(n.*.body) or isRecordStart(n.*.body) or isMapStart(n.*.body) or isTupleStart(n.*.body) or isListStart(n.*.body) or isBitStringStart(n.*.body)) {
                n_count += 1;
            } else if (isParenEnd(n.*.body) or isMapEnd(n.*.body) or isListEnd(n.*.body) or isBitStringEnd(n.*.body)) {
                n_count -= 1;
            }

            try buffer_token_queue.push(n.*.body, n.*.col, n.*.line);
            self.allocator.destroy(n);
        }

        if (buffer_token_queue.empty()) {
            tele_error.setErrorMessage(line, column, tele_error.ErrorType.invalid_expression);
            return ParserError.ParsingFailure;
        }

        // Parse Field Key
        const key = try buffer_token_queue.pop();
        const name = key.*.body;
        const current_col = key.*.col;
        const ast_line = key.*.line;
        errdefer self.allocator.free(name);
        self.allocator.destroy(key);

        if (!util.validateName(name)) {
            tele_error.setErrorMessage(key.*.line, key.*.col, tele_error.ErrorType.invalid_name);
            return ParserError.ParsingFailure;
        }

        var children: ?std.ArrayList(*TeleAst) = std.ArrayList(*TeleAst).init(self.allocator);
        if (!buffer_token_queue.empty()) {
            const op = try buffer_token_queue.pop();
            errdefer self.allocator.destroy(op);
            errdefer self.allocator.free(op.*.body);
            if (isEqual(op.*.body)) {
                const ast = try self.parseRecordFieldValue(buffer_token_queue);
                try children.?.append(ast);
            } else if (isColon(op.*.body) and type_exp) {
                const ast = try self.parseRecordFieldType(buffer_token_queue);
                try children.?.append(ast);
            } else {
                tele_error.setErrorMessage(op.*.line, op.*.col, tele_error.ErrorType.unexpected_token);
                return ParserError.ParsingFailure;
            }

            self.allocator.free(op.*.body);
            self.allocator.destroy(op);
        }

        // Check if buffer_token_queue is not empty second time for type expressions explicitly
        if (!buffer_token_queue.empty() and type_exp) {
            if (children == null) {
                tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.invalid_field);
                return ParserError.ParsingFailure;
            }

            const op = try buffer_token_queue.pop();
            errdefer self.allocator.free(op.*.body);
            errdefer self.allocator.destroy(op);
            defer self.allocator.destroy(op);
            defer self.allocator.free(op.*.body);

            if (isColon(op.*.body) and type_exp) {
                const ast = try self.parseRecordFieldType(buffer_token_queue);
                try children.?.append(ast);
            } else {
                tele_error.setErrorMessage(op.*.line, op.*.col, tele_error.ErrorType.unexpected_token);
                return ParserError.ParsingFailure;
            }
        }

        const t = try self.allocator.create(TeleAst);
        t.*.body = name;
        t.*.children = children;
        t.*.ast_type = TeleAstType.record_field;
        t.*.col = current_col;
        t.*.line = ast_line;

        return t;
    }

    fn parseRecordFieldValue(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        const ast = try self.parseExp(token_queue);
        const t = try self.allocator.create(TeleAst);
        t.*.body = "";
        t.*.children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(t.*.children.?, self.allocator);
        try t.*.children.?.append(ast);
        t.*.ast_type = TeleAstType.record_field_value;
        t.*.col = ast.*.col;
        t.*.line = ast.*.line;

        return t;
    }

    fn parseRecordFieldType(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        const ast = try self.parseTypeExp(token_queue);
        const t = try self.allocator.create(TeleAst);
        t.*.body = "";
        t.*.children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(t.*.children.?, self.allocator);
        try t.*.children.?.append(ast);
        t.*.ast_type = TeleAstType.record_field_type;
        t.*.col = ast.*.col;
        t.*.line = ast.*.line;
        return t;
    }

    fn parseExp(self: *Self, token_queue: *TokenQueue) ParserError!*TeleAst {
        const pn = token_queue.peek() catch {
            return ParserError.ParsingFailure;
        };

        var ast: ?*TeleAst = null;

        if (isFloat(pn.*.body)) {
            ast = self.parseFloat(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isInt(pn.*.body)) {
            ast = self.parseInt(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isAtom(pn.*.body)) {
            ast = self.parseAtom(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isString(pn.*.body)) {
            ast = self.parseString(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isBinary(pn.*.body)) {
            ast = self.parseBinary(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isFun(pn.*.body)) {
            ast = self.parseAnonymousFunction(token_queue, false) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isOperator(pn.*.body)) {
            ast = self.parseOperator(token_queue, false, null) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isParenStart(pn.*.body)) {
            ast = self.parseParenExp(token_queue, false) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isTupleStart(pn.*.body)) {
            ast = self.parseTuple(token_queue, false) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isRecordStart(pn.*.body)) {
            const buf = util.copyString("", self.allocator) catch {
                return ParserError.ParsingFailure;
            };
            // Passing in 0 0 for parseRecord because not used
            ast = self.parseRecord(token_queue, false, buf, false, 0, 0) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isListStart(pn.*.body)) {
            ast = self.parseList(token_queue, false) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isMapStart(pn.*.body)) {
            ast = self.parseMap(token_queue, false) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isCaseKeyword(pn.*.body)) {
            ast = self.parseCaseExpression(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isReceiveKeyword(pn.*.body)) {
            ast = self.parseReceiveExpression(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isMaybeKeyword(pn.*.body)) {
            ast = self.parseMaybeExpression(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isTryKeyword(pn.*.body)) {
            ast = self.parseTryCatch(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else {
            const n = token_queue.pop() catch {
                return ParserError.ParsingFailure;
            };
            errdefer self.allocator.destroy(n);
            errdefer self.allocator.free(n.*.body);

            if (!token_queue.empty()) {
                // Check if next token is a paren_start
                const res = checkParenStartPeek(token_queue) catch {
                    return ParserError.ParsingFailure;
                };
                if (res) {
                    const buf = util.copyString(n.*.body, self.allocator) catch {
                        return ParserError.ParsingFailure;
                    };
                    if (util.containsHash(n.*.body)) {
                        ast = self.parseRecord(token_queue, true, buf, false, n.*.line, n.*.col) catch { // Is a record variable
                            return ParserError.ParsingFailure;
                        };
                    } else { // Is a function call
                        ast = self.parseFunctionCall(token_queue, buf, false, n.*.line, n.*.col) catch {
                            return ParserError.ParsingFailure;
                        };
                    }
                } else {
                    const buf = util.copyString(n.*.body, self.allocator) catch {
                        return ParserError.ParsingFailure;
                    };
                    ast = self.parseVariable(buf, false, n.*.line, n.*.col) catch {
                        self.allocator.free(buf);
                        return ParserError.ParsingFailure;
                    };
                }
            } else {
                const buf = util.copyString(n.*.body, self.allocator) catch {
                    return ParserError.ParsingFailure;
                };
                ast = self.parseVariable(buf, false, n.*.line, n.*.col) catch {
                    self.allocator.free(buf);
                    return ParserError.ParsingFailure;
                };
            }

            self.allocator.free(n.*.body);
            self.allocator.destroy(n);
        }

        if (!token_queue.empty()) {
            const res2 = checkOperatorPeek(token_queue) catch {
                return ParserError.ParsingFailure;
            };
            if (res2) {
                ast = self.parseOperator(token_queue, false, ast) catch {
                    return ParserError.ParsingFailure;
                };
            }
        }

        return ast.?;
    }

    fn parseTypeExp(self: *Self, token_queue: *TokenQueue) ParserError!*TeleAst {
        const pn = token_queue.peek() catch {
            return ParserError.ParsingFailure;
        };

        var ast: ?*TeleAst = null;

        if (isFloat(pn.*.body)) {
            ast = self.parseFloat(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isInt(pn.*.body)) {
            ast = self.parseInt(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isAtom(pn.*.body)) {
            ast = self.parseAtom(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isBinary(pn.*.body)) {
            ast = self.parseBinary(token_queue) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isFun(pn.*.body)) {
            ast = self.parseAnonymousFunction(token_queue, true) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isOperator(pn.*.body)) {
            ast = self.parseOperator(token_queue, true, null) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isParenStart(pn.*.body)) {
            ast = self.parseParenExp(token_queue, true) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isTupleStart(pn.*.body)) {
            ast = self.parseTuple(token_queue, true) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isRecordStart(pn.*.body)) {
            const buf = util.copyString("", self.allocator) catch {
                return ParserError.ParsingFailure;
            };
            // Passing in 0, 0 because values not used
            ast = self.parseRecord(token_queue, false, buf, true, 0, 0) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isListStart(pn.*.body)) {
            ast = self.parseList(token_queue, true) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isMapStart(pn.*.body)) {
            ast = self.parseMap(token_queue, true) catch {
                return ParserError.ParsingFailure;
            };
        } else if (isCaseKeyword(pn.*.body)) {
            return ParserError.ParsingFailure;
        } else if (isTryKeyword(pn.*.body)) {
            return ParserError.ParsingFailure;
        } else if (isReceiveKeyword(pn.*.body)) {
            return ParserError.ParsingFailure;
        } else if (isMaybeKeyword(pn.*.body)) {
            return ParserError.ParsingFailure;
        } else if (isElseKeyword(pn.*.body)) {
            return ParserError.ParsingFailure;
        } else {
            const n = token_queue.pop() catch {
                return ParserError.ParsingFailure;
            };
            const ast_line = n.*.line;
            const current_col = n.*.col;
            defer self.allocator.destroy(n);
            defer self.allocator.free(n.*.body);

            if (!token_queue.empty()) {
                // Check if next token is a paren_start
                const res = checkParenStartPeek(token_queue) catch {
                    tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.unexpected_token);
                    return ParserError.ParsingFailure;
                };
                if (res) {
                    if (util.containsHash(n.*.body)) {
                        tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.unexpected_token);
                        // Record literal syntax not allowed in type expression
                        return ParserError.ParsingFailure;
                    } else { // Is a function call
                        const buf = util.copyString(n.*.body, self.allocator) catch {
                            return ParserError.ParsingFailure;
                        };
                        ast = self.parseFunctionCall(token_queue, buf, true, n.*.line, n.*.col) catch {
                            return ParserError.ParsingFailure;
                        };
                    }
                } else {
                    const buf = util.copyString(n.*.body, self.allocator) catch {
                        return ParserError.ParsingFailure;
                    };
                    ast = self.parseVariable(buf, true, n.*.line, n.*.col) catch {
                        self.allocator.free(buf);
                        return ParserError.ParsingFailure;
                    };
                }
            } else {
                const buf = util.copyString(n.*.body, self.allocator) catch {
                    return ParserError.ParsingFailure;
                };
                ast = self.parseVariable(buf, true, n.*.line, n.*.col) catch {
                    self.allocator.free(buf);
                    return ParserError.ParsingFailure;
                };
            }
        }

        if (!token_queue.empty()) {
            const res2 = checkOperatorPeek(token_queue) catch {
                return ParserError.ParsingFailure;
            };
            if (res2) {
                ast = self.parseOperator(token_queue, true, ast) catch {
                    return ParserError.ParsingFailure;
                };
            }
        }

        return ast.?;
    }

    fn parseFloat(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        const n = try token_queue.peek();
        if (!util.validateFloat(n.*.body)) {
            tele_error.setErrorMessage(n.*.line, n.*.col, tele_error.ErrorType.invalid_float);
            return ParserError.ParsingFailure;
        }
        return try self.parseValue(token_queue, TeleAstType.float);
    }

    fn parseInt(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        const n = try token_queue.peek();
        if (!util.validateInteger(n.*.body)) {
            tele_error.setErrorMessage(n.*.line, n.*.col, tele_error.ErrorType.invalid_integer);
            return ParserError.ParsingFailure;
        }
        return try self.parseValue(token_queue, TeleAstType.int);
    }

    fn parseAtom(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        const n = try token_queue.peek();
        if (!util.validateAtom(n.*.body)) {
            tele_error.setErrorMessage(n.*.line, n.*.col, tele_error.ErrorType.invalid_atom);
            return ParserError.ParsingFailure;
        }
        return try self.parseValue(token_queue, TeleAstType.atom);
    }

    fn parseBinary(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        // Pop off bit string start
        const sn = try token_queue.pop();
        const col = sn.*.col;
        const line = sn.*.line;
        self.allocator.free(sn.*.body);
        self.allocator.destroy(sn);

        var buffer_token_queue = try TokenQueue.init(self.allocator);
        errdefer buffer_token_queue.deinit();
        while (!token_queue.empty()) {
            const n2 = try token_queue.pop();
            errdefer self.allocator.free(n2.*.body);
            errdefer self.allocator.destroy(n2);

            if (isBitStringEnd(n2.*.body)) {
                self.allocator.free(n2.*.body);
                self.allocator.destroy(n2);
                break;
            } else {
                try buffer_token_queue.push(n2.*.body, n2.*.col, n2.*.line);
                self.allocator.destroy(n2);
            }
        }

        var buffer_token_queue2 = try TokenQueue.init(self.allocator);
        errdefer buffer_token_queue2.deinit();

        // Parse bit string pieces
        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(children, self.allocator);
        while (!buffer_token_queue.empty()) {
            const n2 = try buffer_token_queue.pop();
            if (isComma(n2.*.body)) {
                self.allocator.free(n2.*.body);
                self.allocator.destroy(n2);
                const ast = try self.parseBinaryElement(buffer_token_queue2);
                try children.append(ast);
            } else {
                try buffer_token_queue2.push(n2.*.body, n2.*.col, n2.*.line);
                self.allocator.destroy(n2);
            }
        }

        if (!buffer_token_queue2.empty()) {
            const ast = try self.parseBinaryElement(buffer_token_queue2);
            try children.append(ast);
        }

        buffer_token_queue.deinit();
        buffer_token_queue2.deinit();

        const bast = try self.allocator.create(TeleAst);
        bast.*.body = "";
        bast.*.ast_type = TeleAstType.binary;
        bast.*.children = children;
        bast.*.col = col;
        bast.*.line = line;
        return bast;
    }

    fn parseString(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        return try self.parseValue(token_queue, TeleAstType.string);
    }

    fn parseBinaryElement(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        var buffer_token_queue = try TokenQueue.init(self.allocator);
        errdefer buffer_token_queue.deinit();

        var size: bool = false;

        var children = std.ArrayList(*TeleAst).init(self.allocator);

        // support X:4/little-signed-integer-unit:8 syntax for types
        while (!token_queue.empty()) {
            const n = try token_queue.pop();
            if (isSlash(n.*.body)) {
                self.allocator.free(n.*.body);
                self.allocator.destroy(n);

                const c = try self.parseExp(buffer_token_queue);
                try children.append(c);

                const n2 = try token_queue.pop();
                const tast = try self.allocator.create(TeleAst);
                tast.*.body = n2.*.body;
                tast.*.ast_type = TeleAstType.binary_element_type;
                tast.*.children = null;
                tast.*.col = n2.*.col;
                tast.*.line = n2.*.line;
                self.allocator.destroy(n2);

                try children.append(tast);
            } else if (isColon(n.*.body)) {
                self.allocator.free(n.*.body);
                self.allocator.destroy(n);
                if (!buffer_token_queue.empty()) {
                    const c = try self.parseExp(buffer_token_queue);
                    try children.append(c);
                }
                size = true;
            } else {
                try buffer_token_queue.push(n.*.body, n.*.col, n.*.line);
                self.allocator.destroy(n);
            }
        }

        if (!buffer_token_queue.empty()) {
            const child = try self.parseExp(buffer_token_queue);
            if (size) {
                var children2 = std.ArrayList(*TeleAst).init(self.allocator);
                try children2.append(child);
                const sast = try self.allocator.create(TeleAst);
                sast.*.body = "";
                sast.*.ast_type = TeleAstType.binary_element_size;
                sast.*.children = children2;
                sast.*.col = child.*.col;
                sast.*.line = child.*.line;
                try children.append(sast);
            } else {
                try children.append(child);
            }
        }

        buffer_token_queue.deinit();

        const ast = try self.allocator.create(TeleAst);
        ast.*.body = "";
        ast.*.ast_type = TeleAstType.binary_element;
        ast.*.children = children;
        ast.*.col = children.items[0].*.col;
        ast.*.line = children.items[0].*.line;
        return ast;
    }

    fn parseVariable(self: *Self, buf: []const u8, type_exp: bool, line: usize, col: usize) !*TeleAst {
        if (type_exp and buf[0] != '@') {
            const t = try self.allocator.create(TeleAst);
            errdefer self.allocator.destroy(t);
            t.*.body = buf;
            if (buf[0] == '#') {
                t.*.ast_type = TeleAstType.record;
                if (!util.validateFunctionName(buf[1..])) {
                    tele_error.setErrorMessage(line, col, tele_error.ErrorType.invalid_name);
                    return ParserError.ParsingFailure;
                }
            } else {
                t.*.ast_type = TeleAstType.function_call;
                if (!util.validateFunctionName(buf)) {
                    tele_error.setErrorMessage(line, col, tele_error.ErrorType.invalid_name);
                    return ParserError.ParsingFailure;
                }
            }
            t.*.children = null;
            t.*.col = col;
            t.*.line = line;
            return t;
        } else {
            if (buf[0] == '@' or buf[0] == '#' or buf[0] == '?') {
                if (!util.validateName(buf[1..])) {
                    std.debug.print("\nBUF:{s}\n", .{buf});
                    tele_error.setErrorMessage(line, col, tele_error.ErrorType.invalid_name);
                    return ParserError.ParsingFailure;
                }
            } else if (util.findHash(buf) > 0) {
                if (!util.validateRecordVariableName(buf)) {
                    tele_error.setErrorMessage(line, col, tele_error.ErrorType.invalid_name);
                    return ParserError.ParsingFailure;
                }
            } else if (!util.validateVariableName(buf)) {
                tele_error.setErrorMessage(line, col, tele_error.ErrorType.invalid_name);
                return ParserError.ParsingFailure;
            }
            const t = try self.allocator.create(TeleAst);
            t.*.body = buf;
            t.*.ast_type = TeleAstType.variable;
            t.*.children = null;
            t.*.col = col;
            t.*.line = line;
            return t;
        }
    }

    fn parseValue(self: *Self, token_queue: *TokenQueue, ast_type: TeleAstType) !*TeleAst {
        const n = try token_queue.pop();
        const t = try self.allocator.create(TeleAst);
        t.*.body = n.*.body;
        t.*.ast_type = ast_type;
        t.*.children = null;
        t.*.col = n.*.col;
        t.*.line = n.*.line;
        self.allocator.destroy(n);
        return t;
    }

    fn parseFunVal(self: *Self, token_queue: *TokenQueue, line: usize, col: usize) !*TeleAst {
        const name = try token_queue.pop();
        if (!util.validateName(name.*.body)) {
            tele_error.setErrorMessage(name.*.line, name.*.col, tele_error.ErrorType.invalid_name);
            self.allocator.free(name.*.body);
            self.allocator.destroy(name);
            return ParserError.ParsingFailure;
        }
        const div = try token_queue.pop();
        if (div.*.body[0] != '/') {
            tele_error.setErrorMessage(div.*.line, div.*.col, tele_error.ErrorType.unexpected_token);
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
        t.*.col = col;
        t.*.line = line;
        return t;
    }

    fn parseOperator(self: *Self, token_queue: *TokenQueue, type_exp: bool, arg: ?*TeleAst) !*TeleAst {
        // Pop off operator
        const node = token_queue.pop() catch {
            if (arg != null) {
                tele_ast.freeTeleAst(arg.?, self.allocator);
            }
            return ParserError.ParsingFailure;
        };
        var current_col = node.*.col;
        var ast_line = node.*.line;
        errdefer self.allocator.destroy(node);

        // TODO: Allow for negative numbers
        if (type_exp and !isPipeOperator(node.*.body)) {
            if (arg != null) {
                tele_ast.freeTeleAst(arg.?, self.allocator);
            }
            tele_error.setErrorMessage(node.*.line, node.*.col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(node.*.body);
            return ParserError.ParsingFailure;
        }

        var ast: ?*TeleAst = null;
        if (type_exp) {
            ast = self.parseTypeExp(token_queue) catch {
                if (arg != null) {
                    tele_ast.freeTeleAst(arg.?, self.allocator);
                }
                self.allocator.free(node.*.body);
                return ParserError.ParsingFailure;
            };
        } else {
            ast = self.parseExp(token_queue) catch {
                if (arg != null) {
                    tele_ast.freeTeleAst(arg.?, self.allocator);
                }
                self.allocator.free(node.*.body);
                return ParserError.ParsingFailure;
            };
        }
        errdefer tele_ast.freeTeleAst(ast.?, self.allocator);

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(children, self.allocator);
        if (arg != null) {
            current_col = arg.?.*.col;
            ast_line = arg.?.line;
            try children.append(arg.?);
        }
        try children.append(ast.?);

        const t = try self.allocator.create(TeleAst);
        t.*.body = node.*.body;
        t.*.ast_type = TeleAstType.op;
        t.*.children = children;
        t.*.col = current_col;
        t.*.line = ast_line;

        self.allocator.destroy(node);

        return t;
    }

    fn parseParenExp(self: *Self, token_queue: *TokenQueue, type_exp: bool) !*TeleAst {
        // Pop off paren start
        const n = token_queue.pop() catch {
            return ParserError.ParsingFailure;
        };
        if (!isParenStart(n.*.body)) {
            tele_error.setErrorMessage(n.*.line, n.*.col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(n.*.body);
            self.allocator.destroy(n);
            return ParserError.ParsingFailure;
        }
        const col = n.*.col;
        const line = n.*.line;
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);
        var children: ?std.ArrayList(*TeleAst) = null;

        const pn2 = try token_queue.peek();

        if (!isParenEnd(pn2.*.body)) {
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
                    if (isParenEnd(n2.*.body)) {
                        self.allocator.free(n2.*.body);
                        self.allocator.destroy(n2);
                        break;
                    }
                }

                if (isParenStart(n2.*.body)) {
                    n_count += 1;
                } else if (isParenEnd(n2.*.body)) {
                    n_count -= 1;
                }

                try buffer_token_queue.push(n2.*.body, n2.*.line, n2.*.col);
                self.allocator.destroy(n2);
            }

            while (!buffer_token_queue.empty()) {
                const ast = try self.parseParenExpElement(buffer_token_queue, type_exp);
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
        tast.*.line = line;

        return tast;
    }

    fn parseParenExpElement(self: *Self, token_queue: *TokenQueue, type_exp: bool) !*TeleAst {
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
                if (isComma(n2.*.body) or isParenEnd(n2.*.body)) {
                    self.allocator.free(n2.*.body);
                    self.allocator.destroy(n2);
                    break;
                }
            }

            if (isParenStart(n2.*.body) or isTupleStart(n2.*.body) or isListStart(n2.*.body) or isMapStart(n2.*.body) or isRecordStart(n2.*.body) or isBitStringStart(n2.*.body)) {
                n_count += 1;
            } else if (isParenEnd(n2.*.body) or isListEnd(n2.*.body) or isMapEnd(n2.*.body) or isBitStringEnd(n2.*.body)) {
                n_count -= 1;
            }

            try buffer_token_queue.push(n2.*.body, n2.*.line, n2.*.col);
            self.allocator.destroy(n2);
        }

        if (type_exp) {
            return try self.parseTypeExp(buffer_token_queue);
        } else {
            return try self.parseExp(buffer_token_queue);
        }
    }

    fn parseTuple(self: *Self, token_queue: *TokenQueue, type_exp: bool) !*TeleAst {
        //Pop off #(
        const n = try token_queue.pop();
        const current_col = n.*.col;
        const ast_line = n.*.line;
        if (!isTupleStart(n.*.body)) {
            self.allocator.free(n.*.body);
            self.allocator.destroy(n);
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.unexpected_token);
            return ParserError.ParsingFailure;
        }
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

                if (isListStart(node2.*.body) or isTupleStart(node2.*.body) or isMapStart(node2.*.body) or isRecordStart(node2.*.body) or isParenStart(node2.*.body) or isBitStringStart(node2.*.body)) {
                    count += 1;
                } else if (isParenEnd(node2.*.body) or isListEnd(node2.*.body) or isMapEnd(node2.*.body) or isBitStringEnd(node2.*.body)) {
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
            if (!token_queue2.empty()) {
                if (type_exp) {
                    ast = self.parseTypeExp(token_queue2) catch |err| {
                        tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.invalid_expression);
                        return err;
                    };
                } else {
                    ast = self.parseExp(token_queue2) catch |err| {
                        tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.invalid_expression);
                        return err;
                    };
                }
                try children.append(ast);
            }
        }

        if (count != 0) {
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.invalid_expression);
            return ParserError.ParsingFailure;
        }

        const t = try self.allocator.create(TeleAst);
        t.*.body = "";
        t.*.ast_type = TeleAstType.tuple;
        t.*.children = children;
        t.*.col = current_col;
        t.*.line = ast_line;
        token_queue2.deinit();

        return t;
    }

    fn parseList(self: *Self, token_queue: *TokenQueue, type_exp: bool) !*TeleAst {
        // Pop off [
        const n = try token_queue.pop();
        const current_col = n.*.col;
        const ast_line = n.*.line;
        if (!isListStart(n.*.body)) {
            tele_error.setErrorMessage(n.*.line, n.*.col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(n.*.body);
            self.allocator.destroy(n);
            return ParserError.ParsingFailure;
        }
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(children, self.allocator);

        if (token_queue.empty()) {
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.invalid_expression);
            return ParserError.ParsingFailure;
        }

        const pn_end = try token_queue.peek();
        if (isListEnd(pn_end.*.body)) {
            const n2 = try token_queue.pop();
            self.allocator.free(n2.*.body);
            self.allocator.destroy(n2);
        } else {
            var token_queue2 = try TokenQueue.init(self.allocator);

            // Expected to have already parsed start of list so count starts at 1
            var count: usize = 1;
            var end_of_list = false;

            while (!token_queue.empty() and !end_of_list) {
                while (!token_queue.empty()) {
                    const node2 = try token_queue.pop();
                    errdefer self.allocator.destroy(node2);

                    if (isListStart(node2.*.body) or isTupleStart(node2.*.body) or isMapStart(node2.*.body) or isRecordStart(node2.*.body) or isParenStart(node2.*.body) or isBitStringStart(node2.*.body)) {
                        count += 1;
                    } else if (isListEnd(node2.*.body) or isParenEnd(node2.*.body) or isMapEnd(node2.*.body) or isBitStringEnd(node2.*.body)) {
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
                var ast_set: bool = false;
                if (type_exp) {
                    ast = self.parseTypeExp(token_queue2) catch |err| {
                        token_queue2.deinit();
                        tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.invalid_expression);
                        return err;
                    };
                    ast_set = true;
                } else {
                    ast = self.parseExp(token_queue2) catch |err| {
                        token_queue2.deinit();
                        tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.invalid_expression);
                        return err;
                    };
                    ast_set = true;
                }

                if (!token_queue2.empty()) {
                    const peek_n = try token_queue2.peek();
                    tele_error.setErrorMessage(peek_n.*.line, peek_n.*.col, tele_error.ErrorType.unexpected_token);
                    token_queue2.deinit();
                    if (ast_set) {
                        tele_ast.freeTeleAst(ast, self.allocator);
                    }
                    return ParserError.ParsingFailure;
                }
                try children.append(ast);
            }

            if (count != 0) {
                tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.invalid_expression);
                token_queue2.deinit();
                return ParserError.ParsingFailure;
            }

            token_queue2.deinit();
        }

        const t = try self.allocator.create(TeleAst);
        t.*.body = "";
        t.*.ast_type = TeleAstType.list;
        t.*.children = children;
        t.*.col = current_col;
        t.*.line = ast_line;

        return t;
    }

    fn parseMap(self: *Self, token_queue: *TokenQueue, type_exp: bool) !*TeleAst {
        // Pop off {
        const n = try token_queue.pop();
        const current_col = n.*.col;
        const ast_line = n.*.line;
        if (!isMapStart(n.*.body)) {
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(n.*.body);
            self.allocator.destroy(n);
            return ParserError.ParsingFailure;
        }
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        const t = try self.allocator.create(TeleAst);
        errdefer tele_ast.freeTeleAst(t, self.allocator);
        t.*.body = "";
        t.*.children = null;

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(children, self.allocator);

        if (token_queue.empty()) {
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.invalid_expression);
            return ParserError.ParsingFailure;
        }

        const pn_end = try token_queue.peek();
        if (isMapEnd(pn_end.*.body)) {
            const n2 = try token_queue.pop();
            self.allocator.free(n2.*.body);
            self.allocator.destroy(n2);
        } else {
            var token_queue2 = try TokenQueue.init(self.allocator);
            var count: usize = 1;
            var end_of_map: bool = false;
            var map_update: bool = false;
            var key_state: bool = true;

            while (!token_queue.empty() and !end_of_map) {
                while (!token_queue.empty()) {
                    const node2 = token_queue.pop() catch {
                        token_queue2.deinit();
                        tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.invalid_expression);
                        return ParserError.ParsingFailure;
                    };
                    errdefer self.allocator.destroy(node2);
                    errdefer self.allocator.free(node2.*.body);

                    if (isMapStart(node2.*.body) or isListStart(node2.*.body) or isTupleStart(node2.*.body) or isRecordStart(node2.*.body) or isParenStart(node2.*.body) or isBitStringStart(node2.*.body)) {
                        count += 1;
                    } else if (isMapEnd(node2.*.body) or isParenEnd(node2.*.body) or isListEnd(node2.*.body) or isBitStringEnd(node2.*.body)) {
                        count -= 1;
                        if (count == 0) {
                            self.allocator.free(node2.*.body);
                            self.allocator.destroy(node2);
                            end_of_map = true;
                            break;
                        }
                    }

                    if (count == 1 and (isComma(node2.*.body) or isColon(node2.*.body))) {
                        if ((key_state and !isColon(node2.*.body)) or (!key_state and !isComma(node2.*.body))) {
                            tele_error.setErrorMessage(node2.*.line, node2.*.col, tele_error.ErrorType.unexpected_token);
                            token_queue2.deinit();
                            return ParserError.ParsingFailure;
                        }

                        key_state = !key_state;

                        self.allocator.free(node2.*.body);
                        self.allocator.destroy(node2);
                        break;
                    } else if (count == 1 and isPipeOperator(node2.*.body)) {
                        if (!key_state) {
                            tele_error.setErrorMessage(node2.*.line, node2.*.col, tele_error.ErrorType.unexpected_token);
                            token_queue2.deinit();
                            return ParserError.ParsingFailure;
                        }
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
                var ast_set: bool = false;
                if (type_exp) {
                    ast = self.parseTypeExp(token_queue2) catch |err| {
                        token_queue2.deinit();
                        tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.invalid_expression);
                        return err;
                    };
                    ast_set = true;
                } else {
                    if (map_update) {
                        const name_node = try token_queue2.pop();
                        t.*.body = name_node.*.body;
                        self.allocator.destroy(name_node);
                    } else {
                        ast = self.parseExp(token_queue2) catch |err| {
                            token_queue2.deinit();
                            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.invalid_expression);
                            return err;
                        };
                        ast_set = true;
                    }
                }
                if (!token_queue2.empty()) {
                    const peek_n = try token_queue2.peek();
                    tele_error.setErrorMessage(peek_n.*.line, peek_n.*.col, tele_error.ErrorType.unexpected_token);
                    token_queue2.deinit();
                    if (ast_set) {
                        tele_ast.freeTeleAst(ast, self.allocator);
                    }
                    return ParserError.ParsingFailure;
                }
                if (map_update) {
                    map_update = false;
                } else {
                    try children.append(ast);
                }
            }

            if (count != 0) {
                tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.invalid_expression);
                token_queue2.deinit();
                return ParserError.ParsingFailure;
            }

            token_queue2.deinit();
        }

        t.*.ast_type = TeleAstType.map;
        t.*.children = children;
        t.*.col = current_col;
        t.*.line = ast_line;

        return t;
    }

    fn parseRecord(self: *Self, token_queue: *TokenQueue, variable: bool, buf: []const u8, type_exp: bool, line: usize, col: usize) !*TeleAst {
        errdefer self.allocator.free(buf);
        var buf2: []const u8 = undefined;
        var current_col = col;
        var ast_line = line;
        if (!variable) {
            const node = try token_queue.pop();
            // TODO: Make a validateRecordName
            // Trim off # at beginning and paren start at end
            if (!util.validateFunctionName(node.*.body[1 .. node.*.body.len - 1])) {
                tele_error.setErrorMessage(node.*.line, node.*.col, tele_error.ErrorType.invalid_name);
                self.allocator.free(node.*.body);
                self.allocator.destroy(node);
                return ParserError.ParsingFailure;
            }
            buf2 = node.*.body;
            current_col = node.*.col;
            ast_line = node.*.line;
            self.allocator.destroy(node);
        } else {
            var idx: usize = 0;
            for (buf) |c| {
                if (c == '#') {
                    break;
                }
                idx += 1;
            }

            if (!util.validateName(buf[0..idx])) {
                tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.invalid_name);
                return ParserError.ParsingFailure;
            }

            if (!util.validateFunctionName(buf[idx + 1 ..])) {
                tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.invalid_name);
                return ParserError.ParsingFailure;
            }
            buf2 = buf;
            const node = try token_queue.pop();
            current_col = node.*.col;
            ast_line = node.*.line;
            self.allocator.free(node.*.body);
            self.allocator.destroy(node);
        }
        errdefer self.allocator.free(buf2);

        var buffer_token_queue = try TokenQueue.init(self.allocator);
        errdefer buffer_token_queue.deinit();

        var count: usize = 0;

        while (!token_queue.empty()) {
            const node2 = try token_queue.pop();
            errdefer self.allocator.free(node2.*.body);
            errdefer self.allocator.destroy(node2);

            if (count == 0 and isParenEnd(node2.*.body)) {
                try buffer_token_queue.push(node2.*.body, node2.*.col, node2.*.line);
                self.allocator.destroy(node2);
                break;
            }

            if (isTupleStart(node2.*.body) or isParenStart(node2.*.body) or isRecordStart(node2.*.body) or isListStart(node2.*.body) or isMapStart(node2.*.body) or isBitStringStart(node2.*.body)) {
                count += 1;
            } else if (isParenEnd(node2.*.body) or isListEnd(node2.*.body) or isMapEnd(node2.*.body) or isBitStringEnd(node2.*.body)) {
                count -= 1;
            }
            try buffer_token_queue.push(node2.*.body, node2.*.col, node2.*.line);

            self.allocator.destroy(node2);
        }

        if (buffer_token_queue.empty()) {
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.missing_body);
            return ParserError.ParsingFailure;
        }

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(children, self.allocator);

        const pn = try buffer_token_queue.peek();
        if (!isParenEnd(pn.*.body)) {
            while (!buffer_token_queue.empty()) {
                var found_end: bool = false;
                const ast = try self.parseRecordField(buffer_token_queue, type_exp, &found_end, ast_line, current_col);
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
            const peek_n = try buffer_token_queue.peek();
            tele_error.setErrorMessage(peek_n.*.line, peek_n.*.col, tele_error.ErrorType.unexpected_token);
            return ParserError.ParsingFailure;
        }

        buffer_token_queue.deinit();
        const t = try self.allocator.create(TeleAst);
        if (variable) {
            t.*.body = buf2;
        } else {
            t.*.body = try extractRecordName(buf2, self.allocator);
            self.allocator.free(buf2);
        }
        t.*.ast_type = TeleAstType.record;
        t.*.children = children;
        t.*.col = current_col;
        t.*.line = ast_line;
        return t;
    }

    fn parseBody(self: *Self, token_queue: *TokenQueue, line: usize, col: usize) !std.ArrayList(*TeleAst) {
        var alist = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(alist, self.allocator);

        while (!token_queue.empty()) {
            const ast = try self.parseExp(token_queue);
            try alist.append(ast);
        }

        if (alist.items.len == 0) {
            tele_error.setErrorMessage(line, col, tele_error.ErrorType.missing_body);
            return ParserError.ParsingFailure;
        }
        return alist;
    }

    fn parseAnonymousFunction(self: *Self, token_queue: *TokenQueue, type_exp: bool) !*TeleAst {
        // Pop off fun
        const temp = try token_queue.pop();
        const current_col = temp.*.col;
        const ast_line = temp.*.line;
        if (!isFunKeyword(temp.*.body)) {
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(temp.*.body);
            self.allocator.destroy(temp);
            return ParserError.ParsingFailure;
        }
        self.allocator.free(temp.*.body);
        self.allocator.destroy(temp);

        const pn = token_queue.peek() catch {
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.invalid_expression);
            return ParserError.ParsingFailure;
        };

        if (!isParenStart(pn.*.body)) {
            const t = try self.parseFunVal(token_queue, ast_line, current_col);
            return t;
        } else {
            const children = try self.parseFunctionSigAndBody(token_queue, type_exp, current_col);

            // Assemble Anonymous Function Ast
            const t = try self.allocator.create(TeleAst);
            t.*.body = "";
            t.*.ast_type = TeleAstType.anonymous_function;
            t.*.children = children;
            t.*.col = current_col;
            t.*.line = ast_line;

            return t;
        }
    }

    fn parseFunctionSigAndBody(self: *Self, token_queue: *TokenQueue, type_exp: bool, current_col: usize) !std.ArrayList(*TeleAst) {
        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(children, self.allocator);

        var function_end: bool = false;
        while (!token_queue.empty()) {
            var token_queue_section = try self.collectFunctionSection(token_queue, current_col, &function_end);
            errdefer token_queue_section.deinit();

            // Function Signature
            var token_queue2 = try TokenQueue.init(self.allocator);
            // Gather tokens for function signature

            var map_count: usize = 0;
            while (!token_queue_section.empty()) {
                const node2 = try token_queue_section.pop();
                errdefer self.allocator.free(node2.*.body);
                errdefer self.allocator.destroy(node2);

                if (isMapStart(node2.*.body) or isBitStringStart(node2.*.body)) {
                    map_count += 1;
                } else if (isMapEnd(node2.*.body) or isBitStringEnd(node2.*.body)) {
                    map_count -= 1;
                }

                if (map_count == 0 and isColon(node2.*.body)) {
                    self.allocator.free(node2.*.body);
                    self.allocator.destroy(node2);
                    break;
                } else {
                    try token_queue2.push(node2.*.body, node2.*.line, node2.col);
                }

                self.allocator.destroy(node2);
            }

            const ast = self.parseFunctionSignature(token_queue2, type_exp) catch |err| {
                token_queue2.deinit();
                return err;
            };
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
                    tele_error.setErrorMessage(ast.*.line, current_col, tele_error.ErrorType.missing_body);
                    return ParserError.ParsingFailure;
                }

                if (type_exp) {
                    const a = self.parseTypeExp(token_queue3) catch {
                        return ParserError.ParsingFailure;
                    };
                    try children.append(a);
                } else {
                    var alist = self.parseBody(token_queue3, ast.*.line, ast.*.col) catch {
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
        var signature_queue = try self.collectFunctionSignatureSection(token_queue);
        errdefer signature_queue.deinit();

        var body_queue = try self.collectFunctionBodySection(token_queue, current_col, function_end);
        errdefer body_queue.deinit();

        var buffer_token_queue = try TokenQueue.init(self.allocator);
        errdefer buffer_token_queue.deinit();

        // Reverse order for signature
        while (!signature_queue.empty()) {
            const n = try signature_queue.pop();
            try buffer_token_queue.push(n.*.body, n.*.line, n.*.col);
            self.allocator.destroy(n);
        }

        // Reverse order for body
        while (!body_queue.empty()) {
            const n = try body_queue.pop();
            try buffer_token_queue.push(n.*.body, n.*.line, n.*.col);
            self.allocator.destroy(n);
        }

        signature_queue.deinit();
        body_queue.deinit();
        return buffer_token_queue;
    }

    fn collectFunctionSignatureSection(self: *Self, token_queue: *TokenQueue) !*TokenQueue {
        var buffer_token_queue = try TokenQueue.init(self.allocator);

        var paren_count: usize = 0;
        var end_of_signature = false;

        while (!end_of_signature and !token_queue.empty()) {
            const peek_node = try token_queue.peek();

            if (isParenStart(peek_node.*.body) or isTupleStart(peek_node.*.body) or isRecordStart(peek_node.*.body) or containsParenStart(peek_node.*.body)) {
                paren_count += 1;
            } else if (isParenEnd(peek_node.*.body)) {
                paren_count -= 1;
            } else if (paren_count == 0 and isColon(peek_node.*.body)) {
                end_of_signature = true;
            }
            const n = try token_queue.pop();
            try buffer_token_queue.push(n.*.body, n.*.line, n.*.col);
            self.allocator.destroy(n);
        }

        return buffer_token_queue;
    }

    fn collectFunctionBodySection(self: *Self, token_queue: *TokenQueue, current_col: usize, function_end: *bool) !*TokenQueue {
        var buffer_token_queue = try TokenQueue.init(self.allocator);
        var nested_body: bool = false;
        var nested_body_col: usize = 0;
        var paren_count: usize = 0;
        var signature_ready: bool = false;

        while (!token_queue.empty()) {
            const peek_node = try token_queue.peek();
            if (peek_node.*.col <= current_col) {
                function_end.* = true;
                break;
            }

            if (peek_node.*.col <= nested_body_col) {
                nested_body = false;
            }

            if (isParenStart(peek_node.*.body) or isTupleStart(peek_node.*.body) or isRecordStart(peek_node.*.body) or containsParenStart(peek_node.*.body)) {
                paren_count += 1;
            } else if (isParenEnd(peek_node.*.body)) {
                paren_count -= 1;
                if (paren_count == 0 and !nested_body) {
                    signature_ready = true;
                }
            } else if (signature_ready and (isColon(peek_node.*.body) or isWhenKeyword(peek_node.*.body)) and !nested_body) {
                var buffer_list = std.ArrayList(*TokenQueueNode).init(self.allocator);
                // Reverse order
                while (!buffer_token_queue.empty()) {
                    const n = try buffer_token_queue.pop();
                    try buffer_list.append(n);
                }

                var signature_paren_count: usize = 0;

                // Checking paren count in reverse
                while (buffer_list.items.len > 0) {
                    const node = buffer_list.pop().?;
                    if (isParenEnd(node.*.body)) {
                        signature_paren_count += 1;
                    } else if (isParenStart(node.*.body) or isTupleStart(node.*.body) or isRecordStart(node.*.body) or containsParenStart(node.*.body)) {
                        signature_paren_count -= 1;
                    }

                    try token_queue.pushHead(node.*.body, node.*.line, node.*.col);
                    self.allocator.destroy(node);
                    if (signature_paren_count == 0) {
                        break;
                    }
                }

                // Push remaining tokens back into buffer_token_queue
                while (buffer_list.items.len > 0) {
                    const n = buffer_list.pop().?;
                    try buffer_token_queue.pushHead(n.*.body, n.*.line, n.*.col);
                    self.allocator.destroy(n);
                }

                buffer_list.deinit();
                break;
            } else if (signature_ready) {
                signature_ready = false;
            }

            if ((isFun(peek_node.*.body) and paren_count == 0) or isCaseKeyword(peek_node.*.body) or isTryKeyword(peek_node.*.body) or isCatchKeyword(peek_node.*.body) or isReceiveKeyword(peek_node.*.body) or isMaybeKeyword(peek_node.*.body) or isElseKeyword(peek_node.*.body)) {
                if (!nested_body) {
                    nested_body_col = peek_node.*.col;
                }
                nested_body = true;
            }

            const n = try token_queue.pop();
            try buffer_token_queue.push(n.*.body, n.*.line, n.*.col);
            self.allocator.destroy(n);
        }

        return buffer_token_queue;
    }

    fn parseCaseExpression(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        // Pop off case keyword
        const n = try token_queue.pop();
        if (!isCaseKeyword(n.*.body)) {
            tele_error.setErrorMessage(n.*.line, n.*.col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(n.*.body);
            self.allocator.destroy(n);
            return ParserError.ParsingFailure;
        }
        const current_col = n.*.col;
        const ast_line = n.*.line;
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        var buffer_token_queue = try TokenQueue.init(self.allocator);
        errdefer buffer_token_queue.deinit();

        while (!token_queue.empty()) {
            const pnode2 = token_queue.peek() catch {
                tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.invalid_expression);
                return ParserError.ParsingFailure;
            };

            if (pnode2.*.col <= current_col) {
                break;
            } else {
                const node = try token_queue.pop();
                errdefer self.allocator.free(node.*.body);
                errdefer self.allocator.destroy(node);

                try buffer_token_queue.push(node.*.body, node.*.line, node.*.col);
                self.allocator.destroy(node);
            }
        }

        const signature_ast = try self.parseCaseSignature(buffer_token_queue, ast_line, current_col);

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(children, self.allocator);
        try children.append(signature_ast);

        var body = try self.parseCaseBody(buffer_token_queue, ast_line, current_col);
        errdefer tele_ast.freeTeleAstList(body, self.allocator);
        if (!buffer_token_queue.empty()) {
            const peek_n = try buffer_token_queue.peek();
            tele_error.setErrorMessage(peek_n.*.line, peek_n.*.col, tele_error.ErrorType.unexpected_token);
            return ParserError.ParsingFailure;
        }
        buffer_token_queue.deinit();

        for (body.items) |c| {
            try children.append(c);
        }
        body.deinit();
        if (children.items.len == 0) {
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.missing_body);
            return ParserError.ParsingFailure;
        }

        const final_ast = try self.allocator.create(TeleAst);
        final_ast.*.body = "";
        final_ast.*.ast_type = TeleAstType.case;
        final_ast.*.children = children;
        final_ast.*.col = current_col;
        final_ast.*.line = ast_line;

        return final_ast;
    }

    fn parseCaseSignature(self: *Self, token_queue: *TokenQueue, line: usize, col: usize) !*TeleAst {
        var buffer_token_queue = try TokenQueue.init(self.allocator);
        errdefer buffer_token_queue.deinit();

        // Parse Match Signature
        var map_count: usize = 0;
        while (!token_queue.empty()) {
            const node = try token_queue.pop();
            errdefer self.allocator.free(node.*.body);
            errdefer self.allocator.destroy(node);

            if (isMapStart(node.*.body) or isBitStringStart(node.*.body)) {
                map_count += 1;
            } else if (isMapEnd(node.*.body) or isBitStringEnd(node.*.body)) {
                map_count -= 1;
            }

            if (map_count == 0 and isColon(node.*.body)) {
                self.allocator.free(node.*.body);
                self.allocator.destroy(node);
                break;
            } else {
                try buffer_token_queue.push(node.*.body, node.*.line, node.*.col);
                self.allocator.destroy(node);
            }
        }

        if (buffer_token_queue.empty()) {
            tele_error.setErrorMessage(line, col, tele_error.ErrorType.invalid_expression);
            return ParserError.ParsingFailure;
        }

        const ast = try self.parseExp(buffer_token_queue);
        errdefer tele_ast.freeTeleAst(ast, self.allocator);
        if (!buffer_token_queue.empty()) {
            const peek_n = try buffer_token_queue.peek();
            tele_error.setErrorMessage(peek_n.*.line, peek_n.*.col, tele_error.ErrorType.unexpected_token);
            return ParserError.ParsingFailure;
        }
        buffer_token_queue.deinit();
        return ast;
    }

    fn parseCaseBody(self: *Self, token_queue: *TokenQueue, line: usize, col: usize) !std.ArrayList(*TeleAst) {
        var current_col: usize = col;
        var ast_line: usize = line;
        var clause_col: usize = 0;
        var alist = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(alist, self.allocator);

        // Parse Case Clauses
        while (!token_queue.empty()) {
            var children = std.ArrayList(*TeleAst).init(self.allocator);
            errdefer tele_ast.freeTeleAstList(children, self.allocator);

            var alist2 = try self.parseCaseClauseSignature(token_queue, &clause_col);
            if (alist2.items.len == 2) {
                const ast = alist2.pop().?;
                if (ast.*.ast_type != TeleAstType.guard_sequence) {
                    tele_error.setErrorMessage(ast.*.line, ast.*.col, tele_error.ErrorType.invalid_guard_clause);
                    tele_ast.freeTeleAst(ast, self.allocator);
                    tele_ast.freeTeleAstList(alist2, self.allocator);
                    return ParserError.ParsingFailure;
                }
                const ast2 = alist2.pop().?;
                current_col = ast2.*.col;
                ast_line = ast2.*.line;
                try children.append(ast2);
                try children.append(ast);
            } else if (alist2.items.len == 1) {
                const ast = alist2.pop().?;
                clause_col = ast.*.col;
                ast_line = ast.*.line;
                try children.append(ast);
            } else {
                tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.invalid_expression);
                tele_ast.freeTeleAstList(alist2, self.allocator);
                return ParserError.ParsingFailure;
            }
            alist2.deinit();

            var case_clause_body = try self.parseCaseClauseBody(token_queue, ast_line, clause_col);
            errdefer tele_ast.freeTeleAstList(case_clause_body, self.allocator);
            for (case_clause_body.items) |c| {
                try children.append(c);
            }
            case_clause_body.deinit();

            const t = try self.allocator.create(TeleAst);
            t.body = "";
            t.ast_type = TeleAstType.case_clause;
            t.children = children;
            t.col = current_col;
            t.line = ast_line;
            errdefer tele_ast.freeTeleAst(t, self.allocator);

            try alist.append(t);
        }

        return alist;
    }

    fn parseMaybeExpression(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        // Pop off maybe keyword
        const n = try token_queue.pop();
        const current_col = n.*.col;
        const ast_line = n.*.line;

        if (!isMaybeKeyword(n.*.body)) {
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(n.*.body);
            self.allocator.destroy(n);
            return ParserError.ParsingFailure;
        }
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        // Pop off colon
        const n2 = try token_queue.pop();
        if (!isColon(n2.*.body)) {
            tele_error.setErrorMessage(n2.*.line, n2.*.col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(n2.*.body);
            self.allocator.destroy(n2);
            return ParserError.ParsingFailure;
        }
        self.allocator.free(n2.*.body);
        self.allocator.destroy(n2);

        var buffer_token_queue = try TokenQueue.init(self.allocator);
        errdefer buffer_token_queue.deinit();

        // Parse Maybe Expression
        while (!token_queue.empty()) {
            const pnode2 = try token_queue.peek();

            if (pnode2.*.col <= current_col) {
                break;
            } else {
                const node = try token_queue.pop();
                errdefer self.allocator.free(node.*.body);
                errdefer self.allocator.destroy(node);

                try buffer_token_queue.push(node.*.body, node.*.line, node.*.col);
                self.allocator.destroy(node);
            }
        }

        var maybe_body = try self.parseBody(buffer_token_queue, ast_line, current_col);
        errdefer tele_ast.freeTeleAstList(maybe_body, self.allocator);
        buffer_token_queue.deinit();

        if (!token_queue.empty()) {
            const pn2 = try token_queue.peek();

            if (isElseKeyword(pn2.*.body)) {
                const else_ast = try self.parseElseExpression(token_queue);
                try maybe_body.append(else_ast);
            }
        }

        const maybe_ast = try self.allocator.create(TeleAst);
        maybe_ast.*.body = "";
        maybe_ast.*.ast_type = TeleAstType.maybe_exp;
        maybe_ast.*.children = maybe_body;
        maybe_ast.*.col = current_col;
        maybe_ast.*.line = ast_line;

        return maybe_ast;
    }

    fn parseElseExpression(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        // Pop off else keyword
        const n = try token_queue.pop();
        const current_col = n.*.col;
        const ast_line = n.*.line;
        if (!isElseKeyword(n.*.body)) {
            tele_error.setErrorMessage(n.*.line, n.*.col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(n.*.body);
            self.allocator.destroy(n);
            return ParserError.ParsingFailure;
        }
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        // Pop off colon
        const n2 = try token_queue.pop();
        if (!isColon(n2.*.body)) {
            tele_error.setErrorMessage(n2.*.line, n2.*.col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(n2.*.body);
            self.allocator.destroy(n2);
            return ParserError.ParsingFailure;
        }
        self.allocator.free(n2.*.body);
        self.allocator.destroy(n2);

        var buffer_token_queue = try TokenQueue.init(self.allocator);
        errdefer buffer_token_queue.deinit();

        while (!token_queue.empty()) {
            const pnode2 = try token_queue.peek();

            if (pnode2.*.col <= current_col) {
                break;
            } else {
                const node = try token_queue.pop();
                errdefer self.allocator.free(node.*.body);
                errdefer self.allocator.destroy(node);

                try buffer_token_queue.push(node.*.body, node.*.line, node.*.col);
                self.allocator.destroy(node);
            }
        }

        const body = try self.parseCaseBody(buffer_token_queue, ast_line, current_col);
        errdefer tele_ast.freeTeleAstList(body, self.allocator);
        if (!buffer_token_queue.empty()) {
            const peek_n = try buffer_token_queue.peek();
            tele_error.setErrorMessage(peek_n.*.line, peek_n.*.col, tele_error.ErrorType.unexpected_token);
            return ParserError.ParsingFailure;
        }
        buffer_token_queue.deinit();

        const ast = try self.allocator.create(TeleAst);
        ast.*.body = "";
        ast.*.ast_type = TeleAstType.else_exp;
        ast.*.children = body;
        ast.*.col = current_col;
        ast.*.line = ast_line;

        return ast;
    }

    fn parseReceiveExpression(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        // Pop off receive keyword
        const n = try token_queue.pop();
        const current_col = n.*.col;
        const ast_line = n.*.line;
        if (!isReceiveKeyword(n.*.body)) {
            tele_error.setErrorMessage(n.*.line, n.*.col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(n.*.body);
            self.allocator.destroy(n);
            return ParserError.ParsingFailure;
        }
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        // Pop off colon
        const nc = try token_queue.pop();
        if (!isColon(nc.*.body)) {
            tele_error.setErrorMessage(nc.*.line, nc.*.col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(nc.*.body);
            self.allocator.destroy(nc);
            return ParserError.ParsingFailure;
        }
        self.allocator.free(nc.*.body);
        self.allocator.destroy(nc);

        var buffer_token_queue = try TokenQueue.init(self.allocator);
        errdefer buffer_token_queue.deinit();

        while (!token_queue.empty()) {
            const pnode2 = try token_queue.peek();

            if (pnode2.*.col <= current_col) {
                break;
            } else {
                const node = try token_queue.pop();
                errdefer self.allocator.free(node.*.body);
                errdefer self.allocator.destroy(node);

                try buffer_token_queue.push(node.*.body, node.*.line, node.*.col);

                self.allocator.destroy(node);
            }
        }

        const body = try self.parseCaseBody(buffer_token_queue, ast_line, current_col);
        errdefer tele_ast.freeTeleAstList(body, self.allocator);
        if (!buffer_token_queue.empty()) {
            const peek_n = try buffer_token_queue.peek();
            tele_error.setErrorMessage(peek_n.*.line, peek_n.*.col, tele_error.ErrorType.unexpected_token);
            return ParserError.ParsingFailure;
        }
        buffer_token_queue.deinit();

        const final_ast = try self.allocator.create(TeleAst);
        final_ast.*.body = "";
        final_ast.*.ast_type = TeleAstType.receive_exp;
        final_ast.*.children = body;
        final_ast.*.col = current_col;
        final_ast.*.line = ast_line;

        return final_ast;
    }

    fn parseTryCatch(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        // Pop off try keyword
        const n = try token_queue.pop();
        const current_col = n.*.col;
        const ast_line = n.*.line;
        if (!isTryKeyword(n.*.body)) {
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(n.*.body);
            self.allocator.destroy(n);
            return ParserError.ParsingFailure;
        }
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        var buffer_token_queue = try TokenQueue.init(self.allocator);
        errdefer buffer_token_queue.deinit();

        // Parse Try Expression
        while (!token_queue.empty()) {
            const pnode2 = try token_queue.peek();

            if (pnode2.*.col <= current_col) {
                break;
            } else {
                const node = try token_queue.pop();
                errdefer self.allocator.free(node.*.body);
                errdefer self.allocator.destroy(node);

                try buffer_token_queue.push(node.*.body, node.*.line, node.*.col);
                self.allocator.destroy(node);
            }
        }

        const signature_ast = try self.parseCaseSignature(buffer_token_queue, ast_line, current_col);

        var try_children = std.ArrayList(*TeleAst).init(self.allocator);
        try try_children.append(signature_ast);

        var body = self.parseCaseBody(buffer_token_queue, ast_line, current_col) catch |err| {
            tele_ast.freeTeleAstList(try_children, self.allocator);
            return err;
        };
        if (!buffer_token_queue.empty()) {
            const peek_n = try buffer_token_queue.peek();
            tele_error.setErrorMessage(peek_n.*.line, peek_n.*.col, tele_error.ErrorType.unexpected_token);
            tele_ast.freeTeleAstList(body, self.allocator);
            tele_ast.freeTeleAstList(try_children, self.allocator);
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
        try_ast.*.col = current_col;
        try_ast.*.line = ast_line;

        // Pop off catch keyword
        const n_catch = try token_queue.pop();
        const current_col_catch = n_catch.*.col;
        const ast_line_catch = n_catch.*.line;
        if (!isCatchKeyword(n_catch.*.body)) {
            tele_error.setErrorMessage(n_catch.*.line, n_catch.*.col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(n_catch.*.body);
            self.allocator.destroy(n_catch);
            tele_ast.freeTeleAst(try_ast, self.allocator);
            return ParserError.ParsingFailure;
        }
        self.allocator.free(n_catch.*.body);
        self.allocator.destroy(n_catch);

        // Pop off colon
        const n_colon = try token_queue.pop();
        if (!isColon(n_colon.*.body)) {
            tele_error.setErrorMessage(n_colon.*.line, n_colon.*.col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(n_colon.*.body);
            self.allocator.destroy(n_colon);
            tele_ast.freeTeleAst(try_ast, self.allocator);
            return ParserError.ParsingFailure;
        }
        self.allocator.free(n_colon.*.body);
        self.allocator.destroy(n_colon);

        // Parse Catch Expression
        while (!token_queue.empty()) {
            const pnode2 = try token_queue.peek();

            if (pnode2.*.col <= current_col) {
                break;
            } else {
                const node = try token_queue.pop();
                errdefer self.allocator.free(node.*.body);
                errdefer self.allocator.destroy(node);

                try buffer_token_queue.push(node.*.body, node.*.line, node.*.col);
                self.allocator.destroy(node);
            }
        }

        var catch_children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(catch_children, self.allocator);

        var body2 = self.parseCatchBody(buffer_token_queue, ast_line, current_col) catch |err| {
            tele_ast.freeTeleAst(try_ast, self.allocator);
            return err;
        };
        errdefer tele_ast.freeTeleAstList(body2, self.allocator);
        if (!buffer_token_queue.empty()) {
            tele_ast.freeTeleAst(try_ast, self.allocator);
            const peek_n = try buffer_token_queue.peek();
            tele_error.setErrorMessage(peek_n.*.line, peek_n.*.col, tele_error.ErrorType.unexpected_token);
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
        catch_ast.*.col = current_col_catch;
        catch_ast.*.line = ast_line_catch;

        const final_ast = try self.allocator.create(TeleAst);
        final_ast.*.body = "";
        final_ast.*.ast_type = TeleAstType.try_catch;

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(children, self.allocator);
        try children.append(try_ast);
        try children.append(catch_ast);
        final_ast.*.children = children;
        final_ast.*.col = current_col;
        final_ast.*.line = ast_line;

        buffer_token_queue.deinit();

        return final_ast;
    }

    fn parseCatchBody(self: *Self, token_queue: *TokenQueue, line: usize, col: usize) !std.ArrayList(*TeleAst) {
        var current_col: usize = col;
        var ast_line: usize = line;
        var clause_col: usize = 0;
        var alist = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(alist, self.allocator);

        // Parse Case Clauses
        while (!token_queue.empty()) {
            var children = std.ArrayList(*TeleAst).init(self.allocator);
            errdefer tele_ast.freeTeleAstList(children, self.allocator);

            // TODO: Improve line and col for multiple exception statements
            const ast = try self.parseException(token_queue, ast_line, current_col, &clause_col);
            try children.append(ast);

            var case_clause_body = try self.parseCaseClauseBody(token_queue, ast_line, clause_col);
            errdefer tele_ast.freeTeleAstList(case_clause_body, self.allocator);
            for (case_clause_body.items) |c| {
                try children.append(c);
            }
            case_clause_body.deinit();
            ast_line = children.items[children.items.len - 1].*.line;
            current_col = children.items[children.items.len - 1].*.col;

            const t = try self.allocator.create(TeleAst);
            t.body = "";
            t.ast_type = TeleAstType.case_clause;
            t.children = children;
            t.col = current_col;
            t.line = ast_line;
            errdefer tele_ast.freeTeleAst(t, self.allocator);

            try alist.append(t);
        }

        return alist;
    }

    fn parseException(self: *Self, token_queue: *TokenQueue, line: usize, col: usize, clause_col: *usize) !*TeleAst {
        const exception_node = token_queue.pop() catch {
            tele_error.setErrorMessage(line, col, tele_error.ErrorType.expected_token);
            return ParserError.ParsingFailure;
        };
        const colon_node = token_queue.pop() catch {
            tele_error.setErrorMessage(exception_node.*.line, exception_node.*.col, tele_error.ErrorType.expected_token);
            self.allocator.free(exception_node.*.body);
            self.allocator.destroy(exception_node);
            return ParserError.ParsingFailure;
        };
        if (!isColon(colon_node.*.body)) {
            tele_error.setErrorMessage(colon_node.*.line, colon_node.*.col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(exception_node.*.body);
            self.allocator.destroy(exception_node);
            self.allocator.free(colon_node.*.body);
            self.allocator.destroy(colon_node);
            return ParserError.ParsingFailure;
        }
        self.allocator.free(colon_node.*.body);
        self.allocator.destroy(colon_node);

        const t = try self.allocator.create(TeleAst);
        t.*.body = exception_node.*.body;
        t.*.ast_type = TeleAstType.exception;
        t.*.children = null;
        t.*.col = exception_node.*.col;
        t.*.line = exception_node.*.line;

        clause_col.* = exception_node.*.col;

        self.allocator.destroy(exception_node);

        return t;
    }

    fn parseCaseClauseSignature(self: *Self, token_queue: *TokenQueue, clause_col: *usize) !std.ArrayList(*TeleAst) {
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

            if (isColon(node.*.body)) {
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

        const ast = try self.parseExp(buffer_token_queue);
        try alist.append(ast);
        if (!buffer_token_queue.empty()) {
            const pn = try buffer_token_queue.peek();
            if (isWhenKeyword(pn.*.body)) {
                const gc_seq = try self.parseGuardSequence(buffer_token_queue);
                try alist.append(gc_seq);
            } else {
                tele_error.setErrorMessage(pn.*.line, pn.*.col, tele_error.ErrorType.unexpected_token);
                return ParserError.ParsingFailure;
            }
        }
        buffer_token_queue.deinit();
        return alist;
    }

    fn parseCaseClauseBody(self: *Self, token_queue: *TokenQueue, line: usize, clause_col: usize) !std.ArrayList(*TeleAst) {
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

        const alist = try self.parseBody(buffer_token_queue, line, clause_col);
        errdefer tele_ast.freeTeleAstList(alist, self.allocator);

        if (!buffer_token_queue.empty()) {
            const peek_n = try buffer_token_queue.peek();
            tele_error.setErrorMessage(peek_n.*.line, peek_n.*.col, tele_error.ErrorType.unexpected_token);
            return ParserError.ParsingFailure;
        }

        buffer_token_queue.deinit();

        return alist;
    }

    fn parseFunctionCall(self: *Self, token_queue: *TokenQueue, buf: []const u8, type_exp: bool, line: usize, col: usize) !*TeleAst {
        errdefer self.allocator.free(buf);
        if (buf[0] == '?' or buf[0] == '@') {
            const dot_idx = util.findDot(buf);
            if (dot_idx > 0) {
                if (buf[dot_idx + 1] == '@') {
                    if (!util.validateFunctionName(buf[1..dot_idx])) {
                        tele_error.setErrorMessage(line, col, tele_error.ErrorType.invalid_name);
                        return ParserError.ParsingFailure;
                    }
                    if (!util.validateFunctionName(buf[dot_idx + 2 ..])) {
                        tele_error.setErrorMessage(line, col, tele_error.ErrorType.invalid_name);
                        return ParserError.ParsingFailure;
                    }
                } else {
                    if (!util.validateFunctionName(buf[1..])) {
                        tele_error.setErrorMessage(line, col, tele_error.ErrorType.invalid_name);
                        return ParserError.ParsingFailure;
                    }
                }
            } else {
                if (!util.validateFunctionName(buf[1..])) {
                    tele_error.setErrorMessage(line, col, tele_error.ErrorType.invalid_name);
                    return ParserError.ParsingFailure;
                }
            }
        } else {
            if (!util.validateFunctionName(buf)) {
                tele_error.setErrorMessage(line, col, tele_error.ErrorType.invalid_name);
                return ParserError.ParsingFailure;
            }
        }

        // Remove paren start
        const pn = try token_queue.pop();
        const current_col = col;
        const ast_line = line;
        if (!isParenStart(pn.*.body)) {
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(pn.*.body);
            self.allocator.destroy(pn);
            return ParserError.ParsingFailure;
        }
        self.allocator.free(pn.*.body);
        self.allocator.destroy(pn);

        const pn2 = try token_queue.peek();

        var children: ?std.ArrayList(*TeleAst) = null;
        if (!isParenEnd(pn2.*.body)) {
            children = std.ArrayList(*TeleAst).init(self.allocator);
            errdefer tele_ast.freeTeleAstList(children.?, self.allocator);
            while (!token_queue.empty()) {
                var found_end: bool = false;
                const ast = try self.parseFunctionCallArg(token_queue, type_exp, &found_end);
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
        t.*.col = current_col;
        t.*.line = ast_line;

        return t;
    }

    fn parseFunctionCallArg(self: *Self, token_queue: *TokenQueue, type_exp: bool, found_end: *bool) !*TeleAst {
        // Function Call Body Token Queue
        var buffer_token_queue = try TokenQueue.init(self.allocator);
        errdefer buffer_token_queue.deinit();

        var n_count: usize = 0;
        while (!token_queue.empty()) {
            const n2 = try token_queue.pop();
            errdefer self.allocator.free(n2.*.body);
            errdefer self.allocator.destroy(n2);

            if (n_count == 0) {
                if (isComma(n2.*.body) or isParenEnd(n2.*.body)) {
                    if (isParenEnd(n2.*.body)) {
                        found_end.* = true;
                    }
                    self.allocator.free(n2.*.body);
                    self.allocator.destroy(n2);
                    break;
                }
            }

            if (isParenStart(n2.*.body) or isTupleStart(n2.*.body) or isListStart(n2.*.body) or isMapStart(n2.*.body) or isRecordStart(n2.*.body) or isBitStringStart(n2.*.body)) {
                n_count += 1;
            } else if (isParenEnd(n2.*.body) or isListEnd(n2.*.body) or isMapEnd(n2.*.body) or isBitStringEnd(n2.*.body)) {
                n_count -= 1;
            }

            try buffer_token_queue.push(n2.*.body, n2.*.line, n2.*.col);
            self.allocator.destroy(n2);
        }

        var ast: *TeleAst = undefined;
        if (type_exp) {
            ast = try self.parseTypeExp(buffer_token_queue);
        } else {
            ast = try self.parseExp(buffer_token_queue);
        }
        if (!buffer_token_queue.empty()) {
            const peek_n = try buffer_token_queue.peek();
            tele_error.setErrorMessage(peek_n.*.line, peek_n.*.col, tele_error.ErrorType.unexpected_token);
            tele_ast.freeTeleAst(ast, self.allocator);
            return ParserError.ParsingFailure;
        }
        buffer_token_queue.deinit();
        return ast;
    }

    fn parseAttribute(self: *Self, token_queue: *TokenQueue, ast_type: TeleAstType) !*TeleAst {

        // Pop off attribute name
        const name_node = try token_queue.pop();
        const name = name_node.*.body;
        const current_col = name_node.*.col;
        const ast_line = name_node.*.line;
        errdefer self.allocator.free(name);
        self.allocator.destroy(name_node);

        if (!util.validateName(name)) {
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.invalid_name);
            return ParserError.ParsingFailure;
        }

        const bast = self.parseFunctionCall(token_queue, name, false, ast_line, current_col) catch {
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.invalid_expression);
            return ParserError.ParsingFailure;
        };

        bast.*.ast_type = ast_type;
        bast.*.col = current_col;
        bast.*.line = ast_line;

        return bast;
    }

    fn parseBehaviour(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        const n = try token_queue.pop();
        const current_col = n.*.col;
        const ast_line = n.*.line;

        if (!isBehaviourKeyword(n.*.body)) {
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(n.*.body);
            self.allocator.destroy(n);
            return ParserError.ParsingFailure;
        }
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        const pstart = try token_queue.pop();
        if (!isParenStart(pstart.*.body)) {
            tele_error.setErrorMessage(pstart.*.line, pstart.*.col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(pstart.*.body);
            self.allocator.destroy(pstart);
            return ParserError.ParsingFailure;
        }
        self.allocator.free(pstart.*.body);
        self.allocator.destroy(pstart);

        const name_node = try token_queue.pop();
        if (!util.validateName(name_node.*.body)) {
            tele_error.setErrorMessage(name_node.*.line, name_node.*.col, tele_error.ErrorType.invalid_name);
            self.allocator.free(name_node.*.body);
            self.allocator.destroy(name_node);
            return ParserError.ParsingFailure;
        }

        const pend = try token_queue.pop();
        if (!isParenEnd(pend.*.body)) {
            tele_error.setErrorMessage(pend.*.line, pend.*.col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(pend.*.body);
            self.allocator.destroy(pend);
            self.allocator.free(name_node.*.body);
            self.allocator.destroy(name_node);
        }
        self.allocator.free(pend.*.body);
        self.allocator.destroy(pend);

        const buf = name_node.*.body;
        self.allocator.destroy(name_node);

        const ast = try self.allocator.create(TeleAst);
        ast.*.body = buf;
        ast.*.ast_type = TeleAstType.behaviour;
        ast.*.children = null;
        ast.*.col = current_col;
        ast.*.line = ast_line;

        return ast;
    }

    fn parseImport(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        // Pop off import
        const n = try token_queue.pop();
        const current_col = n.*.col;
        const ast_line = n.*.line;
        if (!isImportKeyword(n.*.body)) {
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(n.*.body);
            self.allocator.destroy(n);
            return ParserError.ParsingFailure;
        }
        self.allocator.free(n.*.body);
        self.allocator.destroy(n);

        // Pop off module name
        const name_node = try token_queue.pop();
        const name = name_node.*.body;
        errdefer self.allocator.free(name);
        if (!util.validateName(name)) {
            tele_error.setErrorMessage(name_node.*.line, name_node.*.col, tele_error.ErrorType.invalid_name);
            self.allocator.destroy(name_node);
            return ParserError.ParsingFailure;
        }
        self.allocator.destroy(name_node);

        if (!try checkParenStartPeek(token_queue)) {
            const peek_n = try token_queue.peek();
            tele_error.setErrorMessage(peek_n.*.line, peek_n.*.col, tele_error.ErrorType.unexpected_token);
            return ParserError.ParsingFailure;
        }
        // Pop off paren start
        const paren_start = try token_queue.pop();
        self.allocator.free(paren_start.*.body);
        self.allocator.destroy(paren_start);

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(children, self.allocator);

        while (!token_queue.empty()) {
            const pelement_name_node = try token_queue.peek();

            if (isParenEnd(pelement_name_node.*.body)) {
                const element_name_node = try token_queue.pop();
                self.allocator.free(element_name_node.*.body);
                self.allocator.destroy(element_name_node);
                break;
            } else if (isComma(pelement_name_node.*.body)) {
                const element_name_node = try token_queue.pop();
                self.allocator.free(element_name_node.*.body);
                self.allocator.destroy(element_name_node);
            }

            const et = try self.parseImportElement(token_queue);
            try children.append(et);
        }

        const ast = try self.allocator.create(TeleAst);
        ast.*.body = name;
        ast.*.ast_type = TeleAstType.import_def;
        ast.*.children = children;
        ast.*.col = current_col;
        ast.*.line = ast_line;

        return ast;
    }

    fn parseOnLoad(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        // Pop off on_load
        const n = try token_queue.pop();
        const current_col = n.*.col;
        const ast_line = n.*.line;
        const name = n.*.body;
        if (!isOnLoadKeyword(name)) {
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(name);
            self.allocator.destroy(n);
            return ParserError.ParsingFailure;
        }
        errdefer self.allocator.free(name);
        self.allocator.destroy(n);

        if (!try checkParenStartPeek(token_queue)) {
            const peek_n = try token_queue.peek();
            tele_error.setErrorMessage(peek_n.*.line, peek_n.*.col, tele_error.ErrorType.unexpected_token);
            return ParserError.ParsingFailure;
        }

        // Pop off paren_start
        const paren_start = try token_queue.pop();
        if (!isParenStart(paren_start.*.body)) {
            tele_error.setErrorMessage(paren_start.*.line, paren_start.*.col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(paren_start.*.body);
            self.allocator.destroy(n);
            return ParserError.ParsingFailure;
        }
        self.allocator.free(paren_start.*.body);
        self.allocator.destroy(paren_start);

        var children = std.ArrayList(*TeleAst).init(self.allocator);

        const ast = try self.parseImportElement(token_queue);
        try children.append(ast);

        const paren_end = try token_queue.pop();
        if (!isParenEnd(paren_end.*.body)) {
            tele_error.setErrorMessage(paren_end.*.line, paren_end.*.col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(paren_end.*.body);
            self.allocator.destroy(paren_end);
        }
        self.allocator.free(paren_end.*.body);
        self.allocator.destroy(paren_end);

        const onload = try self.allocator.create(TeleAst);
        onload.*.body = name;
        onload.*.ast_type = TeleAstType.on_load;
        onload.*.children = children;
        onload.*.col = current_col;
        onload.*.line = ast_line;

        return onload;
    }

    fn parseNifs(self: *Self, token_queue: *TokenQueue) !*TeleAst {

        // Pop off nifs keyword
        const name_node = try token_queue.pop();
        const name = name_node.*.body;
        const current_col = name_node.*.col;
        const ast_line = name_node.*.line;
        errdefer self.allocator.free(name);

        if (!isNifsKeyword(name)) {
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.unexpected_token);
            return ParserError.ParsingFailure;
        }

        if (!try checkParenStartPeek(token_queue)) {
            const peek_n = try token_queue.peek();
            tele_error.setErrorMessage(peek_n.*.line, peek_n.*.col, tele_error.ErrorType.unexpected_token);
            return ParserError.ParsingFailure;
        }

        // Pop off paren_start
        const paren_start = try token_queue.pop();
        const paren_current_col = paren_start.*.col;
        const paren_ast_line = paren_start.*.line;
        self.allocator.free(paren_start.*.body);
        self.allocator.destroy(paren_start);

        var children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(children, self.allocator);

        while (!token_queue.empty()) {
            const pn = try token_queue.peek();

            if (isParenEnd(pn.*.body)) {
                const n2 = try token_queue.pop();
                self.allocator.free(n2.*.body);
                self.allocator.destroy(n2);
                break;
            } else if (isComma(pn.*.body)) {
                const n2 = try token_queue.pop();
                self.allocator.free(n2.*.body);
                self.allocator.destroy(n2);
            }

            const e = try self.parseImportElement(token_queue);
            try children.append(e);
        }

        const wrapper = try self.allocator.create(TeleAst);
        wrapper.*.body = "";
        wrapper.*.ast_type = TeleAstType.list;
        wrapper.*.children = children;
        wrapper.*.col = paren_current_col;
        wrapper.*.line = paren_ast_line;

        var final_children = std.ArrayList(*TeleAst).init(self.allocator);
        errdefer tele_ast.freeTeleAstList(final_children, self.allocator);

        try final_children.append(wrapper);

        const ast = try self.allocator.create(TeleAst);
        ast.*.body = name;
        ast.*.ast_type = TeleAstType.nifs;
        ast.*.children = final_children;
        ast.*.col = current_col;
        ast.*.line = ast_line;

        return ast;
    }

    fn parseImportElement(self: *Self, token_queue: *TokenQueue) !*TeleAst {
        const element_name_node = try token_queue.pop();
        const element_name = element_name_node.*.body;
        const current_col = element_name_node.*.col;
        const ast_line = element_name_node.*.line;
        errdefer self.allocator.free(element_name);
        self.allocator.destroy(element_name_node);

        if (!util.validateName(element_name)) {
            tele_error.setErrorMessage(ast_line, current_col, tele_error.ErrorType.invalid_name);
            return ParserError.ParsingFailure;
        }

        const div_node = try token_queue.pop();
        if (!std.mem.eql(u8, "/", div_node.*.body)) {
            tele_error.setErrorMessage(div_node.*.line, div_node.*.col, tele_error.ErrorType.unexpected_token);
            self.allocator.free(div_node.*.body);
            self.allocator.destroy(div_node);
            return ParserError.ParsingFailure;
        }
        self.allocator.free(div_node.*.body);
        self.allocator.destroy(div_node);

        const arg_number_node = try token_queue.pop();
        const arg_number = arg_number_node.*.body;
        errdefer self.allocator.free(arg_number);
        self.allocator.destroy(arg_number_node);

        var buf = try self.allocator.alloc(u8, element_name.len + arg_number.len + 1);
        std.mem.copyForwards(u8, buf, element_name);
        buf[element_name.len] = '/';
        std.mem.copyForwards(u8, buf[element_name.len + 1 ..], arg_number);
        self.allocator.free(element_name);
        self.allocator.free(arg_number);

        const et = try self.allocator.create(TeleAst);
        et.*.body = buf;
        et.*.ast_type = TeleAstType.import_element;
        et.*.children = null;
        et.*.col = current_col;
        et.*.line = ast_line;

        return et;
    }
};

test "parse variable" {
    const token_queue = try TokenQueue.init(talloc);

    const parser = try Parser.init(token_queue, talloc);
    defer parser.deinit();

    const result = try parser.parseVariable(try util.copyString("a", talloc), false, 0, 0);
    const expected = try tele_ast.makeVariable(try util.copyString("a", talloc), talloc);

    try std.testing.expect(tele_ast.equal(result, expected));

    tele_ast.freeTeleAst(result, talloc);
    tele_ast.freeTeleAst(expected, talloc);

    const result2 = try parser.parseVariable(try util.copyString("point", talloc), true, 0, 0);
    const expected2 = try tele_ast.makeValue(try util.copyString("point", talloc), TeleAstType.function_call, talloc);

    try std.testing.expect(tele_ast.equal(result2, expected2));

    tele_ast.freeTeleAst(result2, talloc);
    tele_ast.freeTeleAst(expected2, talloc);

    const result3 = try parser.parseVariable(try util.copyString("@stuff", talloc), true, 0, 0);
    const expected3 = try tele_ast.makeVariable(try util.copyString("@stuff", talloc), talloc);

    try std.testing.expect(tele_ast.equal(result3, expected3));

    tele_ast.freeTeleAst(result3, talloc);
    tele_ast.freeTeleAst(expected3, talloc);

    const result4 = try parser.parseVariable(try util.copyString("#point", talloc), true, 0, 0);
    const expected4 = try tele_ast.makeValue(try util.copyString("#point", talloc), TeleAstType.record, talloc);

    try std.testing.expect(tele_ast.equal(result4, expected4));

    tele_ast.freeTeleAst(result4, talloc);
    tele_ast.freeTeleAst(expected4, talloc);
}

test "parse operator expression" {
    const parser = try fileToParser("snippets/op.tl", talloc);
    defer parser.deinit();

    const init = try tele_ast.makeInt(try util.copyString("1", talloc), talloc);
    const result = try parser.parseOperator(parser.*.token_queue, false, init);

    const arg1 = try tele_ast.makeInt(try util.copyString("1", talloc), talloc);
    const arg2 = try tele_ast.makeInt(try util.copyString("2", talloc), talloc);
    const expected = try tele_ast.makeOp(try util.copyString("+", talloc), arg1, arg2, talloc);
    try std.testing.expect(tele_ast.equal(result, expected));

    tele_ast.freeTeleAst(result, talloc);
    tele_ast.freeTeleAst(expected, talloc);
}

test "parse operator expression chained" {
    const parser = try fileToParser("snippets/op2.tl", talloc);
    defer parser.deinit();

    const init = try tele_ast.makeVariable(try util.copyString("a", talloc), talloc);
    const result = try parser.parseOperator(parser.*.token_queue, false, init);

    const arg1 = try tele_ast.makeInt(try util.copyString("1", talloc), talloc);
    const arg2 = try tele_ast.makeInt(try util.copyString("2", talloc), talloc);
    const op = try tele_ast.makeOp(try util.copyString("+", talloc), arg1, arg2, talloc);

    const v = try tele_ast.makeVariable(try util.copyString("a", talloc), talloc);
    const expected = try tele_ast.makeOp(try util.copyString("=", talloc), v, op, talloc);

    try std.testing.expect(tele_ast.equal(result, expected));

    tele_ast.freeTeleAst(expected, talloc);
    tele_ast.freeTeleAst(result, talloc);
}

test "parse tuple" {
    const parser = try fileToParser("snippets/tuple.tl", talloc);
    defer parser.deinit();

    const result = try parser.parseTuple(parser.*.token_queue, false);

    const e1 = try tele_ast.makeInt(try util.copyString("1", talloc), talloc);
    const e2 = try tele_ast.makeInt(try util.copyString("2", talloc), talloc);
    const e3 = try tele_ast.makeInt(try util.copyString("3", talloc), talloc);
    const a = [_]*TeleAst{ e1, e2, e3 };
    const expected = try tele_ast.makeTuple(&a, talloc);

    try std.testing.expect(tele_ast.equal(result, expected));

    tele_ast.freeTeleAst(expected, talloc);
    tele_ast.freeTeleAst(result, talloc);
}

test "parse list" {
    const parser = try fileToParser("snippets/list.tl", talloc);
    defer parser.deinit();

    const result = try parser.parseList(parser.*.token_queue, false);

    const e1 = try tele_ast.makeInt(try util.copyString("1", talloc), talloc);
    const e2 = try tele_ast.makeInt(try util.copyString("2", talloc), talloc);
    const e3 = try tele_ast.makeInt(try util.copyString("3", talloc), talloc);
    const a = [_]*TeleAst{ e1, e2, e3 };
    const expected = try tele_ast.makeList(&a, talloc);

    try std.testing.expect(tele_ast.equal(result, expected));

    tele_ast.freeTeleAst(expected, talloc);
    tele_ast.freeTeleAst(result, talloc);
}

test "parse map" {
    const parser = try fileToParser("snippets/map.tl", talloc);
    defer parser.deinit();

    const result = try parser.parseMap(parser.*.token_queue, false);

    const e1 = try tele_ast.makeInt(try util.copyString("1", talloc), talloc);
    const e2 = try tele_ast.makeVariable(try util.copyString("a", talloc), talloc);
    const e3 = try tele_ast.makeInt(try util.copyString("2", talloc), talloc);
    const e4 = try tele_ast.makeVariable(try util.copyString("b", talloc), talloc);
    const e5 = try tele_ast.makeInt(try util.copyString("3", talloc), talloc);
    const e6 = try tele_ast.makeVariable(try util.copyString("c", talloc), talloc);
    const a = [_]*TeleAst{ e1, e2, e3, e4, e5, e6 };
    const expected = try tele_ast.makeMap(&a, talloc);

    tele_ast.freeTeleAst(expected, talloc);
    tele_ast.freeTeleAst(result, talloc);
}

test "parse body multi" {
    const parser = try fileToParser("snippets/body_multi.tl", talloc);
    defer parser.deinit();

    const result = try parser.parseBody(parser.*.token_queue, 0, 0);
    try std.testing.expect(result.items.len == 2);

    const ast1 = result.items[0];
    try std.testing.expect(ast1.*.ast_type == TeleAstType.op);
    try std.testing.expect(std.mem.eql(u8, ast1.*.body, "+"));
    try std.testing.expect(ast1.*.children.?.items.len == 2);

    const ast2 = result.items[1];
    try std.testing.expect(ast2.*.ast_type == TeleAstType.op);
    try std.testing.expect(std.mem.eql(u8, ast2.*.body, "-"));
    try std.testing.expect(ast2.*.children.?.items.len == 2);

    tele_ast.freeTeleAstList(result, test_allocator);
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

    tele_ast.freeTeleAstList(result.*.children.?, test_allocator);
    test_allocator.destroy(result);
}

test "parse case body single clause" {
    const parser = try fileToParser("snippets/match_body.tl", talloc);
    defer parser.deinit();

    const result = try parser.parseCaseBody(parser.*.token_queue, 0, 0);
    try std.testing.expect(result.items.len == 1);
    try std.testing.expect(result.items[0].ast_type == TeleAstType.case_clause);
    try std.testing.expect(std.mem.eql(u8, result.items[0].body, ""));

    try std.testing.expect(result.items[0].children.?.items.len == 2);
    const c = result.items[0].children.?;

    const expected1 = try tele_ast.makeInt(try util.copyString("1", talloc), talloc);
    const expected2 = try tele_ast.makeInt(try util.copyString("42", talloc), talloc);
    try std.testing.expect(tele_ast.equal(c.items[0], expected1));
    try std.testing.expect(tele_ast.equal(c.items[1], expected2));

    tele_ast.freeTeleAst(expected1, talloc);
    tele_ast.freeTeleAst(expected2, talloc);
    tele_ast.freeTeleAstList(result, talloc);
}

test "parse case body multi clause" {
    const parser = try fileToParser("snippets/match_body_multi.tl", talloc);
    defer parser.deinit();

    const result = try parser.parseCaseBody(parser.*.token_queue, 0, 0);
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

    tele_ast.freeTeleAstList(result, test_allocator);
}

test "parse case signature" {
    const parser = try fileToParser("snippets/match_signature.tl", talloc);
    defer parser.deinit();

    const result = try parser.parseCaseSignature(parser.*.token_queue, 0, 0);

    const expected = try tele_ast.makeInt(try util.copyString("1", talloc), talloc);
    try std.testing.expect(tele_ast.equal(result, expected));

    tele_ast.freeTeleAst(expected, talloc);
    tele_ast.freeTeleAst(result, talloc);
}

test "parse case clause signature" {
    const parser = try fileToParser("snippets/case_clause_signature.tl", talloc);
    defer parser.deinit();

    var n: usize = 0;
    const result = try parser.parseCaseClauseSignature(parser.*.token_queue, &n);

    const expected = try tele_ast.makeInt(try util.copyString("1", talloc), talloc);
    try std.testing.expect(tele_ast.equal(result.items[0], expected));

    tele_ast.freeTeleAst(expected, talloc);
    tele_ast.freeTeleAstList(result, talloc);
}

test "parse case clause body single line" {
    const parser = try fileToParser("snippets/case_clause_body_single.tl", talloc);
    defer parser.deinit();

    const result = try parser.parseCaseClauseBody(parser.token_queue, 0, 0);
    try std.testing.expect(result.items.len == 1);

    const expected = try tele_ast.makeInt(try util.copyString("42", talloc), talloc);
    try std.testing.expect(tele_ast.equal(result.items[0], expected));

    tele_ast.freeTeleAst(expected, talloc);
    tele_ast.freeTeleAstList(result, talloc);
}

test "parse case clause body multi line" {
    const parser = try fileToParser("snippets/case_clause_body_multi.tl", talloc);
    defer parser.deinit();

    const result = try parser.parseCaseClauseBody(parser.*.token_queue, 0, 0);
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

    tele_ast.freeTeleAstList(result, test_allocator);
}

test "parse else expression" {
    const parser = try fileToParser("snippets/else.tl", talloc);
    defer parser.deinit();

    const result = try parser.parseElseExpression(parser.*.token_queue);
    errdefer tele_ast.freeTeleAst(result, talloc);

    try std.testing.expect(result.*.ast_type == TeleAstType.else_exp);
    try std.testing.expect(result.*.children != null);
    try std.testing.expect(result.*.children.?.items.len == 2);

    const c1 = result.*.children.?.items[0];
    try std.testing.expect(c1.*.ast_type == TeleAstType.case_clause);
    try std.testing.expect(c1.*.children != null);
    try std.testing.expect(c1.*.children.?.items.len == 2);
    try std.testing.expect(c1.*.children.?.items[0].ast_type == TeleAstType.int);
    try std.testing.expect(std.mem.eql(u8, c1.*.children.?.items[0].body, "42"));
    try std.testing.expect(c1.*.children.?.items[1].ast_type == TeleAstType.atom);
    try std.testing.expect(std.mem.eql(u8, c1.*.children.?.items[1].body, "'ok"));

    const c2 = result.*.children.?.items[1];
    try std.testing.expect(c2.*.ast_type == TeleAstType.case_clause);
    try std.testing.expect(c2.*.children != null);
    try std.testing.expect(c2.*.children.?.items.len == 2);
    try std.testing.expect(c2.*.children.?.items[0].ast_type == TeleAstType.variable);
    try std.testing.expect(std.mem.eql(u8, c2.*.children.?.items[0].body, "x"));
    try std.testing.expect(c2.*.children.?.items[1].ast_type == TeleAstType.variable);
    try std.testing.expect(std.mem.eql(u8, c2.*.children.?.items[1].body, "x"));

    tele_ast.freeTeleAst(result, talloc);
}

test "parse maybe expression" {
    const parser = try fileToParser("snippets/maybe.tl", talloc);
    defer parser.deinit();

    const result = try parser.parseMaybeExpression(parser.*.token_queue);
    errdefer tele_ast.freeTeleAst(result, talloc);

    try std.testing.expect(result.*.ast_type == TeleAstType.maybe_exp);

    tele_ast.freeTeleAst(result, talloc);
}

fn extractRecordName(name: []const u8, allocator: std.mem.Allocator) ![]const u8 {
    const buf = try allocator.alloc(u8, name.len - 2);
    var i: usize = 1;
    while (i < name.len - 1) {
        buf[i - 1] = name[i];
        i += 1;
    }

    return buf;
}

test "extract record name" {
    const result = try extractRecordName("#foo(", test_allocator);
    try std.testing.expect(std.mem.eql(u8, result, "foo"));
    test_allocator.free(result);
}

fn checkParenStartPeek(token_queue: *TokenQueue) !bool {
    const peek_node = try token_queue.peek();
    return isParenStart(peek_node.*.body);
}

test "check paren start peek" {
    const token_queue = try TokenQueue.init(test_allocator);
    defer token_queue.deinit();
    try token_queue.push(try util.copyString("(", test_allocator), 0, 0);

    try std.testing.expect(try checkParenStartPeek(token_queue));

    const n = try token_queue.pop();
    test_allocator.free(n.*.body);
    test_allocator.destroy(n);

    try token_queue.push(try util.copyString("foo", test_allocator), 0, 0);

    try std.testing.expect(!try checkParenStartPeek(token_queue));
}

fn checkOperatorPeek(token_queue: *TokenQueue) !bool {
    const peek_node = try token_queue.peek();
    return isOperator(peek_node.*.body);
}

test "check operator peek" {
    const token_queue = try TokenQueue.init(test_allocator);
    defer token_queue.deinit();
    try token_queue.push(try util.copyString("+", test_allocator), 0, 0);

    try std.testing.expect(try checkOperatorPeek(token_queue));

    const n = try token_queue.pop();
    test_allocator.free(n.*.body);
    test_allocator.destroy(n);

    try token_queue.push(try util.copyString("foo", test_allocator), 0, 0);

    try std.testing.expect(!try checkOperatorPeek(token_queue));
}

fn parenExpToFunctionSignature(paren_exp: *TeleAst, allocator: std.mem.Allocator) !*TeleAst {
    if (paren_exp.*.ast_type != TeleAst.paren_exp) {
        tele_error.setErrorMessage(paren_exp.*.line, paren_exp.*.col, tele_error.ErrorType.invalid_expression);
        return ParserError.ParsingFailure;
    }
    const t = try allocator.create(TeleAst);
    t.*.body = "";
    t.*.ast_type = TeleAstType.function_signature;
    t.*.children = null;
    t.*.col = paren_exp.*.col;
    errdefer tele_ast.freeTeleAst(t, allocator);

    if (paren_exp.*.children != null) {
        var children = std.ArrayList(*TeleAst).init(allocator);
        errdefer tele_ast.freeTeleAstList(children, allocator);

        for (paren_exp.*.children.?.items) |c| {
            try children.append(c);
        }
        t.*.children = children;
    }
    return t;
}

fn isStatementKeyword(buf: []const u8) bool {
    if (buf[0] == 's') {
        return isSpecKeyword(buf);
    }

    if (buf[0] == 'f') {
        return isFunKeyword(buf) or isFunpKeyword(buf);
    }

    if (buf[0] == 't') {
        return isTypeKeyword(buf) or isTestKeyword(buf) or isTypepKeyword(buf);
    }

    if (buf[0] == 'r') {
        return isRecordKeyword(buf);
    }

    if (buf[0] == 'b') {
        return isBehaviourKeyword(buf);
    }

    if (buf[0] == 'i') {
        return isIncludeLibKeyword(buf) or isImportKeyword(buf);
    }

    if (buf[0] == 'm') {
        return isModuledocKeyword(buf);
    }

    if (buf[0] == 'o') {
        return isOnLoadKeyword(buf) or isOpaqueKeyword(buf);
    }

    if (buf[0] == 'n') {
        return isNifsKeyword(buf) or isNominalKeyword(buf);
    }

    if (buf[0] == 'd') {
        return isDocKeyword(buf) or isDefineKeyword(buf);
    }

    if (buf[0] == 'c') {
        return isCallbackKeyword(buf);
    }

    return false;
}

test "is statement keyword" {
    try std.testing.expect(isStatementKeyword("fun"));
    try std.testing.expect(isStatementKeyword("funp"));
    try std.testing.expect(isStatementKeyword("spec"));
    try std.testing.expect(isStatementKeyword("type"));
    try std.testing.expect(isStatementKeyword("record"));
    try std.testing.expect(isStatementKeyword("behaviour"));
    try std.testing.expect(isStatementKeyword("import"));
    try std.testing.expect(isStatementKeyword("nifs"));
    try std.testing.expect(isStatementKeyword("callback"));
    try std.testing.expect(isStatementKeyword("include_lib"));
    try std.testing.expect(isStatementKeyword("doc"));
    try std.testing.expect(isStatementKeyword("moduledoc"));
    try std.testing.expect(isStatementKeyword("define"));
    try std.testing.expect(isStatementKeyword("opaque"));
    try std.testing.expect(isStatementKeyword("test"));
    try std.testing.expect(isStatementKeyword("on_load"));
    try std.testing.expect(isStatementKeyword("typep"));
    try std.testing.expect(isStatementKeyword("nominal"));

    try std.testing.expect(!isStatementKeyword("["));
    try std.testing.expect(!isStatementKeyword("]"));
    try std.testing.expect(!isStatementKeyword("try"));
    try std.testing.expect(!isStatementKeyword("match"));
    try std.testing.expect(!isStatementKeyword("catch"));
    try std.testing.expect(!isStatementKeyword("("));
    try std.testing.expect(!isStatementKeyword(")"));
    try std.testing.expect(!isStatementKeyword("include"));
}

fn isTypeKeyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "type");
}

fn isTypepKeyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "typep");
}

fn isOpaqueKeyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "opaque");
}

fn isNominalKeyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "nominal");
}

fn isSpecKeyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "spec");
}

fn isFunKeyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "fun");
}

fn isFunpKeyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "funp");
}

fn isRecordKeyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "record");
}

fn isBehaviourKeyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "behaviour");
}

fn isDocKeyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "doc");
}

fn isModuledocKeyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "moduledoc");
}

fn isCallbackKeyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "callback");
}

fn isNifsKeyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "nifs");
}

fn isIncludeLibKeyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "include_lib");
}

fn isDefineKeyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "define");
}

fn isImportKeyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "import");
}

fn isOnLoadKeyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "on_load");
}

fn isWhenKeyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "when");
}

fn isTestKeyword(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "test");
}

test "is keywords" {
    try std.testing.expect(isTypeKeyword("type"));
    try std.testing.expect(isSpecKeyword("spec"));
    try std.testing.expect(isFunKeyword("fun"));
    try std.testing.expect(isFunpKeyword("funp"));
    try std.testing.expect(isRecordKeyword("record"));
    try std.testing.expect(isBehaviourKeyword("behaviour"));
    try std.testing.expect(isDocKeyword("doc"));
    try std.testing.expect(isModuledocKeyword("moduledoc"));
    try std.testing.expect(isCallbackKeyword("callback"));
    try std.testing.expect(isOnLoadKeyword("on_load"));
    try std.testing.expect(isNifsKeyword("nifs"));
    try std.testing.expect(isIncludeLibKeyword("include_lib"));
    try std.testing.expect(isDefineKeyword("define"));
    try std.testing.expect(isImportKeyword("import"));
    try std.testing.expect(isOpaqueKeyword("opaque"));
    try std.testing.expect(isWhenKeyword("when"));
    try std.testing.expect(isTestKeyword("test"));
}

fn isFloat(buf: []const u8) bool {
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
    try std.testing.expect(isFloat("123.0"));
    try std.testing.expect(!isFloat("123"));
    try std.testing.expect(!isFloat("abc.def"));
}

// Make sure to check for float before because int check is not exhaustive
fn isInt(buf: []const u8) bool {
    return std.ascii.isDigit(buf[0]);
}

test "is int" {
    try std.testing.expect(isInt("123"));

    // TODO
    // try std.testing.expect(!is_int("123.0"));
    // try std.testing.expect(is_int("$f"));
}

fn isAtom(buf: []const u8) bool {
    if (buf[0] == '\'') {
        return true;
    } else if (buf[0] == '#') {
        if (buf.len >= 2 and buf[1] == '\'' and buf[buf.len - 1] == '\'') {
            return true;
        }
    }
    return false;
}

test "is atom" {
    try std.testing.expect(isAtom("'foo"));
    try std.testing.expect(!isAtom("foo"));
    try std.testing.expect(isAtom("#'foo bar'"));
    try std.testing.expect(!isAtom("#'foo bar"));
}

fn isString(buf: []const u8) bool {
    if (buf.len < 2) {
        return false;
    }

    return buf[0] == '\"';
}

test "is string" {
    try std.testing.expect(isString("\"foo\""));
    try std.testing.expect(!isString("foo"));
    try std.testing.expect(!isString("<<\"foo\">>"));
}

fn isBinary(buf: []const u8) bool {
    if (buf.len >= 2) {
        return isBitStringStart(buf[0..2]);
    }

    return false;
}

test "is binary" {
    try std.testing.expect(!isBinary("\"foo\""));
    try std.testing.expect(isBinary("<<\"foo\">>"));
    try std.testing.expect(!isBinary("foo"));

    // TODO: More test cases
}

fn isBitStringStart(buf: []const u8) bool {
    if (buf.len != 2) {
        return false;
    }

    return buf[0] == '<' and buf[1] == '<';
}

test "is bit string start" {
    try std.testing.expect(isBitStringStart("<<"));
    try std.testing.expect(!isBitStringStart("<"));
    try std.testing.expect(!isBitStringStart(">>"));
    try std.testing.expect(!isBitStringStart("123"));
}

fn isBitStringEnd(buf: []const u8) bool {
    if (buf.len != 2) {
        return false;
    }

    return buf[0] == '>' and buf[1] == '>';
}

test "is bit string end" {
    try std.testing.expect(isBitStringEnd(">>"));
    try std.testing.expect(!isBitStringEnd("<<"));
    try std.testing.expect(!isBitStringEnd("<"));
    try std.testing.expect(!isBitStringEnd("123"));
}

fn isTupleStart(buf: []const u8) bool {
    return std.mem.eql(u8, "#(", buf);
}

test "is tuple start" {
    try std.testing.expect(isTupleStart("#("));
    try std.testing.expect(!isTupleStart(")"));
    try std.testing.expect(!isTupleStart("{"));
}

fn isFunVal(buf: []const u8) bool {
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
    try std.testing.expect(isFunVal("#foo/2"));
    try std.testing.expect(!isFunVal("foo/2"));
    try std.testing.expect(!isFunVal("#foo2"));

    // TODO
    //try std.testing.expect(!is_fun_val("#foo/o2"));
}

fn isListStart(buf: []const u8) bool {
    return buf[0] == '[';
}

test "is list start" {
    try std.testing.expect(isListStart("["));
    try std.testing.expect(!isListStart("]"));
}

fn isListEnd(buf: []const u8) bool {
    return buf[0] == ']';
}

test "is list end" {
    try std.testing.expect(isListEnd("]"));
    try std.testing.expect(!isListEnd("["));
}

fn isMapStart(buf: []const u8) bool {
    return buf[0] == '{';
}

test "is map start" {
    try std.testing.expect(isMapStart("{"));
    try std.testing.expect(!isMapStart("}"));
}

fn isMapEnd(buf: []const u8) bool {
    return buf[0] == '}';
}

test "is map end" {
    try std.testing.expect(isMapEnd("}"));
    try std.testing.expect(!isMapEnd("{"));
}

fn isParenStart(buf: []const u8) bool {
    return std.mem.eql(u8, "(", buf);
}

test "is paren start" {
    try std.testing.expect(isParenStart("("));
    try std.testing.expect(!isParenStart(")"));
}

fn isParenEnd(buf: []const u8) bool {
    return std.mem.eql(u8, ")", buf);
}

test "is paren end" {
    try std.testing.expect(isParenEnd(")"));
    try std.testing.expect(!isParenEnd("("));
}

fn isRecordStart(buf: []const u8) bool {
    if (buf[0] == '#') {
        if (buf[buf.len - 1] == '(') {
            return true;
        }
    }
    return false;
}

test "is record start" {
    try std.testing.expect(isRecordStart("#foobar("));
    try std.testing.expect(!isRecordStart("foobar("));
}

fn isFun(buf: []const u8) bool {
    return std.mem.eql(u8, buf, "fun");
}

test "is fun" {
    try std.testing.expect(isFun("fun"));
    try std.testing.expect(!isFun("foo"));
}

fn isOperator(buf: []const u8) bool {
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
            } else if (buf.len == 3) {
                if (buf[1] == '/' and buf[2] == '=') {
                    return true;
                } else if (buf[1] == ':' and buf[2] == '=') {
                    return true;
                } else {
                    return false;
                }
            }
        },
        '?' => {
            if (buf.len < 2) {
                return false;
            } else {
                return buf[1] == '=';
            }
        },
        'a' => {
            return std.mem.eql(u8, "and", buf);
        },
        'o' => {
            return std.mem.eql(u8, "or", buf);
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
    try std.testing.expect(isOperator("+"));
    try std.testing.expect(isOperator("*"));
    try std.testing.expect(isOperator("/"));
    try std.testing.expect(isOperator("!"));
    try std.testing.expect(isOperator("-"));
    try std.testing.expect(isOperator("|"));
    try std.testing.expect(isOperator(">"));
    try std.testing.expect(isOperator("<"));
    try std.testing.expect(isOperator("="));
    try std.testing.expect(isOperator("=="));
    try std.testing.expect(isOperator(">="));
    try std.testing.expect(isOperator("<="));
    try std.testing.expect(isOperator("++"));
    try std.testing.expect(isOperator("and"));
    try std.testing.expect(isOperator("or"));
    try std.testing.expect(isOperator("not"));
    try std.testing.expect(isOperator("::"));
    try std.testing.expect(isOperator("=/="));

    try std.testing.expect(!isOperator("==="));
    try std.testing.expect(!isOperator("+++"));
    try std.testing.expect(!isOperator("+="));
    try std.testing.expect(!isOperator("+-"));
    try std.testing.expect(!isOperator("foobar"));
    try std.testing.expect(!isOperator("<<"));
    try std.testing.expect(!isOperator(">>"));
    try std.testing.expect(!isOperator("andalso"));
    try std.testing.expect(!isOperator("orelse"));
}

fn isPipeOperator(buf: []const u8) bool {
    return std.mem.eql(u8, "|", buf);
}

test "is pipe operator" {
    try std.testing.expect(isPipeOperator("|"));
    try std.testing.expect(!isPipeOperator("foobar"));
}

fn isCaseKeyword(buf: []const u8) bool {
    return std.mem.eql(u8, "case", buf);
}

test "is case keyword" {
    try std.testing.expect(isCaseKeyword("case"));
    try std.testing.expect(!isCaseKeyword("foobar"));
}

fn isMaybeKeyword(buf: []const u8) bool {
    return std.mem.eql(u8, "maybe", buf);
}

test "is maybe keyword" {
    try std.testing.expect(isMaybeKeyword("maybe"));
    try std.testing.expect(!isMaybeKeyword("mabye"));
}

fn isElseKeyword(buf: []const u8) bool {
    return std.mem.eql(u8, "else", buf);
}

test "is else keyword" {
    try std.testing.expect(isElseKeyword("else"));
    try std.testing.expect(!isElseKeyword("esle"));
}

fn isReceiveKeyword(buf: []const u8) bool {
    return std.mem.eql(u8, "receive", buf);
}

test "is receive keyword" {
    try std.testing.expect(isReceiveKeyword("receive"));
    try std.testing.expect(!isReceiveKeyword("recieve"));
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

fn isFunctionDefinition(buf: []const u8) bool {
    return std.mem.eql(u8, "fun", buf);
}

test "is function definition" {
    try std.testing.expect(isFunctionDefinition("fun"));
    try std.testing.expect(!isFunctionDefinition("funp"));
    try std.testing.expect(!isFunctionDefinition("def"));
}

fn isPrivFunctionDefinition(buf: []const u8) bool {
    return std.mem.eql(u8, "funp", buf);
}

test "is priv function definition" {
    try std.testing.expect(isPrivFunctionDefinition("funp"));
    try std.testing.expect(!isPrivFunctionDefinition("fun"));
    try std.testing.expect(!isPrivFunctionDefinition("defp"));
}

fn isTypeDef(buf: []const u8) bool {
    return std.mem.eql(u8, "type", buf);
}

test "is type def" {
    try std.testing.expect(isTypeDef("type"));
    try std.testing.expect(!isTypeDef("foobar"));
}

fn isRecordDef(buf: []const u8) bool {
    return std.mem.eql(u8, "record", buf);
}

test "is record def" {
    try std.testing.expect(isRecordDef("record"));
    try std.testing.expect(!isRecordDef("foobar"));
}

fn isSpecDef(buf: []const u8) bool {
    return std.mem.eql(u8, "spec", buf);
}

test "is spec def" {
    try std.testing.expect(isSpecDef("spec"));
    try std.testing.expect(!isSpecDef("foobar"));
}

fn isColon(buf: []const u8) bool {
    return buf[0] == ':';
}

test "is colon" {
    try std.testing.expect(isColon(":"));
    try std.testing.expect(!isColon("!"));
}

fn isComma(buf: []const u8) bool {
    return buf[0] == ',';
}

test "is comma" {
    try std.testing.expect(isComma(","));
    try std.testing.expect(!isComma("!"));
}

fn isEqual(buf: []const u8) bool {
    return buf[0] == '=';
}

test "is equal" {
    try std.testing.expect(isEqual("="));
    try std.testing.expect(!isEqual("-"));
}

fn isSlash(buf: []const u8) bool {
    return buf[0] == '/';
}

test "is slash" {
    try std.testing.expect(isSlash("/"));
    try std.testing.expect(!isSlash("'"));
}

fn containsParenStart(buf: []const u8) bool {
    // Ignore parens in strings
    if (buf[0] == '"') {
        return false;
    }

    for (buf) |c| {
        if (c == '(') {
            return true;
        }
    }
    return false;
}

test "contains paren start" {
    try std.testing.expect(containsParenStart("foo("));
    try std.testing.expect(containsParenStart("foo()"));
    try std.testing.expect(containsParenStart(")("));
    try std.testing.expect(containsParenStart("("));
    try std.testing.expect(!containsParenStart("foo"));
    try std.testing.expect(!containsParenStart("\"(\""));
    try std.testing.expect(!containsParenStart("\"()\""));
}
