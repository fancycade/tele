const std = @import("std");
const codegen = @import("erlang_codegen.zig");
const Context = codegen.Context;
const tele_codegen = @import("tele_codegen.zig");
const TeleContext = tele_codegen.Context;
const ast = @import("erlang_ast.zig");
const Ast = ast.Ast;
const AstType = ast.AstType;
const tast = @import("tele_ast.zig");
const TeleAst = tast.Ast;
const TeleAstType = tast.AstType;
const tokenizer = @import("tokenizer.zig");
const parser = @import("parser.zig");
const compiler = @import("compiler.zig");
const tele_error = @import("error.zig");
const test_allocator = std.testing.allocator;
const util = @import("util.zig");

const ExecutionError = error{Empty};

const FunctionMetadata = struct { name: []const u8, args: usize };

pub fn main() !void {
    // Setup memory allocator
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const leaked = gpa.deinit();
        switch (leaked) {
            .ok => {},
            .leak => {
                std.debug.print("Memory leak detected!!!\n", .{});
            },
        }
    }

    try handleArgs(allocator);
}

fn handleArgs(allocator: std.mem.Allocator) !void {
    const process = std.process;
    var arg_it = process.args();

    _ = arg_it.skip(); // Skip tele

    const command = arg_it.next() orelse {
        return error.InvalidArgs;
    };

    if (std.mem.eql(u8, "compile", command)) {
        const code_path = arg_it.next() orelse {
            return error.InvalidArgs;
        };

        var output_path: ?[:0]const u8 = arg_it.next();

        if (output_path == null) {
            output_path = ".";
        }

        compileFile(code_path, output_path.?, allocator, isTeleHeaderFile(code_path)) catch |e| {
            try tele_error.printErrorMessage();
            return e;
        };
    } else if (std.mem.eql(u8, "format", command)) {
        const code_path = arg_it.next() orelse {
            return error.InvalidArgs;
        };

        formatFile(code_path, allocator) catch |e| {
            try tele_error.printErrorMessage();
            return e;
        };
    } else if (std.mem.eql(u8, "build", command)) {
        try build(allocator, true);
    } else if (std.mem.eql(u8, "test", command)) {
        try build(allocator, false);
        try eunit(allocator);
    } else if (std.mem.eql(u8, "ct", command)) {
        try build(allocator, false);
        try commonTest(allocator);
    } else {
        // TODO: Better error message
        return error.InvalidArgs;
    }
}

fn handleError() !void {
    tele_error.printErrorMessage();
}

fn build(allocator: std.mem.Allocator, erlang_compile: bool) !void {
    // Make _build/_tele if not exists
    try std.fs.cwd().makePath("_build/_tele");

    // Only needed for tests but doesn't hurt to have it
    try std.fs.cwd().makePath("_build/_test");

    try recursiveCompile(".", allocator);

    // Copy rebar.config to _build/_tele/
    const build_tele_dir = try std.fs.cwd().openDir("_build/_tele", .{});
    try std.fs.cwd().copyFile("rebar.config", build_tele_dir, "rebar.config", .{});

    // Append src dirs option to _build/_tele/rebar.config
    const file = try std.fs.cwd().openFile("_build/_tele/rebar.config", .{ .mode = .read_write });
    const stat = try file.stat();
    try file.seekTo(stat.size);

    var w = file.writer();
    _ = try w.write("\n{src_dirs, [\"src\", \"_build/_tele\"]}.\n");

    if (erlang_compile) {
        // Run rebar3 compile with modified rebar.config
        const argv = [_][]const u8{ "rebar3", "compile" };
        var em = try std.process.getEnvMap(allocator);
        defer em.deinit();
        try em.put("REBAR_CONFIG", "_build/_tele/rebar.config");
        const result = try std.process.Child.run(.{ .argv = &argv, .allocator = allocator, .env_map = &em });
        defer allocator.free(result.stdout);
        defer allocator.free(result.stderr);
        std.debug.print("{s}", .{result.stdout});
    }
}

fn recursiveCompile(path: []const u8, allocator: std.mem.Allocator) !void {
    // Compile all tele files into cwd directory to _build/_tele
    var src = try std.fs.cwd().openDir(path, .{ .iterate = true });
    var walker = try src.walk(allocator);
    defer walker.deinit();

    while (try walker.next()) |entry| {
        if (entry.kind == std.fs.File.Kind.file) {
            if (isTeleFile(entry.basename)) {
                const input_path = try std.fs.path.join(allocator, &[_][]const u8{ path, entry.path });
                defer allocator.free(input_path);

                if (!checkPathContains(input_path, "_build")) {
                    if (checkPathContains(input_path, "test")) {
                        const dir = std.fs.path.dirname(input_path);
                        if (dir == null) {
                            compileFile(input_path, "_build/_test/", allocator, false) catch |e| {
                                try tele_error.printErrorMessage();
                                return e;
                            };
                        } else {
                            const output_path = try std.fs.path.join(allocator, &[_][]const u8{ "_build/_test/", dir.? });
                            defer allocator.free(output_path);
                            compileFile(input_path, output_path, allocator, false) catch |e| {
                                try tele_error.printErrorMessage();
                                return e;
                            };
                        }
                    } else {
                        const dir = std.fs.path.dirname(input_path);
                        if (dir == null) {
                            compileFile(input_path, "_build/_tele/", allocator, false) catch |e| {
                                try tele_error.printErrorMessage();
                                return e;
                            };
                        } else {
                            const output_path = try std.fs.path.join(allocator, &[_][]const u8{ "_build/_tele/", dir.? });
                            defer allocator.free(output_path);
                            compileFile(input_path, output_path, allocator, false) catch |e| {
                                try tele_error.printErrorMessage();
                                return e;
                            };
                        }
                    }
                }
            } else if (isTeleHeaderFile(entry.basename)) {
                const input_path = try std.fs.path.join(allocator, &[_][]const u8{ path, entry.path });
                defer allocator.free(input_path);

                if (!checkPathContains(input_path, "_build")) {
                    const dir = std.fs.path.dirname(input_path);
                    if (dir == null) {
                        compileFile(input_path, "_build/_tele/", allocator, true) catch |e| {
                            try tele_error.printErrorMessage();
                            return e;
                        };
                    } else {
                        if (checkPathContains(input_path, "include")) {
                            compileFile(input_path, dir.?, allocator, true) catch |e| {
                                try tele_error.printErrorMessage();
                                return e;
                            };
                        } else {
                            const output_path = try std.fs.path.join(allocator, &[_][]const u8{ "_build/_tele/", dir.? });
                            defer allocator.free(output_path);

                            compileFile(input_path, output_path, allocator, true) catch |e| {
                                try tele_error.printErrorMessage();
                                return e;
                            };
                        }
                    }
                }
            }
        } else if (entry.kind == std.fs.File.Kind.directory) {
            if (!checkPathContains(entry.path, "_build")) {
                if (checkPathContains(entry.path, "test")) {
                    const input_path = try std.fs.path.join(allocator, &[_][]const u8{ "_build/_test/", entry.path });
                    defer allocator.free(input_path);

                    try std.fs.cwd().makePath(input_path);
                } else {
                    const input_path = try std.fs.path.join(allocator, &[_][]const u8{ "_build/_tele/", entry.path });
                    defer allocator.free(input_path);

                    try std.fs.cwd().makePath(input_path);
                }
            }
        }
    }
}

fn checkPathContains(path: []const u8, match_str: []const u8) bool {
    var it = std.mem.split(u8, path, "/");
    while (it.next()) |x| {
        if (std.mem.eql(u8, x, match_str)) {
            return true;
        }
    }
    return false;
}

test "check path contains" {
    try std.testing.expect(checkPathContains("./_build/src/", "_build"));
    try std.testing.expect(!checkPathContains("foobar", "_build"));
}

fn eunit(allocator: std.mem.Allocator) !void {
    // Run rebar3 eunit with modified rebar.config
    const argv = [_][]const u8{ "rebar3", "eunit" };
    var em = try std.process.getEnvMap(allocator);
    defer em.deinit();
    try em.put("REBAR_CONFIG", "_build/_tele/rebar.config");
    const result = try std.process.Child.run(.{ .argv = &argv, .allocator = allocator, .env_map = &em });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);
    std.debug.print("{s}", .{result.stdout});
}

fn commonTest(allocator: std.mem.Allocator) !void {
    // Run rebar3 common test with modified rebar.config and _build/_test as dir param
    const argv = [_][]const u8{ "rebar3", "ct", "--dir", "_build/_test" };
    var em = try std.process.getEnvMap(allocator);
    defer em.deinit();
    try em.put("REBAR_CONFIG", "_build/_tele/rebar.config");
    const result = try std.process.Child.run(.{ .argv = &argv, .allocator = allocator, .env_map = &em });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);
    std.debug.print("{s}", .{result.stdout});
}

fn isTeleFile(path: []const u8) bool {
    const ext = std.fs.path.extension(path);
    return std.mem.eql(u8, ".tl", ext);
}

test "is tele file" {
    try std.testing.expect(isTeleFile("foo.tl"));
    try std.testing.expect(isTeleFile("./foo/bar/baz.tl"));
    try std.testing.expect(!isTeleFile("baz.erl"));
}

fn isTeleHeaderFile(path: []const u8) bool {
    const ext = std.fs.path.extension(path);
    return std.mem.eql(u8, ".htl", ext);
}

test "is tele header file" {
    try std.testing.expect(isTeleHeaderFile("foo.htl"));
    try std.testing.expect(isTeleHeaderFile("./foo/bar/foo.htl"));
    try std.testing.expect(!isTeleHeaderFile("baz.erl"));
}

fn parseTeleFile(code_path: []const u8, allocator: std.mem.Allocator) !std.ArrayList(*TeleAst) {
    var input_file = try std.fs.cwd().openFile(code_path, .{ .mode = .read_only });
    defer input_file.close();

    const ta = try parser.parseReader(input_file.reader(), allocator);
    errdefer tast.freeTeleAstList(ta, allocator);

    if (ta.items.len == 0) {
        return ExecutionError.Empty;
    }

    return ta;
}

fn formatFile(code_path: []const u8, allocator: std.mem.Allocator) !void {
    const ta2 = try parseTeleFile(code_path, allocator);
    errdefer tast.freeTeleAstList(ta2, allocator);

    tele_error.setPath(code_path);

    if (ta2.items.len == 0) {
        return ExecutionError.Empty;
    }

    var tfile = try std.fs.cwd().createFile(code_path, .{});
    defer tfile.close();

    var tcontext = TeleContext.init(allocator);
    defer tcontext.deinit();

    const tw = tfile.writer();
    for (ta2.items) |c| {
        try tcontext.writeAst(tw, c);
    }

    tast.freeTeleAstList(ta2, allocator);
}

fn compileFile(code_path: []const u8, output_path: []const u8, allocator: std.mem.Allocator, header: bool) !void {
    const erlang_path = try erlangName(code_path, output_path, allocator, header);
    errdefer allocator.free(erlang_path);

    tele_error.setPath(code_path);

    const ta2 = try parseTeleFile(code_path, allocator);
    errdefer tast.freeTeleAstList(ta2, allocator);

    if (ta2.items.len == 0) {
        return ExecutionError.Empty;
    }

    var east_list = std.ArrayList(*const Ast).init(allocator);
    errdefer ast.free_erlang_ast_list(east_list, allocator);

    for (ta2.items) |c| {
        try east_list.append(try compiler.teleToErlang(c, allocator));
    }

    // Use code path to make output file name
    var file = try std.fs.cwd().createFile(erlang_path, .{});
    defer file.close();

    var w = file.writer();

    if (!header) {
        _ = try w.write("-module(");
        _ = try w.write(std.fs.path.basename(erlang_path[0 .. erlang_path.len - 4]));
        _ = try w.write(").\n");
        _ = try w.write("-export([");

        const metadata = try scanFunctionMetadata(ta2, allocator);

        var ctr: usize = 0;
        for (metadata.items) |m| {
            try writeFunctionMetadata(w, m);
            if (ctr < metadata.items.len - 1) {
                _ = try w.write(", ");
            }

            ctr += 1;
        }

        _ = try w.write("]).\n");
        _ = try w.write("\n");

        freeFunctionMetadata(metadata, allocator);
    }

    var context = Context.init(allocator);
    for (east_list.items) |c| {
        try context.writeAst(w, c, false);
    }

    context.deinit();
    allocator.free(erlang_path);
    tast.freeTeleAstList(ta2, allocator);
    ast.free_erlang_ast_list(east_list, allocator);
}

fn erlangName(path: []const u8, output_prefix: ?[]const u8, allocator: std.mem.Allocator, header: bool) ![]const u8 {
    const base = std.fs.path.basename(path);

    var buf: []u8 = undefined;
    if (!header) {
        if (output_prefix == null) {
            buf = try allocator.alloc(u8, base.len + 1);
            std.mem.copyForwards(u8, buf, base[0 .. base.len - 2]);
        } else if (output_prefix.?[output_prefix.?.len - 1] != '/') { // TODO: Make work for windows
            buf = try allocator.alloc(u8, (output_prefix.?.len + 1) + (base.len + 1));
            std.mem.copyForwards(u8, buf, output_prefix.?);
            buf[output_prefix.?.len] = '/';
            std.mem.copyForwards(u8, buf[output_prefix.?.len + 1 .. buf.len], base[0 .. base.len - 2]);
        } else {
            buf = try allocator.alloc(u8, output_prefix.?.len + base.len + 1);
            std.mem.copyForwards(u8, buf, output_prefix.?);
            std.mem.copyForwards(u8, buf[output_prefix.?.len..buf.len], base[0 .. base.len - 2]);
        }
        std.mem.copyForwards(u8, buf[buf.len - 3 .. buf.len], "erl");

        return buf;
    } else {
        if (output_prefix == null) {
            buf = try allocator.alloc(u8, base.len);
            std.mem.copyForwards(u8, buf, base[0 .. base.len - 3]);
        } else if (output_prefix.?[output_prefix.?.len - 1] != '/') {
            buf = try allocator.alloc(u8, output_prefix.?.len + base.len + 1);
            std.mem.copyForwards(u8, buf, output_prefix.?);
            buf[output_prefix.?.len] = '/';
            std.mem.copyForwards(u8, buf[output_prefix.?.len + 1 .. buf.len], base[0 .. base.len - 3]);
        } else {
            buf = try allocator.alloc(u8, output_prefix.?.len + base.len);
            std.mem.copyForwards(u8, buf, output_prefix.?);
            std.mem.copyForwards(u8, buf[output_prefix.?.len..buf.len], base[0 .. base.len - 3]);
        }
        std.mem.copyForwards(u8, buf[buf.len - 3 .. buf.len], "hrl");
        return buf;
    }
}

test "erlang name" {
    const tele_path = "src/foobar/foobar.tl";
    const erlang_path = try erlangName(tele_path, null, test_allocator, false);
    try std.testing.expect(std.mem.eql(u8, erlang_path, "foobar.erl"));
    test_allocator.free(erlang_path);

    const erlang_path2 = try erlangName(tele_path, "_build/tele/", test_allocator, false);
    try std.testing.expect(std.mem.eql(u8, erlang_path2, "_build/tele/foobar.erl"));
    test_allocator.free(erlang_path2);

    const erlang_path3 = try erlangName(tele_path, "_build/tele", test_allocator, false);
    try std.testing.expect(std.mem.eql(u8, erlang_path3, "_build/tele/foobar.erl"));
    test_allocator.free(erlang_path3);

    const tele_header_path = "src/foobar/foobar.htl";
    const erlang_path4 = try erlangName(tele_header_path, null, test_allocator, true);
    try std.testing.expect(std.mem.eql(u8, erlang_path4, "foobar.hrl"));
    test_allocator.free(erlang_path4);

    const erlang_path5 = try erlangName(tele_header_path, "_build/tele/", test_allocator, true);
    try std.testing.expect(std.mem.eql(u8, erlang_path5, "_build/tele/foobar.hrl"));
    test_allocator.free(erlang_path5);

    const erlang_path6 = try erlangName(tele_header_path, "_build/tele", test_allocator, true);
    try std.testing.expect(std.mem.eql(u8, erlang_path6, "_build/tele/foobar.hrl"));
    test_allocator.free(erlang_path6);
}

fn writeFunctionMetadata(w: anytype, md: *const FunctionMetadata) !void {
    _ = try w.write(md.*.name);
    _ = try w.write("/");
    var buf: [24]u8 = undefined;
    const str = try std.fmt.bufPrint(&buf, "{}", .{md.*.args});
    _ = try w.write(str);
}

fn scanFunctionMetadata(ta: std.ArrayList(*TeleAst), allocator: std.mem.Allocator) !std.ArrayList(*const FunctionMetadata) {
    var metadata = std.ArrayList(*const FunctionMetadata).init(allocator);

    for (ta.items) |t| {
        if (t.*.ast_type == TeleAstType.function_def) {
            const md = try allocator.create(FunctionMetadata);

            const buf = try allocator.alloc(u8, t.*.body.len);
            std.mem.copyForwards(u8, buf, t.*.body);
            md.*.name = buf;

            if (t.*.children == null) {
                md.*.args = 0;
            } else if (t.*.children.?.items[0].*.ast_type == TeleAstType.function_signature) {
                const c = t.*.children.?.items[0];
                if (c.*.children == null) {
                    md.*.args = 0;
                } else {
                    var count: usize = 0;
                    for (c.*.children.?.items) |ch| {
                        if (ch.ast_type != TeleAstType.guard_clause) {
                            count += 1;
                        }
                    }
                    md.*.args = count;
                }
            }

            try metadata.append(md);
        }
    }

    return metadata;
}

fn freeFunctionMetadata(metadata: std.ArrayList(*const FunctionMetadata), allocator: std.mem.Allocator) void {
    for (metadata.items) |m| {
        allocator.free(m.name);
        allocator.destroy(m);
    }

    metadata.deinit();
}
