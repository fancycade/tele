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
const test_allocator = std.testing.allocator;

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

        const output_path = arg_it.next() orelse {
            return error.InvalidArgs;
        };

        try compileFile(code_path, output_path, allocator);
    } else if (std.mem.eql(u8, "format", command)) {
        const code_path = arg_it.next() orelse {
            return error.InvalidArgs;
        };

        try formatFile(code_path, allocator);
    } else if (std.mem.eql(u8, "build", command)) {
        try build(allocator);
    } else {
        // TODO: Better error message
        return error.InvalidArgs;
    }
}

fn build(allocator: std.mem.Allocator) !void {
    // Make _build/_tele if not exists
    try std.fs.cwd().makePath("_build/_tele");

    // Compile all tele files in src directory to _build/_tele
    var src = try std.fs.cwd().openDir("src", .{ .iterate = true });
    var walker = try src.walk(allocator);
    defer walker.deinit();

    while (try walker.next()) |entry| {
        if (entry.kind == std.fs.File.Kind.file and isTeleFile(entry.basename)) {
            const input_path = try std.fs.path.join(allocator, &[_][]const u8{ "src", entry.path });
            defer allocator.free(input_path);

            try compileFile(input_path, "_build/_tele/", allocator);
        }
    }

    // Copy rebar.config to _build/_tele/
    const build_tele_dir = try std.fs.cwd().openDir("_build/_tele", .{});
    try std.fs.cwd().copyFile("rebar.config", build_tele_dir, "rebar.config", .{});

    // Append src dirs option to _build/_tele/rebar.config
    const file = try std.fs.cwd().openFile("_build/_tele/rebar.config", .{ .mode = .read_write });
    const stat = try file.stat();
    try file.seekTo(stat.size);

    var w = file.writer();
    _ = try w.write("{src_dirs, [\"src\", \"_build/_tele\"]}.\n");

    // Run rebar3 compile with modified rebar.config
    const argv = [_][]const u8{ "rebar3", "compile" };
    var em = try std.process.getEnvMap(allocator);
    defer em.deinit();
    try em.put("REBAR_CONFIG", "_build/_tele/rebar.config");
    const result = try std.ChildProcess.run(.{ .argv = &argv, .allocator = allocator, .env_map = &em });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);
    std.debug.print("{s}", .{result.stdout});
}

fn isTeleFile(path: []const u8) bool {
    const ext = std.fs.path.extension(path);
    return std.mem.eql(u8, ".tl", ext);
}

fn parseTeleFile(code_path: []const u8, allocator: std.mem.Allocator) !std.ArrayList(*TeleAst) {
    var input_file = try std.fs.cwd().openFile(code_path, .{ .mode = .read_only });
    defer input_file.close();

    const ta = try parser.parse_reader(input_file.reader(), allocator);
    errdefer tast.freeTeleAstList(ta, allocator);

    if (ta.items.len == 0) {
        return ExecutionError.Empty;
    }

    return ta;
}

fn formatFile(code_path: []const u8, allocator: std.mem.Allocator) !void {
    const ta2 = try parseTeleFile(code_path, allocator);
    errdefer tast.freeTeleAstList(ta2, allocator);

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

fn compileFile(code_path: []const u8, output_path: []const u8, allocator: std.mem.Allocator) !void {
    const erlang_path = try erlangName(code_path, output_path, allocator);
    errdefer allocator.free(erlang_path);

    const ta2 = try parseTeleFile(code_path, allocator);
    errdefer tast.freeTeleAstList(ta2, allocator);

    if (ta2.items.len == 0) {
        return ExecutionError.Empty;
    }

    var east_list = std.ArrayList(*const Ast).init(allocator);
    errdefer ast.free_erlang_ast_list(east_list, allocator);

    for (ta2.items) |c| {
        try east_list.append(try compiler.tele_to_erlang(c, allocator));
    }

    // Use code path to make output file name
    var file = try std.fs.cwd().createFile(erlang_path, .{});
    defer file.close();

    var w = file.writer();

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

    var context = Context.init(allocator);
    for (east_list.items) |c| {
        try context.writeAst(w, c, false);
    }

    context.deinit();
    allocator.free(erlang_path);
    tast.freeTeleAstList(ta2, allocator);
    ast.free_erlang_ast_list(east_list, allocator);
    freeFunctionMetadata(metadata, allocator);
}

fn erlangName(path: []const u8, output_prefix: ?[]const u8, allocator: std.mem.Allocator) ![]const u8 {
    const base = std.fs.path.basename(path);

    var buf: []u8 = undefined;
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
}

test "erlang name" {
    const tele_path = "src/foobar/foobar.tl";
    const erlang_path = try erlangName(tele_path, null, test_allocator);
    try std.testing.expect(std.mem.eql(u8, erlang_path, "foobar.erl"));
    test_allocator.free(erlang_path);

    const erlang_path2 = try erlangName(tele_path, "_build/tele/", test_allocator);
    try std.testing.expect(std.mem.eql(u8, erlang_path2, "_build/tele/foobar.erl"));
    test_allocator.free(erlang_path2);

    const erlang_path3 = try erlangName(tele_path, "_build/tele", test_allocator);
    try std.testing.expect(std.mem.eql(u8, erlang_path3, "_build/tele/foobar.erl"));
    test_allocator.free(erlang_path3);
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
