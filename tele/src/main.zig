const std = @import("std");
const codegen = @import("erlang/codegen.zig");
const Context = codegen.Context;
const tele_codegen = @import("tele/codegen.zig");
const TeleContext = tele_codegen.Context;
const ast = @import("erlang/ast.zig");
const Ast = ast.Ast;
const AstType = ast.AstType;
const tast = @import("tele/ast.zig");
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

    const process = std.process;
    var arg_it = process.args();

    _ = arg_it.skip();
    const code_path = arg_it.next() orelse {
        return error.InvalidArgs;
    };

    var output_path: ?[]const u8 = null;
    output_path = arg_it.next();

    const erlang_path = try erlang_name(code_path, output_path, allocator);
    errdefer allocator.free(erlang_path);

    var input_file = try std.fs.cwd().openFile(code_path, .{ .mode = .read_only });
    defer input_file.close();

    var ta = try parser.parse_reader(input_file.reader(), allocator);

    if (ta.items.len == 0) {
        return ExecutionError.Empty;
    }

    const ta2 = try compiler.preprocess(&ta, allocator);

    if (ta2.items.len == 0) {
        return ExecutionError.Empty;
    }

    var east_list = std.ArrayList(*const Ast).init(allocator);

    for (ta2.items) |c| {
        try east_list.append(try compiler.tele_to_erlang(c, allocator));
    }

    // Generate formatted tele file
    // var tfile = try std.fs.cwd().createFile("other.tl", .{});
    // defer tfile.close();
    // var tcontext = TeleContext.init(allocator);
    // defer tcontext.deinit();
    // const tw = tfile.writer();
    // for (ta2.items) |c| {
    //    try tcontext.write_ast(tw, c);
    // }

    // Use code path to make output file name
    var file = try std.fs.cwd().createFile(erlang_path, .{});
    defer file.close();

    var w = file.writer();

    _ = try w.write("-module(");
    _ = try w.write(erlang_path[0 .. erlang_path.len - 4]);
    _ = try w.write(").\n");
    _ = try w.write("-export([");

    const metadata = try scan_function_metadata(ta2, allocator);

    var ctr: usize = 0;
    for (metadata.items) |m| {
        try write_function_metadata(w, m);
        if (ctr < metadata.items.len - 1) {
            _ = try w.write(", ");
        }

        ctr += 1;
    }

    _ = try w.write("]).\n");
    _ = try w.write("\n");

    var context = Context.init(allocator);
    for (east_list.items) |c| {
        try context.write_ast(w, c);
    }

    context.deinit();
    allocator.free(erlang_path);
    ta.deinit();
    tast.free_tele_ast_list(ta2, allocator);
    free_erlang_ast_list(east_list, allocator);
    free_function_metadata(metadata, allocator);
}

fn free_erlang_ast_list(ta: std.ArrayList(*const Ast), allocator: std.mem.Allocator) void {
    for (ta.items) |c| {
        if (c.*.body.len > 0) {
            allocator.free(c.*.body);
        }
        if (c.*.children != null) {
            free_erlang_ast_list(c.*.children.?, allocator);
        }
        allocator.destroy(c);
    }
    ta.deinit();
}

fn erlang_name(path: []const u8, output_prefix: ?[]const u8, allocator: std.mem.Allocator) ![]const u8 {
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
    const erlang_path = try erlang_name(tele_path, "", test_allocator);
    try std.testing.expect(std.mem.eql(u8, erlang_path, "foobar.erl"));
    test_allocator.free(erlang_path);

    const erlang_path2 = try erlang_name(tele_path, "_build/tele/", test_allocator);
    try std.testing.expect(std.mem.eql(u8, erlang_path2, "_build/tele/foobar.erl"));
    test_allocator.free(erlang_path2);

    const erlang_path3 = try erlang_name(tele_path, "_build/tele", test_allocator);
    try std.testing.expect(std.mem.eql(u8, erlang_path3, "_build/tele/foobar.erl"));
    test_allocator.free(erlang_path3);
}

fn write_function_metadata(w: anytype, md: *const FunctionMetadata) !void {
    _ = try w.write(md.*.name);
    _ = try w.write("/");
    var buf: [24]u8 = undefined;
    const str = try std.fmt.bufPrint(&buf, "{}", .{md.*.args});
    _ = try w.write(str);
}

fn scan_function_metadata(ta: std.ArrayList(*TeleAst), allocator: std.mem.Allocator) !std.ArrayList(*const FunctionMetadata) {
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
                    md.*.args = c.*.children.?.items.len;
                }
            }

            try metadata.append(md);
        }
    }

    return metadata;
}

fn free_function_metadata(metadata: std.ArrayList(*const FunctionMetadata), allocator: std.mem.Allocator) void {
    for (metadata.items) |m| {
        allocator.free(m.name);
        allocator.destroy(m);
    }

    metadata.deinit();
}
