const std = @import("std");
const codegen = @import("erlang/codegen.zig");
const ast = @import("erlang/ast.zig");
const Ast = ast.Ast;
const AstType = ast.AstType;
const tast = @import("tele/ast.zig");
const TeleAst = tast.Ast;
const tokenizer = @import("tokenizer.zig");
const parser = @import("parser.zig");
const compiler = @import("compiler.zig");

const ExecutionError = error{Empty};

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

    var input_file = try std.fs.cwd().openFile(code_path, .{ .mode = .read_only });
    defer input_file.close();

    const ta = try parser.parse_reader(input_file.reader(), allocator);

    if (ta.items.len == 0) {
        return ExecutionError.Empty;
    }

    //var east_list = std.ArrayList(*const Ast).init(allocator);

    //for (ta.items) |c| {
    //    try east_list.append(try compiler.tele_to_erlang(c, allocator));
    //}

    // Use code path to make output file name
    //var file = try std.fs.cwd().createFile("basic.erl", .{});
    //defer file.close();

    //var w = file.writer();

    //for (east_list.items) |c| {
    //    try codegen.write_ast(w, c);
    //}

    // Write EOL
    //_ = try w.write("\n");

    free_tele_ast_list(ta, allocator);
}

fn free_tele_ast_list(ta: std.ArrayList(*const TeleAst), allocator: std.mem.Allocator) void {
    for (ta.items) |c| {
        if (c.*.body.len > 0) {
            allocator.free(c.*.body);
        }
        if (c.*.children != null) {
            free_tele_ast_list(c.*.children.?, allocator);
        }
        allocator.destroy(c);
    }
    ta.deinit();
}
