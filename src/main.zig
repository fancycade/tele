const std = @import("std");
const codegen = @import("codegen.zig");
const ast = @import("ast.zig");
const Ast = ast.Ast;
const AstType = ast.AstType;
const tokenizer = @import("tokenizer.zig");

pub fn main() !void {
    // Setup memory allocator
    // var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    // const allocator = gpa.allocator();
    // defer {
    //     const leaked = gpa.deinit();
    //     switch (leaked) {
    //         .ok => {},
    //         .leak => {
    //             std.debug.print("Memory leak detected!!!\n", .{});
    //         },
    //     }
    // }

    // const process = std.process;
    // var arg_it = process.args();

    // _ = arg_it.skip();
    // const code_path = arg_it.next() orelse {
    //     return error.InvalidArgs;
    // };

    // var file = try std.fs.cwd().createFile("basic.erl", .{});
    // defer file.close();

    // try codegen.write_ast(file.writer(), &Ast{ .body = "1", .ast_type = AstType.int, .children = null });
}
