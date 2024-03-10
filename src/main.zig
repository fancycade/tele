const std = @import("std");
const codegen = @import("erlang/codegen.zig");
const ast = @import("erlang/ast.zig");
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

    var file = try std.fs.cwd().createFile("basic.erl", .{});
    defer file.close();

    var w = file.writer();
    try codegen.write_ast(w, &Ast{ .body = "foobar", .ast_type = AstType.atom, .children = null });

    // Write EOL
    _ = try w.write("\n");
}
