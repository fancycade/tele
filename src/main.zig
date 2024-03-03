const std = @import("std");
const codegen = @import("codegen.zig");

pub fn main() !void {
    // Setup memory allocator
    // var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    // const allocator = gpa.allocator();
    // defer {
    //     const leaked = gpa.deinit();
    //     if (leaked) {
    //         std.debug.print("Leaked!\n", .{});
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

    const wtr = file.writer();
    try codegen.write_module(wtr, "basic");
}
