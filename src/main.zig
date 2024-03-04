const std = @import("std");
const Codegen = @import("codegen.zig").Codegen;

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

    _ = Codegen.init(file.writer());
}
