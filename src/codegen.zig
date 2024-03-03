const std = @import("std");
const fs = std.fs;
const test_allocator = std.testing.allocator;

pub const Codegen = struct {
    const Self = @This();

    writer: fs.File.Writer,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, writer: anytype) Self {
        return Self{ .writer = writer, .allocator = allocator };
    }

    pub fn module(self: *Self, name: []const u8) !void {
        try write_module(self.writer, name);
    }
};

// Need to make static functions for easy testing. Wrap these for public API in Codegen struct

fn write_module(w: anytype, name: []const u8) !void {
    _ = try w.write("-module(");
    _ = try w.write(name);
    _ = try w.write(").\n");
}

test "write module" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();
    try write_module(list.writer(), "basic");
    try std.testing.expect(std.mem.eql(u8, list.items, "-module(basic).\n"));
}
