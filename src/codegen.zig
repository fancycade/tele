const std = @import("std");
const test_allocator = std.testing.allocator;

pub fn write_module(wtr: anytype, name: []const u8) !void {
    _ = try wtr.write("-module(");
    _ = try wtr.write(name);
    _ = try wtr.write(").\n");
}

test "write module" {
    var list = std.ArrayList(u8).init(test_allocator);
    defer list.deinit();
    _ = try write_module(list.writer(), "basic");
    try std.testing.expect(std.mem.eql(u8, list.items, "-module(basic).\n"));
}
