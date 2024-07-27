const std = @import("std");
const test_allocator = std.testing.allocator;

pub fn copyString(value: []const u8, allocator: std.mem.Allocator) ![]const u8 {
    const buf = try allocator.alloc(u8, value.len);
    std.mem.copyForwards(u8, buf, value);
    return buf;
}

test "copyString" {
    const s = "foobar";
    const result = try copyString(s, test_allocator);
    try std.testing.expect(std.mem.eql(u8, s, result));
    test_allocator.free(result);
}
