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

pub fn containsHash(buf: []const u8) bool {
    for (buf) |c| {
        if (c == '#') {
            return true;
        }
    }
    return false;
}

test "contains hash" {
    try std.testing.expect(containsHash("foo#bar.a"));
    try std.testing.expect(!containsHash("foobar"));
}

pub fn validateName(buf: []const u8) bool {
    if (buf.len == 0) {
        return false;
    }

    if (!std.ascii.isAlphabetic(buf[0])) {
        return false;
    }

    for (buf) |c| {
        if (!(std.ascii.isAlphanumeric(c) or c == '_')) {
            return false;
        }
    }

    return true;
}

test "validate name" {
    try std.testing.expect(!validateName("123"));
    try std.testing.expect(!validateName("123foo"));
    try std.testing.expect(!validateName("^foo"));
    try std.testing.expect(!validateName("_foo"));
    try std.testing.expect(!validateName("foo^bar"));
    try std.testing.expect(validateName("foo"));
    try std.testing.expect(validateName("foo_bar"));
    try std.testing.expect(validateName("foo_123"));
}

pub fn validateVariableName(buf: []const u8) bool {
    if (buf.len == 0) {
        return false;
    }

    if (!(std.ascii.isAlphabetic(buf[0]) or buf[0] == '_')) {
        return false;
    }

    for (buf) |c| {
        if (!(std.ascii.isAlphanumeric(c) or c == '_')) {
            return false;
        }
    }

    return true;
}

test "validate variable name" {
    try std.testing.expect(!validateVariableName("123"));
    try std.testing.expect(!validateVariableName("123foo"));
    try std.testing.expect(!validateVariableName("^foo"));
    try std.testing.expect(!validateVariableName("foo^bar"));
    try std.testing.expect(validateVariableName("foo"));
    try std.testing.expect(validateVariableName("foo_bar"));
    try std.testing.expect(validateVariableName("foo_123"));
    try std.testing.expect(validateVariableName("_foo"));
}

pub fn validateFunctionName(buf: []const u8) bool {
    if (buf.len == 0) {
        return false;
    }

    if (!std.ascii.isAlphabetic(buf[0])) {
        return false;
    }

    var has_dot: bool = false;

    for (buf) |c| {
        if (!(std.ascii.isAlphanumeric(c) or c == '_' or c == '.')) {
            return false;
        }

        if (c == '.') {
            // Can't have two dots in name
            if (has_dot) {
                return false;
            }

            has_dot = true;
        }
    }

    return true;
}

test "validate function name" {
    try std.testing.expect(!validateFunctionName("123"));
    try std.testing.expect(!validateFunctionName("123foo"));
    try std.testing.expect(!validateFunctionName("^foo"));
    try std.testing.expect(!validateFunctionName("_foo"));
    try std.testing.expect(!validateFunctionName("foo^bar"));
    try std.testing.expect(!validateFunctionName("foo.bar.baz"));
    try std.testing.expect(validateFunctionName("foo"));
    try std.testing.expect(validateFunctionName("foo_bar"));
    try std.testing.expect(validateFunctionName("foo_123"));
    try std.testing.expect(validateFunctionName("foo.bar"));
    try std.testing.expect(validateFunctionName("f"));
}

pub fn findDot(buf: []const u8) usize {
    var i: usize = 0;
    while (i < buf.len) {
        if (buf[i] == '.') {
            return i;
        }
        i += 1;
    }
    return 0;
}

test "find dot" {
    const result = findDot("foo.bar");
    try std.testing.expect(result == 3);

    const result2 = findDot("foo");
    try std.testing.expect(result2 == 0);
}

pub fn findHash(buf: []const u8) usize {
    var i: usize = 0;
    while (i < buf.len) {
        if (buf[i] == '#') {
            return i;
        }
        i += 1;
    }
    return 0;
}

test "find hash" {
    const result = findHash("foo#bar");
    try std.testing.expect(result == 3);

    const result2 = findHash("foo");
    try std.testing.expect(result2 == 0);
}

pub fn validateRecordVariableName(buf: []const u8) bool {
    const hash_idx = findHash(buf);
    if (hash_idx == 0) {
        return false;
    }

    if (!validateName(buf[0..hash_idx])) {
        return false;
    }

    const point1_idx = findDot(buf);

    if (point1_idx == 0) {
        return false;
    }

    if (!validateName(buf[hash_idx + 1 .. point1_idx])) {
        return false;
    }

    if (!validateName(buf[point1_idx + 1 ..])) {
        return false;
    }

    return true;
}

test "validate record variable name" {
    try std.testing.expect(!validateRecordVariableName("123"));
    try std.testing.expect(!validateRecordVariableName("123foo"));
    try std.testing.expect(!validateRecordVariableName("^foo"));
    try std.testing.expect(!validateRecordVariableName("_foo"));
    try std.testing.expect(!validateRecordVariableName("foo^bar"));
    try std.testing.expect(!validateRecordVariableName("foo.bar"));
    try std.testing.expect(!validateRecordVariableName("#foo"));
    try std.testing.expect(!validateRecordVariableName("#foo("));
    try std.testing.expect(!validateRecordVariableName("p#point"));
    try std.testing.expect(validateRecordVariableName("p#point.x"));
}
