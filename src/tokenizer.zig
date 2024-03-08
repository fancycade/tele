const std = @import("std");
const test_allocator = std.testing.allocator;

const Token = union(enum) { lparen, rparen, open_list, close_list, open_tuple, rsquiggle, open_map, quote, equal, plus, minus, divide, multiply, greater, lesser, greater_equal, lesser_equal, open_record: []const u8, whitespace: usize, word: []const u8 };
