const std = @import("std");

var error_message: []const u8 = "";
var column: usize = 0;
var line: usize = 0;

pub const ErrorType = enum { invalid_statement };

pub fn printErrorMessage() !void {
    const stderr = std.io.getStdErr().writer();
    try stderr.print("Line: {d} Column: {d}\n{s}\n", .{ line + 1, column + 1, error_message });
}

pub fn setErrorMessage(l: usize, c: usize, e: ErrorType) void {
    line = l;
    column = c;

    handleErrorType(e);
}

fn handleErrorType(e: ErrorType) void {
    switch (e) {
        .invalid_statement => {
            error_message = "Invalid Statement";
        },
    }
}
