const std = @import("std");

var error_message: []const u8 = "";
var column: usize = 0;
var line: usize = 0;
var path: []const u8 = "";

pub const ErrorType = enum { invalid_statement, missing_name, missing_signature, missing_body, unexpected_token, invalid_signature_param, invalid_guard_clause, invalid_definition, invalid_field, invalid_expression, invalid_name, expected_token, invalid_integer, invalid_float };

pub fn printErrorMessage() !void {
    const stderr = std.io.getStdErr().writer();
    if (!std.mem.eql(u8, error_message, "") and !std.mem.eql(u8, path, "")) {
        try stderr.print("{s} Line: {d} Column: {d}\n", .{ error_message, line + 1, column + 1 });
        try stderr.print("In file: {s}\n", .{path});
        try printFileMessage(stderr);
    } else if (!std.mem.eql(u8, path, "")) {
        try stderr.print("In file: {s}\n", .{path});
    }
}

pub fn setErrorMessage(l: usize, c: usize, e: ErrorType) void {
    line = l;
    column = c;

    handleErrorType(e);
}

pub fn setPath(p: []const u8) void {
    path = p;
}

fn printFileMessage(writer: anytype) !void {
    var f = try std.fs.cwd().openFile(path, .{ .mode = .read_only });
    defer f.close();

    const rdr = f.reader();
    var buf: [1024]u8 = undefined;
    var line_count: usize = 0;
    while (try rdr.readUntilDelimiterOrEof(&buf, '\n')) |l| {
        if (line_count == line) {
            try writer.print("{s}\n\n", .{l});
            break;
        }

        line_count += 1;
    }
}

fn handleErrorType(e: ErrorType) void {
    switch (e) {
        .invalid_statement => {
            error_message = "Invalid Statement";
        },
        .missing_name => {
            error_message = "Missing Name";
        },
        .missing_signature => {
            error_message = "Missing Signature";
        },
        .missing_body => {
            error_message = "Missing Body";
        },
        .unexpected_token => {
            error_message = "Unexpected Token";
        },
        .expected_token => {
            error_message = "Missing Expected Token";
        },
        .invalid_signature_param => {
            error_message = "Invalid Signature Param";
        },
        .invalid_guard_clause => {
            error_message = "Invalid Guard Clause";
        },
        .invalid_definition => {
            error_message = "Invalid Definition";
        },
        .invalid_field => {
            error_message = "Invalid Field";
        },
        .invalid_expression => {
            error_message = "Invalid Expression";
        },
        .invalid_name => {
            error_message = "Invalid Name";
        },
        .invalid_integer => {
            error_message = "Invalid Integer";
        },
        .invalid_float => {
            error_message = "Invalid Float";
        },
    }
}
