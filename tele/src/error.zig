const std = @import("std");

var error_message: []const u8 = "";
var column: usize = 0;
var line: usize = 0;

pub const ErrorType = enum { invalid_statement, missing_name, missing_signature, missing_body, unexpected_token, invalid_signature_param, invalid_guard_clause, invalid_definition, invalid_field, invalid_expression };

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
    }
}
