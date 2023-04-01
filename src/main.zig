const std = @import("std");

pub fn main() !void {
    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
    std.debug.print("All your {s} are belong to us.\n", .{"codebase"});

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("Run `zig build test` to run the tests.\n", .{});

    try bw.flush(); // don't forget to flush!
}

test "simple test" {
    try std.testing.expectEqual(2+2, 4);
}

const MAX_VERBATIM = 1000_000;
const MAX_EXPORTS = 2000;
const MAX_BINDS = 10_000;
const MAX_EXPRESSIONS = 100_000;

const Ast = struct {
    verbatim_start: [MAX_VERBATIM]u32,
    verbatim_end: [MAX_VERBATIM]u32,
    num_verbatims: u32,

    has_module_name: bool,
    module_name: u32,

    export_: [MAX_EXPORTS]u32,
    num_exports: u32,
    export_expose: u32[MAX_EXPORTS],
    num_export_expose: u32,
    newline_exports: bool,

    bind_left: [MAX_BINDS]u32,
    bind_right: [MAX_BINDS]u32,
    num_binds: u32,

    is_top_bind: u32[MAX_BINDS],
    num_is_top_binds: u32,

    list_id: u32[MAX_EXPRESSIONS],
    list_item: u32[MAX_EXPRESSIONS],
    num_list_items: u32,

    has_newlines: u32[MAX_EXPRESSIONS],
    num_has_newlines: u32,

    expression_type: u8[MAX_EXPRESSIONS],
    expression_id: u32[MAX_EXPRESSIONS],
    num_expressions: u32,

    next_free_id: u32,
};
