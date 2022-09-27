const std = @import("std");

var buf: [1_000_000_000]u8 = undefined;

pub fn main() !void {
    for (buf) |_, i| {
        buf[i] = 1;
    }
    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\n", .{buf[999_999_999]});
}
