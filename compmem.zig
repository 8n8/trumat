const std = @import("std");
const parseInt = std.fmt.parseInt;

const big = 1000_000;

const total = buf_total();

pub fn main() void {
    std.debug.print("total: {}\n", .{total});
}

fn buf_total() u32 {
    var buffer: [big]u8 = undefined;
    @setEvalBranchQuota(big*3);
    inline for (0..big) |i| {
        buffer[i] = (i % 256) / 2;
    }
    var sum: u32 = 0;
    for (buffer) |byte| {
        sum = sum + byte;
    }
    return sum;
}
