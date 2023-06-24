const std = @import("std");
const parseInt = std.fmt.parseInt;

test "parse integers" {
    const input = "123 67 89,99";
    const ally = std.testing.allocator;

    var list = std.ArrayList(u32).init(ally);
    // Ensure the list is freed at scope exit.
    // Try commenting out this line!
    defer list.deinit();

    var it = std.mem.tokenize(u8, input, " ,");
    while (it.next()) |num| {
        const n = try parseInt(u32, num, 10);
        try list.append(n);
    }

    const expected = [_]u32{ 123, 67, 89, 99 };

    for (expected, list.items) |exp, actual| {
        try std.testing.expectEqual(exp, actual);
    }
}

test "format hello world" {
    const input =
        \\module X exposing (x)
        \\
        \\
        \\x =
        \\    0
        \\
    ;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const inPtr = try allocator.create([big]u8);
    for (0..big) |i| {
        inPtr[i] = 0;
    }
    for (input, 0..) |byte, i| {
        inPtr[i] = byte;
    }

    const outPtr = try allocator.create([big]u8);
    for (0..big) |i| {
        outPtr[i] = 0;
    }

    const memory = try allocator.create(Memory);

    try format(inPtr.*, outPtr, memory);
}

const ElmChar = enum(u8) {
    newline,
    space,
    exclamationMark,
    doubleQuote,
    hash,
    dollar,
    percentage,
    ampersand,
    singleQuote,
    openParenthesis,
    closeParenthesis,
    star,
    plus,
    comma,
    hyphen,
    fullstop,
    forwardSlash,
    zero,
    one,
    two,
    three,
    four,
    five,
    six,
    seven,
    eight,
    nine,
    colon,
    semiColon,
    lessThan,
    equals,
    greaterThan,
    questionMark,
    at,
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
    I,
    J,
    K,
    L,
    M,
    N,
    O,
    P,
    Q,
    R,
    S,
    T,
    U,
    V,
    W,
    X,
    Y,
    Z,
    openBracket,
    backSlash,
    closeBracket,
    pointUp,
    underscore,
    backtick,
    a,
    b,
    c,
    d,
    e,
    f,
    g,
    h,
    i,
    j,
    k,
    l,
    m,
    n,
    o,
    p,
    q,
    r,
    s,
    t,
    u,
    v,
    w,
    x,
    y,
    z,
    openCurly,
    pipe,
    closeCurly,
    tilde,
    afterEnd,
};

const Memory = struct {
    elmChars: [big]ElmChar,
    tokens: [big]Token,
};

fn format(in: [big]u8, _: *[big]u8, memory: *Memory) !void {
    try makeElmChars(in, &memory.elmChars);
}

fn makeElmChars(in: [big]u8, elmChars: *[big]ElmChar) !void {
    for (in, 0..) |rawChar, i| {
        elmChars.*[i] = try makeElmChar(rawChar);
    }
}

fn makeElmChar(raw: u8) !ElmChar {
    return switch (raw) {
        0 => ElmChar.afterEnd,
        1...9 => error.InvalidChar,
        10 => ElmChar.newline,
        11...31 => error.InvalidChar,
        32 => ElmChar.space,
        33 => ElmChar.exclamationMark,
        34 => ElmChar.doubleQuote,
        35 => ElmChar.hash,
        36 => ElmChar.dollar,
        37 => ElmChar.percentage,
        38 => ElmChar.ampersand,
        39 => ElmChar.singleQuote,
        40 => ElmChar.openParenthesis,
        41 => ElmChar.closeParenthesis,
        42 => ElmChar.star,
        43 => ElmChar.plus,
        44 => ElmChar.comma,
        45 => ElmChar.hyphen,
        46 => ElmChar.fullstop,
        47 => ElmChar.forwardSlash,
        48 => ElmChar.zero,
        49 => ElmChar.one,
        50 => ElmChar.two,
        51 => ElmChar.three,
        52 => ElmChar.four,
        53 => ElmChar.five,
        54 => ElmChar.six,
        55 => ElmChar.seven,
        56 => ElmChar.eight,
        57 => ElmChar.nine,
        58 => ElmChar.colon,
        59 => ElmChar.semiColon,
        60 => ElmChar.lessThan,
        61 => ElmChar.equals,
        62 => ElmChar.greaterThan,
        63 => ElmChar.questionMark,
        64 => ElmChar.at,
        65 => ElmChar.A,
        66 => ElmChar.B,
        67 => ElmChar.C,
        68 => ElmChar.D,
        69 => ElmChar.E,
        70 => ElmChar.F,
        71 => ElmChar.G,
        72 => ElmChar.H,
        73 => ElmChar.I,
        74 => ElmChar.J,
        75 => ElmChar.K,
        76 => ElmChar.L,
        77 => ElmChar.M,
        78 => ElmChar.N,
        79 => ElmChar.O,
        80 => ElmChar.P,
        81 => ElmChar.Q,
        82 => ElmChar.R,
        83 => ElmChar.S,
        84 => ElmChar.T,
        85 => ElmChar.U,
        86 => ElmChar.V,
        87 => ElmChar.W,
        88 => ElmChar.X,
        89 => ElmChar.Y,
        90 => ElmChar.Z,
        91 => ElmChar.openBracket,
        92 => ElmChar.backSlash,
        93 => ElmChar.closeBracket,
        94 => ElmChar.pointUp,
        95 => ElmChar.underscore,
        96 => ElmChar.backtick,
        97 => ElmChar.a,
        98 => ElmChar.b,
        99 => ElmChar.c,
        100 => ElmChar.d,
        101 => ElmChar.e,
        102 => ElmChar.f,
        103 => ElmChar.g,
        104 => ElmChar.h,
        105 => ElmChar.i,
        106 => ElmChar.j,
        107 => ElmChar.k,
        108 => ElmChar.l,
        109 => ElmChar.m,
        110 => ElmChar.n,
        111 => ElmChar.o,
        112 => ElmChar.p,
        113 => ElmChar.q,
        114 => ElmChar.r,
        115 => ElmChar.s,
        116 => ElmChar.t,
        117 => ElmChar.u,
        118 => ElmChar.v,
        119 => ElmChar.w,
        120 => ElmChar.x,
        121 => ElmChar.y,
        122 => ElmChar.z,
        123 => ElmChar.openCurly,
        124 => ElmChar.pipe,
        125 => ElmChar.closeCurly,
        126 => ElmChar.tilde,
        127...255 => error.InvalidChar,
    };
}

const Token = enum {
    module,
};

const big = 1000_000;

pub fn main() void {
    std.debug.print("Hello", .{});
}
