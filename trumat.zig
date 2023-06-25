const std = @import("std");
const parseInt = std.fmt.parseInt;

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

    for (input, 0..) |in, i| {
        try std.testing.expectEqual(in, outPtr[i]);
    }
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
    subsequentUtf8,
};

const Memory = struct {
    elmChars: [big]ElmChar,
};

const StateTag = enum {
    startModule,
    moduleStartsWithM,
};
const State = union(StateTag) {
    startModule: void,
    moduleStartsWithM: void,
};

const Step = struct {
    state: State,
    moveTo: u32,
    action: Action,
};
const ActionTag = enum {
    commit,
    none,
};
const Action = union(ActionTag) {
    commit: []u8,
    none: void,
};

fn makeStep(state: State, char: ElmChar, i: u32) Step {
    return switch (state) {
    .startModule => stepStartModule(char, i),
    .moduleStartsWithM => stepModuleStartsWithM(char, i),
    };
}

fn stepModuleStartsWithM(char: ElmChar, i: u32) Step {
    return switch(char) {
    .o => .{.state = .moduleStartsWithMo, .moveTo = i + 1, .action = .none},

    .exclamationMark, .hash, .dollar, .percentage, .ampersand, .subsequentUtf8, .afterEnd, .tilde, .forwardSlash, .fullstop, .comma, .plus, .star, .closeParenthesis, .closeCurly, .pipe, .backtick, .pointUp, .closeBracket, .backSlash, .openBracket, .at, .questionMark, .greaterThan, .lessThan, .semiColon, .nine => .{.state = .failed, .moveTo = i, .action = .none},

    .openCurly => { .state = .getTopLevelNamesForExport { "m" }, .moveTo = i + 1
    };
}

fn stepStartModule(char: ElmChar, i: u32) Step {
    return switch(char) {
    .a, .b, .c, .d, .e, .f, .g, .h, .i, .j, .k, .l, .n, .o, .p, .q, .r, .s, .t, .u, .v, .w, .x, .y, .z, .underscore =>
        .{ .state = .getTopLevelNamesForExport {}, .moveTo = i, .action = .none },
    .subsequentUtf8, .comma, .plus, .star, .closeParenthesis, .openParenthesis, .singleQuote, .ampersand, .percentage, .dollar, .hash, .doubleQuote, .exclamationMark, .backtick, .pointUp, .closeBracket, .backSlash, .openBracket, .at, .questionMark, .greaterThan, .equals, .lessThan, .semiColon, .colon, .nine, .eight, .seven, .six, .five, .four, .three, .two, .one, .zero, .forwardSlash, .fullstop, .A, .B, .C, .D, .E, .F, .G, .H, .I, .J, .K, .L, .M, .N, .O, .P, .Q, .R, .S, .T, .U, .V, .W, .X, .Y, .Z, .afterEnd, .tilde, .closeCurly, .pipe =>
        .{ .state = .failed, .moveTo = i, .action = .none },
    .m =>
        .{ .state = .moduleStartsWithM, .moveTo = i + 1, .action = .none },
    .openCurly =>
        .{ .state = .moduleStartsWithOpenCurly, .moveTo = i + 1, .action = .none },
    .hyphen =>
        .{ .state = .moduleStartsWithHyphen, .moveTo = i + 1, .action = .none },

    .space =>
        .{ .state = .moduleStartsWithSpace, .moveTo = i + 1, .action = .none },
    .newline =>
        .{ .state = .startModule, .moveTo = i + 1, .action = .none },
    };
}

fn format(in: [big]u8, out: *[big]u8, memory: *Memory) !void {
    try makeElmChars(in, &memory.elmChars);

    var outI: u32 = 0;
    var state: State = .startModule;
    var inI: u32 = 0;
    while (true) {
        const step: Step = makeStep(state, memory.elmChars[inI], inI);
        inI = step.moveTo;
        state = step.state;
        switch (step.action) {
        .commit => |formattedFragment| {
            for (formattedFragment) |byte| {
                out[outI] = byte;
                outI = outI + 1;
            }
        },
        .none => {},
        }
    }
}

fn makeElmChars(in: [big]u8, elmChars: *[big]ElmChar) !void {
    for (in, 0..) |rawChar, i| {
        elmChars.*[i] = try makeElmChar(rawChar);
    }
}

fn makeElmChar(raw: u8) !ElmChar {
    return switch (raw) {
        0 => .afterEnd,
        1...9 => error.InvalidChar,
        10 => .newline,
        11...31 => error.InvalidChar,
        32 => .space,
        33 => .exclamationMark,
        34 => .doubleQuote,
        35 => .hash,
        36 => .dollar,
        37 => .percentage,
        38 => .ampersand,
        39 => .singleQuote,
        40 => .openParenthesis,
        41 => .closeParenthesis,
        42 => .star,
        43 => .plus,
        44 => .comma,
        45 => .hyphen,
        46 => .fullstop,
        47 => .forwardSlash,
        48 => .zero,
        49 => .one,
        50 => .two,
        51 => .three,
        52 => .four,
        53 => .five,
        54 => .six,
        55 => .seven,
        56 => .eight,
        57 => .nine,
        58 => .colon,
        59 => .semiColon,
        60 => .lessThan,
        61 => .equals,
        62 => .greaterThan,
        63 => .questionMark,
        64 => .at,
        65 => .A,
        66 => .B,
        67 => .C,
        68 => .D,
        69 => .E,
        70 => .F,
        71 => .G,
        72 => .H,
        73 => .I,
        74 => .J,
        75 => .K,
        76 => .L,
        77 => .M,
        78 => .N,
        79 => .O,
        80 => .P,
        81 => .Q,
        82 => .R,
        83 => .S,
        84 => .T,
        85 => .U,
        86 => .V,
        87 => .W,
        88 => .X,
        89 => .Y,
        90 => .Z,
        91 => .openBracket,
        92 => .backSlash,
        93 => .closeBracket,
        94 => .pointUp,
        95 => .underscore,
        96 => .backtick,
        97 => .a,
        98 => .b,
        99 => .c,
        100 => .d,
        101 => .e,
        102 => .f,
        103 => .g,
        104 => .h,
        105 => .i,
        106 => .j,
        107 => .k,
        108 => .l,
        109 => .m,
        110 => .n,
        111 => .o,
        112 => .p,
        113 => .q,
        114 => .r,
        115 => .s,
        116 => .t,
        117 => .u,
        118 => .v,
        119 => .w,
        120 => .x,
        121 => .y,
        122 => .z,
        123 => .openCurly,
        124 => .pipe,
        125 => .closeCurly,
        126 => .tilde,
        127 => error.InvalidChar,
        128...255 => .subsequentUtf8,
    };
}

const Token = enum {
    module,
};

const big = 1000_000;

pub fn main() void {
    std.debug.print("Hello", .{});
}
