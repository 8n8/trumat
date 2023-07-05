const std = @import("std");
const testing = std.testing;
const fs = std.fs;

test "hello world formatted" {
    var tmpDir = testing.tmpDir(.{});
    defer tmpDir.cleanup();

    const helloWorld =
        \\module X exposing (x)
        \\
        \\
        \\x =
        \\    0
        \\
    ;

    {
        var inputFile = try tmpDir.dir.createFile("unformatted.elm", .{});
        try inputFile.writeAll(helloWorld);
        inputFile.close();
    }

    var inputFile = try tmpDir.dir.createFile("unformatted.elm", .{ .read = true });
    defer inputFile.close();
    var outputFile = try tmpDir.dir.createFile("formatted.elm", .{ .read = true });
    defer outputFile.close();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const memory = try allocator.create(Memory);

    try format(inputFile, memory, outputFile);

    var result: [helloWorld.len]u8 = undefined;
    _ = try outputFile.read(&result);

    try testing.expect(std.mem.eql(u8, helloWorld, &result));
}

fn ImplicitId(comptime capacity: u32, comptime T: type) type {
    return struct {
        len: u32,
        array: [capacity]T,

        fn append(self: *ImplicitId(capacity, T), item: T) !void {
            if (self.len == capacity) {
                return error.Overflow;
            }

            self.array[self.len] = item;
            self.len = self.len + 1;
        }
    };
}

fn ExplicitId(comptime capacity: u32, comptime T: type) type {
    return struct {
        len: u32,
        ids: [capacity]u32,
        items: [capacity]T,
    };
}

const Quote = packed struct {
    start: u24,
    end: u24,
};

const Bind = packed struct {
    left: Quote,
    right: u32,
};

fn Expressions(comptime IdType: type, comptime capacity: IdType, comptime Expression: type) type {
    return struct {
        globalIds: [capacity]IdType,
        expressions: [capacity]Expression,
        len: u32,
    };
}

const Token = enum(u8) {
    module,
    space,
    upName,
    lowName,
    exposing,
    openParens,
    closeParens,
    newline,
    equals,
    hex,
    int,
    floatExp,
};

const Memory = struct {
    unformatted: ImplicitId(1000_000, u8),

    tokenEnd: ImplicitId(500_000, u32),
    tokens: ImplicitId(500_000, Token),

    moduleName: Quote,
    names: ImplicitId(100_000, Quote),
    topBinds: ImplicitId(10_000, u32),
    binds: ImplicitId(100_000, Bind),
    quoteExpression: ExplicitId(100_000, Quote),
    exports: ImplicitId(1000, Quote),

    nextId: u32,
};

const tokenizer = struct {
    const State = enum {
        init,
        m,
        mo,
        mod,
        modu,
        modul,
        module,
        insideLowName,
        insideUpName,
        e,
        ex,
        exp,
        expo,
        expos,
        exposi,
        exposin,
        exposing,
        zero,
        hex0x,
        inHexLiteral,
        inIntLiteral,
        expE,
        expEdigit,
    };

    const ActionTag = enum {
        commit,
        commit2,
        next,
    };
    const Action = union(ActionTag) {
        commit: Token,
        commit2: [2]Token,
        next: void,
    };

    const Char = enum(u8) {
        m,
        o,
        d,
        u,
        l,
        e,
        space,
        X,
        x,
        p,
        s,
        i,
        n,
        g,
        openParens,
        closeParens,
        newline,
        equals,
        zero,
    };

    fn parseChar(byte: u8) !Char {
        return switch (byte) {
            0...9 => error.invalidChar,
            10 => .newline,
            11...31 => error.invaliChar,
            32 => .space,
            33...39 => error.invalidChar,
            40 => .openParens,
            41 => .closeParens,
            42...47 => error.invalidChar,
            48 => .zero,
            49...60 => error.invalidChar,
            61 => .equals,
            62...87 => error.invalidChar,
            88 => .X,
            89...99 => error.invalidChar,
            100 => .d,
            101 => .e,
            102 => error.invalidChar,
            103 => .g,
            104 => error.invalidChar,
            105 => .i,
            106 => error.invalidChar,
            107 => error.invalidChar,
            108 => .l,
            109 => .m,
            110 => .n,
            111 => .o,
            112 => .p,
            113 => error.invalidChar,
            114 => error.invalidChar,
            115 => .s,
            116 => error.invalidChar,
            117 => .u,
            118 => error.invalidChar,
            119 => error.invalidChar,
            120 => .x,
            121...255 => error.invalidChar,
        };
    }

    const Step = struct {
        state: State,
        action: Action,
    };

    fn step(state: State, char: Char) !Step {
        return switch (state) {
            .expEdigit => switch (char) {
                .zero => .{ .state = .expEdigit, .action = .next },
                .equals => .{ .state = .init, .action = .{.commit2 = .{.floatExp, .equals}}},
                .newline => .{ .state = .init, .action = .{.commit2 = .{.floatExp, .newline}}},
                .closeParens => .{ .state = .init, .action = .{.commit2 = .{.floatExp, .closeParens}}},
                .openParens => .{ .state = .init, .action = .{.commit2 = .{.floatExp, .openParens}}},
                .space => .{ .state = .init, .action = .{.commit2 = .{.floatExp, .space}}},
                .g, .n, .i, .s, .p, .x, .X, .e, .l, .u, .d, .o, .m => error.invalidChar,
            },
            .expE => switch (char) {
                .zero => .{ .state = .expEdigit, .action = .next },
                .g, .e, .n, .i, .s, .p, .x, .X, .l, .u, .d, .o, .m, .equals, .newline, .closeParens, .openParens, .space  => error.invalidChar,
            },
            .inIntLiteral => switch (char) {
                .zero => .{ .state = .inIntLiteral, .action = .next },
                .equals => .{ .state = .init, .action = .{.commit2 = .{.int, .equals}}},
                .newline => .{ .state = .init, .action = .{.commit2 = .{.int, .newline}}},
                .closeParens => .{ .state = .init, .action = .{.commit2 = .{.int, .closeParens}}},
                .openParens => .{ .state = .init, .action = .{.commit2 = .{.int, .openParens}}},
                .space => .{ .state = .init, .action = .{.commit2 = .{.int, .space}}},
                .g, .n, .i, .s, .p, .x, .X, .l, .u, .d, .o, .m  => error.invalidChar,
                .e => .{ .state = .expE, .action = .next },
            },
            .inHexLiteral => switch (char) {
                .zero => .{ .state = .inHexLiteral, .action = .next },
                .equals => .{ .state = .init, .action = .{.commit2 = .{.hex, .equals }}},
                .newline => .{ .state = .init, .action = .{.commit2 = .{.hex, .newline }}},
                .closeParens => .{ .state = .init, .action = .{.commit2 = .{.hex, .closeParens }}},
                .openParens => .{ .state = .init, .action = .{.commit2 = .{.hex, .openParens }}},
                .space => .{ .state = .init, .action = .{.commit2 = .{.hex, .space }}},
                .l, .o, .m, .u, .g, .n, .i, .s, .p, .x, .X => error.invalidChar,
                .e, .d => .{.state = .inHexLiteral, .action = .next },
            },
            .hex0x => switch (char) {
                .zero, .e, .d => .{ .state = .inHexLiteral, .action = .next },
                .equals, .l, .u, .newline, .m, .o, .closeParens, .openParens, .g, .n, .i, .s, .p, .x, .X, .space => error.invalidChar,
            },
            .zero => switch (char) {
                .x => .{ .state = .hex0x, .action = .next },
                .zero => .{ .state = .inIntLiteral, .action = .next },
                .equals => .{ .state = .init, .action = .{.commit2 = .{.int, .equals}}},
                .newline => .{ .state = .init, .action = .{.commit2 = .{.int, .newline}}},
                .closeParens => .{ .state = .init, .action = .{.commit2 = .{.int, .closeParens}}},
                .openParens => .{ .state = .init, .action = .{.commit2 = .{.int, .openParens}}},
                .space => .{ .state = .init, .action = .{.commit2 = .{.int, .space}}},
                .g, .n, .i, .s, .p, .X, .e, .l, .u, .d, .o, .m => error.invalidChar,
            },
            .insideUpName => switch (char) {
                .zero, .g, .n, .i, .s, .p, .x, .X, .e, .l, .u, .d, .o, .m => .{.state = .insideUpName, .action = .next},
                .equals => .{.state = .init, .action = .{.commit2 = .{.upName, .equals}}},
                .newline => .{.state = .init, .action = .{.commit2 = .{.upName, .newline}}},
                .closeParens => .{.state = .init, .action = .{.commit2 = .{.upName, .closeParens}}},
                .openParens => .{.state = .init, .action = .{.commit2 = .{.upName, .openParens}}},
                .space => .{.state = .init, .action = .{.commit2 = .{.upName, .space}}},
            },
            .insideLowName => switch (char) {
                .zero, .g, .n, .i, .s, .p, .x, .X, .e, .l, .u, .d, .o, .m => .{.state = .insideLowName, .action = .next},
                .equals => .{.state = .init, .action = .{.commit2 = .{.lowName, .equals}}},
                .newline => .{.state = .init, .action = .{.commit2 = .{.lowName, .newline}}},
                .closeParens => .{.state = .init, .action = .{.commit2 = .{.lowName, .closeParens}}},
                .openParens => .{.state = .init, .action = .{.commit2 = .{.lowName, .openParens}}},
                .space => .{.state = .init, .action = .{.commit2 = .{.lowName, .space}}},
            },
            .module => switch (char) {
                .zero, .g, .n, .i, .s, .p, .x, .d, .X, .o, .m, .u, .e, .l => .{.state = .insideLowName, .action = .next},
                .equals => .{.state = .init, .action = .{.commit2 = .{.module, .equals}}},
                .newline => .{.state = .init, .action = .{.commit2 = .{.module, .newline}}},
                .closeParens => .{.state = .init, .action = .{.commit2 = .{.module, .closeParens}}},
                .openParens => .{.state = .init, .action = .{.commit2 = .{.module, .openParens}}},
                .space => .{.state = .init, .action = .{.commit2 = .{.module, .space}}},
            },
            .modul => switch (char) {
                .zero, .g, .n, .i, .s, .p, .x, .d, .X, .o, .m, .u, .l => .{.state = .insideLowName, .action = .next},
                .e => .{. state = .module, .action = .next },
                .equals => .{.state = .init, .action = .{.commit2 = .{.lowName, .equals}}},
                .newline => .{.state = .init, .action = .{.commit2 = .{.lowName, .newline}}},
                .closeParens => .{.state = .init, .action = .{.commit2 = .{.lowName, .closeParens}}},
                .openParens => .{.state = .init, .action = .{.commit2 = .{.lowName, .openParens}}},
                .space => .{.state = .init, .action = .{.commit2 = .{.lowName, .space}}},
            },
            .modu => switch (char) {
                .zero, .g, .n, .i, .s, .p, .x, .d, .X, .e, .o, .m, .u => .{.state = .insideLowName, .action = .next},
                .l => .{. state = .modul, .action = .next },
                .equals => .{.state = .init, .action = .{.commit2 = .{.lowName, .equals}}},
                .newline => .{.state = .init, .action = .{.commit2 = .{.lowName, .newline}}},
                .closeParens => .{.state = .init, .action = .{.commit2 = .{.lowName, .closeParens}}},
                .openParens => .{.state = .init, .action = .{.commit2 = .{.lowName, .openParens}}},
                .space => .{.state = .init, .action = .{.commit2 = .{.lowName, .space}}},
            },
            .mod => switch (char) {
                .zero, .g, .n, .i, .s, .p, .x, .d, .X, .e, .l, .o, .m => .{.state = .insideLowName, .action = .next},
                .u => .{. state = .modu, .action = .next },
                .equals => .{.state = .init, .action = .{.commit2 = .{.lowName, .equals}}},
                .newline => .{.state = .init, .action = .{.commit2 = .{.lowName, .newline}}},
                .closeParens => .{.state = .init, .action = .{.commit2 = .{.lowName, .closeParens}}},
                .openParens => .{.state = .init, .action = .{.commit2 = .{.lowName, .openParens}}},
                .space => .{.state = .init, .action = .{.commit2 = .{.lowName, .space}}},
            },
            .mo => switch (char) {
                .zero, .g, .n, .i, .s, .p, .x, .X, .e, .l, .u, .o, .m => .{.state = .insideLowName, .action = .next},
                .d => .{. state = .mod, .action = .next },
                .equals => .{.state = .init, .action = .{.commit2 = .{.lowName, .equals}}},
                .newline => .{.state = .init, .action = .{.commit2 = .{.lowName, .newline}}},
                .closeParens => .{.state = .init, .action = .{.commit2 = .{.lowName, .closeParens}}},
                .openParens => .{.state = .init, .action = .{.commit2 = .{.lowName, .openParens}}},
                .space => .{.state = .init, .action = .{.commit2 = .{.lowName, .space}}},
            },
            .m => switch (char) {
                .m, .zero, .g, .n, .i, .s, .p, .x, .X, .e, .l, .u, .d => .{ .state = .insideLowName, .action = .next },
                .o => .{ .state = .mo, .action = .next },
                .equals => .{ .state = .init, .action = .{.commit2 = .{.lowName, .equals}}},
                .newline => .{ .state = .init, .action = .{.commit2 = .{.lowName, .newline}}},
                .space => .{ .state = .init, .action = .{.commit2 = .{.lowName, .space}}},
                .closeParens => .{ .state = .init, .action = .{.commit2 = .{.lowName, .closeParens}}},
                .openParens => .{ .state = .init, .action = .{.commit2 = .{.lowName, .openParens}}},
            },
            .exposing => switch (char) {
                .g, .m, .zero, .n, .i, .s, .o, .x, .p, .X, .e, .l, .u, .d => .{ .state = .insideLowName, .action = .next },
                .equals => .{ .state = .init, .action = .{.commit2 = .{.exposing, .equals}}},
                .newline => .{ .state = .init, .action = .{.commit2 = .{.exposing, .newline}}},
                .space => .{ .state = .init, .action = .{.commit2 = .{.exposing, .space}}},
                .closeParens => .{ .state = .init, .action = .{.commit2 = .{.exposing, .closeParens}}},
                .openParens => .{ .state = .init, .action = .{.commit2 = .{.exposing, .openParens}}},
            },
            .exposin => switch (char) {
                .m, .zero, .n, .i, .s, .o, .x, .p, .X, .e, .l, .u, .d => .{ .state = .insideLowName, .action = .next },
                .g => .{ .state = .exposing, .action = .next },
                .equals => .{ .state = .init, .action = .{.commit2 = .{.lowName, .equals}}},
                .newline => .{ .state = .init, .action = .{.commit2 = .{.lowName, .newline}}},
                .space => .{ .state = .init, .action = .{.commit2 = .{.lowName, .space}}},
                .closeParens => .{ .state = .init, .action = .{.commit2 = .{.lowName, .closeParens}}},
                .openParens => .{ .state = .init, .action = .{.commit2 = .{.lowName, .openParens}}},
            },
            .exposi => switch (char) {
                .m, .zero, .g, .i, .s, .o, .x, .p, .X, .e, .l, .u, .d => .{ .state = .insideLowName, .action = .next },
                .n => .{ .state = .exposin, .action = .next },
                .equals => .{ .state = .init, .action = .{.commit2 = .{.lowName, .equals}}},
                .newline => .{ .state = .init, .action = .{.commit2 = .{.lowName, .newline}}},
                .space => .{ .state = .init, .action = .{.commit2 = .{.lowName, .space}}},
                .closeParens => .{ .state = .init, .action = .{.commit2 = .{.lowName, .closeParens}}},
                .openParens => .{ .state = .init, .action = .{.commit2 = .{.lowName, .openParens}}},
            },
            .expos => switch (char) {
                .m, .zero, .g, .n, .s, .o, .x, .p, .X, .e, .l, .u, .d => .{ .state = .insideLowName, .action = .next },
                .i => .{ .state = .exposi, .action = .next },
                .equals => .{ .state = .init, .action = .{.commit2 = .{.lowName, .equals}}},
                .newline => .{ .state = .init, .action = .{.commit2 = .{.lowName, .newline}}},
                .space => .{ .state = .init, .action = .{.commit2 = .{.lowName, .space}}},
                .closeParens => .{ .state = .init, .action = .{.commit2 = .{.lowName, .closeParens}}},
                .openParens => .{ .state = .init, .action = .{.commit2 = .{.lowName, .openParens}}},
            },
            .expo => switch (char) {
                .m, .zero, .g, .n, .i, .o, .x, .p, .X, .e, .l, .u, .d => .{ .state = .insideLowName, .action = .next },
                .s => .{ .state = .expos, .action = .next },
                .equals => .{ .state = .init, .action = .{.commit2 = .{.lowName, .equals}}},
                .newline => .{ .state = .init, .action = .{.commit2 = .{.lowName, .newline}}},
                .space => .{ .state = .init, .action = .{.commit2 = .{.lowName, .space}}},
                .closeParens => .{ .state = .init, .action = .{.commit2 = .{.lowName, .closeParens}}},
                .openParens => .{ .state = .init, .action = .{.commit2 = .{.lowName, .openParens}}},
            },
            .exp => switch (char) {
                .m, .zero, .g, .n, .i, .s, .x, .p, .X, .e, .l, .u, .d => .{ .state = .insideLowName, .action = .next },
                .o => .{ .state = .expo, .action = .next },
                .equals => .{ .state = .init, .action = .{.commit2 = .{.lowName, .equals}}},
                .newline => .{ .state = .init, .action = .{.commit2 = .{.lowName, .newline}}},
                .space => .{ .state = .init, .action = .{.commit2 = .{.lowName, .space}}},
                .closeParens => .{ .state = .init, .action = .{.commit2 = .{.lowName, .closeParens}}},
                .openParens => .{ .state = .init, .action = .{.commit2 = .{.lowName, .openParens}}},
            },
            .ex => switch (char) {
                .m, .zero, .g, .n, .i, .s, .x, .o, .X, .e, .l, .u, .d => .{ .state = .insideLowName, .action = .next },
                .p => .{ .state = .exp, .action = .next },
                .equals => .{ .state = .init, .action = .{.commit2 = .{.lowName, .equals}}},
                .newline => .{ .state = .init, .action = .{.commit2 = .{.lowName, .newline}}},
                .space => .{ .state = .init, .action = .{.commit2 = .{.lowName, .space}}},
                .closeParens => .{ .state = .init, .action = .{.commit2 = .{.lowName, .closeParens}}},
                .openParens => .{ .state = .init, .action = .{.commit2 = .{.lowName, .openParens}}},
            },
            .e => switch (char) {
                .m, .zero, .g, .n, .i, .s, .p, .o, .X, .e, .l, .u, .d => .{ .state = .insideLowName, .action = .next },
                .x => .{ .state = .ex, .action = .next },
                .equals => .{ .state = .init, .action = .{.commit2 = .{.lowName, .equals}}},
                .newline => .{ .state = .init, .action = .{.commit2 = .{.lowName, .newline}}},
                .space => .{ .state = .init, .action = .{.commit2 = .{.lowName, .space}}},
                .closeParens => .{ .state = .init, .action = .{.commit2 = .{.lowName, .closeParens}}},
                .openParens => .{ .state = .init, .action = .{.commit2 = .{.lowName, .openParens}}},
            },
            .init => switch (char) {
                .m => .{ .state = .m, .action = .next },
                .e => .{ .state = .e, .action = .next },
                .o, .d, .u, .l, .x, .p, .s, .i, .n, .g => .{ .state = .insideLowName, .action = .next },
                .X => .{ .state = .insideUpName, .action = .next },
                .zero => .{ .state = .zero, .action = .next },
                .space => .{ .state = .init, .action = .{ .commit = .space }},
                .openParens => .{ .state = .init, .action = .{ .commit = .openParens }},
                .equals => .{ .state = .init, .action = .{ .commit = .equals }},
                .closeParens => .{ .state = .init, .action = .{ .commit = .closeParens }},
                .newline => .{ .state = .init, .action = .{ .commit = .newline }},
            },
        };
    }

    fn run(unformatted: ImplicitId(1000_000, u8), tokenEnd: *ImplicitId(500_000, u32), tokens: *ImplicitId(500_000, Token)) !void {
        var state: State = .init;
        for (unformatted.array, 0..unformatted.len) |byte, i| {
            const char: Char = try parseChar(byte);
            const result: Step = try step(state, char);
            state = result.state;
            _ = switch (result.action) {
                .commit => |token| {
                    try tokenEnd.append(@truncate(i));
                    try tokens.append(token);
                },
                .commit2 => |twoTokens| {
                    try tokenEnd.append(@truncate(i-1));
                    try tokens.append(twoTokens[0]);

                    try tokenEnd.append(@truncate(i));
                    try tokens.append(twoTokens[1]);
                },
                .next => {},
            };
        }
    }
};

fn format(input: fs.File, memory: *Memory, _: fs.File) !void {
    {
        const size: usize = try input.readAll(&memory.unformatted.array);
        if (size > 1000_000) {
            return error.Overflow;
        }
        memory.unformatted.len = @truncate(size);
    }

    try tokenizer.run(memory.unformatted, &memory.tokenEnd, &memory.tokens);
}
