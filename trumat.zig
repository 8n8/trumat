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

        fn append(self: *ExplicitId(capacity, T), id: u32, item: T) !void {
            if (self.len == capacity) {
                return error.Overflow;
            }

            self.ids[self.len] = id;
            self.items[self.len] = item;
            self.len = self.len + 1;
        }
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
    int,
};

const Memory = struct {
    unformatted: ImplicitId(1000_000, u8),

    tokenId: u32,
    tokenEnd: ImplicitId(500_000, u32),
    tokens: ImplicitId(500_000, Token),
    column: ImplicitId(500_000, u16),

    state: parser.State,

    isNewlineInModuleExports: bool,
    isModuleName: bool,
    moduleName: u32,
    topBind : ImplicitId(10_000, u32),
    bindLeft : ImplicitId(100_000, u32),
    bindRight : ImplicitId(100_000, u32),
    quote: ExplicitId(100_000, u32),
    export_: ImplicitId(1000, u32),
    parameter: ExplicitId(100_000, u32),
    inParens: ImplicitId(10_000, u32),

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
        inIntLiteral,
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
            .inIntLiteral => switch (char) {
                .zero => .{ .state = .inIntLiteral, .action = .next },
                .equals => .{ .state = .init, .action = .{ .commit2 = .{ .int, .equals } } },
                .newline => .{ .state = .init, .action = .{ .commit2 = .{ .int, .newline } } },
                .closeParens => .{ .state = .init, .action = .{ .commit2 = .{ .int, .closeParens } } },
                .openParens => .{ .state = .init, .action = .{ .commit2 = .{ .int, .openParens } } },
                .space => .{ .state = .init, .action = .{ .commit2 = .{ .int, .space } } },
                .g, .n, .i, .s, .p, .x, .X, .l, .u, .d, .o, .m => error.invalidChar,
                .e => error.invalidChar,
            },
            .zero => switch (char) {
                .x => error.invalidChar,
                .zero => .{ .state = .inIntLiteral, .action = .next },
                .equals => .{ .state = .init, .action = .{ .commit2 = .{ .int, .equals } } },
                .newline => .{ .state = .init, .action = .{ .commit2 = .{ .int, .newline } } },
                .closeParens => .{ .state = .init, .action = .{ .commit2 = .{ .int, .closeParens } } },
                .openParens => .{ .state = .init, .action = .{ .commit2 = .{ .int, .openParens } } },
                .space => .{ .state = .init, .action = .{ .commit2 = .{ .int, .space } } },
                .g, .n, .i, .s, .p, .X, .e, .l, .u, .d, .o, .m => error.invalidChar,
            },
            .insideUpName => switch (char) {
                .zero, .g, .n, .i, .s, .p, .x, .X, .e, .l, .u, .d, .o, .m => .{ .state = .insideUpName, .action = .next },
                .equals => .{ .state = .init, .action = .{ .commit2 = .{ .upName, .equals } } },
                .newline => .{ .state = .init, .action = .{ .commit2 = .{ .upName, .newline } } },
                .closeParens => .{ .state = .init, .action = .{ .commit2 = .{ .upName, .closeParens } } },
                .openParens => .{ .state = .init, .action = .{ .commit2 = .{ .upName, .openParens } } },
                .space => .{ .state = .init, .action = .{ .commit2 = .{ .upName, .space } } },
            },
            .insideLowName => switch (char) {
                .zero, .g, .n, .i, .s, .p, .x, .X, .e, .l, .u, .d, .o, .m => .{ .state = .insideLowName, .action = .next },
                .equals => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .equals } } },
                .newline => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .newline } } },
                .closeParens => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .closeParens } } },
                .openParens => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .openParens } } },
                .space => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .space } } },
            },
            .module => switch (char) {
                .zero, .g, .n, .i, .s, .p, .x, .d, .X, .o, .m, .u, .e, .l => .{ .state = .insideLowName, .action = .next },
                .equals => .{ .state = .init, .action = .{ .commit2 = .{ .module, .equals } } },
                .newline => .{ .state = .init, .action = .{ .commit2 = .{ .module, .newline } } },
                .closeParens => .{ .state = .init, .action = .{ .commit2 = .{ .module, .closeParens } } },
                .openParens => .{ .state = .init, .action = .{ .commit2 = .{ .module, .openParens } } },
                .space => .{ .state = .init, .action = .{ .commit2 = .{ .module, .space } } },
            },
            .modul => switch (char) {
                .zero, .g, .n, .i, .s, .p, .x, .d, .X, .o, .m, .u, .l => .{ .state = .insideLowName, .action = .next },
                .e => .{ .state = .module, .action = .next },
                .equals => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .equals } } },
                .newline => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .newline } } },
                .closeParens => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .closeParens } } },
                .openParens => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .openParens } } },
                .space => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .space } } },
            },
            .modu => switch (char) {
                .zero, .g, .n, .i, .s, .p, .x, .d, .X, .e, .o, .m, .u => .{ .state = .insideLowName, .action = .next },
                .l => .{ .state = .modul, .action = .next },
                .equals => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .equals } } },
                .newline => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .newline } } },
                .closeParens => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .closeParens } } },
                .openParens => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .openParens } } },
                .space => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .space } } },
            },
            .mod => switch (char) {
                .zero, .g, .n, .i, .s, .p, .x, .d, .X, .e, .l, .o, .m => .{ .state = .insideLowName, .action = .next },
                .u => .{ .state = .modu, .action = .next },
                .equals => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .equals } } },
                .newline => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .newline } } },
                .closeParens => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .closeParens } } },
                .openParens => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .openParens } } },
                .space => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .space } } },
            },
            .mo => switch (char) {
                .zero, .g, .n, .i, .s, .p, .x, .X, .e, .l, .u, .o, .m => .{ .state = .insideLowName, .action = .next },
                .d => .{ .state = .mod, .action = .next },
                .equals => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .equals } } },
                .newline => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .newline } } },
                .closeParens => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .closeParens } } },
                .openParens => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .openParens } } },
                .space => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .space } } },
            },
            .m => switch (char) {
                .m, .zero, .g, .n, .i, .s, .p, .x, .X, .e, .l, .u, .d => .{ .state = .insideLowName, .action = .next },
                .o => .{ .state = .mo, .action = .next },
                .equals => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .equals } } },
                .newline => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .newline } } },
                .space => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .space } } },
                .closeParens => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .closeParens } } },
                .openParens => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .openParens } } },
            },
            .exposing => switch (char) {
                .g, .m, .zero, .n, .i, .s, .o, .x, .p, .X, .e, .l, .u, .d => .{ .state = .insideLowName, .action = .next },
                .equals => .{ .state = .init, .action = .{ .commit2 = .{ .exposing, .equals } } },
                .newline => .{ .state = .init, .action = .{ .commit2 = .{ .exposing, .newline } } },
                .space => .{ .state = .init, .action = .{ .commit2 = .{ .exposing, .space } } },
                .closeParens => .{ .state = .init, .action = .{ .commit2 = .{ .exposing, .closeParens } } },
                .openParens => .{ .state = .init, .action = .{ .commit2 = .{ .exposing, .openParens } } },
            },
            .exposin => switch (char) {
                .m, .zero, .n, .i, .s, .o, .x, .p, .X, .e, .l, .u, .d => .{ .state = .insideLowName, .action = .next },
                .g => .{ .state = .exposing, .action = .next },
                .equals => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .equals } } },
                .newline => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .newline } } },
                .space => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .space } } },
                .closeParens => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .closeParens } } },
                .openParens => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .openParens } } },
            },
            .exposi => switch (char) {
                .m, .zero, .g, .i, .s, .o, .x, .p, .X, .e, .l, .u, .d => .{ .state = .insideLowName, .action = .next },
                .n => .{ .state = .exposin, .action = .next },
                .equals => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .equals } } },
                .newline => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .newline } } },
                .space => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .space } } },
                .closeParens => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .closeParens } } },
                .openParens => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .openParens } } },
            },
            .expos => switch (char) {
                .m, .zero, .g, .n, .s, .o, .x, .p, .X, .e, .l, .u, .d => .{ .state = .insideLowName, .action = .next },
                .i => .{ .state = .exposi, .action = .next },
                .equals => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .equals } } },
                .newline => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .newline } } },
                .space => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .space } } },
                .closeParens => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .closeParens } } },
                .openParens => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .openParens } } },
            },
            .expo => switch (char) {
                .m, .zero, .g, .n, .i, .o, .x, .p, .X, .e, .l, .u, .d => .{ .state = .insideLowName, .action = .next },
                .s => .{ .state = .expos, .action = .next },
                .equals => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .equals } } },
                .newline => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .newline } } },
                .space => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .space } } },
                .closeParens => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .closeParens } } },
                .openParens => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .openParens } } },
            },
            .exp => switch (char) {
                .m, .zero, .g, .n, .i, .s, .x, .p, .X, .e, .l, .u, .d => .{ .state = .insideLowName, .action = .next },
                .o => .{ .state = .expo, .action = .next },
                .equals => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .equals } } },
                .newline => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .newline } } },
                .space => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .space } } },
                .closeParens => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .closeParens } } },
                .openParens => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .openParens } } },
            },
            .ex => switch (char) {
                .m, .zero, .g, .n, .i, .s, .x, .o, .X, .e, .l, .u, .d => .{ .state = .insideLowName, .action = .next },
                .p => .{ .state = .exp, .action = .next },
                .equals => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .equals } } },
                .newline => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .newline } } },
                .space => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .space } } },
                .closeParens => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .closeParens } } },
                .openParens => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .openParens } } },
            },
            .e => switch (char) {
                .m, .zero, .g, .n, .i, .s, .p, .o, .X, .e, .l, .u, .d => .{ .state = .insideLowName, .action = .next },
                .x => .{ .state = .ex, .action = .next },
                .equals => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .equals } } },
                .newline => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .newline } } },
                .space => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .space } } },
                .closeParens => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .closeParens } } },
                .openParens => .{ .state = .init, .action = .{ .commit2 = .{ .lowName, .openParens } } },
            },
            .init => switch (char) {
                .m => .{ .state = .m, .action = .next },
                .e => .{ .state = .e, .action = .next },
                .o, .d, .u, .l, .x, .p, .s, .i, .n, .g => .{ .state = .insideLowName, .action = .next },
                .X => .{ .state = .insideUpName, .action = .next },
                .zero => .{ .state = .zero, .action = .next },
                .space => .{ .state = .init, .action = .{ .commit = .space } },
                .openParens => .{ .state = .init, .action = .{ .commit = .openParens } },
                .equals => .{ .state = .init, .action = .{ .commit = .equals } },
                .closeParens => .{ .state = .init, .action = .{ .commit = .closeParens } },
                .newline => .{ .state = .init, .action = .{ .commit = .newline } },
            },
        };
    }

    fn run(unformatted: ImplicitId(1000_000, u8), tokenEnd: *ImplicitId(500_000, u32), tokens: *ImplicitId(500_000, Token)) !void {
        var state: State = .init;
        for (0..unformatted.len) |i| {
            const byte: u8 = unformatted.array[i];
            const char: Char = try parseChar(byte);
            const result: Step = try step(state, char);
            state = result.state;
            _ = switch (result.action) {
                .commit => |token| {
                    try tokenEnd.append(@truncate(i));
                    try tokens.append(token);
                },
                .commit2 => |twoTokens| {
                    try tokenEnd.append(@truncate(i - 1));
                    try tokens.append(twoTokens[0]);

                    try tokenEnd.append(@truncate(i));
                    try tokens.append(twoTokens[1]);
                },
                .next => {},
            };
        }
    }
};

const parser = struct {
    const ActionTag = enum {
        next,
        moreNesting,
        lessNesting,
    };
    const Action = union(ActionTag) {
        next: void,
        moreNesting: State,
        lessNesting: void,
    };

    const Step = struct {
        state: State,
        action : Action,
    };

    const State = enum {
        init,
        justAfterModuleKeyword,
        beforeModuleName,
        afterModuleName,
        afterModuleExposing,
        beforeNameInModuleExposing,
        finished,
        atTopLevel,
        beforeBindEquals,
    };

    fn step(memory: *Memory) !State {
        const token: Token = memory.tokens.array[memory.tokenId];
        const tokenId: u32 = memory.tokenId;
        return switch (memory.state) {
            .finished => .finished,
            .beforeBindEquals => switch (token) {
                .int => error.intParameterPattern,
                .equals => .afterBindEquals,
                .newline => .afterBindEquals,
                .closeParens => error.closeParensBeforeBindEquals,
                .openParens => {
                    memory.state = .patternInParens;
                    try run(memory);
                },
            },
            .atTopLevel => switch (token) {
                .int => error.intAtTopLevel,
                .equals => error.equalsAtTopLevel,
                .newline => .atTopLevel,
                .closeParens => error.closeParensAtTopLevel,
                .openParens => error.openParensAtTopLevel,
                .exposing => error.exposingAtTopLevel,
                .lowName => {
                    if (memory.column.array[tokenId] != 0) {
                        return error.indentedTopLevelName;
                    }
                    const quoteId: u32 = nextId(memory);
                    try memory.quote.append(quoteId, tokenId);

                    try memory.bindLeft.append(quoteId);
                    try memory.topBind.append(memory.bindLeft.len - 1);

                    memory.state = .beforeBindEquals;

                    try run(memory);
                    return .atTopLevel;
                },
                .module => error.moduleAtTopLevel,
                .space => .atTopLevel,
                .upName => error.upNameAtTopLevel,
            },
            .beforeNameInModuleExposing => switch (token) {
                .int => error.intInModuleExposing,
                .equals => error.equalsInModuleExposing,
                .closeParens => .betweenTopLevels,
                .newline => {
                    memory.isNewlineInModuleExports = true;
                    return .{ .state = .beforeNameInModuleExposing, .action = .next };
                },
                .openParens => error.openParensBeforeNameInModuleExposing,
                .exposing => error.exposingBeforeNameInModuleExposing,
                .lowName => {
                    memory.export_.append(tokenId);
                    return .{ .state = .afterNameInModuleExposing, .action = .next };
                },
                .upName => {
                    memory.export_.append(tokenId);
                    return .{ .state = .afterNameInModuleExposing, .action = .next };
                },
                .space => .{ .state = .beforeNameInModuleExposing, .action = .next },
                    
                .module => error.moduleBeforeNameInModuleExposing,
            },

            .afterModuleExposing => switch (memory.tokens.array[tokenId]) {
                .int => error.intAfterModuleExposing,
                .equals => error.equalsAfterModuleExposing,
                .newline => .{ .state = .afterModuleExposing, .action = .next },
                .closeParens => error.closeParensAfterMOduleExposing,
                .openParens => .{ .state = .beforeNameInModuleExposing, .action = .next},
                .exposing => error.exposingAfterModuleExposing,
                .lowName => error.lowNameAfterModuleExposing,
                .upName => error.upNameAfterModuleExposing,
                .space => .{ .state = .afterModuleExposing, .action = .next },
                .module => error.moduleAfterModuleExposing,
            },
            .justAfterModuleKeyword => switch (memory.tokens.array[tokenId]) {
                .int => error.intJustAfterModuleKeyword,
                .equals => error.equalsJustAfterModuleKeyword,
                .newline => .{ .state = .beforeModuleName, .action = .next },
                .closeParens => error.closeParensJustAfterModuleKeyword,
                .openParens => error.openParensJustAfterModuleKeyword,
                .exposing => error.exposingJustAfterModuleKeyword,
                .lowName => error.lowNameJustAfterModuleKeyword,
                .upName => error.upNameJustAfterModuleKeyword,
                .space => .{ .state = .beforeModuleName, .action = .next },
                .module => error.moduleKeywordJustAfterModuleKeyword,
            },
            .afterModuleName => switch (memory.tokens.array[tokenId]) {
                .int => error.intAfterModuleName,
                .equals => error.equalsAfterModuleName,
                .newline => .{ .state = .afterModuleName, .action = .next },
                .closeParens => error.closeParensAfterModuleName,
                .openParens => error.openParensAfterModuleName,
                .exposing => .{ .state = .afterExposing, .action = .next },
                .lowName => error.lowNameAfterModuleName,
                .upName => error.upNameAfterModuleName,
                .space => .{ .state = .afterModuleName, .action = .next },
                .module => error.moduleKeywordAfterModuleName,
            },
            .beforeModuleName => switch (memory.tokens.array[tokenId]) {
                .module => error.moduleKeywordInsteadOfName,
                .space => .{ .state = .beforeModuleName, .action = .next },
                .upName => {
                    memory.isModuleName = true;
                    memory.moduleName = tokenId;
                    return .{ .state = .afterModuleName, .action = .next };
                },
                .int => error.intInsteadOfModuleName,
                .equals => error.equalsInsteadOfModuleName,
                .newline => error.newlineInsteadOfModuleName,
                .closeParens => error.closeParensInsteadOfModuleName,
                .openParens => error.openParensInsteadOfModuleName,
                .exposing => error.exposingInsteadOfModuleName,
                .lowName => error.lowNameInsteadOfModuleName,
            },
            .init => switch (memory.tokens.array[tokenId]) {
                .module =>
                    {
                        if (memory.column.array[tokenId] != 0) {
                            return error.indentedModuleDeclaration;
                        }
                        return .{ .state = .afterModuleKeyword, .action = .next };
                    },
                .int => error.intAtStartOfModule,
                .equals => error.equalsAtStartOfModule,
                .newline => .{ .state = .init, .action = .next },
                .closeParens => error.closeParensAtStartOfModule,
                .openParens => error.openParensAtStartOfModule,
                .exposing => error.exposingAtStartOfModule,
                .lowName => {
                    if (memory.column[tokenId] != 0) {
                        error.indentedTopLevelBind;
                    }

                    memory.isModuleName = false;    
                    memory.bindLeft.append(tokenId);
                    .{ .state = .afterFirstTopBindName, .action = .next };
                },
                .space => .{ .state = .init, .action = .next },
                .upName => error.upNameOnInit,
            },
        };
    }

    pub fn run(memory: *Memory) !void {
        for (0..memory.tokens.len) |i| {
            memory.tokenId = @truncate(i);
            try step(memory);
        }
    }
};

fn calculateColumnNumbers(tokens: ImplicitId(500_000, Token), columns: *ImplicitId(500_000, u16)) void {
    var column: u16 = 0;
    for (0..tokens.len) |tokenI| {
        if (tokens.array[tokenI] == .newline) {
            column = 0;
        } else {
            column = column + 1;
        }

        columns.array[tokenI] = column;
    }

    columns.len = tokens.len;
}

fn nextId(memory: *Memory) u32 {
    const newId = memory.nextId;
    memory.nextId = memory.nextId + 1;
    return newId;
}

fn format(input: fs.File, memory: *Memory, _: fs.File) !void {
    {
        const size: usize = try input.readAll(&memory.unformatted.array);
        if (size > 1000_000) {
            return error.Overflow;
        }
        memory.unformatted.len = @truncate(size);
    }

    try tokenizer.run(memory.unformatted, &memory.tokenEnd, &memory.tokens);
    calculateColumnNumbers(memory.tokens, &memory.column);
    try parser.run(memory);
}
