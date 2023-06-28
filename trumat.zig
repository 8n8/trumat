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

const stateMachine = generateStateMachine();

const stateMachineSize = 1 << 16;

const StateMachine = struct{
    states: [stateMachineSize]State,
    actions: [stateMachineSize]Action,
};

fn generateStateMachine() StateMachine {
    var states: [stateMachineSize]State = undefined;
    var actions: [stateMachineSize]Action = undefined;

    for (0..stateMachineSize) |rawInput| {
        const step: Step = generateStep(rawInput); 
        states[rawInput] = step.state;
        actions[rawInput] = step.action;
    }

    return .{.states = states, .actions = actions};
}

fn parseState(raw: u8) State {
    return switch (raw) {
    0 => .startModule,
    1 => .moduleStartsWithM,
    2 => .moduleStartsWithMo,
    3 => .moduleStartsWithMod,
    4 => .moduleStartsWithModu,
    5 => .moduleStartsWithModul,
    6 => .moduleStartsWithModule,
    7 => .empty,
    8...255 => .invalid,
    };
}

fn parseElmChar(raw: u8) ElmChar {
    return switch(raw) {
    0 => .newline,
    1 => .space,
    2...255 => .invalid,
    };
}

fn generateStep(rawInput: u16) Step {
    const rawState: u8 = @truncate(rawInput >> 8);
    const rawChar: u8 = @truncate(rawInput);

    const state: State = parseState(rawState);
    const char: ElmChar = parseElmChar(rawChar);

    return generateStepHelp(state, char);
}

fn generateStepHelp(state: State, char: ElmChar) Step {
    return switch (state) {
    .startModule =>
        switch(char) {
        .newline =>
            .doNothing,
        },
    .moduleStartsWithM =>
        switch(char) {
        .newline =>
            
        },
    };
}

const ElmChar = enum(u8) {
    newline,
    space,
    openParenthesis,
    closeParenthesis,
    zero,
    equals,
    X,
    d,
    e,
    g,
    i,
    l,
    m,
    n,
    o,
    p,
    s,
    u,
    x,
    afterEnd,
    invalid,
};

const Memory = struct {
    elmChars: [big]ElmChar,
    exports: [numNames][maxName]u8,
};

const State = enum(u8) {
    startModule,
    moduleStartsWithM,
    moduleStartsWithMo,
    moduleStartsWithMod,
    moduleStartsWithModu,
    moduleStartsWithModul,
    moduleStartsWithModule,
    empty,
    failed,
};

const Step = struct {
    state: State,
    action: Action,
};

const Action = enum(u8) {
    none,
    finished,
};

const Out = struct {
    buf: *[big]u8,
    i: *u32,
};

fn makeStep(state: State, char8: u8) Step {
    const state8: u8 = @intFromEnum(state);
    const state16: u16 = @intCast(state8);
    const char16: u16 = @intCast(char8);
    const index: u16 = (state16 << 8) + char16;

    const newState = stateMachine.states[index];
    const action = stateMachine.actions[index];

    return .{ .state = newState, .action = action };
}

//     return switch (state) {
//     .startModule =>
//         stepStartModule(char, i, out),
//     .moduleStartsWithM =>
//         stepModuleStartsWithM(char, i, out),
//     .moduleStartsWithMo =>
//         stepModuleStartsWithMo(char, i, out),
//     .moduleStartsWithMod =>
//         stepModuleStartsWithMod(char, i, out),
//     .moduleStartsWithModu =>
//         stepModuleStartsWithModu(char, i, out),
//     .moduleStartsWithModul =>
//         stepModuleStartsWithModul(char, i, out),
//     .moduleStartsWithModule =>
//         stepModuleStartsWithModule(char, i, out),
//     .openCurlyAfterModuleKeyword =>
//         stepOpenCurlyAfterModuleKeyword(char, i, out),
//     .blockCommentAfterModuleKeyword =>
//         stepBlockCommentAfterModuleKeyword(char, i, out),
//     .empty =>
//         .{.state = .failed, .moveTo = i, .action = .none},
//     .failed =>
//         .{.state = .failed, .moveTo = i, .action = .none},
//     };
// }

fn stepBlockCommentAfterModuleKeyword(char: ElmChar, i: u32) Step {
    return switch(char) {
    .hyphen => .{.state = .blockCommentAfterModuleKeyword, .moveTo = i + 1, .action = .none },
    .subsequentUtf8, .afterEnd, .tilde, .closeCurly, .pipe, .openCurly, .z, .y, .x, .w, .v, .u, .t, .s, .r, .q, .p, .o, .n, .m, .l, .k, .j, .i, .h, .g, .f, .e, .d, .c, .b, .a, .backtick, .underscore, .pointUp, .closeBracket, .backSlash, .openBracket, .Z, .Y, .X, .W, .V, .U, .T, .R, .Q, .P, .O, .N, .M, .L, .S, .K, .J, .I, .H, .G, .F, .E, .D, .C, .B, .A, .at, .questionMark, .greaterThan, .equals, .lessThan, .semiColon, .colon, .nine, .eight, .seven, .six, .five, .four, .three, .two, .one, .zero, .forwardSlash, .fullstop, .comma, .plus, .star, .closeParenthesis, .openParenthesis, .singleQuote, .ampersand, .percentage, .dollar, .hash, .doubleQuote, .exclamationMark, .space, .newline =>
        .{.state = .failed, .moveTo = i, .action = .none },
    };
}

fn stepOpenCurlyAfterModuleKeyword(char: ElmChar, i: u32) Step {
    return switch(char) {
    .hyphen => .{.state = .blockCommentAfterModuleKeyword, .moveTo = i + 1, .action = .none },
    .subsequentUtf8, .afterEnd, .tilde, .closeCurly, .pipe, .openCurly, .z, .y, .x, .w, .v, .u, .t, .s, .r, .q, .p, .o, .n, .m, .l, .k, .j, .i, .h, .g, .f, .e, .d, .c, .b, .a, .backtick, .underscore, .pointUp, .closeBracket, .backSlash, .openBracket, .Z, .Y, .X, .W, .V, .U, .T, .R, .Q, .P, .O, .N, .M, .L, .S, .K, .J, .I, .H, .G, .F, .E, .D, .C, .B, .A, .at, .questionMark, .greaterThan, .equals, .lessThan, .semiColon, .colon, .nine, .eight, .seven, .six, .five, .four, .three, .two, .one, .zero, .forwardSlash, .fullstop, .comma, .plus, .star, .closeParenthesis, .openParenthesis, .singleQuote, .ampersand, .percentage, .dollar, .hash, .doubleQuote, .exclamationMark, .space, .newline =>
        .{.state = .failed, .moveTo = i, .action = .none },
    };
}

fn stepModuleStartsWithModule(char: ElmChar, i: u32) Step {
    return switch(char) {
    .space, .newline =>
        .{.state = .afterModuleKeyword, .moveTo = i + 1, .action = Action { .commit = "module" } },

    .openCurly =>
        .{.state = .openCurlyAfterModuleKeyword, .moveTo = i + 1, .action = Action { .commit = "module" } },

     .hyphen =>
        .{.state = .hyphenAfterModuleKeyword, .moveTo = i + 1, .action = Action { .commit = "module" } },

     .subsequentUtf8, .afterEnd, .tilde, .closeCurly, .pipe, .backtick, .openBracket, .at, .questionMark, .comma, .plus, .star, .closeParenthesis, .openParenthesis, .singleQuote, .ampersand, .percentage, .dollar, .hash, .doubleQuote, .exclamationMark, .greaterThan, .equals, .lessThan, .semiColon, .colon, .pointUp, .closeBracket, .backSlash, .forwardSlash, .fullstop =>
        .{.state = .failed, .moveTo = i, .action = .none},

     .a, .b, .c, .d, .e, .f, .g, .h, .i, .j, .k, .l, .m, .n, .o, .p, .q, .r, .s, .t, .u, .v, .w, .x, .y, .z, .A, .B, .C, .D, .E, .F, .G, .H, .I, .J, .K, .L, .M, .N, .O, .P, .Q, .R, .S, .T, .U, .V, .W, .X, .Y, .Z, .underscore, .nine, .eight, .seven, .six, .five, .four, .three, .two, .one, .zero =>
        |value| .{.state = State { .moduleStartsWithName = []ElmChar{.m, .o, .d, .u, .l, .e, value}}, .moveTo = i + 1, .action = .none,},
    };
}


fn stepModuleStartsWithModul(char: ElmChar, i: u32) Step {
    return switch(char) {
    .e => .{.state = .moduleStartsWithModule, .moveTo = i + 1, .action = .none },

    .exclamationMark, .subsequentUtf8, .afterEnd, .tilde, .closeCurly, .pipe, .backtick, .pointUp, .closeBracket, .backSlash, .at, .questionMark, .greaterThan, .lessThan, .semiColon, .forwardSlash, .fullstop, .comma, .plus, .star, .closeParenthesis, .ampersand, .percentage, .dollar, .hash =>
        .{.state = .failed, .moveTo = i, .action = .none},

    .openCurly, .openBracket, .equals, .openParenthesis, .colon, .hyphen, .singleQuote, .doubleQuote, .space, .newline =>
        .{ .state = .getTopLevelNamesForExport,
           .moveTo = i + 1,
           .action = Action { .storeExportName = []ElmChar{.m, .o, .d, .u, .l}},
        },

    .l, .z, .y, .x, .w, .v, .t, .s, .r, .q, .p, .o, .n, .m, .u, .k, .j, .i, .d, .h, .g, .f, .c, .b, .a, .underscore, .Z, .Y, .X, .W, .V, .U, .T, .S, .R, .Q, .P, .O, .N, .M, .L, .K, .J, .I, .H, .G, .F, .E, .D, .C, .B, .A, .nine, .eight, .seven, .six, .five, .four, .three, .two, .one, .zero =>
        |value|
        .{ .state = State { .moduleStartsWithName = []ElmChar{.m, .o, .d, .u, .l, value} },
           .moveTo = i + 1,
           .action = .none,
         }
    };
}

fn stepModuleStartsWithModu(char: ElmChar, i: u32) Step {
    return switch(char) {
    .l => .{.state = .moduleStartsWithModul, .moveTo = i + 1, .action = .none },
    .exclamationMark, .subsequentUtf8, .afterEnd, .tilde, .closeCurly, .pipe, .backtick, .pointUp, .closeBracket, .backSlash, .at, .questionMark, .greaterThan, .lessThan, .semiColon, .forwardSlash, .fullstop, .comma, .plus, .star, .closeParenthesis, .ampersand, .percentage, .dollar, .hash =>
        .{.state = .failed, .moveTo = i, .action = .none},

    .openCurly, .openBracket, .equals, .openParenthesis, .colon, .hyphen, .singleQuote, .doubleQuote, .space, .newline =>
        .{ .state = .getTopLevelNamesForExport,
           .moveTo = i + 1,
           .action = Action { .storeExportName = []ElmChar{.m, .o, .d, .u}},
        },

    .z, .y, .x, .w, .v, .t, .s, .r, .q, .p, .o, .n, .m, .u, .k, .j, .i, .d, .h, .g, .f, .e, .c, .b, .a, .underscore, .Z, .Y, .X, .W, .V, .U, .T, .S, .R, .Q, .P, .O, .N, .M, .L, .K, .J, .I, .H, .G, .F, .E, .D, .C, .B, .A, .nine, .eight, .seven, .six, .five, .four, .three, .two, .one, .zero =>
        |value|
        .{ .state = State { .moduleStartsWithName = []ElmChar{.m, .o, .d, .u, value} },
           .moveTo = i + 1,
           .action = .none,
         }
    };
}


fn stepModuleStartsWithMod(char: ElmChar, i: u32) Step {
    return switch(char) {
    .u => .{.state = .moduleStartsWithModu, .moveTo = i + 1, .action = .none },
    .exclamationMark, .subsequentUtf8, .afterEnd, .tilde, .closeCurly, .pipe, .backtick, .pointUp, .closeBracket, .backSlash, .at, .questionMark, .greaterThan, .lessThan, .semiColon, .forwardSlash, .fullstop, .comma, .plus, .star, .closeParenthesis, .ampersand, .percentage, .dollar, .hash =>
        .{.state = .failed, .moveTo = i, .action = .none},

    .openCurly, .openBracket, .equals, .openParenthesis, .colon, .hyphen, .singleQuote, .doubleQuote, .space, .newline =>
        .{ .state = .getTopLevelNamesForExport,
           .moveTo = i + 1,
           .action = Action { .storeExportName = []ElmChar{.m, .o, .d}},
        },

    .z, .y, .x, .w, .v, .t, .s, .r, .q, .p, .o, .n, .m, .l, .k, .j, .i, .d, .h, .g, .f, .e, .c, .b, .a, .underscore, .Z, .Y, .X, .W, .V, .U, .T, .S, .R, .Q, .P, .O, .N, .M, .L, .K, .J, .I, .H, .G, .F, .E, .D, .C, .B, .A, .nine, .eight, .seven, .six, .five, .four, .three, .two, .one, .zero =>
        |value|
        .{ .state = State { .moduleStartsWithName = []ElmChar{.m, .o, .d, value} },
           .moveTo = i + 1,
           .action = .none,
         }
    };
}

fn stepModuleStartsWithMo(char: ElmChar, i: u32) Step {
    return switch(char) {
    .d => .{.state = .moduleStartsWithMod, .moveTo = i + 1, .action = .none },
    .exclamationMark, .subsequentUtf8, .afterEnd, .tilde, .closeCurly, .pipe, .backtick, .pointUp, .closeBracket, .backSlash, .at, .questionMark, .greaterThan, .lessThan, .semiColon, .forwardSlash, .fullstop, .comma, .plus, .star, .closeParenthesis, .ampersand, .percentage, .dollar, .hash =>
        .{.state = .failed, .moveTo = i, .action = .none},

    .openCurly, .openBracket, .equals, .openParenthesis, .colon, .hyphen, .singleQuote, .doubleQuote, .space, .newline =>
        .{ .state = .getTopLevelNamesForExport,
           .moveTo = i + 1,
           .action = Action { .storeExportName = []ElmChar{.m, .o}},
        },

    .z, .y, .x, .w, .v, .u, .t, .s, .r, .q, .p, .o, .n, .m, .l, .k, .j, .i, .h, .g, .f, .e, .c, .b, .a, .underscore, .Z, .Y, .X, .W, .V, .U, .T, .S, .R, .Q, .P, .O, .N, .M, .L, .K, .J, .I, .H, .G, .F, .E, .D, .C, .B, .A, .nine, .eight, .seven, .six, .five, .four, .three, .two, .one, .zero =>
        |value|
        .{ .state = State { .moduleStartsWithName = []ElmChar{.m, .o, value} },
           .moveTo = i + 1,
           .action = .none,
         }
    };
}

fn stepModuleStartsWithM(char: ElmChar, i: u32) Step {
    return switch(char) {
    .o => .{.state = .moduleStartsWithMo, .moveTo = i + 1, .action = .none},

    .exclamationMark, .hash, .dollar, .percentage, .ampersand, .subsequentUtf8, .afterEnd, .tilde, .forwardSlash, .fullstop, .comma, .plus, .star, .closeParenthesis, .closeCurly, .pipe, .backtick, .pointUp, .closeBracket, .backSlash, .openBracket, .at, .questionMark, .greaterThan, .lessThan, .semiColon, .nine =>
        .{.state = .failed, .moveTo = i, .action = .none},

    .openCurly, .equals, .colon, .hyphen, .openParenthesis, .singleQuote, .doubleQuote, .space, .newline =>
        .{ .state = .getTopLevelNamesForExport,
           .moveTo = i + 1,
           .action = Action { .storeExportName = []ElmChar{.m} },
        },

    .z, .y, .x, .w, .v, .u, .t, .s, .r, .q, .p, .n, .m, .l, .k, .j, .i, .h, .g, .f, .e, .d, .c, .b, .a, .underscore, .Z, .Y, .X, .W, .V, .U, .T, .S, .R, .Q, .P, .O, .N, .M, .L, .K, .J, .I, .H, .G, .F, .E, .D, .C, .B, .A, .eight, .seven, .six, .five, .four, .three, .two, .one, .zero => |value|
        .{.state = State { .moduleStartsWithName = []ElmChar{.m, value } },
          .moveTo = i + 1,
          .action = .none,
        },
    };
}

const numNames = 1000;
const maxName = 50;

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

fn format(in: [big]u8, _: *[big]u8, _: *Memory) !void {
    var state: [maxNesting]State = .{.empty, .empty, .empty, .empty, .empty, .empty, .empty, .empty, .empty, .empty};
    var stateI: u8 = 0;
    var inI: u32 = 0;
    while (true) {
        const step: Step = makeStep(state[stateI], in[inI]);
        state[stateI] = step.state;
        switch (step.action) {
            .none => {},
            .finished => {
                if (stateI == 0) {
                    return;
                }

                stateI = stateI - 1;
            },
        }
    }
}

const Error = error {
    tooMuchNesting,
};

const maxNesting = 10;

const Token = enum {
    module,
};

const big = 1000_000;

pub fn main() void {
    std.debug.print("Hello", .{});
}
