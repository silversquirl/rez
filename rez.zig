// TODO: Unicode support
// TODO: Compile regex to bytecode for more compact representation (or maybe generate Zig code?)

const std = @import("std");

pub const Pattern = struct {
    start: usize,
    states: []const State,

    const State = union(enum) {
        accept: void, // Accept the string and finish the match
        split: [2]usize, // Split off to two different nodes
        lit: struct { // Consume one literal byte
            ch: u8,
            next: usize,
        },
        set: struct { // Consume one byte in set
            set: std.StaticBitSet(1 << 8),
            next: usize,
        },
    };
    const Fragment = struct {
        start: usize,
        out: []const *usize,
    };

    pub fn init(comptime pattern: []const u8, comptime flags: PatternFlags) Pattern {
        const toks = comptime parse(pattern);
        comptime var stack: []const Fragment = &.{};
        comptime var states: [toks.len + 1]State = undefined; // +1 because of accept state
        comptime var statei: usize = 0;

        comptime for (toks) |tok| switch (tok) {
            .ch => |ch| {
                // Push literal match
                states[statei] = .{ .lit = .{
                    .ch = ch,
                    .next = undefined,
                } };
                stack = stack ++ [_]Fragment{.{
                    .start = statei,
                    .out = &.{&states[statei].lit.next},
                }};
                statei += 1;
            },

            .class => unreachable,

            .dot => {
                // Create char set
                var set = std.StaticBitSet(1 << 8).initFull();
                if (!flags.oneline) {
                    set.unset('.');
                }

                // Push set match
                states[statei] = .{ .set = .{
                    .set = set,
                    .next = undefined,
                } };
                stack = stack ++ [_]Fragment{.{
                    .start = statei,
                    .out = &.{&states[statei].set.next},
                }};
                statei += 1;
            },

            .repeat => unreachable, // TODO

            // TODO: capture groups
            .group => |group| if (group.n > 0) {
                // Store out array of top fragment for later
                const out = stack[stack.len - 1].out;

                // Pop and link all but last fragment
                var i = 1;
                while (i < group.n) : (i += 1) {
                    const frag = stack[stack.len - 1];
                    stack.len -= 1;
                    for (stack[stack.len - 1].out) |p| {
                        p.* = frag.start;
                    }
                }

                // Patch in the new out array
                var frag = stack[stack.len - 1];
                frag.out = out;
                stack.len -= 1;
                stack = stack ++ [_]Fragment{frag};
            },
        };

        comptime {
            // Push accept
            states[statei] = .accept;
            var state = statei;
            statei += 1;

            // Concat all fragments
            var i = stack.len;
            while (i > 0) {
                i -= 1;
                for (stack[i].out) |p| {
                    p.* = state;
                }
                state = stack[i].start;
            }
        }

        const state_slice: []const State = states[0..statei];
        return .{
            .start = stack[0].start,
            // Concatenating with an empty array copies the slice, ensuring any unused entries are discarded
            .states = state_slice ++ [_]State{},
        };
    }

    pub fn match(comptime self: Pattern, str: []const u8) bool {
        const Set = std.StaticBitSet(self.states.len);
        const EvalState = if (@sizeOf(Set) <= @sizeOf(usize))
            struct {
                front: Set = Set.initEmpty(),
                back: Set = undefined,
                fn init(_: @This()) void {}
                fn swap(st: *@This()) void {
                    std.mem.swap(Set, &st.front, &st.back);
                    st.front = Set.initEmpty();
                }
            }
        else
            struct {
                sets: [2]Set = [_]Set{Set.initEmpty()} ** 2,
                front: *Set = undefined,
                back: *Set = undefined,
                fn init(st: *@This()) void {
                    st.front = &st.sets[0];
                    st.back = &st.sets[1];
                    st.front.* = Set.initEmpty();
                }
                fn swap(st: *@This()) void {
                    std.mem.swap(*Set, &st.front, &st.back);
                    st.front.* = Set.initEmpty();
                }
            };

        var state = EvalState{};
        state.init();
        state.front.set(0);

        for (str) |ch| {
            state.swap();
            var it = state.back.iterator(.{});
            while (it.next()) |i| switch (self.states[i]) {
                .accept => return true,
                .split => |next| {
                    state.front.set(next[0]);
                    state.front.set(next[1]);
                },
                .lit => |lit| if (lit.ch == ch) {
                    state.front.set(lit.next);
                },
                .set => |set| if (set.set.isSet(ch)) {
                    state.front.set(set.next);
                },
            };

            if (state.front.findFirstSet() == null) {
                // All routes have failed
                return false;
            }
        }

        var it = state.front.iterator(.{});
        while (it.next()) |i| {
            if (self.states[i] == .accept) {
                return true;
            }
        }
        return false;
    }

    const StateList = std.ArrayListUnmanaged(usize);
};

pub const PatternFlags = struct {
    multiline: bool = false, // ^ and $ match start/end of line
    insensitive: bool = false, // Matches are performed case-insensitively
    oneline: bool = false, // Dot matches newline
    ungreedy: bool = false, // Quantifiers are lazy
};

test "literal pattern" {
    const pat = comptime Pattern.init("Hello, world!", .{});
    try std.testing.expect(pat.match("Hello, world!"));
    try std.testing.expect(pat.match("Hello, world! foobar"));
    try std.testing.expect(!pat.match("Hello, world"));
    try std.testing.expect(!pat.match("Hello, world !"));
    try std.testing.expect(!pat.match("Hello, world foobar"));
    try std.testing.expect(!pat.match("Hello"));
}

test "capture groups" {
    const pat = comptime Pattern.init("(ab)(cd)ef", .{});
    try std.testing.expect(pat.match("abcdef"));
    try std.testing.expect(!pat.match("(ab)(cd)ef"));
}

// TODO
// test "repetition" {
//     const pat = Pattern.init("a*b", .{});
// }
// test "complex pattern" {
//     const pat = Pattern.init("Hello, (hello, )* wo+rld!?", .{});
// }

/// Parses a regex pattern into a sequence of tokens
fn parse(comptime pattern: []const u8) []const Token {
    comptime {
        var groups: usize = 0;
        var stack: []const usize = &.{}; // Group stack - stores start indices into toks
        var toks: []const Token = &.{}; // Output token stream
        var escape = false;

        for (pattern) |ch, i| {
            const err = struct {
                fn unexpected(comptime sym: []const u8) noreturn {
                    @compileError(std.fmt.comptimePrint(
                        "Unexpected '{}' at index {}",
                        .{ std.fmt.fmtSliceEscapeLower(sym), i },
                    ));
                }
            };

            var tok: ?Token = null;
            if (escape) {
                escape = false;
                tok = .{ .ch = ch };
            } else switch (ch) {
                '\\' => escape = true,

                '[' => unreachable,
                '.' => tok = .dot,

                '*' => tok = .{ .repeat = .{ .kind = .star } },
                '+' => tok = .{ .repeat = .{ .kind = .plus } },
                '{' => unreachable,

                '?' => {
                    if (toks.len > 0 and toks[toks.len - 1] == .repeat) {
                        // If we succeed another repetition, make that one lazy
                        var prev = toks[toks.len - 1];
                        if (prev.repeat.greedy) {
                            prev.repeat.greedy = false;
                            toks.len -= 1;
                            toks = toks ++ [_]Token{prev};
                        } else {
                            err.unexpected("?");
                        }
                    } else {
                        tok = .{ .repeat = .{ .kind = .question } };
                    }
                },

                // TODO: named groups
                // TODO: non-capturing groups
                '(' => stack = stack ++ [_]usize{toks.len},
                ')' => {
                    const start = stack[stack.len - 1];
                    stack.len -= 1;

                    const name = std.fmt.comptimePrint("{}", .{groups});
                    groups += 1;

                    tok = .{ .group = .{
                        .n = toks.len - start,
                        .name = name,
                    } };
                },

                else => tok = .{ .ch = ch },
            }

            if (tok) |t| {
                toks = toks ++ [_]Token{t};
            }
        }

        if (stack.len > 0) {
            @compileError(std.fmt.comptimePrint(
                "Unclosed group starting at index {}",
                .{stack[stack.len - 1]},
            ));
        }

        return toks;
    }
}

const Token = union(enum) {
    ch: u8, // Literal character (TODO: unicode support)
    class: []const u8, // Character class, as an un-parsed string
    dot: void, // Dot (any char, or [^\r\n] depending on oneline flag)
    repeat: struct { // Repetition: *, + or ?
        kind: enum { star, plus, question },
        greedy: bool = true, // Inverse if ungreedy flag is set
    },
    group: struct { // Group the previous n items
        n: usize,
        name: ?[]const u8, // null if non-capturing
    },
};

test "parse" {
    try expectParse(&.{
        .{ .ch = 'a' },
        .{ .ch = 'b' },
        .{ .ch = 'c' },
    }, "abc");

    try expectParse(&.{
        .{ .ch = 'a' },
        .{ .group = .{
            .n = 1,
            .name = "0",
        } },
        .{ .ch = 'b' },
        .{ .ch = 'c' },
        .{ .group = .{
            .n = 2,
            .name = "1",
        } },
        .{ .repeat = .{ .kind = .star } },
        .{ .ch = 'd' },
        .{ .ch = 'e' },
        .{ .repeat = .{ .kind = .question } },
    }, "(a)(bc)*de?");

    try expectParse(&.{
        .{ .ch = 'a' },
        .{ .repeat = .{
            .kind = .star,
            .greedy = false,
        } },
        .{ .ch = 'b' },
        .{ .repeat = .{
            .kind = .plus,
            .greedy = false,
        } },
        .dot,
        .{ .repeat = .{
            .kind = .question,
            .greedy = false,
        } },
    }, "a*?b+?.??");

    try expectParse(&.{
        .{ .ch = ' ' },
        .{ .ch = '.' },
        .{ .ch = '*' },
        .{ .ch = '+' },
        .{ .ch = '{' },
        .{ .ch = '(' },
    },
        \\ \.\*\+\{\(
    );
}

fn expectParse(expected: []const Token, comptime pattern: []const u8) !void {
    const actual = parse(pattern);
    if (expected.len != actual.len) {
        std.debug.print("slice lengths differ. expected {}, found {}\n", .{ expected.len, actual.len });
        return error.TestExpectedEqual;
    }
    for (expected) |e, i| {
        const a = actual[i];
        if (!std.meta.eql(e, a)) {
            std.debug.print("index {} incorrect. expected {}, found {}\n", .{ i, e, a });
            return error.TestExpectedEqual;
        }
    }
}
