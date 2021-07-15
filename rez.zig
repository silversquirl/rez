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

        // for debugging
        pub fn format(self: State, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = options;
            try writer.print("{s: >6}", .{@tagName(self)});
            switch (self) {
                .accept => {},
                .split => |next| try writer.print("     -> {} {}", .{ next[0], next[1] }),
                .lit => |lit| try writer.print(" '{}' -> {}", .{ std.fmt.fmtSliceEscapeLower(&.{lit.ch}), lit.next }),
                .set => |set| try writer.print(" #set -> {}", .{set.next}), // TODO
            }
        }
    };
    const Fragment = struct {
        start: usize,
        out: []const *usize,

        fn patch(self: Fragment, out: usize) void {
            for (self.out) |p| {
                p.* = out;
            }
        }
    };

    pub fn init(comptime pattern: []const u8, comptime flags: PatternFlags) Pattern {
        const toks = comptime parse(pattern);
        comptime var stack: []const Fragment = &.{};
        comptime var states: [toks.len + 1]State = undefined; // +1 because accept state is not represented in the tokens
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

            .class => |set| {
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

            .dot => {
                // Create char set
                var set = std.StaticBitSet(1 << 8).initFull();
                if (!flags.oneline) {
                    set.unset('\r');
                    set.unset('\n');
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

            // TODO: greedy vs. ungreedy
            .repeat => |repeat| {
                // Pop repeated fragment
                const frag = stack[stack.len - 1];
                stack.len -= 1;

                // Create split node
                states[statei] = .{ .split = .{
                    frag.start,
                    undefined,
                } };
                const split_out = [_]*usize{&states[statei].split[1]};

                switch (repeat.kind) {
                    .star => {
                        // Patch repeated fragment
                        frag.patch(statei);
                        // Push fragment starting with split node
                        stack = stack ++ [_]Fragment{.{
                            .start = statei,
                            .out = &split_out,
                        }};
                    },

                    .plus => {
                        // Patch repeated fragment
                        frag.patch(statei);
                        // Push fragment starting with repeated fragment
                        stack = stack ++ [_]Fragment{.{
                            .start = frag.start,
                            .out = &split_out,
                        }};
                    },

                    .question => {
                        // Push fragment starting with split node
                        stack = stack ++ [_]Fragment{.{
                            .start = statei,
                            .out = frag.out ++ split_out,
                        }};
                    },
                }

                statei += 1;
            },

            // TODO: capture groups
            .group => |group| if (group.n > 0) {
                // Store out array of top fragment for later
                const out = stack[stack.len - 1].out;

                // Concat grouped fragments
                var i = 1;
                while (i < group.n) : (i += 1) {
                    const frag = stack[stack.len - 1];
                    stack.len -= 1;
                    stack[stack.len - 1].patch(frag.start);
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
                stack[i].patch(state);
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

    pub fn matchStr(comptime self: Pattern, str: []const u8) bool {
        var stream = std.io.fixedBufferStream(str);
        return self.match(stream.reader()) catch |err| switch (err) {};
    }

    pub fn match(comptime self: Pattern, r: anytype) !bool {
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
        state.front.set(self.start);

        var ch: ?u8 = undefined;
        var get_next = true;
        while (true) {
            if (get_next) {
                // We've consumed a byte, get the next one
                ch = r.readByte() catch |err| switch (err) {
                    error.EndOfStream => null,
                    else => |e| return e,
                };
                get_next = false;
            } else if (state.front.findFirstSet() == null) {
                // All routes have failed
                return false;
            }

            state.swap();
            var it = state.back.iterator(.{});
            while (it.next()) |i| switch (self.states[i]) {
                .accept => return true, // A route has succeded

                .split => |next| {
                    state.front.set(next[0]);
                    state.front.set(next[1]);
                },
                .lit => |lit| if (ch) |c| if (lit.ch == c) {
                    state.front.set(lit.next);
                    get_next = true;
                },
                .set => |set| if (ch) |c| if (set.set.isSet(c)) {
                    state.front.set(set.next);
                    get_next = true;
                },
            };
        }
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
    try std.testing.expect(pat.matchStr("Hello, world!"));
    try std.testing.expect(pat.matchStr("Hello, world! foobar"));
    try std.testing.expect(!pat.matchStr("Hello, world"));
    try std.testing.expect(!pat.matchStr("Hello, world !"));
    try std.testing.expect(!pat.matchStr("Hello, world foobar"));
    try std.testing.expect(!pat.matchStr("Hello"));
}

test "character class" {
    const pat = comptime Pattern.init("a[bcd]e[f-i]j", .{});
    try std.testing.expect(pat.matchStr("abefj"));
    try std.testing.expect(pat.matchStr("acegj"));
    try std.testing.expect(pat.matchStr("adehj"));
    try std.testing.expect(pat.matchStr("abeij"));
    try std.testing.expect(!pat.matchStr("aeefj"));
    try std.testing.expect(!pat.matchStr("abejj"));
    try std.testing.expect(!pat.matchStr("ace-j"));
    try std.testing.expect(!pat.matchStr("adeej"));
}

test "dot" {
    const pat = comptime Pattern.init(
        \\a.b\.c
    , .{});
    try std.testing.expect(pat.matchStr("a*b.c"));
    try std.testing.expect(pat.matchStr("axb.c"));
    try std.testing.expect(pat.matchStr("a1b.c"));
    try std.testing.expect(pat.matchStr("a-b.c"));
    try std.testing.expect(pat.matchStr("a.b.c"));
    try std.testing.expect(!pat.matchStr("a\nb.c"));
    try std.testing.expect(!pat.matchStr("a\rb.c"));
    try std.testing.expect(!pat.matchStr("a*b*c"));
    try std.testing.expect(!pat.matchStr("axbxc"));
    try std.testing.expect(!pat.matchStr("a1b1c"));
    try std.testing.expect(!pat.matchStr("a-b-c"));
    try std.testing.expect(!pat.matchStr("a.b,c"));
}

test "dot oneline" {
    const pat = comptime Pattern.init(
        \\a.b\.c
    , .{ .oneline = true });
    try std.testing.expect(pat.matchStr("a*b.c"));
    try std.testing.expect(pat.matchStr("axb.c"));
    try std.testing.expect(pat.matchStr("a1b.c"));
    try std.testing.expect(pat.matchStr("a-b.c"));
    try std.testing.expect(pat.matchStr("a.b.c"));
    try std.testing.expect(pat.matchStr("a\nb.c"));
    try std.testing.expect(pat.matchStr("a\rb.c"));
    try std.testing.expect(!pat.matchStr("a*b*c"));
    try std.testing.expect(!pat.matchStr("axbxc"));
    try std.testing.expect(!pat.matchStr("a1b1c"));
    try std.testing.expect(!pat.matchStr("a-b-c"));
    try std.testing.expect(!pat.matchStr("a.b,c"));
    try std.testing.expect(!pat.matchStr("a\nb\nc"));
    try std.testing.expect(!pat.matchStr("a\rb\rc"));
}

test "capture group" {
    const pat = comptime Pattern.init("(ab)(cd)ef", .{});
    try std.testing.expect(pat.matchStr("abcdef"));
    try std.testing.expect(!pat.matchStr("(ab)(cd)ef"));
}

test "repetition" {
    const pat = comptime Pattern.init("a*b", .{});
    try std.testing.expect(pat.matchStr("b"));
    try std.testing.expect(pat.matchStr("ab"));
    try std.testing.expect(pat.matchStr("abc"));
    try std.testing.expect(pat.matchStr("aaaaaaab"));
    try std.testing.expect(pat.matchStr("aaaaaaabaa"));
    try std.testing.expect(!pat.matchStr("c"));
    try std.testing.expect(!pat.matchStr("acb"));
    try std.testing.expect(!pat.matchStr("aaaacb"));
}

test "complex pattern" {
    const pat = comptime Pattern.init("Hello, (hello, )*wo+rld!?", .{});
    try std.testing.expect(pat.matchStr("Hello, world!"));
    try std.testing.expect(pat.matchStr("Hello, world"));
    try std.testing.expect(pat.matchStr("Hello, wooooorld"));
    try std.testing.expect(pat.matchStr("Hello, wooooorld!"));
    try std.testing.expect(pat.matchStr("Hello, hello, hello, world!"));
    try std.testing.expect(pat.matchStr("Hello, hello, hello, woooooorld!"));
    try std.testing.expect(pat.matchStr("Hello, hello, wooorld"));
    try std.testing.expect(!pat.matchStr("hello, hello, wooorld"));
    try std.testing.expect(!pat.matchStr("Hello, hello, wrld"));
    try std.testing.expect(!pat.matchStr("Hello, Hello, world!"));
}

/// Parses a regex pattern into a sequence of tokens
fn parse(comptime pattern: []const u8) []const Token {
    comptime {
        var stream = std.io.fixedBufferStream(pattern);
        const r = stream.reader();
        const err = struct {
            fn unexpected(sym: []const u8) noreturn {
                @compileError(std.fmt.comptimePrint(
                    "Unexpected '{}' at index {}",
                    .{ std.fmt.fmtSliceEscapeLower(sym), stream.pos },
                ));
            }

            fn unclosed(thing: []const u8, start: usize) noreturn {
                @compileError(std.fmt.comptimePrint(
                    "Unclosed {s} starting at index {}",
                    .{ thing, start },
                ));
            }
        };

        var groups: usize = 0;
        var stack: []const usize = &.{}; // Group stack - stores start indices into toks
        var toks: []const Token = &.{}; // Output token stream
        var escape = false;

        while (r.readByte()) |ch| {
            var tok: ?Token = null;
            if (escape) {
                escape = false;
                tok = .{ .ch = ch };
            } else switch (ch) {
                '\\' => escape = true,

                '[' => {
                    const start = stream.pos;
                    var set = std.StaticBitSet(1 << 8).initEmpty();
                    var invert = false;
                    var prev: ?u8 = null;
                    while (r.readByte()) |ch2| {
                        const at_start = stream.pos == start + 1;
                        if (escape) {
                            escape = false;
                        } else switch (ch2) {
                            '\\' => { // Escape char
                                escape = true;
                                continue;
                            },
                            '^' => if (at_start) { // Invert set
                                invert = true;
                                continue;
                            },

                            '-' => if (prev) |sc| {
                                var ec = r.readByte() catch continue;
                                if (ec == ']') { // Trailing -, add it to the set and finish
                                    set.set(ch2);
                                    break;
                                }

                                while (ec > sc) : (ec -= 1) {
                                    set.set(ec);
                                }
                                continue;
                            },

                            ']' => break, // End of set
                            else => {},
                        }

                        set.set(ch2);
                        prev = ch2;
                    } else |e| switch (e) {
                        error.EndOfStream => err.unclosed("character class", start),
                    }

                    if (invert) {
                        set.toggleAll();
                    }
                    tok = .{ .class = set };
                },

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
        } else |e| switch (e) {
            error.EndOfStream => {},
        }

        if (stack.len > 0) {
            err.unclosed("group", stack[stack.len - 1]);
        }

        return toks;
    }
}

const Token = union(enum) {
    ch: u8, // Literal character (TODO: unicode support)
    class: std.StaticBitSet(1 << 8), // Character class (TODO: unicode support)
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

    try expectParse(&.{
        .{ .ch = ' ' },
        .{ .class = comptime blk: {
            var set = std.StaticBitSet(1 << 8).initEmpty();
            var i: u8 = 'a';
            while (i <= 'z') : (i += 1) {
                set.set(i);
            }
            set.set(']');
            set.set('\\');
            break :blk set;
        } },
        .{ .repeat = .{ .kind = .plus } },
        .{ .class = comptime blk: {
            var set = std.StaticBitSet(1 << 8).initEmpty();
            set.set('-');
            set.set('a');
            break :blk set;
        } },
        .{ .class = comptime blk: {
            var set = std.StaticBitSet(1 << 8).initFull();
            set.unset('a');
            set.unset('b');
            set.unset('c');
            break :blk set;
        } },
        .{ .class = comptime blk: {
            var set = std.StaticBitSet(1 << 8).initFull();
            var i: u8 = 'a';
            while (i <= 'z') : (i += 1) {
                set.unset(i);
            }
            set.unset('A');
            set.unset('-');
            break :blk set;
        } },
    },
        \\ [a-z\]\\]+[-a][^abc][^a-zA-]
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
