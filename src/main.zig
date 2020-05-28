const std = @import("std");
const meta = std.meta;
const Alloc = std.mem.Allocator;

const OOM = Alloc.Error;

/// the stdlib comptime hashmap is only created once and makes a "perfect hash"
/// so this works I guess. for strings with type values;
///
/// comptime "hash"map. all accesses and sets are ~~O(1)~~ O(n)
pub fn ComptimeHashMap(comptime Key: type, comptime Value: type) type {
    if (Key != []const u8) @compileError("key must be []const u8");
    const Item = struct { key: Key, value: Value };
    return struct {
        const HM = @This();
        items: []const Item,
        pub fn init() HM {
            return HM{
                .items = &[_]Item{},
            };
        }
        fn findIndex(comptime hm: *HM, comptime key: Key) ?u64 {
            for (hm.items) |itm, i| {
                if (std.mem.eql(u8, itm.key, key)) {
                    return i;
                }
            }
            return null;
        }
        fn get(comptime hm: *HM, comptime key: Key) ?Value {
            if (hm.findIndex(key)) |indx| return hm.items[indx].value;
            return null;
        }
        pub fn set(comptime hm: *HM, comptime key: Key, comptime value: Value) ?Value {
            if (hm.findIndex(key)) |prevIndex| {
                const prev = hm.items[prevIndex];
                hm.items[prevIndex] = value;
                return prev;
            }
            hm.items = hm.items ++ &[_]Item{Item{ .key = key, .value = value }};
            return null;
        }
    };
}

// type Parse = {
//     parse: ParseFN;
//     toString: ToStringFN;
//     scb: (cb: PostFN) => Parse;
// };

// errors are going to be user defined (eg with error()) so that is why ?*void
// so how do user defined error texts propagate? union{error: ..., value: *void}?
const Point = struct {
    byte: u64,
    line: u64,
    char: u64,
    pub const start = Point{ .byte = 0, .line = 0, .char = 0 };
    pub fn extend(start_: Point, text: []const u8) Range {
        var end: Point = .{ .byte = start_.byte, .line = start_.line, .char = start_.char };
        for (text) |char| {
            end.byte += 1;
            end.char += 1;
            if (char == '\n') {
                end.line += 1;
                end.char = 0;
            }
        }
        return .{ .start = start_, .end = end };
    }
};
const Range = struct {
    start: Point,
    end: Point,
    pub const todo = Range{
        .start = .{ .byte = 0, .line = 0, .char = 0 },
        .end = .{ .byte = 0, .line = 0, .char = 0 },
    };
};
const UncastedValue = *const @OpaqueType(); // @Type(.Opaque) seems to be TODO in 0.6.0
const ErrResult = struct {
    message: []const u8,
    range: Range,
};
const ParseResult = union(enum) {
    result: struct {
        value: UncastedValue,
        range: Range,
    },
    errmsg: ErrResult,
};
const ParseFn = fn (text: []const u8, start: Point, alloc: *Alloc) OOM!ParseResult;
const ParseDetails = struct {
    parse: ParseFn,
    ReturnType: type,
};

fn castParseResult(comptime RV: type, anyPtr: UncastedValue) *const RV {
    return @ptrCast(*const RV, @alignCast(8, anyPtr));
}

/// handler: null | fn(a: var, b: var) type;
pub fn createStringParse(
    comptime spec: []const u8,
    comptime handler: var,
    comptime HandlerReturnType: ?type,
) ParseDetails {
    const nullHandler = @TypeOf(handler) == @TypeOf(null);
    const ReturnType = if (nullHandler)
        struct { value: []const u8, range: Range }
    else
        HandlerReturnType.?;
    // @typeInfo(handler).Fn.return_type.?;

    const parseFn: ParseFn = struct {
        fn a(text: []const u8, start: Point, alloc: *Alloc) OOM!ParseResult {
            if (text.len < spec.len)
                return ParseResult{
                    .errmsg = .{
                        .message = "Expected `" ++ spec ++ "`",
                        .range = start.extend("  "),
                    },
                };
            for (spec) |char, i| {
                if (char != text[i])
                    return ParseResult{
                        .errmsg = .{
                            .message = "Expected `" ++ spec ++ "`",
                            .range = start.extend("  "),
                        },
                    };
            }
            const range = Range.todo;
            const strCopy = try std.mem.dupe(alloc, u8, spec);
            if (nullHandler) {
                return ReturnType{
                    .result = .{
                        .value = strCopy,
                        .range = range,
                    },
                };
            }
            var res = try alloc.create(ReturnType);
            res.* = handler(strCopy, range);
            return ParseResult{
                .result = .{
                    .value = @ptrCast(UncastedValue, res),
                    .range = range,
                },
            };
        }
    }.a;
    return ParseDetails{
        .parse = parseFn,
        .ReturnType = ReturnType,
    };
}

pub fn isString(comptime SomeT: type) bool {
    const ti = @typeInfo(SomeT);
    if (ti == .Pointer and ti.Pointer.child == u8 and ti.Pointer.size == .Slice) return true;
    if (ti == .Pointer) {
        const chldi = @typeInfo(ti.Pointer.child);
        if (chldi == .Array and chldi.Array.child == u8) return true;
    }
    if (ti == .Array and ti.Array.child == u8) return true;
    return false;
}

pub fn userToReal(
    comptime spec: var,
    comptime handler: var,
    comptime HandlerReturnType: ?type,
) ParseDetails {
    const Spec = @TypeOf(spec);
    if (isString(Spec))
        return createStringParse(@as([]const u8, spec), handler, HandlerReturnType);
    @compileError("Unsupported: " ++ @typeName(Spec));
}

pub fn createParse(
    comptime name: []const u8,
    comptime spec: var,
    comptime handler: var,
    comptime HandlerReturnType: ?type,
) ParseDetails {
    return userToReal(spec, handler, HandlerReturnType);
}

pub fn Parser(comptime spec: var, comptime Handlers: type) type {
    comptime {
        var parses = ComptimeHashMap([]const u8, ParseDetails).init();

        for (meta.fields(@TypeOf(spec))) |field| {
            if (parses.set(field.name, createParse(
                field.name,
                @field(spec, field.name),
                @field(Handlers, field.name),
                @field(Handlers, field.name ++ "_RV"),
            ))) |detyls| @compileError("Duplicate key " ++ field.name);
        }

        return struct {
            pub fn parse(
                comptime key: var,
                text: []const u8,
                start: Point,
                alloc: *Alloc,
            ) OOM!union {
                result: struct {
                    value: *const parses.get(@tagName(key)).?.ReturnType,
                    range: Range,
                },
                errmsg: ErrResult,
            } {
                const parseDetails = parses.get(@tagName(key)).?;
                return switch (try parseDetails.parse(text, start, alloc)) {
                    .result => |res| blk: {
                        const result = castParseResult(parseDetails.ReturnType, res.value);
                        break :blk .{ .result = .{ .value = result, .range = res.range } };
                    },
                    .errmsg => |emsg| blk: {
                        break :blk .{ .errmsg = emsg };
                    },
                };
            }
        };
    }
}

const Ast = union(enum) {
    ifStatment: struct {
        moreAst: *Ast,
    },
};
pub fn main() !void {
    var alloc = std.heap.page_allocator;
    var arenaAllocator = std.heap.ArenaAllocator.init(alloc);
    defer arenaAllocator.deinit();
    var arena = &arenaAllocator.allocator;

    const parser = Parser(.{
        .expression = "test",
    }, struct {
        pub const expression_RV = []const u8;
        pub fn expression(text: var, pos: var) expression_RV {
            return text;
        }
    });
    const res = (try parser.parse(.expression, "test", Point.start, arena));
    std.debug.warn("res: {}\n", .{res.result.value.*});
    const res2 = (try parser.parse(.expression, "tst", Point.start, arena));
    std.debug.warn("err: {}\n", .{res2.errmsg.message});
}

test "" {}
