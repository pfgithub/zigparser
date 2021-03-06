const std = @import("std");
const DiscardableAllocator = @import("DiscardableAllocator.zig");
const meta = std.meta;
const testing = std.testing;
const Alloc = std.mem.Allocator;

const OOM = Alloc.Error;

/// the stdlib comptime hashmap is only created once and makes a "perfect hash"
/// so this works I guess.
///
/// comptime "hash"map. all accesses and sets are ~~O(1)~~ O(n)
fn ComptimeHashMap(comptime Key: type, comptime Value: type) type {
    const Item = struct { key: Key, value: Value };
    return struct {
        const HM = @This();
        items: []const Item,
        pub fn init() HM {
            return HM{
                .items = &[_]Item{},
            };
        }
        fn findIndex(comptime hm: HM, comptime key: Key) ?u64 {
            for (hm.items) |itm, i| {
                if (Key == []const u8) {
                    if (std.mem.eql(u8, itm.key, key))
                        return i;
                } else if (itm.key == key)
                    return i;
            }
            return null;
        }
        pub fn get(comptime hm: HM, comptime key: Key) ?Value {
            if (hm.findIndex(key)) |indx| return hm.items[indx].value;
            return null;
        }
        pub fn set(comptime hm: *HM, comptime key: Key, comptime value: Value) ?Value {
            if (hm.findIndex(key)) |prevIndex| {
                const prev = hm.items[prevIndex].value;
                // hm.items[prevIndex].value = value; // did you really think it would be that easy?
                var newItems: [hm.items.len]Item = undefined;
                for (hm.items) |prevItem, i| {
                    if (i == prevIndex) {
                        newItems[i] = Item{ .key = prevItem.key, .value = value };
                    } else {
                        newItems[i] = prevItem;
                    }
                }
                hm.items = &newItems;
                return prev;
            }
            hm.items = hm.items ++ &[_]Item{Item{ .key = key, .value = value }};
            return null;
        }
    };
}

const TypeIDMap = struct {
    latestID: u64,
    hm: ComptimeHashMap(type, u64),
    infoString: []const u8,
    pub fn init() TypeIDMap {
        return .{
            .latestID = 0,
            .hm = ComptimeHashMap(type, u64).init(),
            .infoString = "",
        };
    }
    pub fn get(comptime tidm: *TypeIDMap, comptime Type: type) u64 {
        if (tidm.hm.get(Type)) |index| return index;
        tidm.latestID += 1;
        if (tidm.hm.set(Type, tidm.latestID)) |_| @compileError("never");
        tidm.infoString = tidm.infoString ++ comptime comptimeFmt("ID {} - {}", .{ tidm.latestID, typePrint(Type, 0) }) ++ "\n";
        // @compileLog("ID", tidm.latestID, "=", Type);
        return tidm.latestID;
    }
};

pub const Point = struct {
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
pub const Range = struct {
    start: Point,
    end: Point,
};
/// a pointer to arbitrary data. panics if attempted to be read as the wrong type.
const AnyPtr = comptime blk: {
    var typeIDMap = TypeIDMap.init();
    break :blk struct {
        pointer: usize,
        typeID: u64,
        fn fromPtr(value: var) AnyPtr {
            const ti = @typeInfo(@TypeOf(value));
            if (ti != .Pointer) @compileError("must be *ptr");
            if (ti.Pointer.size != .One) @compileError("must be ptr to one item");
            const typeID = comptime typeIDMap.get(ti.Pointer.child);
            return .{ .pointer = @ptrToInt(value), .typeID = typeID };
        }
        fn readAs(any: AnyPtr, comptime RV: type) *const RV {
            const typeID = comptime typeIDMap.get(RV);
            if (any.typeID != typeID)
                std.debug.panic(
                    "\x1b[31mError!\x1b(B\x1b[m Item is of type {}, but was read as type {}. Type names ({}):\n{}",
                    .{ any.typeID, typeID, typeIDMap.latestID, typeIDMap.infoString },
                );
            return @intToPtr(*const RV, any.pointer);
        }
    };
};
const ErrResult = struct {
    message: []const u8,
    range: Range,
    /// if it is possible to allow this error, eg by going to the next item
    /// in an or or repeating again in a star/plus
    recoverable: bool,
};
const ParseResult = union(enum) {
    result: struct {
        value: AnyPtr,
        range: Range,
    },
    errmsg: ErrResult,
};
const ParseFn = fn (text: []const u8, start: Point, x: InternalExtra) OOM!ParseResult;
const TopLevelParseDetails = struct {
    details: ?type,
    ReturnType: type,
    index: usize,
};

const InternalExtra = struct {
    alloc: *Alloc,
    discardable: *DiscardableAllocator,
    parseFns: []const ParseFn,
};
pub const Extra = struct {
    range: Range,
    alloc: *Alloc,
    discardable: *DiscardableAllocator,
};

fn ErrorOr(comptime Type: type) type {
    return union(enum) {
        result: struct {
            value: Type,
            range: Range,
            // only the value should get passed
            // to the handler
        },
        errmsg: ErrResult,
    };
}

// unrelated note how to fix memory leaks:
// require _RV struct/enum/unions to have a deinit method
// disallow pointers in _RV
// that would be annoying though so arena allocator is fine for now
// or, at no cost to the user:
// temporaryallocator
//
// talloc = TemporaryAllocator(std.heap.page_allocator);
// var tblk = talloc.start();
//   // attempt to parse some item in union
// if(err) tblk.trash();
// tblk.end();
//
// that would work well and not require the user to do anything
// neat

// with this new system, ranges won't be given any more by default
// so there will need to be a range() to use if you need a range for
// some reason.
// pointers are only created with runtime parse fns (vvv) which
// are only used by futureParses.
fn createParseFn(comptime parseDetails: type) ParseFn {
    return struct {
        pub fn f(text: []const u8, start: Point, x: InternalExtra) OOM!ParseResult {
            const result = try parseDetails.parse(text, start, x);
            if (result == .errmsg) {
                return ParseResult{ .errmsg = result.errmsg };
            }
            const allocatedResult = try x.alloc.create(@TypeOf(result.result.value));
            allocatedResult.* = result.result.value;
            return ParseResult{
                .result = .{
                    .value = AnyPtr.fromPtr(allocatedResult),
                    .range = result.result.range,
                },
            };
        }
    }.f;
}

pub fn CreateStringParse(
    comptime spec: []const u8,
    comptime handler: var,
    comptime HandlerReturnType: ?type,
) type {
    const Handler = CreateHandler([]const u8, handler, HandlerReturnType);

    return struct {
        pub fn parse(fulltext: []const u8, start: Point, x: InternalExtra) OOM!Handler.FnReturn {
            const text = fulltext[start.byte..];
            if (text.len < spec.len)
                return Handler.errorValue(
                    "Expected `" ++ spec ++ "` (too short)",
                    start.extend("  "),
                );
            for (spec) |char, i| {
                if (char != text[i])
                    return Handler.errorValue(
                        "Expected `" ++ spec ++ "`",
                        start.extend("  "),
                    );
            }
            const range = start.extend(spec);
            const strCopy = try std.mem.dupe(x.alloc, u8, spec);
            return Handler.returnValue(strCopy, range, x);
        }
        pub const ReturnType = Handler.Return;
    };
}
pub fn CreateRangeParse(
    comptime ranges: []const CharRange,
    comptime handler: var,
    comptime HandlerReturnType: ?type,
) type {
    const Handler = CreateHandler(u8, handler, HandlerReturnType);

    return struct {
        pub fn parse(fulltext: []const u8, start: Point, x: InternalExtra) OOM!Handler.FnReturn {
            const text = fulltext[start.byte..];
            if (text.len < 1) return Handler.errorValue("Expected char in range, got (too short)", start.extend("   "));
            const char = text[0];
            var pass = false; // once again, would use for else but no
            inline for (ranges) |rng| {
                if (char >= rng.start and char <= rng.end) {
                    pass = true;
                    break;
                }
            }
            const range = start.extend(char);
            return Handler.returnValue(char, range, x);
        }
        pub const ReturnType = Handler.Return;
    };
}
pub fn comptimeFmt(comptime fmt: []const u8, comptime arg: var) []const u8 {
    return "comptime fmt disabled";
    // comptime {
    //     const width = std.fmt.count(fmt, arg);
    //     var buf: [width]u8 = undefined;
    //     return std.fmt.bufPrint(&buf, fmt, arg) catch unreachable;
    // }
}
//todo check seen and don't reprint infinitely
pub fn typePrint(comptime Type: type, comptime indentationLevel: u64) []const u8 {
    if (indentationLevel > 10) return "..."; // good enough
    const indent = " " ** 4;
    const ti = @typeInfo(Type);
    return switch (ti) {
        .Struct => |stru| blk: {
            var res: []const u8 = "struct {";
            const newline = "\n" ++ (indent ** indentationLevel);
            for (stru.fields) |field| {
                res = res ++ newline ++ indent ++ field.name ++ ": " ++ typePrint(field.field_type, indentationLevel + 1) ++ ",";
            }
            for (stru.decls) |decl| {
                res = res ++ newline ++ indent ++ "const " ++ decl.name ++ ";";
            }
            res = res ++ newline ++ "}";
            break :blk @typeName(Type);
        },
        else => @typeName(Type),
    };
}
/// hmm...
pub fn CreateTuple(comptime items: var) type {
    const oft = struct {
        fn oft(comptime T: type) T {
            return undefined;
        }
    }.oft;
    var resultTuple: type = @TypeOf(.{});
    for (items) |Item| {
        resultTuple = @TypeOf(oft(resultTuple) ++ .{oft(Item)});
    }
    return resultTuple;
}
// rip:
// test "createTuple" {
//     const Q = CreateTuple(.{ i32, i64, []const u8 });
//     var q: Q = undefined;
//     // f these are const: (#5486)
//     q[0] = 5;
//     q[1] = 10;
//     q[2] = "test";
// }
// staying with []AnyPtr until @Type(structs/tuples) I guess

// ideally, this would be a tuple created at comptime
// and with the above CreateTuple function that can almost be accomplished
// unfortunately, https://github.com/ziglang/zig/issues/5486
pub fn TypedList(comptime spec: []const type) type {
    return struct {
        items: [spec.len]struct { value: AnyPtr, range: Range } = undefined,
        range: Range, // although I'm getting rid of ranges, if .get() is required anyway why not include a range
        pub fn get(list: @This(), comptime index: u64) spec[index].ReturnType {
            return list.items[index].value.readAs(spec[index].ReturnType).*;
        }
        // is this necessary? wouldn't range(SomeParse) be better? and then this could just be []AnyPtr
        // until comptime tuple creation is available?
        pub fn getRange(list: @This(), index: u64) Range {
            return list.items[index].range;
        }
        pub fn set(list: *@This(), i: usize, value: var, range: Range, alloc: *Alloc) OOM!void {
            var allocatedValue = try alloc.create(@TypeOf(value));
            allocatedValue.* = value;
            list.items[i] = .{ .value = AnyPtr.fromPtr(allocatedValue), .range = range };
        }
    };
}

// ideally, this would be a union(enum) created at comptime
// might have some extra space but probably better than
// allocating for everything stored in a union
pub fn TypedUnion(comptime spec: []const type) type {
    return struct {
        value: AnyPtr,
        range: Range, // same as above
        index: std.math.IntFittingRange(0, spec.len - 1), // not really necessary, usize is basically the same
        const Ths = @This();
        pub fn from(comptime idx: usize, value: var, range: Range, alloc: *Alloc) OOM!Ths {
            if (@TypeOf(value) != spec[idx].ReturnType) @compileError("wrong");
            var allocatedValue = try alloc.create(@TypeOf(value));
            allocatedValue.* = value;
            return Ths{ .value = AnyPtr.fromPtr(allocatedValue), .range = range, .index = idx };
        }
        pub fn get(me: @This(), comptime idx: var) ?spec[idx].ReturnType {
            if (@typeInfo(@TypeOf(idx)) == .Struct) {
                const MustMatch = spec[idx[0]].ReturnType;
                inline for (idx) |iv, i| {
                    if (spec[i].ReturnType != MustMatch)
                        @compileError("All listed values must be the same to use .get(.{1, 2, 3...}).\none was: " ++
                            typePrint(MustMatch, 0) ++ ", but another was: " ++ typePrint(spec[i].ReturnType, 0));
                    if (me.index == i)
                        break true;
                } else return null;
            } else if (me.index != idx) return null;
            const details = spec[idx];
            return this.value.readAs(details.ReturnType).*;
        }
        // has to be generic so the comptime doesn't run unless
        // .any is actually used
        /// pub fn any(me: Ths) spec[0].ReturnType
        pub fn any(me_generic: var) spec[0].ReturnType {
            const MustMatch = spec[0].ReturnType;
            comptime {
                for (spec) |itm| {
                    if (itm.ReturnType != MustMatch)
                        @compileError("all possible values must be the same to use .any() on TypedUnion\n" ++
                            "one was: " ++ typePrint(MustMatch, 0) ++ "\nbut another was: " ++ typePrint(itm.ReturnType, 0));
                }
            }
            if (@typeInfo(@TypeOf(me_generic)) == .Pointer)
                return realAny(me_generic.*);
            return realAny(me_generic);
        }
        fn realAny(me: Ths) spec[0].ReturnType {
            const MustMatch = spec[0].ReturnType;
            return me.value.readAs(MustMatch).*;
        }
    };
}
pub fn CreateHandler(comptime Arg0: type, comptime handler: var, comptime HandlerReturnType: ?type) type {
    const DefaultReturnType = Arg0;
    return struct {
        pub const Return = HandlerReturnType orelse DefaultReturnType;
        pub const FnReturn = ErrorOr(Return);
        pub fn handle(value: Arg0, x: Extra) OOM!Return {
            if (@TypeOf(handler) == @TypeOf(null)) {
                return value; // f range. you will not be missed.
            } else {
                const argslen = @typeInfo(@TypeOf(handler)).Fn.args.len;
                if (argslen == 2)
                    return handler(value, x)
                else if (argslen == 1)
                    return handler(value)
                else
                    @compileError("wrong number of arguments");
            }
        }

        pub fn errorValue(emsg: []const u8, range: Range) FnReturn {
            return FnReturn{
                .errmsg = .{
                    .message = emsg,
                    .range = range,
                    .recoverable = true,
                },
            };
        }
        pub fn errorCopy(e: ErrResult) FnReturn {
            return .{ .errmsg = e };
        }
        pub fn returnValue(value: var, range: Range, x: InternalExtra) OOM!FnReturn {
            return FnReturn{
                .result = .{
                    .value = try handle(value, .{
                        .range = range,
                        .alloc = x.alloc,
                        .discardable = x.discardable,
                    }),
                    .range = range,
                },
            };
        }
    };
}

pub fn CreateOrderedParse(
    comptime spec: []const type,
    comptime handler: var,
    comptime HandlerReturnType: ?type,
) type {
    const List = TypedList(spec);

    const Handler = CreateHandler(List, handler, HandlerReturnType);

    return struct {
        fn parse(fulltext: []const u8, start: Point, x: InternalExtra) OOM!Handler.FnReturn {
            const text = fulltext[start.byte..];
            var result: List = .{ .range = undefined };
            var currentPoint: Point = start;
            inline for (spec) |specItem, i| {
                const parseResult = try specItem.parse(fulltext, currentPoint, x);
                if (parseResult == .errmsg) return Handler.errorCopy(parseResult.errmsg);
                currentPoint = parseResult.result.range.end;
                // uh oh items[i] is not going to be of the right type
                // oops
                // f this is going to allocate until we can create tuples at comptime
                try result.set(i, parseResult.result.value, parseResult.result.range, x.alloc);
            }

            const range: Range = .{ .start = start, .end = currentPoint };
            result.range = range;
            return Handler.returnValue(result, range, x);
        }
        pub const ReturnType = Handler.Return;
    };
}
pub fn CreateOrParse(
    comptime spec: []const type,
    comptime handler: var,
    comptime HandlerReturnType: ?type,
) type {
    const Union = TypedUnion(spec);
    const Handler = CreateHandler(Union, handler, HandlerReturnType);

    return struct {
        fn parse(fulltext: []const u8, start: Point, x: InternalExtra) OOM!Handler.FnReturn {
            const text = fulltext[start.byte..];
            // would use for else, but that doesn't seem to like inline for very much
            var resultOpt: ?Union = null;
            inline for (spec) |specItem, i| {
                try x.discardable.start(); // if this fails, it's ok because it will throw up until it reaches the thing that will free it
                defer x.discardable.end();
                const parseResult = try specItem.parse(fulltext, start, x);
                if (parseResult == .errmsg)
                    if (!parseResult.errmsg.recoverable)
                        return Handler.errorCopy(parseResult.errmsg)
                    else
                        x.discardable.trash()
                else
                    resultOpt = try Union.from(i, parseResult.result.value, parseResult.result.range, x.alloc);
            }
            const result = resultOpt orelse return Handler.errorValue("all ors failed", start.extend("  "));
            return Handler.returnValue(result, result.range, x);
        }
        pub const ReturnType = Handler.Return;
    };
}
pub fn CreateOptionalParse(
    comptime spec: type,
    comptime handler: var,
    comptime HandlerReturnType: ?type,
) type {
    const Result = ?spec.ReturnType;
    const Handler = CreateHandler(Result, handler, HandlerReturnType);

    return struct {
        fn parse(fulltext: []const u8, start: Point, x: InternalExtra) OOM!Handler.FnReturn {
            const text = fulltext[start.byte..];
            // would use for else, but that doesn't seem to like inline for very much
            var result: ?Result = null;
            const parseResult = try specItem.parse(fulltext, start, x);
            if (parseResult == .errmsg)
                if (!parseResult.errmsg.recoverable)
                    return Handler.errorCopy(parseResult.errmsg)
                else
                    result = null
            else
                result = parseResult.result.value;
            return Handler.returnValue(result, result.range, x);
        }
        pub const ReturnType = Handler.Return;
    };
}
pub fn CreateRequiredParse(
    comptime spec: type,
    comptime handler: var,
    comptime HandlerReturnType: ?type,
) type {
    const Result = spec.ReturnType;
    const Handler = CreateHandler(Result, handler, HandlerReturnType);

    return struct {
        fn parse(fulltext: []const u8, start: Point, x: InternalExtra) OOM!Handler.FnReturn {
            const text = fulltext[start.byte..];
            // would use for else, but that doesn't seem to like inline for very much
            var result: ?Result = null;
            const parseResult = try specItem.parse(fulltext, start, x);
            if (parseResult == .errmsg)
                return Handler.errorCopy(parseResult.errmsg)
            else {
                if (parseResult.result.range.start.byte == parseResult.result.range.end.byte)
                    return Handler.errorValue("Missing required", start.extend("  "));
                // extend "  " is kind of bad because it can make wrong positions sometimes
                result = parseResult.result.value;
            }
            return Handler.returnValue(result, result.range, x);
        }
        pub const ReturnType = Handler.Return;
    };
}
pub fn CreateNotParse(
    comptime spec: type,
    comptime handler: var,
    comptime HandlerReturnType: ?type,
) type {
    const Result = void;
    const Handler = CreateHandler(Result, handler, HandlerReturnType);

    return struct {
        fn parse(fulltext: []const u8, start: Point, x: InternalExtra) OOM!Handler.FnReturn {
            discardable.start();
            defer discardable.end();

            const text = fulltext[start.byte..];
            // would use for else, but that doesn't seem to like inline for very much
            const parseResult = try specItem.parse(fulltext, start, x);
            if (parseResult == .errmsg) {
                if (!parseResult.errmsg.recoverable)
                    return Handler.errorCopy(parseResult.errmsg);
            } else {
                discardable.trash(); // maybe any things after this should go into an allocator that is not trashed instead of unreachable
                // that way, if we needed to, we could allocate memory here and it would not be discarded on .end();
                return Handler.errorValue("Unexpected match", parseResult.result.range);
            }
            return Handler.returnValue({}, result.range, x);
        }
        pub const ReturnType = Handler.Return;
    };
}

pub fn CreateFutureParse(
    comptime spec: var,
    comptime handler: var,
    comptime HandlerReturnType: ?type,
    comptime parseTypesMap: ComptimeHashMap([]const u8, TopLevelParseDetails),
) type {
    const resultDetails = parseTypesMap.get(@tagName(spec)) orelse @compileError("Unknown tldecl " ++ @tagName(spec));

    const Handler = CreateHandler(*const resultDetails.ReturnType, handler, HandlerReturnType);

    return struct {
        fn parse(fulltext: []const u8, start: Point, x: InternalExtra) OOM!Handler.FnReturn {
            const parseResult = try x.parseFns[resultDetails.index](fulltext, start, x);
            if (parseResult == .errmsg) return Handler.errorCopy(parseResult.errmsg);
            const result = parseResult.result.value.readAs(resultDetails.ReturnType);
            const range = parseResult.result.range;

            return Handler.returnValue(result, range, x);
        }
        pub const ReturnType = Handler.Return;
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

// eg .{"1", Or, "2", Or, "3", "4"}
// matches "1" | "2" | "34"
pub const Or = struct {};

/// match zero or more parses -> []T
pub fn Star(comptime spec: var) type {
    return struct {
        pub const __STAR_ARGS = spec;
        pub const __REPEAT_MIN: usize = 0;
        pub const __REPEAT_MAX: usize = std.math.maxInt(usize);
    };
}
/// match one or more parses -> []T
pub fn Plus(comptime spec: var) type {
    return struct {
        pub const __STAR_ARGS = spec;
        pub const __REPEAT_MIN: usize = 1;
        pub const __REPEAT_MAX: usize = std.math.maxInt(usize);
    };
}
/// match a single parse, or not -> ?T
pub fn Optional(comptime spec: var) type {
    return struct {
        pub const __OPTIONAL_ARGS = spec;
    };
}
/// ensure that a given parse does not match -> void
pub fn Not(comptime spec: var) type {
    return struct {
        pub const __NOT_ARGS = spec;
    };
}
const CharRange = struct { start: u8, end: u8 };
// Char(.{.{'1', '9'}, .{'a', 'z'}, .{'A', 'Z'}});
// this vs
// .{'1', To, '9', Or, 'a', To, 'z', Or, 'A', To, 'Z'}
// vs
// .{Char('1', '9'), Or, Char('a', 'z'), Or, Char('A', 'Z')}
//
// the reason for the first one is to make it easier to implement an exclude
// NotChar('\\')
//
// all this vs
// regex(\\[1-9a-zA-Z]
// )
//
// char + notchar is probably better
// how about not()
// matches .{0} that does not match first arg
// Not(.{Char('1', '9')}), AnyChar()
// :: matches [^1-9]
// Star(.{Not(.{Char('1', '9')}), AnyChar()})
// :: matches [^1-9]*
// what if Char{1, 2} was a thing? unfortunately it isn't and can't be
// (#issue about constant stuff) but that would be neat

/// match a single char in the range start-end -> u8
pub fn Char(comptime start: u8, comptime end: u8) type {
    return struct {
        pub const __CHAR_RANGES: []const CharRange = &[_]CharRange{CharRange{ .start = start, .end = end }};
    };
}
/// match any character -> u8
pub fn AnyChar() type {
    return struct {
        pub const __CHAR_RANGES: []const CharRange = &[_]CharRange{CharRange{ .start = 0, .end = 255 }};
    };
}
/// match a single char in one of the provided ranges -> u8
pub fn CharRanges(comptime ranges: []const CharRange) type {
    return struct {
        pub const __CHAR_RANGES: []const CharRange = ranges;
    };
}
// a*
// a+
// a{1,2}
// do not use this for optionals, it's dumb for optionals to be arrays
fn CreateRepeatedParse(
    comptime spec: type,
    comptime handler: var,
    comptime HandlerReturnType: ?type,
    comptime minCount: u64,
    comptime maxCount: u64,
) type {
    const Handler = CreateHandler([]spec.ReturnType, handler, HandlerReturnType);

    return struct {
        fn parse(fulltext: []const u8, start: Point, x: InternalExtra) OOM!Handler.FnReturn {
            const text = fulltext[start.byte..];
            var fres = std.ArrayList(spec.ReturnType).init(x.alloc);
            var cpos: Point = start;
            var index: usize = 0;
            while (fres.items.len <= maxCount) : (index += 1) {
                try x.discardable.start();
                defer x.discardable.end();

                const pres = try spec.parse(fulltext, cpos, x);
                if (pres == .errmsg) {
                    if (!pres.errmsg.recoverable or fres.items.len < minCount)
                        return Handler.errorCopy(pres.errmsg);
                    x.discardable.trash(); // this error message is not used
                    // so there is no reason to keep around all the junk memory
                    // found in the process of creating this error
                    break;
                }
                cpos = pres.result.range.end;
                try fres.append(pres.result.value);
                if (pres.result.range.start.byte == pres.result.range.end.byte)
                    break; // eg star(optional(" "))
                // imagine what terrible things we could do with operator overloading
                // *?" " | ?(" ", "|", " ") | .someName
            }
            if (fres.items.len < minCount) unreachable;
            const range = Range{ .start = start, .end = cpos };
            // here this can support plus with
            // if fres.len == 0 && isPlus
            //     return error
            return Handler.returnValue(fres.toOwnedSlice(), range, x);
        }
        pub const ReturnType = Handler.Return;
    };
}

pub fn UserToReal(
    comptime name: []const u8,
    comptime spec: var,
    comptime handler: var,
    comptime HandlerReturnType: ?type,
    comptime parseTypesMap: ComptimeHashMap([]const u8, TopLevelParseDetails),
) type {
    const Spec = @TypeOf(spec);
    if (isString(Spec))
        return CreateStringParse(@as([]const u8, spec), handler, HandlerReturnType);
    if (@typeInfo(Spec) == .Type and @typeInfo(spec) == .Struct) {
        if (@hasDecl(spec, "__STAR_ARGS")) {
            const realArg = UserToReal(name ++ " > anon", spec.__STAR_ARGS, null, null, parseTypesMap);
            return CreateRepeatedParse(realArg, handler, HandlerReturnType, spec.__REPEAT_MIN, spec.__REPEAT_MAX);
        }
        if (@hasDecl(spec, "__OPTIONAL_ARGS")) {
            const realArg = userToReal(name ++ " > anon", spec.__OPTIONAL_ARGS, null, null, parseTypesMap);
            return CreateOptionalParse(realArg, handler, HandlerReturnType);
        }
        if (@hasDecl(spec, "__NOT_ARGS")) {
            const realArg = userToReal(name ++ " > anon", spec.__NOT_ARGS, null, null, parseTypesMap);
            return CreateNotParse(realArg, handler, HandlerReturnType);
        }
        if (@hasDecl(spec, "__CHAR_RANGES")) {
            return CreateRangeParse(spec.__CHAR_RANGES, handler, HandlerReturnType);
        }
        @compileError("wrong");
    }
    if (@typeInfo(Spec) == .Struct) {
        // oh no this is a bit of a mess because handler, HandlerReturnType has to be routed to only one thing
        var orMatches: []const []const type = &[_][]const type{};
        var pdArray: []const type = &[_]type{};
        for (spec) |userItem, i| {
            if (@TypeOf(userItem) == type and userItem == Or) {
                orMatches = orMatches ++ &[_][]const type{pdArray};
                pdArray = &[_]type{};
            } else
                pdArray = pdArray ++ [_]type{UserToReal(name ++ " > anon", userItem, null, null, parseTypesMap)};
        }
        orMatches = orMatches ++ &[_][]const type{pdArray};
        var allPMatches: []const type = &[_]type{};
        for (orMatches) |om| {
            if (om.len > 1 and orMatches.len == 1)
                allPMatches = allPMatches ++ [_]type{CreateOrderedParse(om, handler, HandlerReturnType)};
            if (om.len > 1)
                allPMatches = allPMatches ++ [_]type{CreateOrderedParse(om, null, null)};
            if (om.len == 1)
                allPMatches = allPMatches ++ [_]type{om[0]};
            if (om.len == 0)
                @compileError("must have at least one thing per or");
        }
        if (orMatches.len > 1)
            return CreateOrParse(allPMatches, handler, HandlerReturnType);
        if (orMatches.len == 1) {
            if (orMatches[0].len == 1)
                @compileError("there must be at least two items in p");
            return allPMatches[0];
        }
        @compileError("must have at least one thing");
    }
    if (@TypeOf(Spec) == @TypeOf(.EnumLiteral) or std.mem.eql(u8, @typeName(Spec), "(enum literal)")) {
        return CreateFutureParse(spec, handler, HandlerReturnType, parseTypesMap);
    }
    @compileLog(spec);
    @compileError("Unsupported: " ++ typePrint(Spec, 0) ++ " (named " ++ name ++ ")");
}

pub fn CreateParse(
    comptime name: []const u8,
    comptime spec: var,
    comptime handler: var,
    comptime HandlerReturnType: ?type,
    comptime parseTypesMap: ComptimeHashMap([]const u8, TopLevelParseDetails),
) type {
    return comptime UserToReal(name, spec, handler, HandlerReturnType, parseTypesMap);
}

pub fn Parser(comptime spec: var, comptime Handlers: type) type {
    comptime {
        var parses = ComptimeHashMap([]const u8, TopLevelParseDetails).init();

        const fields = meta.fields(@TypeOf(spec));
        for (fields) |field, i| {
            if (!@hasDecl(Handlers, field.name ++ "_RV"))
                @compileError("Missing pub decl " ++ field.name ++ "_RV");
            if (!@hasDecl(Handlers, field.name))
                @compileError("Missing pub fn " ++ field.name);
            if (parses.set(field.name, TopLevelParseDetails{
                .details = null,
                .ReturnType = @field(Handlers, field.name ++ "_RV"),
                .index = i,
            })) |detyls| @compileError("Duplicate key " ++ field.name);
        }
        for (fields) |field, i| {
            var item = parses.get(field.name).?;
            const parseDetails = CreateParse(
                field.name,
                @field(spec, field.name),
                @field(Handlers, field.name),
                @field(Handlers, field.name ++ "_RV"),
                parses,
            );
            if (item.ReturnType != parseDetails.ReturnType)
                @compileError("never");
            _ = parses.set(field.name, TopLevelParseDetails{
                .details = parseDetails,
                .ReturnType = item.ReturnType,
                .index = item.index,
            });
        }
        const parseFns = blk: {
            var parseFns: [fields.len]ParseFn = undefined;
            for (parses.items) |item| {
                parseFns[item.value.index] = createParseFn(item.value.details.?);
            }
            break :blk parseFns;
        };

        return struct {
            pub fn getReturnType(comptime key: var) type {
                return parses.get(@tagName(key)).?.ReturnType;
            }
            pub fn parse(
                comptime key: var,
                text: []const u8,
                start: Point,
                discardable: *DiscardableAllocator,
            ) OOM!union {
                result: struct {
                    value: parses.get(@tagName(key)).?.ReturnType,
                    range: Range,
                },
                errmsg: ErrResult,
            } {
                const parseDetails = parses.get(@tagName(key)).?;
                const xtra: InternalExtra = .{
                    .alloc = &discardable.allocator,
                    .discardable = discardable,
                    .parseFns = &parseFns,
                };

                try discardable.start();
                defer discardable.end();
                errdefer discardable.trash();

                const parseResult = try parseDetails.details.?.parse(text, start, xtra);
                return switch (parseResult) {
                    .result => |res| blk: {
                        const result = res.value;
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
test "" {
    try main();
}

pub fn anotherTest() !void {
    const ast = struct {
        // it would be nice if I could have a field here .range
        // that was on every union value
        // unfortunately I can't.
        // instead, I must have struct {range, expr: ...}
        // then instead of switch(v) I must switch(v.expr)
        // oh and initialization is a pain
        pub const BinOp = enum { add, sub, mul, div };
        pub const Expr = struct {
            range: Range, expr: union(enum) {
                binexpr: struct {
                    op: BinOp,
                    l: *const Expr,
                    r: *const Expr,
                },
                number: u64,
            }
        };
    };
    const parser = Parser(.{
        .math = .addsub,
        .addsub = .{ .muldiv, Star(.{ .{ "+", Or, "-" }, .muldiv }) },
        .muldiv = .{ .number, Star(.{ .{ "*", Or, "/" }, .number }) },
        .number = .{plus(range('0', '9'))}, // fun! *const []const *const u8
        // even though often user code might want pointers, it's a good
        // idea for the user code to have to allocate explicitly except
        // across borders
        // this way plus(range('0', '9')) could return a sensible []const u8
        // and range('0', '9') returns u8 instead of ending with the terrible mess
        // of *const []const *const u8
    }, struct {
        pub const math_RV = Expr;
        pub fn math(items: var, range: Range) Expr {
            return items.get(0); // .getpos for positions
        }
        pub const addsub_RV = Expr;
        // items: var, x: InternalExtra incl range and alloc?
        // what if instead of .get/.getrange there was a seperate
        // range() that returned {value: ?, range: Range}
        // and then there wasn't the mess of .value.value.value
        // when it's unnecessary
        fn binexpr(mappings: []ast.BinOp, items: var, range: Range) ast.Expr {
            const a0 = items.get(0);
            const list = items.get(1);
            if (list.len == 0) {
                // .* should not be required, especially since
                // this is just going to be allocated again
                return items.get(0).*;
            }
            if (list.len == 1) {
                return .{
                    .range = range,
                    .binexpr = .{
                        .op = switch (list.get(1).get(0).index) {
                            0 => .add,
                            1 => .sub,
                        },
                        .l = a0,
                        .r = list.get(1).get(1),
                    },
                };
            }
            // (requires allocator and some thought)
            @panic("not supported yet multiple items");
        }
        pub fn addsub(items: var, range: Range) Expr {
            binexpr(.{ .add, .sub }, items, range);
        }
    });
}

pub fn main() !void {
    try demotest();
}

fn timetest() !void {
    const count = 10000;
    var i: u64 = 0;
    const start = try std.time.Timer.start();
    while (i < count) : (i += 1) {
        try demotest();
    }
    const end = start.read();
    std.debug.warn("Took ~{d:.04}ms/i\n", .{@intToFloat(f64, end) / @intToFloat(f64, count) / 1_000_000});
}

fn demotest() !void {
    // takes a lot of eval branch quota because of things like the O(n) "hash"map
    @setEvalBranchQuota(10000000000000);

    var allocator = std.heap.page_allocator;
    var discardable = DiscardableAllocator.init(allocator);
    defer discardable.deinit();
    var alloc = &discardable.allocator;
    const parser = Parser(.{
        .stringtest = "test",
        .ordertest = .{ "test", "-", "interesting" },
        .nestedtest = .{ "one", .{ " ", "two" } },
        .reftest = .{ "=", .reftest }, // will always error but should be useful for testing before unions
        .math = .addsub,
        .addsub = .{ .muldiv, Star(.{ .{ "+", Or, "-" }, .muldiv }) },
        .muldiv = .{ .number, Star(.{ .{ "*", Or, "/" }, .number }) },
        .number = .{ "1", Or, "2", Or, "3" },
        .sizetest = .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2" } } } } } } },
        .sizetest2 = .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2" } } } } } } },
        .sizetest3 = .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2" } } } } } } },
        .sizetest4 = .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2" } } } } } } },
        .sizetest5 = .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2" } } } } } } },
        .sizetest6 = .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2", .{ "1", Or, "2" } } } } } } },
        // optional support next?
        // most of the places I use optional are starlastoptional
        // .{star(.{.expr, _, ",", _}), optional(.expr)}

        // lockin has to be handled by the p()
        // a, Lockin, b, c, d
        // if anything past lockin fails, it has to error unrecoverable

        // const _ = ._;
        // starlastoptional(.{.arg, _}, .{",", _});
    }, struct {
        pub const stringtest_RV = []const u8;
        pub fn stringtest(text: var, x: Extra) stringtest_RV {
            return text;
        }
        pub const ordertest_RV = u1;
        pub fn ordertest(items: var, x: Extra) ordertest_RV {
            return 0;
        }
        pub const nestedtest_RV = []const u8;
        pub fn nestedtest(items: var, x: Extra) []const u8 {
            return items.get(0);
        }
        pub const reftest_RV = u1;
        pub fn reftest(items: var, x: Extra) reftest_RV {
            return 1;
        }
        pub const math_RV = u64;
        pub fn math(items: var, x: Extra) math_RV {
            return items.*;
        }
        pub const addsub_RV = u64;
        pub fn addsub(items: var, x: Extra) addsub_RV {
            var result = items.get(0).*;
            for (items.get(1)) |itm| {
                // if(itm.value.get(0).value.value.*[0] == '+')
                //    add
                // else
                //    subtract
                result += itm.get(1).*;
            }
            return result;
        }
        pub const muldiv_RV = u64;
        pub fn muldiv(items: var, x: Extra) muldiv_RV {
            var result = items.get(0).*;
            for (items.get(1)) |itm| {
                result *= itm.get(1).*;
            }
            return result;
        }
        pub const number_RV = u64;
        pub fn number(items: var, x: Extra) number_RV {
            const value = items.any();
            if (std.mem.eql(u8, value, "1")) return 1;
            if (std.mem.eql(u8, value, "2")) return 2;
            if (std.mem.eql(u8, value, "3")) return 3;
            unreachable;
            // or go based on value.index, but this works too
            // in js, I would do or(c`1`.scb(r => 1), c`2`.scb(r => 2), c`3`.scb(r => 3))
            // is that better?
            // it might be possible to make something slightly similar to that in zig, but
            // I do want to avoid inline functions because it makes the spec less clear.
        }
        // we could add something so you only need _RV
        // if RV is provided but no fn, ensure the actual type
        // matches what you wrote in rv and use that
        pub const sizetest_RV = u1;
        pub fn sizetest(items: var, x: Extra) u1 {
            return 0;
        }
        pub const sizetest2_RV = u1;
        pub fn sizetest2(items: var, x: Extra) u1 {
            return 0;
        }
        pub const sizetest3_RV = u1;
        pub fn sizetest3(items: var, x: Extra) u1 {
            return 0;
        }
        pub const sizetest4_RV = u1;
        pub fn sizetest4(items: var, x: Extra) u1 {
            return 0;
        }
        pub const sizetest5_RV = u1;
        pub fn sizetest5(items: var, x: Extra) u1 {
            return 0;
        }
        pub const sizetest6_RV = u1;
        pub fn sizetest6(items: var, x: Extra) u1 {
            return 0;
        }
    });

    try discardable.start();
    defer discardable.trashEnd();
    testing.expect(
        std.mem.eql(u8, (try parser.parse(.stringtest, "test", Point.start, &discardable)).result.value, "test"),
    );
    // const res = try parser.parse(.stringtest, "test", Point.start, &discardable);

    // const res3 = try parser.parse(.stringtest, "testing", Point.start, &discardable);
    // std.debug.warn("res: {}, rng: {}\n", .{ res3.result.value, res3.result.range });
    // const res2 = try parser.parse(.stringtest, "tst", Point.start, &discardable);
    // std.debug.warn("err: {}\n", .{res2.errmsg.message});

    // const res4 = try parser.parse(.ordertest, "test-interesting", Point.start, &discardable);
    // std.debug.warn("res: {}\n", .{res4.result.value});
    // const res5 = try parser.parse(.ordertest, "test!interesting", Point.start, &discardable);
    // std.debug.warn("res: {}, rng: {}\n", .{ res5.errmsg.message, res5.errmsg.range });

    // const res7 = try parser.parse(.nestedtest, "one two", Point.start, &discardable);
    // std.debug.warn("res7: {}\n", .{res7.result.value});

    // const res8 = try parser.parse(.reftest, "====huh", Point.start, &discardable);
    // std.debug.warn("res: {}, rng: {}\n", .{ res8.errmsg.message, res8.errmsg.range });

    const mathequ = try parser.parse(.math, "1+3*2+2", Point.start, &discardable);
    testing.expectEqual(mathequ.result.value, 9);

    // const sizetest = try parser.parse(.sizetest, "fail", Point.start, &discardable);
    // std.debug.warn("error: {}\n", .{sizetest.errmsg.message});
}
