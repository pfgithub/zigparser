const std = @import("std");
const meta = std.meta;
const Alloc = std.mem.Allocator;

const OOM = Alloc.Error;

/// the stdlib comptime hashmap is only created once and makes a "perfect hash"
/// so this works I guess. for strings with type values;
///
/// comptime "hash"map. all accesses and sets are ~~O(1)~~ O(n)
pub fn ComptimeHashMap(comptime Key: type, comptime Value: type) type {
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
                if (Key == []const u8) {
                    if (std.mem.eql(u8, itm.key, key))
                        return i;
                } else if (itm.key == key)
                    return i;
            }
            return null;
        }
        pub fn get(comptime hm: *HM, comptime key: Key) ?Value {
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
// a pointer to arbitrary data. panics if attempted to be read as the wrong type.
const AnyPtr = comptime blk: {
    var typeIDMap = TypeIDMap.init();
    break :blk struct {
        const AnyRawPtrType = *const @OpaqueType();
        pointer: AnyRawPtrType,
        typeID: u64,
        fn fromPtr(value: var) AnyPtr {
            const ti = @typeInfo(@TypeOf(value));
            if (ti != .Pointer) @compileError("must be *ptr");
            if (ti.Pointer.size != .One) @compileError("must be ptr to one item");
            const typeID = comptime typeIDMap.get(ti.Pointer.child);
            return .{ .pointer = @ptrCast(AnyRawPtrType, value), .typeID = typeID };
        }
        fn readAs(any: AnyPtr, comptime RV: type) *const RV {
            const typeID = comptime typeIDMap.get(RV);
            if (any.typeID != typeID)
                std.debug.panic(
                    "\x1b[31mError!\x1b(B\x1b[m Item is of type {}, but was read as type {}. Type names ({}):\n{}",
                    .{ any.typeID, typeID, typeIDMap.latestID, typeIDMap.infoString },
                );
            return @ptrCast(*const RV, @alignCast(@alignOf(RV), any.pointer));
        }
    };
};
const ErrResult = struct {
    message: []const u8,
    range: Range,
};
const ParseResult = union(enum) {
    result: struct {
        value: AnyPtr,
        range: Range,
    },
    errmsg: ErrResult,
};
const ParseFn = fn (text: []const u8, start: Point, alloc: *Alloc) OOM!ParseResult;
const ParseDetails = struct {
    parse: ParseFn,
    ReturnType: type,
};

fn errorValue(emsg: []const u8, range: Range) ParseResult {
    return .{
        .errmsg = .{
            .message = emsg,
            .range = range,
        },
    };
}
fn returnValue(value: var, range: Range) ParseResult {
    return .{
        .result = .{
            .value = AnyPtr.fromPtr(value),
            .range = range,
        },
    };
}

/// handler: null | fn(a: var, b: var) type;
pub fn createStringParse(
    comptime spec: []const u8,
    comptime handler: var,
    comptime HandlerReturnType: ?type,
) ParseDetails {
    const Handler = CreateHandler([]const u8, handler, HandlerReturnType);

    const parseFn: ParseFn = struct {
        fn a(fulltext: []const u8, start: Point, alloc: *Alloc) OOM!ParseResult {
            const text = fulltext[start.byte..];
            if (text.len < spec.len)
                return errorValue(
                    "Expected `" ++ spec ++ "` (too short)",
                    start.extend("  "),
                );
            for (spec) |char, i| {
                if (char != text[i])
                    return errorValue(
                        "Expected `" ++ spec ++ "`",
                        start.extend("  "),
                    );
            }
            const range = start.extend(spec);
            const strCopy = try std.mem.dupe(alloc, u8, spec);
            var res = try alloc.create(Handler.Return);
            res.* = Handler.handle(strCopy, range);
            return returnValue(res, range);
        }
    }.a;
    return ParseDetails{
        .parse = parseFn,
        .ReturnType = Handler.Return,
    };
}
pub fn comptimeFmt(comptime fmt: []const u8, comptime arg: var) []const u8 {
    comptime {
        const width = std.fmt.count(fmt, arg);
        var buf: [width]u8 = undefined;
        return std.fmt.bufPrint(&buf, fmt, arg) catch unreachable;
    }
}
pub fn typePrint(comptime Type: type, comptime indentationLevel: u64) []const u8 {
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
            break :blk res;
        },
        else => @typeName(Type),
    };
}
pub fn TypedList(comptime spec: []const ParseDetails) type {
    return struct {
        items: [spec.len]struct { value: AnyPtr, range: Range } = undefined,
        pub fn get(list: @This(), comptime index: u64) struct { value: *const spec[index].ReturnType, range: Range } {
            return .{ .value = list.items[index].value.readAs(spec[index].ReturnType), .range = list.items[index].range };
        }
    };
}
pub fn CreateHandler(comptime Arg0: type, comptime handler: var, comptime HandlerReturnType: ?type) type {
    const DefaultReturnType = struct { value: Arg0, range: Range };
    return struct {
        pub const Return = HandlerReturnType orelse DefaultReturnType;
        pub fn handle(value: Arg0, range: Range) Return {
            if (HandlerReturnType == null) {
                return .{ .value = value, .range = range };
            } else {
                return handler(value, range);
            }
        }
    };
}
pub fn createOrderedParse(
    comptime spec: []const ParseDetails,
    comptime handler: var,
    comptime HandlerReturnType: ?type,
) ParseDetails {
    // list type has to be a thing
    // .get(index) => *const ActualResultType
    // default is struct {value: ListType, range: Range}
    // otherwise HandlerReturnTypet
    const List = TypedList(spec);

    const Handler = CreateHandler(List, handler, HandlerReturnType);

    const parseFn = struct {
        fn a(fulltext: []const u8, start: Point, alloc: *Alloc) OOM!ParseResult {
            const text = fulltext[start.byte..];
            var result: List = .{};
            var currentPoint: Point = start;
            inline for (spec) |specItem, i| {
                const parseResult = try specItem.parse(fulltext, currentPoint, alloc);
                if (parseResult == .errmsg) return parseResult;
                currentPoint = parseResult.result.range.end;
                result.items[i] = .{ .value = parseResult.result.value, .range = parseResult.result.range };
            }

            const range: Range = .{ .start = start, .end = currentPoint };
            var res = try alloc.create(Handler.Return);
            res.* = Handler.handle(result, range);
            return returnValue(res, range);
        }
    }.a;

    return ParseDetails{
        .parse = parseFn,
        .ReturnType = Handler.Return,
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
    if (@typeInfo(Spec) == .Struct) {
        var pdArray: []const ParseDetails = &[_]ParseDetails{};
        for (spec) |userItem| {
            pdArray = pdArray ++ [_]ParseDetails{userToReal(userItem, null, null)};
        }
        return createOrderedParse(pdArray, handler, HandlerReturnType);
    }
    @compileError("Unsupported: " ++ @typeName(Spec));
}

pub fn createParse(
    comptime name: []const u8,
    comptime spec: var,
    comptime handler: var,
    comptime HandlerReturnType: ?type,
) ParseDetails {
    return comptime userToReal(spec, handler, HandlerReturnType);
}

pub fn Parser(comptime spec: var, comptime Handlers: type) type {
    comptime {
        var parses = ComptimeHashMap([]const u8, ParseDetails).init();

        for (meta.fields(@TypeOf(spec))) |field| {
            const hasHandler = @hasDecl(Handlers, field.name) or @hasDecl(Handlers, field.name ++ "_RV");
            if (parses.set(field.name, createParse(
                field.name,
                @field(spec, field.name),
                if (hasHandler) @field(Handlers, field.name) else null,
                if (hasHandler) @field(Handlers, field.name ++ "_RV") else null,
            ))) |detyls| @compileError("Duplicate key " ++ field.name);
        }

        return struct {
            pub fn getReturnType(comptime key: var) type {
                return parses.get(@tagName(key)).?.ReturnType;
            }
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
                        const result = res.value.readAs(parseDetails.ReturnType);
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
        .stringtest = "test",
        .stringdefaultout = "test",
        .ordertest = .{ "test", "-", "interesting" },
        .nestedtest = .{ "one", .{ " ", "two" } },
        .ordertestdefaultout = .{ "test", "-", "interesting" },
    }, struct {
        pub const stringtest_RV = []const u8;
        pub fn stringtest(text: var, range: Range) stringtest_RV {
            return text;
        }
        pub const ordertest_RV = u1;
        pub fn ordertest(items: var, range: Range) ordertest_RV {
            return 0;
        }
        pub const nestedtest_RV = []const u8;
        pub fn nestedtest(items: var, range: Range) []const u8 {
            return items.get(0).value.value;
        }
    });
    const res = (try parser.parse(.stringtest, "test", Point.start, arena));
    std.debug.warn("res: {}\n", .{res.result.value.*});
    const res3 = (try parser.parse(.stringtest, "testing", Point.start, arena));
    std.debug.warn("res: {}, rng: {}\n", .{ res3.result.value.*, res3.result.range });
    const res2 = (try parser.parse(.stringtest, "tst", Point.start, arena));
    std.debug.warn("err: {}\n", .{res2.errmsg.message});

    const res4 = (try parser.parse(.ordertest, "test-interesting", Point.start, arena));
    std.debug.warn("res: {}\n", .{res4.result.value.*});
    const res5 = (try parser.parse(.ordertest, "test!interesting", Point.start, arena));
    std.debug.warn("res: {}, rng: {}\n", .{ res5.errmsg.message, res5.errmsg.range });

    const res8 = (try parser.parse(.stringdefaultout, "test", Point.start, arena));
    std.debug.warn("res8: {}\n", .{res8.result.value.*});

    const res7 = try parser.parse(.nestedtest, "one two", Point.start, arena);
    std.debug.warn("res7: {}\n", .{res7.result.value.*});

    const res6 = (try parser.parse(.ordertestdefaultout, "test-interesting", Point.start, arena));
    std.debug.warn("res: {}\n", .{res6.result.value.value.get(0)});
}

test "" {}
