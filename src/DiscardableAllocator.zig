const std = @import("std");
const Allocator = std.mem.Allocator;
const DiscardableAllocator = @This();

allocator: Allocator,

child_allocator: *Allocator,
arenas: std.ArrayList(std.heap.ArenaAllocator),
// allocator: arenas[arenas.len - 1].allocator
// start: arenas.append(ArenaAllocator.init(child_allocator))
// trash: arenas.pop().deinit();
// end:   lastArena = arenas.pop();
//        arenas[arenas.len - 1]:mergeWith(lastArena)

/// starts empty. make sure to call .start() after initializing
pub fn init(child_allocator: *Allocator) DiscardableAllocator {
    return DiscardableAllocator{
        .allocator = Allocator{
            .reallocFn = realloc,
            .shrinkFn = shrink,
        },
        .child_allocator = child_allocator,
        .arenas = std.ArrayList(std.heap.ArenaAllocator).init(child_allocator),
    };
}

/// assert empty and free list.
pub fn deinit(self: *DiscardableAllocator) void {
    if (self.arenas.items.len != 0) unreachable;
    self.arenas.deinit();
}

// start a section. make sure to end or trash it when done.
pub fn start(self: *DiscardableAllocator) !void {
    try self.arenas.append(std.heap.ArenaAllocator.init(self.child_allocator));
}
// free the memory of the latest section
pub fn trash(self: *DiscardableAllocator) void {
    self.arenas.pop().deinit();
}
// end the latest section succesfully
pub fn end(self: *DiscardableAllocator) void {
    var childArena = self.arenas.pop();
    var parentArena = &self.arenas.items[self.arenas.items.len - 1];

    if (childArena.buffer_list.first == null) return; // nothing to merge;
    const childFirst = childArena.buffer_list.first.?;
    var childLast = childFirst;
    while (childLast.next) |next| { // f
        childLast = next;
    }
    if (parentArena.buffer_list.first == null) {
        parentArena.buffer_list.first = childFirst;
        if (parentArena.end_index != 0) unreachable;
        return;
    }
    var insertAfter = parentArena.buffer_list.first.?.next.?;
    childLast.next = insertAfter.next;
    insertAfter.next = childFirst;
}

fn realloc(allocator: *Allocator, old_mem: []u8, old_align: u29, new_size: usize, new_align: u29) Allocator.Error![]u8 {
    const self = @fieldParentPtr(DiscardableAllocator, "allocator", allocator);
    if (self.arenas.items.len == 0) unreachable;
    const arena = &self.arenas.items[self.arenas.items.len - 1].allocator;
    return arena.reallocFn(arena, old_mem, old_align, new_size, new_align);
}

fn shrink(allocator: *Allocator, old_mem: []u8, old_align: u29, new_size: usize, new_align: u29) []u8 {
    const self = @fieldParentPtr(DiscardableAllocator, "allocator", allocator);
    if (self.arenas.items.len == 0) unreachable;
    const arena = &self.arenas.items[self.arenas.items.len - 1].allocator;
    return arena.shrinkFn(arena, old_mem, old_align, new_size, new_align);
}

test "discardable allocator" {
    const expectEqual = std.testing.expectEqual;

    var talloc = DiscardableAllocator.init(std.heap.page_allocator);
    defer talloc.deinit();
    try talloc.start();
    defer talloc.trash();

    var alloc = &talloc.allocator;

    var u8mem = blk: {
        try talloc.start();
        defer talloc.end();

        var u8mem = try alloc.create(u8);
        u8mem.* = 32;
        break :blk u8mem;
    };
    expectEqual(u8mem.*, 32);

    var trashedmem = blk: {
        try talloc.start();
        defer talloc.trash();

        var trashedmem = try alloc.create(u8);
        trashedmem.* = 32;
        break :blk trashedmem;
    };
    // expect eg segmentation fault on trashedmem.*
}
