// if this were c/c++, I never would have thought of something like this
const std = @import("std");
const Allocator = std.mem.Allocator;
const DiscardableAllocator = @This();

allocator: Allocator,

child_allocator: *Allocator,
arenas: std.ArrayList(std.heap.ArenaAllocator),
flag_trash: bool,

// could also have an interface where arenas are user controlled
// let a1 = DiscardableAllocator.start()
// a1.allocator
// this current interface works well though as long as you don't want to backtrack
// eg one case is in p()
//     current = disc.start()
//     if(err) {for(allocs) |alloc| disc.trashEnd(); current.end()}
//     allocs.append(current)
//   for(allocs) |alloc| alloc.end();
// that way an error halfway through p() frees the things before it but
// keeps the memory for the error
// anyway, not really necessary right now
//
// maybe suggest stdlib to have an ArenaAllocator join method?
// that does what the current .end() does?
//
// also it might be useful if this was backed by something like
// arenaallocator but that actually supported freeing
// just kept a list of memory to free and freed it on deinit, but
// things could be freed early

/// starts empty. make sure to call .start() before using
pub fn init(child_allocator: *Allocator) DiscardableAllocator {
    return DiscardableAllocator{
        .allocator = Allocator{
            .reallocFn = realloc,
            .shrinkFn = shrink,
        },
        .child_allocator = child_allocator,
        .arenas = std.ArrayList(std.heap.ArenaAllocator).init(child_allocator),
        .flag_trash = false,
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
// free the memory of the latest section and end.
pub fn trashEnd(self: *DiscardableAllocator) void {
    self.trash();
    self.end();
}
// free the memory of the latest section. make sure to
// call .end() after this before allocating any more memory!
pub fn trash(self: *DiscardableAllocator) void {
    if (self.flag_trash) unreachable;
    self.flag_trash = true;
}
// end the latest section succesfully
pub fn end(self: *DiscardableAllocator) void {
    var childArena = self.arenas.pop();

    defer self.flag_trash = false;
    if (self.flag_trash) {
        childArena.deinit();
        return;
    }

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
    const insertAfter = parentArena.buffer_list.first.?;
    childLast.next = insertAfter.next;
    insertAfter.next = childFirst;
}

fn realloc(allocator: *Allocator, old_mem: []u8, old_align: u29, new_size: usize, new_align: u29) Allocator.Error![]u8 {
    const self = @fieldParentPtr(DiscardableAllocator, "allocator", allocator);
    if (self.arenas.items.len == 0) unreachable;
    if (self.flag_trash) unreachable;
    const arena = &self.arenas.items[self.arenas.items.len - 1].allocator;
    return arena.reallocFn(arena, old_mem, old_align, new_size, new_align);
}

fn shrink(allocator: *Allocator, old_mem: []u8, old_align: u29, new_size: usize, new_align: u29) []u8 {
    const self = @fieldParentPtr(DiscardableAllocator, "allocator", allocator);
    if (self.arenas.items.len == 0) unreachable;
    if (self.flag_trash) unreachable;
    const arena = &self.arenas.items[self.arenas.items.len - 1].allocator;
    return arena.shrinkFn(arena, old_mem, old_align, new_size, new_align);
}

test "discardable allocator" {
    const expectEqual = std.testing.expectEqual;

    var talloc = DiscardableAllocator.init(std.heap.page_allocator);
    defer talloc.deinit();
    try talloc.start();
    defer talloc.trashEnd();

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
        defer talloc.trashEnd();

        var trashedmem = try alloc.create(u8);
        trashedmem.* = 25;
        break :blk trashedmem;
    };
    // expectEqual(trashedmem.*, 25);
    // expect segmentation fault on trashedmem.*
}
