const std = @import("std");
const builtin = @import("builtin");
const Error = error{OutOfMemory};

pub fn grow_capacity(capacity: usize) usize {
    return if (capacity < 8) 8 else capacity * 2;
}

var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
const allocator = if (builtin.is_test) std.testing.allocator else gpa.allocator();

pub fn reallocate(old_mem: anytype, new_n: usize) t: {
    const Slice = @typeInfo(@TypeOf(old_mem)).pointer;
    break :t Error![]Slice.child;
} {
    return try allocator.realloc(old_mem, new_n);
}

pub fn free(memory: anytype) void {
    _ = reallocate(memory, 0) catch unreachable;
}
