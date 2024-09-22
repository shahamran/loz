const std = @import("std");
const Allocator = std.mem.Allocator;
const Error = error{OutOfMemory};

pub fn grow_capacity(capacity: usize) usize {
    return if (capacity < 8) 8 else capacity * 2;
}

pub fn reallocate(allocator: Allocator, old_mem: anytype, new_n: usize) t: {
    const Slice = @typeInfo(@TypeOf(old_mem)).Pointer;
    break :t Error![]Slice.child;
} {
    return try allocator.realloc(old_mem, new_n);
}
