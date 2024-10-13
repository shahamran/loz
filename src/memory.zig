const std = @import("std");
const allocator = @import("main.zig").allocator;
const vm = @import("vm.zig");
const Error = error{OutOfMemory};

pub fn grow_capacity(capacity: usize) usize {
    return if (capacity < 8) 8 else capacity * 2;
}

pub fn reallocate(old_mem: anytype, new_n: usize) t: {
    const Slice = @typeInfo(@TypeOf(old_mem)).pointer;
    break :t Error![]Slice.child;
} {
    return try allocator.realloc(old_mem, new_n);
}

pub fn free(memory: anytype) void {
    _ = reallocate(memory, 0) catch unreachable;
}

pub fn free_objects() void {
    var object = vm.vm.objects;
    while (object) |o| {
        const next = o.next;
        switch (o.kind) {
            .string => o.downcast_string().deinit(),
            .native => o.downcast_native().deinit(),
            .function => o.downcast_function().deinit(),
        }
        object = next;
    }
}
