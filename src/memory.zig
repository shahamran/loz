const std = @import("std");
const allocator = @import("main.zig").allocator;
const vm = @import("vm.zig");
const Error = error{OutOfMemory};
const config = @import("config");

pub fn grow_capacity(capacity: usize) usize {
    return if (capacity < 8) 8 else capacity * 2;
}

pub fn reallocate(old_mem: anytype, new_n: usize) t: {
    const Slice = @typeInfo(@TypeOf(old_mem)).pointer;
    break :t Error![]Slice.child;
} {
    if (new_n > old_mem.len) {
        if (config.stress_gc) {
            collect_garbage();
        }
    }
    return try allocator.realloc(old_mem, new_n);
}

pub fn free(memory: anytype) void {
    _ = reallocate(memory, 0) catch unreachable;
}

pub fn free_objects() void {
    var object = vm.vm.objects;
    while (object) |o| {
        const next = o.next;
        o.deinit();
        object = next;
    }
}

fn collect_garbage() void {
    if (config.log_gc) {
        std.debug.print("-- gc begin\n", .{});
    }

    if (config.log_gc) {
        std.debug.print("-- gc end\n", .{});
    }
}
