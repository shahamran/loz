const std = @import("std");
const memory = @import("memory.zig");

pub fn List(comptime T: type) type {
    return struct {
        const Self = @This();

        items: []T,
        capacity: usize,

        pub fn init() Self {
            return .{
                .items = &[_]T{},
                .capacity = 0,
            };
        }

        pub fn deinit(self: *Self) void {
            memory.free(self.allocated_slice());
            self.* = init();
        }

        /// Make sure the list has enough capacity to store a total of `capacity` items.
        pub fn reserve(self: *Self, capacity: usize) !void {
            if (capacity <= self.capacity) return;
            const new_capacity = if (capacity == 8) 8 else grow_capacity(capacity);
            const old_len = self.items.len;
            const new_items = try memory.reallocate(self.allocated_slice(), new_capacity);
            self.capacity = new_items.len;
            self.items = new_items[0..old_len];
        }

        /// Append a value to the end of the list.
        pub fn push(self: *Self, value: T) !void {
            try self.reserve(self.items.len + 1);
            self.items.len += 1;
            self.items[self.items.len - 1] = value;
        }

        pub fn pop(self: *Self) T {
            std.debug.assert(self.items.len > 0);
            const item = self.items[self.items.len - 1];
            self.items.len -= 1;
            return item;
        }

        pub fn clear(self: *Self) void {
            self.items.len = 0;
        }

        pub fn last(self: *const Self) T {
            std.debug.assert(self.items.len > 0);
            return self.items[self.items.len - 1];
        }

        pub fn eql(self: *const Self, other: *const Self) bool {
            return std.mem.eql(T, self.items, other.items);
        }

        fn allocated_slice(self: *Self) []T {
            return self.items.ptr[0..self.capacity];
        }
    };
}

pub fn grow_capacity(capacity: usize) usize {
    return if (capacity < 8)
        8
    else
        // assuming we'll run out of memory before we hit usize.max
        std.math.ceilPowerOfTwo(usize, capacity) catch unreachable;
}

test "list" {
    const expectEqual = std.testing.expectEqual;
    // deinit empty list
    {
        var list = List(i32).init();
        list.deinit();
    }
    // reserve
    {
        var list = List(i32).init();
        defer list.deinit();
        try list.reserve(10);
        try expectEqual(16, list.capacity);
    }
    // reserve small
    {
        var list = List(i32).init();
        defer list.deinit();
        try list.reserve(5);
        try expectEqual(8, list.capacity);
    }
    // push and pop
    {
        var list = List(i32).init();
        defer list.deinit();
        try list.push(1);
        try list.push(2);
        try list.push(3);
        try expectEqual(3, list.items.len);
        try expectEqual(3, list.pop());
        try expectEqual(2, list.pop());
        try expectEqual(1, list.pop());
        try expectEqual(0, list.items.len);
    }
}
