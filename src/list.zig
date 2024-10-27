const std = @import("std");
const Allocator = std.mem.Allocator;
const common = @import("common.zig");

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

        pub fn deinit(self: *Self, allocator: Allocator) void {
            allocator.free(self.allocated_slice());
            self.* = init();
        }

        /// Make sure the list has enough capacity to store a total of `capacity` items.
        pub fn reserve(self: *Self, allocator: Allocator, capacity: usize) !void {
            if (capacity <= self.capacity) return;
            const new_capacity = common.grow_capacity(capacity);
            const old_len = self.items.len;
            const new_items = try allocator.realloc(self.allocated_slice(), new_capacity);
            self.capacity = new_items.len;
            self.items = new_items[0..old_len];
        }

        /// Append a value to the end of the list.
        pub inline fn push(self: *Self, allocator: Allocator, value: T) !void {
            try self.reserve(allocator, self.items.len + 1);
            self.items.len += 1;
            self.items[self.items.len - 1] = value;
        }

        pub inline fn pop(self: *Self) T {
            std.debug.assert(self.items.len > 0);
            const item = self.items[self.items.len - 1];
            self.items.len -= 1;
            return item;
        }

        pub inline fn clear(self: *Self) void {
            self.items.len = 0;
        }

        pub inline fn last(self: *const Self) T {
            std.debug.assert(self.items.len > 0);
            return self.items[self.items.len - 1];
        }

        pub inline fn eql(self: *const Self, other: *const Self) bool {
            return std.mem.eql(T, self.items, other.items);
        }

        inline fn allocated_slice(self: *Self) []T {
            return self.items.ptr[0..self.capacity];
        }
    };
}

const expectEqual = std.testing.expectEqual;
const testing_allocator = std.testing.allocator;

test "list deinit empty" {
    var list = List(i32).init();
    list.deinit(testing_allocator);
}

test "list reserve" {
    var list = List(i32).init();
    defer list.deinit(testing_allocator);
    try list.reserve(testing_allocator, 8);
    try expectEqual(16, list.capacity);
}

test "list reserve small" {
    var list = List(i32).init();
    defer list.deinit(testing_allocator);
    try list.reserve(testing_allocator, 5);
    try expectEqual(8, list.capacity);
    try list.reserve(testing_allocator, 8);
    try expectEqual(8, list.capacity);
}

test "list push pop" {
    var list = List(i32).init();
    defer list.deinit(testing_allocator);
    try list.push(testing_allocator, 1);
    try list.push(testing_allocator, 2);
    try list.push(testing_allocator, 3);
    try expectEqual(3, list.items.len);
    try expectEqual(3, list.pop());
    try expectEqual(2, list.pop());
    try expectEqual(1, list.pop());
    try expectEqual(0, list.items.len);
}

test "list eql" {
    var list1 = List(u8).init();
    var list2 = List(u8).init();
    defer list1.deinit(testing_allocator);
    defer list2.deinit(testing_allocator);
    try list1.reserve(testing_allocator, 16); // to make the lists capacity differ
    try list1.push(testing_allocator, 11);
    try list1.push(testing_allocator, 12);
    try list1.push(testing_allocator, 13);

    try list2.push(testing_allocator, 11);
    try list2.push(testing_allocator, 12);
    try list2.push(testing_allocator, 13);
    try list2.push(testing_allocator, 14);
    try std.testing.expect(!list1.eql(&list2));
    _ = list2.pop();
    try std.testing.expect(list1.eql(&list2));
}
