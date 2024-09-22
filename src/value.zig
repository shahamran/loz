const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

pub const Value = f64;

pub const ValueArray = struct {
    values: ArrayList(Value),

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return ValueArray{
            .values = ArrayList(Value).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.values.deinit();
        self.* = Self.init(self.values.allocator);
    }

    pub fn write(self: *Self, value: Value) Allocator.Error!usize {
        try self.values.append(value);
        return self.values.items.len - 1;
    }
};

pub fn print_value(val: Value) void {
    std.debug.print("{d}", .{val});
}
