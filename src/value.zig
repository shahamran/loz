const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

pub const Value = union(enum) {
    bool_: bool,
    nil,
    number: f64,

    pub fn get_bool(self: *Value) ?bool {
        switch (self.*) {
            .bool_ => |b| return b,
            else => return null,
        }
    }

    pub fn get_number(self: *Value) ?f64 {
        switch (self.*) {
            .number => |n| return n,
            else => return null,
        }
    }

    pub fn is_nil(self: *Value) bool {
        return self.* == .nil;
    }
};

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
    switch (val) {
        .bool_ => |b| std.debug.print("{s}", .{if (b) "true" else "false"}),
        .nil => std.debug.print("nil", .{}),
        .number => |n| std.debug.print("{d}", .{n}),
    }
}
