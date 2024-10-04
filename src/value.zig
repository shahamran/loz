const std = @import("std");

pub const Value = union(enum) {
    bool_: bool,
    nil,
    number: f64,
};

pub fn print_value(val: Value) void {
    switch (val) {
        .bool_ => |b| std.debug.print("{s}", .{if (b) "true" else "false"}),
        .nil => std.debug.print("nil", .{}),
        .number => |n| std.debug.print("{d}", .{n}),
    }
}
