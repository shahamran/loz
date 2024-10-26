const std = @import("std");
const Obj = @import("Obj.zig");

pub const Value = union(enum) {
    const Self = @This();

    bool_: bool,
    nil,
    number: f64,
    obj: *Obj,
    undefined_,

    pub fn eql(self: Self, other: Self) bool {
        switch (self) {
            .bool_ => |b| return @as(?bool, b) == other.as_bool(),
            .nil => return other == .nil,
            .number => |n| return @as(?f64, n) == other.as_number(),
            .obj => |o| return o.eql(other.as_object() orelse return false),
            .undefined_ => unreachable, // probably?
        }
    }

    pub inline fn is_string(self: Self) bool {
        return self == .obj and self.obj.kind == .string;
    }

    fn as_bool(self: Self) ?bool {
        if (self == .bool_) return self.bool_;
        return null;
    }

    fn as_number(self: Self) ?f64 {
        if (self == .number) return self.number;
        return null;
    }

    fn as_object(self: Self) ?*Obj {
        if (self == .obj) return self.obj;
        return null;
    }

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try switch (self) {
            .bool_ => |b| writer.print("{s}", .{if (b) "true" else "false"}),
            .nil => writer.print("nil", .{}),
            .number => |n| writer.print("{d}", .{n}),
            .obj => |o| switch (o.kind) {
                .string => writer.print("{s}", .{o.as(Obj.String)}),
                .native => writer.print("<native fn>", .{}),
                .closure => writer.print("{s}", .{o.as(Obj.Closure).function}),
                .function => writer.print("{s}", .{o.as(Obj.Function)}),
                // probably unreachable
                .upvalue => writer.print("upvalue", .{}),
            },
            .undefined_ => writer.print("undefined", .{}),
        };
    }
};
