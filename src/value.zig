const std = @import("std");
const Obj = @import("object.zig").Obj;
const ObjFunction = @import("object.zig").ObjFunction;

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
            .undefined_ => unreachable,
        }
    }

    pub fn is_string(self: Self) bool {
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
};

pub fn print_value(val: Value) void {
    switch (val) {
        .bool_ => |b| std.debug.print("{s}", .{if (b) "true" else "false"}),
        .nil => std.debug.print("nil", .{}),
        .number => |n| std.debug.print("{d}", .{n}),
        .obj => |o| switch (o.kind) {
            .string => std.debug.print("{s}", .{o.downcast_string().value.as_slice()}),
            .native => std.debug.print("<native fn>", .{}),
            .closure => print_function(o.downcast_closure().function),
            .function => print_function(o.downcast_function()),
            .upvalue => std.debug.print("upvalue", .{}),
        },
        .undefined_ => std.debug.print("undefined", .{}),
    }
}

fn print_function(fun: *ObjFunction) void {
    if (fun.name) |name| {
        std.debug.print("<fn {s}>", .{name.value.as_slice()});
    } else {
        std.debug.print("<script>", .{});
    }
}
