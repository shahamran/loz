const std = @import("std");
const config = @import("config");
const memory = @import("memory.zig");
const string = @import("string.zig");
const vm = @import("vm.zig");
const Chunk = @import("Chunk.zig");
const Value = @import("value.zig").Value;

const Obj = @This();
const Kind = enum {
    closure,
    function,
    native,
    string,
    upvalue,
};

kind: Kind, // underlying object type
is_marked: bool = false, // true if reachable and should not be GCed
next: ?*Obj, // intrusive linked list of all allocated objects

/// Return a Lox object Value that points to this object.
pub inline fn value(obj: *Obj) Value {
    return .{ .obj = obj };
}

/// Downcast this object to the given type.
pub inline fn as(obj: *Obj, comptime T: type) *T {
    std.debug.assert(obj.kind == T.kind);
    return @alignCast(@fieldParentPtr("obj", obj));
}

/// Free this object.
pub fn deinit(obj: *Obj) void {
    if (config.log_gc) {
        std.debug.print("{s} free\n", .{obj});
    }
    switch (obj.kind) {
        .closure => obj.as(Closure).deinit(),
        .function => obj.as(Function).deinit(),
        .native => obj.as(Native).deinit(),
        .string => obj.as(String).deinit(),
        .upvalue => obj.as(Upvalue).deinit(),
    }
}

pub fn eql(self: *Obj, other: *Obj) bool {
    if (self.kind != other.kind) {
        return false;
    }
    return switch (self.kind) {
        .closure => self.as(Closure).function.obj.eql(&other.as(Closure).function.obj),
        // strings are interned, so we can compare them by pointer
        .string => self == other,
        .native => self == other,
        .function => self == other,
        .upvalue => self.as(Upvalue).location.eql(other.as(Upvalue).location.*),
    };
}

pub const Closure = struct {
    const Self = @This();
    const kind = Kind.closure;

    obj: Obj,
    function: *Function,
    upvalues: []?*Upvalue,

    pub fn init(function: *Function) !*Self {
        var upvalues: []?*Upvalue = &[_]?*Upvalue{};
        upvalues = try memory.reallocate(upvalues, function.upvalue_count);
        for (upvalues) |*v| v.* = null;
        return try allocate(Self, .{ .function = function, .upvalues = upvalues });
    }

    pub fn deinit(self: *Self) void {
        memory.free(self.upvalues);
        memory.free(self[0..1]);
    }
};

pub const Function = struct {
    const Self = @This();
    const kind = Kind.function;

    obj: Obj,
    arity: u8 = 0,
    upvalue_count: u8 = 0,
    chunk: Chunk,
    name: ?*String = null,

    pub fn init() !*Self {
        return try allocate(Self, .{ .chunk = Chunk.init() });
    }

    pub fn deinit(self: *Self) void {
        self.chunk.deinit();
        memory.free(self[0..1]);
    }

    pub fn format(
        self: *const Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try if (self.name) |name|
            writer.print("<fn {s}>", .{name.value.as_slice()})
        else
            writer.print("<script>", .{});
    }
};

pub const NativeFn = *const fn (u8, [*]Value) Value;

pub const Native = struct {
    const kind = Kind.native;

    obj: Obj,
    function: NativeFn,
    arity: u8,

    pub fn init(arity: u8, function: NativeFn) !*Native {
        return try allocate(Native, .{ .function = function, .arity = arity });
    }

    pub fn deinit(self: *Native) void {
        memory.free(self[0..1]);
    }
};

pub const String = struct {
    const Self = @This();
    const kind = Kind.string;

    obj: Obj,
    value: string.String,
    hash: u32,

    pub fn copy(chars: []const u8) !*Self {
        const hash = hash_fn(chars);
        if (vm.vm.strings.find_key(chars, hash)) |interned| return interned;
        const s = try string.String.init_from(chars);
        return try init(s, hash);
    }

    pub fn take(s: *string.String) !*Self {
        const chars = s.chars.items;
        const hash = hash_fn(chars);
        if (vm.vm.strings.find_key(chars, hash)) |interned| {
            s.deinit();
            return interned;
        }
        return try init(s.*, hash);
    }

    fn init(s: string.String, hash: u32) !*Self {
        const ptr = try allocate(Self, .{ .value = s, .hash = hash });
        vm.push(ptr.obj.value());
        const is_new = try vm.vm.strings.insert(ptr, .nil);
        _ = vm.pop();
        std.debug.assert(is_new);
        return ptr;
    }

    pub fn deinit(self: *Self) void {
        self.value.deinit();
        memory.free(self[0..1]);
    }

    fn hash_fn(chars: []const u8) u32 {
        var hash: u32 = 2166136261;
        for (chars) |char| {
            hash ^= @intCast(char);
            hash *%= 16777619;
        }
        return hash;
    }

    pub fn format(
        self: *const Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("{s}", .{self.value.as_slice()});
    }
};

pub const Upvalue = struct {
    const kind = Kind.upvalue;

    obj: Obj,
    location: *Value,
    closed: Value = .nil,
    next: ?*Upvalue,

    pub fn init(args: struct { location: *Value, next: ?*Upvalue = null }) !*Upvalue {
        return try allocate(Upvalue, args);
    }

    pub fn deinit(self: *Upvalue) void {
        memory.free(self[0..1]);
    }
};

fn allocate(comptime T: type, args: anytype) !*T {
    var slice: []T = &[_]T{};
    slice = try memory.reallocate(slice, 1);
    const ptr = &slice[0];
    ptr.obj = .{
        .kind = T.kind,
        .is_marked = false,
        .next = vm.vm.objects,
    };
    inline for (@typeInfo(T).@"struct".fields) |field| {
        if (field.default_value) |opaque_ptr| {
            const default_ptr: *const field.type = @alignCast(@ptrCast(opaque_ptr));
            @field(ptr, field.name) = default_ptr.*;
        }
    }
    inline for (@typeInfo(@TypeOf(args)).@"struct".fields) |field| {
        @field(ptr, field.name) = @field(args, field.name);
    }
    vm.vm.objects = &ptr.obj;
    if (config.log_gc) {
        std.debug.print("{s} allocate {d}\n", .{ &ptr.obj, @sizeOf(T) });
    }
    return ptr;
}

// format object pointer with its kind
pub fn format(
    obj: *const Obj,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;
    _ = options;
    try writer.print(
        "0x{x}-{s}",
        .{ @intFromPtr(obj), @tagName(obj.kind) },
    );
}
