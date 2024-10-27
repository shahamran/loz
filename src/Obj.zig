const std = @import("std");
const config = @import("config");
const string = @import("string.zig");
const Chunk = @import("Chunk.zig");
const Value = @import("value.zig").Value;
const Vm = @import("Vm.zig");

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
pub fn deinit(obj: *Obj, vm: *Vm) void {
    if (config.log_gc) {
        std.debug.print("0x{x} free {s}\n", .{ @intFromPtr(obj), @tagName(obj.kind) });
    }
    switch (obj.kind) {
        .closure => obj.as(Closure).deinit(vm),
        .function => obj.as(Function).deinit(vm),
        .native => obj.as(Native).deinit(vm),
        .string => obj.as(String).deinit(vm),
        .upvalue => obj.as(Upvalue).deinit(vm),
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
    pub const kind = Kind.closure;
    const Self = @This();

    obj: Obj,
    function: *Function,
    upvalues: []?*Upvalue,

    pub fn init(vm: *Vm, function: *Function) !*Self {
        const upvalues = try vm.allocator.alloc(?*Upvalue, function.upvalue_count);
        for (upvalues) |*v| v.* = null;
        return try vm.allocate_object(Self, .{ .function = function, .upvalues = upvalues });
    }

    pub fn deinit(self: *Self, vm: *Vm) void {
        vm.allocator.free(self.upvalues);
        vm.allocator.destroy(self);
    }
};

pub const Function = struct {
    pub const kind = Kind.function;
    const Self = @This();

    obj: Obj,
    arity: u8 = 0,
    upvalue_count: u8 = 0,
    chunk: Chunk,
    name: ?*String = null,

    pub fn init(vm: *Vm) !*Self {
        return try vm.allocate_object(
            Self,
            .{ .chunk = Chunk.init(vm.allocator) },
        );
    }

    pub fn deinit(self: *Self, vm: *Vm) void {
        self.chunk.deinit();
        vm.allocator.destroy(self);
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
    pub const kind = Kind.native;

    obj: Obj,
    function: NativeFn,
    arity: u8,

    pub fn init(vm: *Vm, arity: u8, function: NativeFn) !*Native {
        return try vm.allocate_object(Native, .{ .function = function, .arity = arity });
    }

    pub fn deinit(self: *Native, vm: *Vm) void {
        vm.allocator.destroy(self);
    }
};

pub const String = struct {
    pub const kind = Kind.string;
    const Self = @This();

    obj: Obj,
    value: string.String,
    hash: u32,

    pub fn copy(vm: *Vm, chars: []const u8) !*Self {
        const hash = hash_fn(chars);
        if (vm.strings.find_key(chars, hash)) |interned| return interned;
        const s = try string.String.init_from(vm.allocator, chars);
        return try init(vm, s, hash);
    }

    pub fn take(vm: *Vm, s: *string.String) !*Self {
        const chars = s.as_slice();
        const hash = hash_fn(chars);
        if (vm.strings.find_key(chars, hash)) |interned| {
            s.deinit(vm.allocator);
            return interned;
        }
        return try init(vm, s.*, hash);
    }

    fn init(vm: *Vm, s: string.String, hash: u32) !*Self {
        const ptr = try vm.allocate_object(Self, .{ .value = s, .hash = hash });
        vm.push(ptr.obj.value());
        const is_new = try vm.strings.insert(ptr, .nil);
        _ = vm.pop();
        std.debug.assert(is_new);
        return ptr;
    }

    pub fn deinit(self: *Self, vm: *Vm) void {
        self.value.deinit(vm.allocator);
        vm.allocator.destroy(self);
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
    pub const kind = Kind.upvalue;

    obj: Obj,
    location: *Value,
    closed: Value = .nil,
    next: ?*Upvalue,

    pub fn init(vm: *Vm, args: struct { location: *Value, next: ?*Upvalue = null }) !*Upvalue {
        return try vm.allocate_object(Upvalue, args);
    }

    pub fn deinit(self: *Upvalue, vm: *Vm) void {
        vm.allocator.destroy(self);
    }
};
