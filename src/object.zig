const std = @import("std");
const config = @import("config");
const memory = @import("memory.zig");
const Chunk = @import("chunk.zig").Chunk;
const String = @import("string.zig").String;
const Value = @import("value.zig").Value;
const vm = @import("vm.zig");

pub const Obj = struct {
    kind: ObjKind,
    is_marked: bool = false,
    next: ?*Obj,

    pub fn deinit(self: *Obj) void {
        if (config.log_gc) {
            std.debug.print("{*} free type {s}\n", .{ self, @tagName(self.kind) });
        }
        switch (self.kind) {
            .closure => self.downcast_closure().deinit(),
            .string => self.downcast_string().deinit(),
            .native => self.downcast_native().deinit(),
            .function => self.downcast_function().deinit(),
            .upvalue => self.downcast_upvalue().deinit(),
        }
    }

    pub inline fn downcast(self: *Obj, comptime T: type) *T {
        std.debug.assert(self.kind == T.obj_kind);
        return @alignCast(@fieldParentPtr("obj", self));
    }

    pub inline fn downcast_closure(self: *Obj) *ObjClosure {
        return self.downcast(ObjClosure);
    }

    pub inline fn downcast_function(self: *Obj) *ObjFunction {
        return self.downcast(ObjFunction);
    }

    pub inline fn downcast_native(self: *Obj) *ObjNative {
        return self.downcast(ObjNative);
    }

    pub inline fn downcast_string(self: *Obj) *ObjString {
        return self.downcast(ObjString);
    }

    pub inline fn downcast_upvalue(self: *Obj) *ObjUpvalue {
        return self.downcast(ObjUpvalue);
    }

    pub fn eql(self: *Obj, other: *Obj) bool {
        if (self.kind != other.kind) {
            return false;
        }
        return switch (self.kind) {
            .closure => other.kind == .closure and
                self.downcast_closure().function.obj.eql(&other.downcast_closure().function.obj),
            // strings are interned, so we can compare them by pointer
            .string => self == other,
            .native => self == other,
            .function => other.kind == .function and
                self.downcast_function().name == other.downcast_function().name,
            .upvalue => other.kind == .upvalue and
                self.downcast_upvalue().location.eql(other.downcast_upvalue().location.*),
        };
    }
};

pub const ObjClosure = struct {
    const Self = @This();
    const obj_kind = ObjKind.closure;

    obj: Obj,
    function: *ObjFunction,
    upvalues: []?*ObjUpvalue,

    pub fn init(function: *ObjFunction) !*Self {
        var upvalues: []?*ObjUpvalue = &[_]?*ObjUpvalue{};
        upvalues = try memory.reallocate(upvalues, function.upvalue_count);
        for (0..function.upvalue_count) |i| {
            upvalues[i] = null;
        }
        const closure = try allocate_object(Self);
        closure.function = function;
        closure.upvalues = upvalues;
        return closure;
    }

    pub fn deinit(self: *Self) void {
        memory.free(self.upvalues);
        memory.free(self[0..1]);
    }

    pub fn upcast(self: *Self) *Obj {
        return &self.obj;
    }
};

pub const ObjUpvalue = struct {
    const Self = @This();
    const obj_kind = ObjKind.upvalue;

    obj: Obj,
    location: *Value,
    closed: Value,
    next: ?*ObjUpvalue,

    pub fn init(slot: *Value) !*Self {
        const upvalue = try allocate_object(Self);
        upvalue.location = slot;
        upvalue.closed = .nil;
        upvalue.next = null;
        return upvalue;
    }

    pub fn deinit(self: *Self) void {
        memory.free(self[0..1]);
    }

    pub inline fn upcast(self: *Self) *Obj {
        return &self.obj;
    }
};

pub const ObjFunction = struct {
    const Self = @This();
    const obj_kind = ObjKind.function;

    obj: Obj,
    arity: u8,
    upvalue_count: u8,
    chunk: Chunk,
    name: ?*ObjString,

    pub fn init() !*Self {
        const fun = try allocate_object(Self);
        fun.arity = 0;
        fun.upvalue_count = 0;
        fun.chunk = Chunk.init();
        fun.name = null;
        return fun;
    }

    pub fn deinit(self: *Self) void {
        self.chunk.deinit();
        const f: []Self = self[0..1]; // memory.reallocate expects a slice
        memory.free(f);
    }

    pub inline fn upcast(self: *Self) *Obj {
        return &self.obj;
    }
};

pub const ObjNative = struct {
    const Self = @This();
    const obj_kind = ObjKind.native;
    pub const NativeFn = *const fn (u8, [*]Value) Value;

    obj: Obj,
    arity: u8,
    function: NativeFn,

    pub fn init(arity: u8, function: NativeFn) !*Self {
        const fun = try allocate_object(Self);
        fun.arity = arity;
        fun.function = function;
        return fun;
    }

    pub fn deinit(self: *Self) void {
        const f: []Self = self[0..1];
        memory.free(f);
    }

    pub inline fn upcast(self: *Self) *Obj {
        return &self.obj;
    }
};

pub const ObjString = struct {
    const Self = @This();
    const obj_kind = ObjKind.string;

    obj: Obj,
    value: String,
    hash: u32,

    pub fn copy(chars: []const u8) !*Self {
        const hash = hash_string(chars);
        if (vm.vm.strings.find_key(chars, hash)) |interned| return interned;
        return try allocate(try String.init_from(chars), hash);
    }

    pub fn take(string: *String) !*Self {
        const chars = string.chars.items;
        const hash = hash_string(chars);
        if (vm.vm.strings.find_key(chars, hash)) |interned| {
            string.deinit();
            return interned;
        }
        return try allocate(string.*, hash);
    }

    pub fn deinit(self: *Self) void {
        self.value.deinit();
        const s: []Self = self[0..1]; // memory.reallocate expects a slice
        memory.free(s);
    }

    pub inline fn upcast(self: *Self) *Obj {
        return &self.obj;
    }

    fn allocate(string: String, hash: u32) !*Self {
        const s = try allocate_object(Self);
        s.value = string;
        s.hash = hash;
        const is_new = try vm.vm.strings.insert(s, .nil);
        std.debug.assert(is_new);
        return s;
    }
};

pub const ObjKind = enum {
    closure,
    function,
    native,
    string,
    upvalue,
};

fn allocate_object(comptime T: type) !*T {
    var t: []T = &[_]T{}; // get a slice of size 0
    t = try memory.reallocate(t, 1); // allocate a single object
    const ptr = &t[0];
    // update the linked list of objects
    ptr.obj = .{ .kind = T.obj_kind, .next = vm.vm.objects };
    vm.vm.objects = ptr.upcast();
    if (config.log_gc) {
        std.debug.print(
            "{*} allocate {d} for {s}\n",
            .{ ptr, @sizeOf(T), @tagName(T.obj_kind) },
        );
    }
    return ptr;
}

fn hash_string(chars: []const u8) u32 {
    var hash: u32 = 2166136261;
    for (chars) |char| {
        hash ^= @intCast(char);
        hash *%= 16777619;
    }
    return hash;
}
