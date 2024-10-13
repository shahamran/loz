const std = @import("std");
const memory = @import("memory.zig");
const Chunk = @import("chunk.zig").Chunk;
const String = @import("string.zig").String;
const vm = @import("vm.zig");

pub const Obj = struct {
    kind: ObjKind,
    next: ?*Obj,

    pub inline fn downcast(self: *Obj, comptime T: type) *T {
        std.debug.assert(self.kind == T.obj_kind);
        return @alignCast(@fieldParentPtr("obj", self));
    }

    pub inline fn downcast_function(self: *Obj) *ObjFunction {
        return self.downcast(ObjFunction);
    }

    pub inline fn downcast_string(self: *Obj) *ObjString {
        return self.downcast(ObjString);
    }

    pub fn eql(self: *Obj, other: *Obj) bool {
        if (self.kind != other.kind) {
            return false;
        }
        return switch (self.kind) {
            // strings are interned, so we can compare them by pointer
            .string => self == other,
            .function => other.kind == .function and
                self.downcast_function().name == other.downcast_function().name,
        };
    }
};

pub const ObjFunction = struct {
    const Self = @This();
    const obj_kind = ObjKind.function;

    obj: Obj,
    arity: u8,
    chunk: Chunk,
    name: ?*ObjString,

    pub fn init() !*Self {
        const fun = try allocate_object(Self);
        fun.arity = 0;
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
    function,
    string,
};

fn allocate_object(comptime T: type) !*T {
    var t: []T = &[_]T{}; // get a slice of size 0
    t = try memory.reallocate(t, 1); // allocate a single object
    const ptr = &t[0];
    // update the linked list of objects
    ptr.obj = .{ .kind = T.obj_kind, .next = vm.vm.objects };
    vm.vm.objects = ptr.upcast();
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
