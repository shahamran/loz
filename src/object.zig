const std = @import("std");
const memory = @import("memory.zig");
const String = @import("string.zig").String;
const vm = @import("vm.zig");

pub const Obj = struct {
    kind: ObjKind,
    next: ?*Obj,

    pub inline fn downcast(self: *Obj, comptime T: type) *T {
        std.debug.assert(self.kind == T.obj_kind);
        return @alignCast(@fieldParentPtr("obj", self));
    }

    pub inline fn downcast_string(self: *Obj) *ObjString {
        return self.downcast(ObjString);
    }

    pub fn eql(self: *Obj, other: *Obj) bool {
        if (self.kind != other.kind) {
            return false;
        }
        switch (self.kind) {
            .string => return self.downcast_string().value.chars
                .eql(&other.downcast_string().value.chars),
        }
    }
};

pub const ObjString = struct {
    const Self = @This();
    const obj_kind = ObjKind.string;

    obj: Obj,
    value: String,

    pub fn init(chars: []const u8) !*Self {
        var s = try allocate_object(Self);
        s.value = try String.init_from(chars);
        return s;
    }

    pub fn take(string: String) !*Self {
        var s = try init("");
        s.value = string;
        return s;
    }

    pub fn deinit(self: *Self) void {
        self.value.deinit();
        const s: []Self = self[0..1]; // memory.reallocate expects a slice
        memory.free(s);
    }

    pub inline fn upcast(self: *Self) *Obj {
        return &self.obj;
    }
};

pub const ObjKind = enum {
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
