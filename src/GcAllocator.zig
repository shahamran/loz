const builtin = @import("builtin");
const std = @import("std");
const config = @import("config");
const Allocator = std.mem.Allocator;
const Compiler = @import("Compiler.zig");
const Obj = @import("Obj.zig");
const Table = @import("Table.zig");
const Value = @import("value.zig").Value;
const Vm = @import("Vm.zig");

const Self = @This();
const GC_HEAP_GROW_FACTOR = 2;

vm: *Vm,
parent_allocator: Allocator,
bytes_allocated: usize,
next_gc: usize,
gray_stack: std.ArrayList(*Obj), // objects whose references we need to trace

pub fn init(vm: *Vm, backing_allocator: Allocator) Self {
    return .{
        .vm = vm,
        .parent_allocator = backing_allocator,
        .bytes_allocated = 0,
        .next_gc = 1024 * 1024,
        .gray_stack = std.ArrayList(*Obj).init(backing_allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.gray_stack.deinit();
}

pub inline fn allocator(self: *Self) Allocator {
    return .{
        .ptr = self,
        .vtable = &.{
            .alloc = alloc,
            .resize = resize,
            .free = free,
        },
    };
}

fn alloc(ctx: *anyopaque, len: usize, log2_buf_align: u8, ret_addr: usize) ?[*]u8 {
    const self: *Self = @ptrCast(@alignCast(ctx));
    self.bytes_allocated += len;
    self.maybeCollect();
    return self.parent_allocator.rawAlloc(len, log2_buf_align, ret_addr);
}

fn resize(ctx: *anyopaque, buf: []u8, log2_buf_align: u8, new_len: usize, ret_addr: usize) bool {
    const self: *Self = @ptrCast(@alignCast(ctx));
    const old_len = buf.len;
    const abs_diff = if (new_len > old_len) new_len - old_len else old_len - new_len;
    if (new_len > old_len) {
        self.bytes_allocated += abs_diff;
    } else {
        self.bytes_allocated -= abs_diff;
    }
    if (new_len > old_len) self.maybeCollect();
    return self.parent_allocator.rawResize(buf, log2_buf_align, new_len, ret_addr);
}

fn free(ctx: *anyopaque, buf: []u8, log2_buf_align: u8, ret_addr: usize) void {
    const self: *Self = @ptrCast(@alignCast(ctx));
    self.bytes_allocated -= buf.len;
    self.parent_allocator.rawFree(buf, log2_buf_align, ret_addr);
}

inline fn maybeCollect(self: *Self) void {
    if (config.stress_gc or self.bytes_allocated > self.next_gc) {
        self.collectGarbage();
    }
}

fn collectGarbage(self: *Self) void {
    var before: usize = 0;
    if (config.log_gc) {
        std.debug.print("-- gc begin\n", .{});
        before = self.bytes_allocated;
    }

    self.markRoots();
    self.traceReferences();
    tableRemoveWhite(&self.vm.strings);
    self.sweep();

    self.next_gc = self.bytes_allocated * GC_HEAP_GROW_FACTOR;

    if (config.log_gc) {
        std.debug.print("-- gc end\n", .{});
        const current = self.bytes_allocated;
        std.debug.print(
            "   collected {d} bytes (from {d} to {d}) next at {d}\n",
            .{ before - current, before, current, self.next_gc },
        );
    }
}

fn markRoots(self: *Self) void {
    for (&self.vm.stack) |*slot| {
        if (@intFromPtr(slot) >= @intFromPtr(self.vm.stack_top)) break;
        self.markValue(slot.*);
    }
    for (0..self.vm.frame_count) |i| {
        self.markObject(&self.vm.frames[i].closure.obj);
    }
    var upvalue = self.vm.open_upvalues;
    while (upvalue) |v| {
        self.markObject(&v.obj);
        upvalue = v.next;
    }
    self.markTable(&self.vm.global_names);
    for (self.vm.global_values.items) |global| self.markValue(global.value);
    self.markCompilerRoots();
    if (self.vm.init_string) |s| self.markObject(&s.obj);
}

fn markCompilerRoots(self: *Self) void {
    if (!self.vm.compiler.valid) return;
    var node: ?*Compiler.Node = self.vm.compiler.current;
    while (node) |n| {
        self.markObject(&n.function.obj);
        node = n.enclosing;
    }
}

inline fn markValue(self: *Self, value: Value) void {
    if (value == .obj) self.markObject(value.obj);
}

fn markTable(self: *Self, table: *Table) void {
    for (table.entries) |*entry| {
        const key: ?*Obj = if (entry.key) |k| &k.obj else null;
        self.markObject(key);
        self.markValue(entry.value);
    }
}

fn markObject(self: *Self, object: ?*Obj) void {
    if (object == null) return;
    const obj = object.?;
    if (obj.is_marked) return;
    if (config.log_gc) {
        std.debug.print("0x{x} mark {s}\n", .{ @intFromPtr(obj), obj.value() });
    }
    obj.is_marked = true;
    // crash and burn if allocating for gray object fails.
    self.gray_stack.append(obj) catch unreachable;
}

fn traceReferences(self: *Self) void {
    while (self.gray_stack.items.len > 0) {
        const object = self.gray_stack.pop();
        self.blackenObject(object);
    }
}

fn blackenObject(self: *Self, object: *Obj) void {
    if (config.log_gc) {
        std.debug.print("0x{x} blacken {s}\n", .{ @intFromPtr(object), object.value() });
    }
    switch (object.kind) {
        .bound_method => {
            const bound = object.as(Obj.BoundMethod);
            self.markValue(bound.receiver);
            self.markObject(&bound.method.obj);
        },
        .class => {
            const class = object.as(Obj.Class);
            self.markObject(&class.name.obj);
            self.markTable(&class.methods);
        },
        .closure => {
            const closure = object.as(Obj.Closure);
            self.markObject(&closure.function.obj);
            for (closure.upvalues) |v|
                if (v) |upvalue| self.markObject(&upvalue.obj);
        },
        .function => {
            const fun = object.as(Obj.Function);
            if (fun.name) |s| self.markObject(&s.obj);
            for (fun.chunk.constants.items) |constant| self.markValue(constant);
        },
        .instance => {
            const inst = object.as(Obj.Instance);
            self.markObject(&inst.class.obj);
            self.markTable(&inst.fields);
        },
        .upvalue => self.markValue(object.as(Obj.Upvalue).closed),
        .native, .string => {},
    }
}

fn tableRemoveWhite(table: *Table) void {
    for (table.entries) |entry| {
        if (entry.key) |key| {
            if (!key.obj.is_marked) {
                _ = table.delete(key);
            }
        }
    }
}

fn sweep(self: *Self) void {
    var previous: ?*Obj = null;
    var object = self.vm.objects;
    while (object) |obj| {
        if (obj.is_marked) {
            obj.is_marked = false;
            previous = obj;
            object = obj.next;
        } else {
            var unreached = obj;
            object = obj.next;
            if (previous) |p| {
                p.next = object;
            } else {
                self.vm.objects = object;
            }
            unreached.deinit(self.vm);
        }
    }
}
