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
    self.maybe_collect(len);
    const ret = self.parent_allocator
        .rawAlloc(len, log2_buf_align, ret_addr) orelse return null;
    self.bytes_allocated += len;
    return ret;
}

fn resize(ctx: *anyopaque, buf: []u8, log2_buf_align: u8, new_len: usize, ret_addr: usize) bool {
    const self: *Self = @ptrCast(@alignCast(ctx));
    const old_len = buf.len;
    const abs_diff = if (new_len > old_len) new_len - old_len else old_len - new_len;

    if (new_len > old_len) self.maybe_collect(abs_diff);

    const resized = self.parent_allocator
        .rawResize(buf, log2_buf_align, new_len, ret_addr);
    if (!resized) return false;
    if (new_len > old_len) {
        self.bytes_allocated += abs_diff;
    } else {
        self.bytes_allocated -= abs_diff;
    }
    return true;
}

fn free(ctx: *anyopaque, buf: []u8, log2_buf_align: u8, ret_addr: usize) void {
    const self: *Self = @ptrCast(@alignCast(ctx));
    self.parent_allocator
        .rawFree(buf, log2_buf_align, ret_addr);
    self.bytes_allocated -= buf.len;
}

inline fn maybe_collect(self: *Self, additional: usize) void {
    if (config.stress_gc or self.bytes_allocated + additional > self.next_gc) {
        self.collect_garbage();
    }
}

fn collect_garbage(self: *Self) void {
    var before: usize = 0;
    if (config.log_gc) {
        std.debug.print("-- gc begin\n", .{});
        before = self.bytes_allocated;
    }

    self.mark_roots();
    self.trace_references();
    table_remove_white(&self.vm.strings);
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

fn mark_roots(self: *Self) void {
    for (&self.vm.stack) |*slot| {
        if (@intFromPtr(slot) >= @intFromPtr(self.vm.stack_top)) break;
        self.mark_value(slot.*);
    }
    for (0..self.vm.frame_count) |i| {
        self.mark_object(&self.vm.frames[i].closure.obj);
    }
    var upvalue = self.vm.open_upvalues;
    while (upvalue) |v| {
        self.mark_object(&v.obj);
        upvalue = v.next;
    }
    self.mark_table(&self.vm.global_names);
    for (self.vm.global_values.items) |global| self.mark_value(global);
    self.mark_compiler_roots();
}

fn mark_compiler_roots(self: *Self) void {
    if (!self.vm.compiler.valid) return;
    var node: ?*Compiler.Node = self.vm.compiler.current;
    while (node) |n| {
        self.mark_object(&n.function.obj);
        node = n.enclosing;
    }
}

inline fn mark_value(self: *Self, value: Value) void {
    if (value == .obj) self.mark_object(value.obj);
}

fn mark_table(self: *Self, table: *Table) void {
    for (table.entries) |*entry| {
        self.mark_object(@ptrCast(entry.key));
        self.mark_value(entry.value);
    }
}

fn mark_object(self: *Self, object: ?*Obj) void {
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

fn trace_references(self: *Self) void {
    while (self.gray_stack.items.len > 0) {
        const object = self.gray_stack.pop();
        self.blacken_object(object);
    }
}

fn blacken_object(self: *Self, object: *Obj) void {
    if (config.log_gc) {
        std.debug.print("0x{x} blacken {s}\n", .{ @intFromPtr(object), object.value() });
    }
    switch (object.kind) {
        .closure => {
            const closure = object.as(Obj.Closure);
            self.mark_object(&closure.function.obj);
            for (closure.upvalues) |v|
                if (v) |upvalue| self.mark_object(&upvalue.obj);
        },
        .function => {
            const fun = object.as(Obj.Function);
            if (fun.name) |s| self.mark_object(&s.obj);
            for (fun.chunk.constants.items) |constant| self.mark_value(constant);
        },
        .upvalue => self.mark_value(object.as(Obj.Upvalue).closed),
        .native, .string => {},
    }
}

fn table_remove_white(table: *Table) void {
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
