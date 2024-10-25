const std = @import("std");
const allocator = @import("main.zig").allocator;
const compiler = @import("compiler.zig");
const vm = @import("vm.zig");
const Error = error{OutOfMemory};
const Obj = @import("Obj.zig");
const Value = @import("value.zig").Value;
const Table = @import("Table.zig");
const config = @import("config");

const GC_HEAP_GROW_FACTOR = 2;

pub fn grow_capacity(capacity: usize) usize {
    return if (capacity < 8) 8 else capacity * 2;
}

pub fn reallocate(old_mem: anytype, new_n: usize) t: {
    const Slice = @typeInfo(@TypeOf(old_mem)).pointer;
    break :t Error![]Slice.child;
} {
    const old_n = old_mem.len;
    if (new_n > old_n) {
        vm.vm.bytes_allocated += new_n - old_n;
    } else {
        vm.vm.bytes_allocated -= old_n - new_n;
    }
    if (new_n > old_n) {
        if (config.stress_gc or vm.vm.bytes_allocated > vm.vm.next_gc) {
            collect_garbage();
        }
    }
    return try allocator.realloc(old_mem, new_n);
}

pub fn free(memory: anytype) void {
    _ = reallocate(memory, 0) catch unreachable;
}

pub fn free_objects() void {
    var object = vm.vm.objects;
    while (object) |o| {
        const next = o.next;
        o.deinit();
        object = next;
    }
    vm.vm.gray_stack.deinit();
}

fn collect_garbage() void {
    var before: usize = 0;
    if (config.log_gc) {
        std.debug.print("-- gc begin\n", .{});
        before = vm.vm.bytes_allocated;
    }

    mark_roots();
    trace_references();
    table_remove_white(&vm.vm.strings);
    sweep();

    vm.vm.next_gc = vm.vm.bytes_allocated * GC_HEAP_GROW_FACTOR;

    if (config.log_gc) {
        std.debug.print("-- gc end\n", .{});
        const current = vm.vm.bytes_allocated;
        std.debug.print(
            "   collected {d} bytes (from {d} to {d}) next at {d}\n",
            .{ before - current, before, current, vm.vm.next_gc },
        );
    }
}

fn mark_roots() void {
    for (&vm.vm.stack) |*slot| {
        if (@intFromPtr(slot) >= @intFromPtr(vm.vm.stack_top)) break;
        mark_value(slot.*);
    }
    for (0..vm.vm.frame_count) |i| {
        mark_object(&vm.vm.frames[i].closure.obj);
    }
    var upvalue = vm.vm.open_upvalues;
    while (upvalue) |v| {
        mark_object(&v.obj);
        upvalue = v.next;
    }
    mark_table(&vm.vm.global_names);
    for (vm.vm.global_values.items) |global| mark_value(global);
    compiler.mark_compiler_roots();
}

fn trace_references() void {
    while (vm.vm.gray_stack.items.len > 0) {
        const object = vm.vm.gray_stack.pop();
        blacken_object(object);
    }
}

fn sweep() void {
    var previous: ?*Obj = null;
    var object = vm.vm.objects;
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
                vm.vm.objects = object;
            }
            unreached.deinit();
        }
    }
}

fn mark_value(value: Value) void {
    if (value == .obj) mark_object(value.obj);
}

fn blacken_object(object: *Obj) void {
    if (config.log_gc) {
        std.debug.print("{s} blacken {s}\n", .{ object, object.value() });
    }
    switch (object.kind) {
        .closure => {
            const closure = object.as(Obj.Closure);
            mark_object(&closure.function.obj);
            for (closure.upvalues) |v|
                if (v) |upvalue| mark_object(&upvalue.obj);
        },
        .function => {
            const fun = object.as(Obj.Function);
            if (fun.name) |s| mark_object(&s.obj);
            for (fun.chunk.constants.items) |constant| mark_value(constant);
        },
        .upvalue => mark_value(object.as(Obj.Upvalue).closed),
        .native, .string => {},
    }
}

pub fn mark_object(object: ?*Obj) void {
    if (object == null) return;
    const obj = object.?;
    if (obj.is_marked) return;
    if (config.log_gc) {
        std.debug.print("{s} mark {s}\n", .{ obj, obj.value() });
    }
    obj.is_marked = true;
    // crash and burn if allocating for gray object fails.
    vm.vm.gray_stack.append(obj) catch unreachable;
}

fn mark_table(table: *Table) void {
    for (table.entries) |*entry| {
        mark_object(@ptrCast(entry.key));
        mark_value(entry.value);
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
