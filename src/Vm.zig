const std = @import("std");
const config = @import("config");

const Chunk = @import("Chunk.zig");
const OpCode = Chunk.OpCode;
const Compiler = @import("Compiler.zig");
const List = @import("list.zig").List;
const Obj = @import("Obj.zig");
const Table = @import("Table.zig");
const Value = @import("value.zig").Value;

const UINT8_COUNT = @import("main.zig").UINT8_COUNT;
const FRAMES_MAX = 64;
const STACK_MAX = FRAMES_MAX * UINT8_COUNT;

const Vm = @This();

allocator: std.mem.Allocator,
compiler: Compiler,
frames: [FRAMES_MAX]CallFrame,
frame_count: u8,

stack: [STACK_MAX]Value,
stack_top: [*]Value,

global_names: Table, // maps global variable names to their indices in the globals list
global_values: List(Value),

strings: Table, // interned strings

open_upvalues: ?*Obj.Upvalue, // singly linked list of open upvalues
objects: ?*Obj, // singly linked list of all allocated objects

pub fn init(vm: *Vm, allocator: std.mem.Allocator) void {
    vm.reset_stack();
    vm.compiler.init(vm);
    vm.allocator = allocator;
    vm.global_names = Table.init(allocator);
    vm.global_values = List(Value).init(allocator);
    vm.strings = Table.init(allocator);
    vm.objects = null;
    vm.define_native("clock", 0, clock_native) catch unreachable;
}

pub fn interpret(vm: *Vm, source: []const u8) !InterpretResult {
    var function = try vm.compiler.compile(source) orelse return .compile_error;

    vm.push(function.obj.value());
    const closure = try Obj.Closure.init(vm, function);
    _ = vm.pop();

    vm.push(closure.obj.value());
    _ = vm.call(closure, 0);

    return try vm.run();
}

pub const InterpretResult = enum {
    ok,
    compile_error,
    runtime_error,
};

pub fn allocate_object(vm: *Vm, comptime T: type, args: anytype) !*T {
    var ptr = try vm.allocator.create(T);
    ptr.obj = .{
        .kind = T.kind,
        .is_marked = false,
        .next = vm.objects,
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
    vm.objects = &ptr.obj;
    if (config.log_gc) {
        std.debug.print("{s} allocate {d}\n", .{ &ptr.obj, @sizeOf(T) });
    }
    return ptr;
}

pub fn deinit(vm: *Vm) void {
    vm.free_objects();
    vm.global_names.deinit();
    vm.global_values.deinit();
    vm.strings.deinit();
}

pub inline fn push(vm: *Vm, value: Value) void {
    vm.stack_top[0] = value;
    vm.stack_top += 1;
}

pub inline fn pop(vm: *Vm) Value {
    vm.stack_top -= 1;
    return vm.stack_top[0];
}

fn free_objects(vm: *Vm) void {
    var object = vm.objects;
    while (object) |o| {
        const next = o.next;
        o.deinit(vm);
        object = next;
    }
}

const CallFrame = struct {
    const Self = @This();

    closure: *Obj.Closure,
    ip: [*]u8,
    slots: [*]Value,

    inline fn read_byte(self: *Self) u8 {
        const byte = self.ip[0];
        self.ip += 1;
        return byte;
    }

    inline fn read_u16(self: *Self) u16 {
        const byte1 = self.ip[0];
        const byte2 = self.ip[1];
        self.ip += 2;
        return (@as(u16, byte1) << 8) | @as(u16, byte2);
    }

    inline fn read_constant(self: *Self) Value {
        return self.constants()[self.read_byte()];
    }

    inline fn constants(self: *Self) []Value {
        return self.closure.function.chunk.constants.items;
    }

    inline fn code(self: *Self) []u8 {
        return self.closure.function.chunk.code.items;
    }

    inline fn offset(self: *Self) usize {
        return self.ip - self.code().ptr;
    }
};

fn run(vm: *Vm) !InterpretResult {
    var frame = &vm.frames[vm.frame_count - 1];
    while (true) {
        if (comptime config.trace_execution) {
            std.debug.print("          ", .{});
            var slot: [*]Value = &vm.stack;
            while (slot != vm.stack_top) : (slot += 1) {
                std.debug.print("[ {s} ]", .{slot[0]});
            }
            std.debug.print("\n", .{});
            _ = @import("debug.zig")
                .disassemble_instruction(&frame.closure.function.chunk, frame.offset());
        }
        const instruction: OpCode = @enumFromInt(frame.read_byte());
        switch (instruction) {
            .op_constant => {
                const constant = frame.read_constant();
                vm.push(constant);
            },
            .op_nil => vm.push(Value{ .nil = {} }),
            .op_true => vm.push(Value{ .bool_ = true }),
            .op_false => vm.push(Value{ .bool_ = false }),
            .op_pop => _ = vm.pop(),
            .op_get_local => {
                const slot = frame.read_byte();
                vm.push(frame.slots[slot]);
            },
            .op_set_local => {
                const slot = frame.read_byte();
                frame.slots[slot] = vm.peek(0);
            },
            .op_get_global => {
                const value = vm.global_values.items[frame.read_byte()];
                if (value == .undefined_) {
                    vm.runtime_error("Undefined variable.", .{});
                    return .runtime_error;
                }
                vm.push(value);
            },
            .op_define_global => {
                vm.global_values.items[frame.read_byte()] = vm.pop();
            },
            .op_set_global => {
                const index = frame.read_byte();
                if (vm.global_values.items[index] == .undefined_) {
                    vm.runtime_error("Undefined variable.", .{});
                    return .runtime_error;
                }
                vm.global_values.items[index] = vm.peek(0);
            },
            .op_get_upvalue => {
                const slot = frame.read_byte();
                vm.push(frame.closure.upvalues[slot].?.location.*);
            },
            .op_set_upvalue => {
                const slot = frame.read_byte();
                frame.closure.upvalues[slot].?.location.* = vm.peek(0);
            },
            .op_equal => {
                const b = vm.pop();
                const a = vm.pop();
                vm.push(Value{ .bool_ = a.eql(b) });
            },
            .op_add => {
                if (vm.peek(0).is_string() and vm.peek(1).is_string()) {
                    const b = vm.peek(0).obj.as(Obj.String);
                    const a = vm.peek(1).obj.as(Obj.String);
                    var result = try a.value.clone();
                    try result.append(b.value.as_slice());
                    const obj = try Obj.String.take(vm, &result);
                    _ = vm.pop();
                    _ = vm.pop();
                    vm.push(obj.obj.value());
                } else if (vm.peek(0) == .number and vm.peek(1) == .number) {
                    const b = vm.pop().number;
                    const a = vm.pop().number;
                    vm.push(Value{ .number = a + b });
                } else {
                    vm.runtime_error("Operands must be two numbers or two strings.", .{});
                    return .runtime_error;
                }
            },
            .op_subtract => vm.binary_op(f64, subtract) orelse return .runtime_error,
            .op_multiply => vm.binary_op(f64, multiply) orelse return .runtime_error,
            .op_divide => vm.binary_op(f64, divide) orelse return .runtime_error,
            .op_greater => vm.binary_op(bool, greater) orelse return .runtime_error,
            .op_less => vm.binary_op(bool, less) orelse return .runtime_error,
            .op_not => {
                const value = vm.pop();
                vm.push(Value{ .bool_ = is_falsey(value) });
            },
            .op_negate => {
                if (vm.peek(0) != .number) {
                    vm.runtime_error("Operand must be a number.", .{});
                    return .runtime_error;
                }
                vm.push(Value{ .number = -vm.pop().number });
            },
            .op_print => std.debug.print("{s}\n", .{vm.pop()}),
            .op_jump => {
                const offset = frame.read_u16();
                frame.ip += offset;
            },
            .op_jump_if_false => {
                const offset = frame.read_u16();
                if (is_falsey(vm.peek(0))) frame.ip += offset;
            },
            .op_loop => {
                const offset = frame.read_u16();
                frame.ip -= offset;
            },
            .op_call => {
                const arg_count = frame.read_byte();
                if (!vm.call_value(vm.peek(arg_count), arg_count)) {
                    return .runtime_error;
                }
                frame = &vm.frames[vm.frame_count - 1];
            },
            .op_closure => {
                const function = frame.read_constant().obj.as(Obj.Function);
                const closure = try Obj.Closure.init(vm, function);
                vm.push(closure.obj.value());
                for (closure.upvalues) |*upvalue| {
                    const is_local = frame.read_byte();
                    const index = frame.read_byte();
                    if (is_local == 1) {
                        upvalue.* = try vm.capture_upvalue(&frame.slots[index]);
                    } else {
                        upvalue.* = frame.closure.upvalues[index];
                    }
                }
            },
            .op_close_upvalue => {
                vm.close_upvalues(vm.stack_top - 1);
                _ = vm.pop();
            },
            .op_return => {
                const result = vm.pop();
                vm.close_upvalues(frame.slots);
                vm.frame_count -= 1;
                if (vm.frame_count == 0) {
                    _ = vm.pop();
                    return .ok;
                }
                vm.stack_top = frame.slots;
                vm.push(result);
                frame = &vm.frames[vm.frame_count - 1];
            },
        }
    }
}

fn reset_stack(vm: *Vm) void {
    vm.stack_top = &vm.stack;
    vm.frame_count = 0;
    vm.open_upvalues = null;
}

inline fn peek(vm: *Vm, distance: usize) Value {
    return (vm.stack_top - 1 - distance)[0];
}

fn call_value(vm: *Vm, callee: Value, arg_count: u8) bool {
    switch (callee) {
        .obj => |o| switch (o.kind) {
            .closure => return vm.call(o.as(Obj.Closure), arg_count),
            .native => {
                const native = o.as(Obj.Native);
                const arity = native.arity;
                if (arg_count != arity) {
                    vm.runtime_error("Expected {d} arguments but got {d}.", .{ arity, arg_count });
                    return false;
                }
                const result = native.function(arg_count, vm.stack_top - arg_count);
                vm.stack_top -= arg_count + 1;
                vm.push(result);
                return true;
            },
            else => {},
        },
        else => {},
    }
    vm.runtime_error("Can only call functions and classes.", .{});
    return false;
}

fn capture_upvalue(vm: *Vm, local: *Value) !*Obj.Upvalue {
    var prev_upvalue: ?*Obj.Upvalue = null;
    var upvalue = vm.open_upvalues;
    while (upvalue != null and @intFromPtr(upvalue.?.location) > @intFromPtr(local)) {
        prev_upvalue = upvalue;
        upvalue = upvalue.?.next;
    }
    if (upvalue != null and upvalue.?.location == local) {
        return upvalue.?;
    }
    const created_upvalue = try Obj.Upvalue.init(vm, .{ .location = local, .next = upvalue });
    if (prev_upvalue) |v| {
        v.next = created_upvalue;
    } else {
        vm.open_upvalues = created_upvalue;
    }
    return created_upvalue;
}

fn close_upvalues(vm: *Vm, last: [*]Value) void {
    while (vm.open_upvalues) |upvalue| {
        // same as while condition having: upvalue.location >= last
        if (@intFromPtr(last) > @intFromPtr(upvalue.location)) break;
        upvalue.closed = upvalue.location.*;
        upvalue.location = &upvalue.closed;
        vm.open_upvalues = upvalue.next;
    }
}

fn call(vm: *Vm, closure: *Obj.Closure, arg_count: u8) bool {
    const arity = closure.function.arity;
    if (arg_count != arity) {
        vm.runtime_error("Expected {d} arguments but got {d}.", .{ arity, arg_count });
        return false;
    }
    if (vm.frame_count == Vm.FRAMES_MAX) {
        vm.runtime_error("Stack overflow.", .{});
        return false;
    }
    var frame = &vm.frames[vm.frame_count];
    vm.frame_count += 1;
    frame.closure = closure;
    frame.ip = frame.code().ptr;
    frame.slots = vm.stack_top - arg_count - 1;
    return true;
}

fn is_falsey(value: Value) bool {
    return switch (value) {
        .nil => true,
        .bool_ => |b| !b,
        else => false,
    };
}

fn binary_op(vm: *Vm, comptime Ret: type, fun: *const fn (f64, f64) Ret) ?void {
    if (vm.peek(0) != .number or vm.peek(1) != .number) {
        vm.runtime_error("Operands must be numbers.", .{});
        return null;
    }
    const b = vm.pop().number;
    const a = vm.pop().number;
    vm.push(if (Ret == bool)
        Value{ .bool_ = fun(a, b) }
    else
        Value{ .number = fun(a, b) });
}

fn subtract(a: f64, b: f64) f64 {
    return a - b;
}

fn multiply(a: f64, b: f64) f64 {
    return a * b;
}

fn divide(a: f64, b: f64) f64 {
    return a / b;
}

fn greater(a: f64, b: f64) bool {
    return a > b;
}

fn less(a: f64, b: f64) bool {
    return a < b;
}

fn runtime_error(vm: *Vm, comptime fmt: []const u8, args: anytype) void {
    std.debug.print(fmt, args);
    std.debug.print("\n", .{});

    var i = vm.frame_count;
    while (i > 0) {
        i -= 1;
        const frame = &vm.frames[i];
        const function = frame.closure.function;
        const instruction = frame.offset() - 1;
        const line = function.chunk.get_line(instruction);
        std.debug.print("[line {d}] in ", .{line});
        if (function.name) |name| {
            std.debug.print("{s}()\n", .{name.value.as_slice()});
        } else {
            std.debug.print("script\n", .{});
        }
    }
    vm.reset_stack();
}

fn define_native(vm: *Vm, name: []const u8, arity: u8, function: Obj.NativeFn) !void {
    vm.push((try Obj.String.copy(vm, name)).obj.value());
    vm.push((try Obj.Native.init(vm, arity, function)).obj.value());
    const value = Value{ .number = @floatFromInt(vm.global_values.items.len) };
    _ = try vm.global_names.insert(vm.stack[0].obj.as(Obj.String), value);
    try vm.global_values.push(vm.stack[1]);
    _ = vm.pop();
    _ = vm.pop();
}

fn clock_native(arg_count: u8, args: [*]Value) Value {
    _ = arg_count;
    _ = args;
    const time = std.time;
    const timestamp: f64 = @floatFromInt(time.microTimestamp());
    const us_per_s: f64 = @floatFromInt(time.us_per_s);
    return Value{ .number = timestamp / us_per_s };
}