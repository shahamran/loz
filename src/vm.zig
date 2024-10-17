const std = @import("std");
const config = @import("config");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const List = @import("list.zig").List;
const Obj = @import("object.zig").Obj;
const ObjClosure = @import("object.zig").ObjClosure;
const ObjFunction = @import("object.zig").ObjFunction;
const ObjNative = @import("object.zig").ObjNative;
const ObjString = @import("object.zig").ObjString;
const ObjUpvalue = @import("object.zig").ObjUpvalue;
const Table = @import("table.zig").Table;
const compiler = @import("compiler.zig");
const print_value = @import("value.zig").print_value;
const memory = @import("memory.zig");

const UINT8_COUNT = std.math.maxInt(u8) + 1;

pub var vm: VM = undefined;

const VM = struct {
    const FRAMES_MAX = 64;
    const STACK_MAX = FRAMES_MAX * UINT8_COUNT;

    frames: [FRAMES_MAX]CallFrame,
    frame_count: u8,

    stack: [STACK_MAX]Value,
    stack_top: [*]Value,
    objects: ?*Obj, // linked list of all allocated objects
    global_names: Table, // maps global variable names to their indices in the globals array
    global_values: List(Value),
    strings: Table, // interned strings
    open_upvalues: ?*ObjUpvalue, // linked list of open upvalues
};

const CallFrame = struct {
    const Self = @This();

    closure: *ObjClosure,
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
        return self.ip - self.closure.function.chunk.code.items.ptr;
    }
};

pub fn interpret(source: []const u8) !InterpretResult {
    var function = try compiler.compile(source) orelse return .compile_error;
    push(.{ .obj = function.upcast() });
    const closure = try ObjClosure.init(function);
    _ = pop();
    push(.{ .obj = closure.upcast() });
    _ = call(closure, 0);

    return try run();
}

pub const InterpretResult = enum {
    ok,
    compile_error,
    runtime_error,
};

pub fn init_vm() void {
    reset_stack();
    vm.objects = null;
    vm.global_names = Table.init();
    vm.global_values = List(Value).init();
    vm.strings = Table.init();
    define_native("clock", 0, clock_native) catch unreachable;
}

pub fn deinit_vm() void {
    memory.free_objects();
    vm.global_names.deinit();
    vm.global_values.deinit();
    vm.strings.deinit();
}

fn run() !InterpretResult {
    var frame = &vm.frames[vm.frame_count - 1];
    while (true) {
        if (comptime config.trace_execution) {
            std.debug.print("          ", .{});
            var slot: [*]Value = &vm.stack;
            while (slot != vm.stack_top) : (slot += 1) {
                std.debug.print("[ ", .{});
                print_value(slot[0]);
                std.debug.print(" ]", .{});
            }
            std.debug.print("\n", .{});
            _ = @import("debug.zig")
                .disassemble_instruction(&frame.closure.function.chunk, frame.offset());
        }
        const instruction: OpCode = @enumFromInt(frame.read_byte());
        switch (instruction) {
            .op_constant => {
                const constant = frame.read_constant();
                push(constant);
            },
            .op_nil => push(Value{ .nil = {} }),
            .op_true => push(Value{ .bool_ = true }),
            .op_false => push(Value{ .bool_ = false }),
            .op_pop => _ = pop(),
            .op_get_local => {
                const slot = frame.read_byte();
                push(frame.slots[slot]);
            },
            .op_set_local => {
                const slot = frame.read_byte();
                frame.slots[slot] = peek(0);
            },
            .op_get_global => {
                const value = vm.global_values.items[frame.read_byte()];
                if (value == .undefined_) {
                    runtime_error("Undefined variable.", .{});
                    return .runtime_error;
                }
                push(value);
            },
            .op_define_global => {
                vm.global_values.items[frame.read_byte()] = pop();
            },
            .op_set_global => {
                const index = frame.read_byte();
                if (vm.global_values.items[index] == .undefined_) {
                    runtime_error("Undefined variable.", .{});
                    return .runtime_error;
                }
                vm.global_values.items[index] = peek(0);
            },
            .op_get_upvalue => {
                const slot = frame.read_byte();
                push(frame.closure.upvalues[slot].?.location.*);
            },
            .op_set_upvalue => {
                const slot = frame.read_byte();
                frame.closure.upvalues[slot].?.location.* = peek(0);
            },
            .op_equal => {
                const b = pop();
                const a = pop();
                push(Value{ .bool_ = a.eql(b) });
            },
            .op_add => {
                if (peek(0).is_string() and peek(1).is_string()) {
                    const b = pop().obj.downcast_string();
                    const a = pop().obj.downcast_string();
                    var result = try a.value.clone();
                    try result.append(b.value.as_slice());
                    const obj = try ObjString.take(&result);
                    push(Value{ .obj = obj.upcast() });
                } else if (peek(0) == .number and peek(1) == .number) {
                    const b = pop().number;
                    const a = pop().number;
                    push(Value{ .number = a + b });
                } else {
                    runtime_error("Operands must be two numbers or two strings.", .{});
                    return .runtime_error;
                }
            },
            .op_subtract => binary_op(f64, subtract) orelse return .runtime_error,
            .op_multiply => binary_op(f64, multiply) orelse return .runtime_error,
            .op_divide => binary_op(f64, divide) orelse return .runtime_error,
            .op_greater => binary_op(bool, greater) orelse return .runtime_error,
            .op_less => binary_op(bool, less) orelse return .runtime_error,
            .op_not => {
                const value = pop();
                push(Value{ .bool_ = is_falsey(value) });
            },
            .op_negate => {
                if (peek(0) != .number) {
                    runtime_error("Operand must be a number.", .{});
                    return .runtime_error;
                }
                push(Value{ .number = -pop().number });
            },
            .op_print => {
                print_value(pop());
                std.debug.print("\n", .{});
            },
            .op_jump => {
                const offset = frame.read_u16();
                frame.ip += offset;
            },
            .op_jump_if_false => {
                const offset = frame.read_u16();
                if (is_falsey(peek(0))) frame.ip += offset;
            },
            .op_loop => {
                const offset = frame.read_u16();
                frame.ip -= offset;
            },
            .op_call => {
                const arg_count = frame.read_byte();
                if (!call_value(peek(arg_count), arg_count)) {
                    return .runtime_error;
                }
                frame = &vm.frames[vm.frame_count - 1];
            },
            .op_closure => {
                const function = frame.read_constant().obj.downcast_function();
                const closure = try ObjClosure.init(function);
                push(.{ .obj = closure.upcast() });
                for (closure.upvalues) |*upvalue| {
                    const is_local = frame.read_byte();
                    const index = frame.read_byte();
                    if (is_local == 1) {
                        upvalue.* = try capture_upvalue(&frame.slots[index]);
                    } else {
                        upvalue.* = frame.closure.upvalues[index];
                    }
                }
            },
            .op_close_upvalue => {
                close_upvalues(vm.stack_top - 1);
                _ = pop();
            },
            .op_return => {
                const result = pop();
                close_upvalues(frame.slots);
                vm.frame_count -= 1;
                if (vm.frame_count == 0) {
                    _ = pop();
                    return .ok;
                }
                vm.stack_top = frame.slots;
                push(result);
                frame = &vm.frames[vm.frame_count - 1];
            },
        }
    }
}

fn reset_stack() void {
    vm.stack_top = &vm.stack;
    vm.frame_count = 0;
    vm.open_upvalues = null;
}

fn push(value: Value) void {
    vm.stack_top[0] = value;
    vm.stack_top += 1;
}

fn pop() Value {
    vm.stack_top -= 1;
    return vm.stack_top[0];
}

fn peek(distance: usize) Value {
    return (vm.stack_top - 1 - distance)[0];
}

fn call_value(callee: Value, arg_count: u8) bool {
    switch (callee) {
        .obj => |o| switch (o.kind) {
            .closure => return call(o.downcast_closure(), arg_count),
            .native => {
                const native = o.downcast_native();
                if (arg_count != native.arity) {
                    runtime_error("Expected {d} arguments but got {d}.", .{ native.arity, arg_count });
                    return false;
                }
                const result = native.function(arg_count, vm.stack_top - arg_count);
                vm.stack_top -= arg_count + 1;
                push(result);
                return true;
            },
            else => {},
        },
        else => {},
    }
    runtime_error("Can only call functions and classes.", .{});
    return false;
}

fn capture_upvalue(local: *Value) !*ObjUpvalue {
    var prev_upvalue: ?*ObjUpvalue = null;
    var upvalue = vm.open_upvalues;
    while (upvalue != null and @intFromPtr(upvalue.?.location) > @intFromPtr(local)) {
        prev_upvalue = upvalue;
        upvalue = upvalue.?.next;
    }
    if (upvalue != null and upvalue.?.location == local) {
        return upvalue.?;
    }
    const created_upvalue = try ObjUpvalue.init(local);
    created_upvalue.next = upvalue;
    if (prev_upvalue) |v| {
        v.next = created_upvalue;
    } else {
        vm.open_upvalues = created_upvalue;
    }
    return created_upvalue;
}

fn close_upvalues(last: [*]Value) void {
    while (vm.open_upvalues) |upvalue| {
        // same as while condition having: upvalue.location >= last
        if (@intFromPtr(last) > @intFromPtr(upvalue.location)) break;
        upvalue.closed = upvalue.location.*;
        upvalue.location = &upvalue.closed;
        vm.open_upvalues = upvalue.next;
    }
}

fn call(closure: *ObjClosure, arg_count: u8) bool {
    if (arg_count != closure.function.arity) {
        runtime_error("Expected {d} arguments but got {d}.", .{ closure.function.arity, arg_count });
        return false;
    }
    if (vm.frame_count == VM.FRAMES_MAX) {
        runtime_error("Stack overflow.", .{});
        return false;
    }
    var frame = &vm.frames[vm.frame_count];
    vm.frame_count += 1;
    frame.closure = closure;
    frame.ip = closure.function.chunk.code.items.ptr;
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

fn binary_op(comptime Ret: type, fun: *const fn (f64, f64) Ret) ?void {
    if (peek(0) != .number or peek(1) != .number) {
        runtime_error("Operands must be numbers.", .{});
        return null;
    }
    const b = pop().number;
    const a = pop().number;
    push(if (Ret == bool)
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

fn runtime_error(comptime fmt: []const u8, args: anytype) void {
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
    reset_stack();
}

fn define_native(name: []const u8, arity: u8, function: ObjNative.NativeFn) !void {
    push(.{ .obj = (try ObjString.copy(name)).upcast() });
    push(.{ .obj = (try ObjNative.init(arity, function)).upcast() });
    const value = Value{ .number = @floatFromInt(vm.global_values.items.len) };
    _ = try vm.global_names.insert(vm.stack[0].obj.downcast_string(), value);
    try vm.global_values.push(vm.stack[1]);
    _ = pop();
    _ = pop();
}

fn clock_native(arg_count: u8, args: [*]Value) Value {
    _ = arg_count;
    _ = args;
    const time = std.time;
    const timestamp: f64 = @floatFromInt(time.microTimestamp());
    const us_per_s: f64 = @floatFromInt(time.us_per_s);
    return Value{ .number = timestamp / us_per_s };
}
