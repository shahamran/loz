const std = @import("std");
const config = @import("config");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const List = @import("list.zig").List;
const Obj = @import("object.zig").Obj;
const ObjFunction = @import("object.zig").ObjFunction;
const ObjString = @import("object.zig").ObjString;
const Table = @import("table.zig").Table;
const compiler = @import("compiler.zig");
const print_value = @import("value.zig").print_value;
const get_line = @import("debug.zig").get_line;
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
};

const CallFrame = struct {
    const Self = @This();

    function: *ObjFunction,
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
        return self.function.chunk.constants.items[self.read_byte()];
    }
};

pub fn interpret(source: []const u8) !InterpretResult {
    var function = try compiler.compile(source) orelse return .compile_error;
    push(Value{ .obj = function.upcast() });
    var frame = &vm.frames[vm.frame_count];
    vm.frame_count += 1;
    frame.function = function;
    frame.ip = function.chunk.code.items.ptr;
    frame.slots = vm.stack[0..];

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
                .disassemble_instruction(&frame.function.chunk, frame.ip - frame.function.chunk.code.items.ptr);
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
            .op_return => {
                return .ok;
            },
        }
    }
}

fn reset_stack() void {
    vm.stack_top = &vm.stack;
    vm.frame_count = 0;
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
    const frame = &vm.frames[vm.frame_count - 1];
    const instruction = frame.ip - frame.function.chunk.code.items.ptr - 1;
    const line = get_line(&frame.function.chunk, instruction);
    std.debug.print("[line {d}] in script \n", .{line});
    reset_stack();
}
