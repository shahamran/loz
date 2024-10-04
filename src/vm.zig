const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const compiler = @import("compiler.zig");
const print_value = @import("value.zig").print_value;
const get_line = @import("debug.zig").get_line;
const config = @import("config");

var vm: VM = undefined;

const VM = struct {
    const STACK_MAX = 256;

    chunk: *Chunk,
    ip: [*]u8,
    stack: [STACK_MAX]Value,
    stack_top: [*]Value,
};

pub fn interpret(source: []const u8) !InterpretResult {
    var chunk = Chunk.init();
    defer chunk.deinit();

    if (!try compiler.compile(source, &chunk)) {
        return .compile_error;
    }
    vm.chunk = &chunk;
    vm.ip = vm.chunk.code.items.ptr;
    return try run();
}

pub fn init_vm() void {
    reset_stack();
}

pub fn deinit_vm() void {}

fn run() !InterpretResult {
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
                .disassemble_instruction(vm.chunk, vm.ip - vm.chunk.code.items.ptr);
        }
        const instruction: OpCode = @enumFromInt(read_byte());
        switch (instruction) {
            .op_constant => {
                const constant = read_constant();
                push(constant);
            },
            .op_constant_long => {
                const constant = read_constant_long();
                push(constant);
            },
            .op_nil => push(Value{ .nil = {} }),
            .op_true => push(Value{ .bool_ = true }),
            .op_false => push(Value{ .bool_ = false }),
            .op_equal => {
                const b = pop();
                const a = pop();
                push(Value{ .bool_ = std.meta.eql(a, b) });
            },
            .op_add => binary_op(f64, add) orelse return .runtime_error,
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
            .op_return => {
                print_value(pop());
                std.debug.print("\n", .{});
                return InterpretResult.ok;
            },
        }
    }
}

fn reset_stack() void {
    vm.stack_top = &vm.stack;
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

fn add(a: f64, b: f64) f64 {
    return a + b;
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
    const instruction = vm.ip - vm.chunk.code.items.ptr - 1;
    const line = get_line(vm.chunk, instruction);
    std.debug.print("[line {d}] in script \n", .{line});
    reset_stack();
}

fn read_byte() u8 {
    const byte = vm.ip[0];
    vm.ip += 1;
    return byte;
}

fn read_constant() Value {
    return vm.chunk.constants.items[read_byte()];
}

fn read_constant_long() Value {
    var index = @as(usize, read_byte());
    index = index | (@as(usize, read_byte()) << 8);
    index = index | (@as(usize, read_byte()) << 16);
    return vm.chunk.constants.items[index];
}

pub const InterpretResult = enum {
    ok,
    compile_error,
    runtime_error,
};
