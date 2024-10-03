const std = @import("std");
const Allocator = @import("std").mem.Allocator;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const compiler = @import("compiler.zig");
const print_value = @import("value.zig").print_value;

const DEBUG_TRACE_EXECUTION = false;

var vm: VM = undefined;

const VM = struct {
    const STACK_MAX = 256;

    chunk: *Chunk,
    ip: [*]u8,
    stack: [STACK_MAX]Value,
    stack_top: [*]Value,
};

pub fn interpret(allocator: Allocator, source: []const u8) Allocator.Error!InterpretResult {
    var chunk = Chunk.init(allocator);
    defer chunk.deinit();

    if (!try compiler.compile(allocator, source, &chunk)) {
        return .compile_error;
    }
    vm.chunk = &chunk;
    vm.ip = vm.chunk.code.ptr;
    return try run();
}

pub fn init_vm() void {
    reset_stack();
}

pub fn deinit_vm() void {}

fn run() Allocator.Error!InterpretResult {
    while (true) {
        if (comptime DEBUG_TRACE_EXECUTION) {
            std.debug.print("          ", .{});
            var slot: [*]Value = &vm.stack;
            while (slot != vm.stack_top) : (slot += 1) {
                std.debug.print("[ ", .{});
                print_value(slot[0]);
                std.debug.print(" ]", .{});
            }
            std.debug.print("\n", .{});
            _ = @import("debug.zig").disassemble_instruction(vm.chunk, vm.ip - vm.chunk.code.ptr);
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
            .op_add, .op_subtract, .op_multiply, .op_divide => {
                const b = pop();
                const a = pop();
                push(switch (instruction) {
                    .op_add => a + b,
                    .op_subtract => a - b,
                    .op_multiply => a * b,
                    .op_divide => a / b,
                    else => unreachable,
                });
            },
            .op_negate => push(-pop()),
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

fn read_byte() u8 {
    const byte = vm.ip[0];
    vm.ip += 1;
    return byte;
}

fn read_constant() Value {
    return vm.chunk.constants.values.items[read_byte()];
}

fn read_constant_long() Value {
    var index = @as(usize, read_byte());
    index = index | (@as(usize, read_byte()) << 8);
    index = index | (@as(usize, read_byte()) << 16);
    return vm.chunk.constants.values.items[index];
}

pub const InterpretResult = enum {
    ok,
    compile_error,
    runtime_error,
};
