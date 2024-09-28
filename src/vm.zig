const std = @import("std");
const Allocator = @import("std").mem.Allocator;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const print_value = @import("value.zig").print_value;

const DEBUG_TRACE_EXECUTION = false;

var vm: VM = undefined;

const VM = struct {
    chunk: *Chunk,
    ip: [*]u8,
};

pub fn interpret(chunk: *Chunk) Allocator.Error!InterpretResult {
    vm = VM{ .chunk = chunk, .ip = chunk.code.ptr };
    return run();
}

pub fn init_vm() void {}

pub fn deinit_vm() void {}

fn run() Allocator.Error!InterpretResult {
    while (true) {
        if (comptime DEBUG_TRACE_EXECUTION) {
            _ = @import("debug.zig").disassemble_instruction(vm.chunk, vm.ip - vm.chunk.code.ptr);
        }
        const instruction: OpCode = @enumFromInt(read_byte());
        switch (instruction) {
            OpCode.op_constant => {
                const constant = read_constant();
                print_value(constant);
                std.debug.print("\n", .{});
            },
            OpCode.op_constant_long => {
                const constant = read_constant_long();
                print_value(constant);
                std.debug.print("\n", .{});
            },
            OpCode.op_return => {
                return InterpretResult.ok;
            },
        }
    }
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
