const std = @import("std");
const Chunk = @import("Chunk.zig");
const OpCode = Chunk.OpCode;
const Obj = @import("Obj.zig");

pub fn disassembleChunk(chunk: *const Chunk, name: []const u8) void {
    std.debug.print("== {s} ==\n", .{name});
    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        offset = disassembleInstruction(chunk, offset);
    }
}

pub fn disassembleInstruction(chunk: *const Chunk, offset: usize) usize {
    std.debug.print("{d:0>4} ", .{offset});
    const line = chunk.get_line(offset);
    if (offset > 0 and line == chunk.get_line(offset - 1)) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d: >4} ", .{line});
    }
    const code: OpCode = @enumFromInt(chunk.code.items[offset]);
    switch (code) {
        .op_constant => return constantInstruction("OP_CONSTANT", chunk, offset),
        .op_nil => return simpleInstruction("OP_NIL", offset),
        .op_true => return simpleInstruction("OP_TRUE", offset),
        .op_false => return simpleInstruction("OP_FALSE", offset),
        .op_pop => return simpleInstruction("OP_POP", offset),
        .op_get_local => return byteInstruction("OP_GET_LOCAL", chunk, offset),
        .op_set_local => return byteInstruction("OP_SET_LOCAL", chunk, offset),
        .op_get_global => return byteInstruction("OP_GET_GLOBAL", chunk, offset),
        .op_define_global => return byteInstruction("OP_DEFINE_GLOBAL", chunk, offset),
        .op_set_global => return byteInstruction("OP_SET_GLOBAL", chunk, offset),
        .op_get_upvalue => return byteInstruction("OP_GET_UPVALUE", chunk, offset),
        .op_set_upvalue => return byteInstruction("OP_SET_UPVALUE", chunk, offset),
        .op_get_property => return byteInstruction("OP_GET_PROPERTY", chunk, offset),
        .op_set_property => return byteInstruction("OP_SET_PROPERTY", chunk, offset),
        .op_get_super => return byteInstruction("OP_GET_SUPER", chunk, offset),
        .op_equal => return simpleInstruction("OP_EQUAL", offset),
        .op_greater => return simpleInstruction("OP_GREATER", offset),
        .op_less => return simpleInstruction("OP_LESS", offset),
        .op_add => return simpleInstruction("OP_ADD", offset),
        .op_subtract => return simpleInstruction("OP_SUBTRACT", offset),
        .op_multiply => return simpleInstruction("OP_MULTIPLY", offset),
        .op_divide => return simpleInstruction("OP_DIVIDE", offset),
        .op_not => return simpleInstruction("OP_NOT", offset),
        .op_negate => return simpleInstruction("OP_NEGATE", offset),
        .op_print => return simpleInstruction("OP_PRINT", offset),
        .op_jump => return jumpInstruction("OP_JUMP", .forward, chunk, offset),
        .op_jump_if_false => return jumpInstruction("OP_JUMP_IF_FALSE", .forward, chunk, offset),
        .op_loop => return jumpInstruction("OP_LOOP", .backward, chunk, offset),
        .op_call => return byteInstruction("OP_CALL", chunk, offset),
        .op_invoke => return invokeInstruction("OP_INVOKE", chunk, offset),
        .op_super_invoke => return invokeInstruction("OP_SUPER_INVOKE", chunk, offset),
        .op_closure => {
            const constant = chunk.code.items[offset + 1];
            const val = chunk.constants.items[constant];
            var i = offset + 2;
            std.debug.print("{s: <16} {d: >4} {s}\n", .{ "OP_CLOSURE", constant, val });
            const function = val.obj.as(Obj.Function);
            for (0..function.upvalue_count) |_| {
                const is_local = chunk.code.items[i] == 1;
                const index = chunk.code.items[i + 1];
                const upvalue_type = if (is_local) "local" else "upvalue";
                std.debug.print(
                    "{d:0>4}      |                     {s} {d}\n",
                    .{ i, upvalue_type, index },
                );
                i += 2;
            }
            return i;
        },
        .op_close_upvalue => return simpleInstruction("OP_CLOSE_UPVALUE", offset),
        .op_return => return simpleInstruction("OP_RETURN", offset),
        .op_class => return byteInstruction("OP_CLASS", chunk, offset),
        .op_inherit => return simpleInstruction("OP_INHERIT", offset),
        .op_method => return byteInstruction("OP_METHOD", chunk, offset),
    }
    std.debug.print("Unknown opcode {}\n", .{chunk.code[offset]});
    return offset + 1;
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}

fn constantInstruction(name: []const u8, chunk: *const Chunk, offset: usize) usize {
    const constant = chunk.code.items[offset + 1];
    std.debug.print(
        "{s: <16} {d: >4} '{s}'\n",
        .{ name, constant, chunk.constants.items[constant] },
    );
    return offset + 2;
}

fn byteInstruction(name: []const u8, chunk: *const Chunk, offset: usize) usize {
    const slot = chunk.code.items[offset + 1];
    std.debug.print("{s: <16} {d: >4}\n", .{ name, slot });
    return offset + 2;
}

const Direction = enum {
    forward,
    backward,
};

fn jumpInstruction(name: []const u8, direction: Direction, chunk: *const Chunk, offset: usize) usize {
    var jump = (@as(usize, chunk.code.items[offset + 1]) << 8) | @as(usize, chunk.code.items[offset + 2]);
    jump = if (direction == .forward) offset + 3 + jump else offset + 3 - jump;
    std.debug.print("{s: <16} {d: >4} -> {d}\n", .{ name, offset, jump });
    return offset + 3;
}

fn invokeInstruction(name: []const u8, chunk: *const Chunk, offset: usize) usize {
    const constant = chunk.code.items[offset + 1];
    const arg_count = chunk.code.items[offset + 2];
    std.debug.print("{s: <16} ({d} args) {d: >4}\n", .{ name, arg_count, constant });
    return offset + 3;
}
