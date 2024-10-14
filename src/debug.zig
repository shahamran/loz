const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const print_value = @import("value.zig").print_value;

pub fn disassemble_chunk(chunk: *const Chunk, name: []const u8) void {
    std.debug.print("== {s} ==\n", .{name});
    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        offset = disassemble_instruction(chunk, offset);
    }
}

pub fn disassemble_instruction(chunk: *const Chunk, offset: usize) usize {
    std.debug.print("{d:0>4} ", .{offset});
    const line = get_line(chunk, offset);
    if (offset > 0 and line == get_line(chunk, offset - 1)) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d: >4} ", .{line});
    }
    const code: OpCode = @enumFromInt(chunk.code.items[offset]);
    switch (code) {
        .op_constant => return constant_instruction("OP_CONSTANT", chunk, offset),
        .op_nil => return simple_instruction("OP_NIL", offset),
        .op_true => return simple_instruction("OP_TRUE", offset),
        .op_false => return simple_instruction("OP_FALSE", offset),
        .op_pop => return simple_instruction("OP_POP", offset),
        .op_get_local => return byte_instruction("OP_GET_LOCAL", chunk, offset),
        .op_set_local => return byte_instruction("OP_SET_LOCAL", chunk, offset),
        .op_get_global => return byte_instruction("OP_GET_GLOBAL", chunk, offset),
        .op_define_global => return byte_instruction("OP_DEFINE_GLOBAL", chunk, offset),
        .op_set_global => return byte_instruction("OP_SET_GLOBAL", chunk, offset),
        .op_equal => return simple_instruction("OP_EQUAL", offset),
        .op_greater => return simple_instruction("OP_GREATER", offset),
        .op_less => return simple_instruction("OP_LESS", offset),
        .op_add => return simple_instruction("OP_ADD", offset),
        .op_subtract => return simple_instruction("OP_SUBTRACT", offset),
        .op_multiply => return simple_instruction("OP_MULTIPLY", offset),
        .op_divide => return simple_instruction("OP_DIVIDE", offset),
        .op_not => return simple_instruction("OP_NOT", offset),
        .op_negate => return simple_instruction("OP_NEGATE", offset),
        .op_print => return simple_instruction("OP_PRINT", offset),
        .op_jump => return jump_instruction("OP_JUMP", .forward, chunk, offset),
        .op_jump_if_false => return jump_instruction("OP_JUMP_IF_FALSE", .forward, chunk, offset),
        .op_loop => return jump_instruction("OP_LOOP", .backward, chunk, offset),
        .op_call => return byte_instruction("OP_CALL", chunk, offset),
        .op_closure => {
            const constant = chunk.code.items[offset + 1];
            std.debug.print("{s: <16} {d: >4} ", .{ "OP_CLOSURE", constant });
            print_value(chunk.constants.items[constant]);
            std.debug.print("\n", .{});
            return offset + 2;
        },
        .op_return => return simple_instruction("OP_RETURN", offset),
    }
    std.debug.print("Unknown opcode {}\n", .{chunk.code[offset]});
    return offset + 1;
}

fn simple_instruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}

fn constant_instruction(name: []const u8, chunk: *const Chunk, offset: usize) usize {
    const constant = chunk.code.items[offset + 1];
    std.debug.print("{s: <16} {d: >4} '", .{ name, constant });
    print_value(chunk.constants.items[constant]);
    std.debug.print("'\n", .{});
    return offset + 2;
}

fn byte_instruction(name: []const u8, chunk: *const Chunk, offset: usize) usize {
    const slot = chunk.code.items[offset + 1];
    std.debug.print("{s: <16} {d: >4}\n", .{ name, slot });
    return offset + 2;
}

const Direction = enum {
    forward,
    backward,
};

fn jump_instruction(name: []const u8, direction: Direction, chunk: *const Chunk, offset: usize) usize {
    var jump = (@as(usize, chunk.code.items[offset + 1]) << 8) | @as(usize, chunk.code.items[offset + 2]);
    jump = if (direction == .forward) offset + 3 + jump else offset + 3 - jump;
    std.debug.print("{s: <16} {d: >4} -> {d}\n", .{ name, offset, jump });
    return offset + 3;
}

pub fn get_line(chunk: *const Chunk, offset: usize) usize {
    // TODO: binary search
    for (chunk.lines.items, 0..) |line, i| {
        if (line.start > offset) {
            return chunk.lines.items[i - 1].line;
        }
    }
    return chunk.lines.last().line;
}
