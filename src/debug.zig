const std = @import("std");
const chunk = @import("chunk.zig");
const value = @import("value.zig");
const Chunk = chunk.Chunk;
const OpCode = chunk.OpCode;

pub fn disassemble_chunk(c: *const Chunk, name: []const u8) void {
    std.debug.print("== {s} ==\n", .{name});
    var offset: usize = 0;
    while (offset < c.size) {
        offset = disassemble_instruction(c, offset);
    }
}

pub fn disassemble_instruction(c: *const Chunk, offset: usize) usize {
    std.debug.print("{d:0>4} ", .{offset});
    if (offset > 0 and c.lines[offset] == c.lines[offset - 1]) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d: >4} ", .{c.lines[offset]});
    }
    const code: OpCode = @enumFromInt(c.code[offset]);
    switch (code) {
        OpCode.op_constant => {
            return constant_instruction("OP_CONSTANT", c, offset);
        },
        OpCode.op_return => {
            return simple_instruction("OP_RETURN", offset);
        },
        // else => {
        //     std.debug.print("Unknown opcode {}\n", .{chunk.code[offset]});
        //     return offset + 1;
        // }
    }
}

fn simple_instruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}

fn constant_instruction(name: []const u8, c: *const Chunk, offset: usize) usize {
    const constant = c.code[offset + 1];
    std.debug.print("{s: <16} {d: >4} '", .{ name, constant });
    value.print_value(c.constants.values.items[constant]);
    std.debug.print("'\n", .{});
    return offset + 2;
}
