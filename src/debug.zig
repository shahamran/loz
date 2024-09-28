const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const print_value = @import("value.zig").print_value;

pub fn disassemble_chunk(chunk: *const Chunk, name: []const u8) void {
    std.debug.print("== {s} ==\n", .{name});
    var offset: usize = 0;
    while (offset < chunk.size) {
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
    const code: OpCode = @enumFromInt(chunk.code[offset]);
    switch (code) {
        OpCode.op_constant => {
            return constant_instruction("OP_CONSTANT", chunk, offset);
        },
        OpCode.op_constant_long => {
            return constant_long_instruction("OP_CONSTANT_LONG", chunk, offset);
        },
        OpCode.op_return => {
            return simple_instruction("OP_RETURN", offset);
        },
    }
    std.debug.print("Unknown opcode {}\n", .{chunk.code[offset]});
    return offset + 1;
}

fn simple_instruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}

fn constant_instruction(name: []const u8, chunk: *const Chunk, offset: usize) usize {
    const constant = chunk.code[offset + 1];
    std.debug.print("{s: <16} {d: >4} '", .{ name, constant });
    print_value(chunk.constants.values.items[constant]);
    std.debug.print("'\n", .{});
    return offset + 2;
}

fn constant_long_instruction(name: []const u8, chunk: *const Chunk, offset: usize) usize {
    const bytes = .{
        @as(usize, chunk.code[offset + 1]),
        @as(usize, chunk.code[offset + 2]),
        @as(usize, chunk.code[offset + 3]),
    };
    const constant = bytes[0] | (bytes[1] << 8) | (bytes[2] << 16);
    std.debug.print("{s: <16} {d: >4} '", .{ name, constant });
    print_value(chunk.constants.values.items[constant]);
    std.debug.print("'\n", .{});
    return offset + 4;
}

fn get_line(chunk: *const Chunk, offset: usize) usize {
    // TODO: binary search
    for (chunk.lines.items, 0..) |line, i| {
        if (line.start > offset) {
            return chunk.lines.items[i - 1].line;
        }
    }
    return chunk.lines.getLast().line;
}
