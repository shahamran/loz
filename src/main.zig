const std = @import("std");
const chunk = @import("chunk.zig");
const debug = @import("debug.zig");
const Chunk = chunk.Chunk;
const OpCode = chunk.OpCode;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var c = Chunk.init(allocator);
    defer c.deinit();
    const constant: u8 = @intCast(try c.add_constant(1.2));
    try c.write(@intFromEnum(OpCode.op_constant), 123);
    try c.write(constant, 123);
    try c.write(@intFromEnum(OpCode.op_return), 123);
    debug.disassemble_chunk(&c, "test chunk");
}
