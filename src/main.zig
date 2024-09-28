const std = @import("std");
const debug = @import("debug.zig");
const vm = @import("vm.zig");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    vm.init_vm();
    var chunk = Chunk.init(allocator);
    defer chunk.deinit();
    try chunk.write_constant(1.2, 123);
    try chunk.write_constant(3.4, 123);
    try chunk.write(@intFromEnum(OpCode.op_add), 123);
    try chunk.write_constant(5.6, 123);
    try chunk.write(@intFromEnum(OpCode.op_divide), 123);
    try chunk.write(@intFromEnum(OpCode.op_negate), 123);
    try chunk.write(@intFromEnum(OpCode.op_return), 123);
    debug.disassemble_chunk(&chunk, "test chunk");
    _ = try vm.interpret(&chunk);
    vm.deinit_vm();
}
