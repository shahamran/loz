const std = @import("std");
const Allocator = std.mem.Allocator;
const List = @import("list.zig").List;
const Value = @import("value.zig").Value;
const Vm = @import("Vm.zig");

const Chunk = @This();

code: List(u8), // bytecode
lines: List(LineInfo),
constants: List(Value),

pub fn init() Chunk {
    return .{
        .code = List(u8).init(),
        .lines = List(LineInfo).init(),
        .constants = List(Value).init(),
    };
}

pub fn deinit(self: *Chunk, allocator: Allocator) void {
    self.code.deinit(allocator);
    self.lines.deinit(allocator);
    self.constants.deinit(allocator);
    self.* = init();
}

pub fn write(self: *Chunk, allocator: Allocator, byte: u8, line: usize) Allocator.Error!void {
    try self.code.push(allocator, byte);
    if (self.lines.items.len == 0 or self.lines.last().line != line) {
        try self.lines.push(
            allocator,
            .{ .line = line, .start = self.code.items.len - 1 },
        );
    }
}

pub inline fn add_constant(self: *Chunk, allocator: Allocator, val: Value) Allocator.Error!usize {
    try self.constants.push(allocator, val);
    return self.constants.items.len - 1;
}

pub fn get_line(self: *const Chunk, offset: usize) usize {
    // TODO: binary search
    std.debug.assert(self.lines.items.len > 0);
    for (self.lines.items, 0..) |line, i| {
        if (line.start > offset) {
            return self.lines.items[i - 1].line;
        }
    }
    return self.lines.last().line;
}

pub const OpCode = enum(u8) {
    op_constant,
    op_nil,
    op_true,
    op_false,
    op_pop,
    op_get_local,
    op_set_local,
    op_get_global,
    op_define_global,
    op_set_global,
    op_get_upvalue,
    op_set_upvalue,
    op_get_property,
    op_set_property,
    op_equal,
    op_greater,
    op_less,
    op_add,
    op_subtract,
    op_multiply,
    op_divide,
    op_not,
    op_negate,
    op_print,
    op_jump,
    op_jump_if_false,
    op_loop,
    op_call,
    op_invoke,
    op_closure,
    op_close_upvalue,
    op_return,
    op_class,
    op_inherit,
    op_method,
};

pub const LineInfo = struct {
    line: usize,
    start: usize,
};

test "basic" {
    const expectEqual = std.testing.expectEqual;
    const allocator = std.testing.allocator;
    var chunk = Chunk.init();
    defer chunk.deinit(allocator);
    try chunk.write(allocator, @intFromEnum(OpCode.op_return), 123);
    const actual: OpCode = @enumFromInt(chunk.code.items[0]);
    try expectEqual(.op_return, actual);
    try expectEqual(123, chunk.get_line(0));
    var constant = try chunk.add_constant(allocator, .{ .number = 42.0 });
    try expectEqual(0, constant);
    constant = try chunk.add_constant(allocator, .{ .bool_ = true });
    try expectEqual(1, constant);
}
