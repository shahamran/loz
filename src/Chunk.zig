const std = @import("std");
const Allocator = std.mem.Allocator;
const List = @import("list.zig").List;
const Value = @import("value.zig").Value;
const Vm = @import("Vm.zig");

const Chunk = @This();

code: List(u8), // bytecode
lines: List(LineInfo),
constants: List(Value),

pub fn init(allocator: Allocator) Chunk {
    return .{
        .code = List(u8).init(allocator),
        .lines = List(LineInfo).init(allocator),
        .constants = List(Value).init(allocator),
    };
}

pub fn deinit(self: *Chunk) void {
    self.code.deinit();
    self.lines.deinit();
    self.constants.deinit();
    self.* = init(self.code.allocator);
}

pub fn write(self: *Chunk, byte: u8, line: usize) Allocator.Error!void {
    try self.code.push(byte);
    if (self.lines.items.len == 0 or self.lines.last().line != line) {
        try self.lines.push(.{ .line = line, .start = self.code.items.len - 1 });
    }
}

pub fn add_constant(self: *Chunk, vm: *Vm, val: Value) Allocator.Error!usize {
    vm.push(val);
    try self.constants.push(val);
    _ = vm.pop();
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
    op_closure,
    op_close_upvalue,
    op_return,
};

pub const LineInfo = struct {
    line: usize,
    start: usize,
};

test "basic" {
    const expectEqual = std.testing.expectEqual;
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();
    try chunk.write(@intFromEnum(OpCode.op_return), 123);
    const actual: OpCode = @enumFromInt(chunk.code.items[0]);
    try expectEqual(.op_return, actual);
    try expectEqual(123, chunk.get_line(0));
    // var constant = try chunk.add_constant(.{ .number = 42.0 });
    // try expectEqual(0, constant);
    // constant = try chunk.add_constant(.{ .bool_ = true });
    // try expectEqual(1, constant);
}
