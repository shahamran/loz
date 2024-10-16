const std = @import("std");
const Allocator = std.mem.Allocator;
const memory = @import("memory.zig");
const List = @import("list.zig").List;
const Value = @import("value.zig").Value;

pub const Chunk = struct {
    const Self = @This();

    code: List(u8), // bytecode
    lines: List(LineInfo),
    constants: List(Value),

    pub fn init() Self {
        return .{
            .code = List(u8).init(),
            .lines = List(LineInfo).init(),
            .constants = List(Value).init(),
        };
    }

    pub fn deinit(self: *Self) void {
        self.code.deinit();
        self.lines.deinit();
        self.constants.deinit();
        self.* = init();
    }

    pub fn write(self: *Self, byte: u8, line: usize) Allocator.Error!void {
        try self.code.push(byte);
        if (self.lines.items.len == 0 or self.lines.last().line != line) {
            try self.lines.push(.{ .line = line, .start = self.code.items.len - 1 });
        }
    }

    pub fn add_constant(self: *Self, val: Value) Allocator.Error!usize {
        try self.constants.push(val);
        return self.constants.items.len - 1;
    }

    pub fn get_line(self: *const Self, offset: usize) usize {
        // TODO: binary search
        for (self.lines.items, 0..) |line, i| {
            if (line.start > offset) {
                return self.lines.items[i - 1].line;
            }
        }
        return self.lines.last().line;
    }
};

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
    op_return,
};

pub const LineInfo = struct {
    line: usize,
    start: usize,
};

test "write" {
    const expect_eq = std.testing.expectEqual;
    var chunk = Chunk.init();
    defer chunk.deinit();
    try chunk.write(@intFromEnum(OpCode.op_return), 123);
    const actual: OpCode = @enumFromInt(chunk.code.items[0]);
    try expect_eq(actual, .op_return);
    try expect_eq(chunk.get_line(0), 123);
}
