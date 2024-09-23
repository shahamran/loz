const std = @import("std");
const memory = @import("memory.zig");
const value = @import("value.zig");
const Allocator = std.mem.Allocator;
const Value = value.Value;
const ValueArray = value.ValueArray;

pub const Chunk = struct {
    code: []u8,
    lines: std.ArrayList(LineInfo),
    size: usize,
    allocator: Allocator,
    constants: ValueArray,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .code = &[_]u8{},
            .size = 0,
            .allocator = allocator,
            .constants = ValueArray.init(allocator),
            .lines = std.ArrayList(LineInfo).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        _ = memory.reallocate(self.allocator, self.code, 0) catch unreachable;
        self.lines.deinit();
        self.constants.deinit();
        self.* = init(self.allocator);
    }

    pub fn write(self: *Self, byte: u8, line: usize) Allocator.Error!void {
        if (self.size + 1 > self.code.len) {
            const new_n = memory.grow_capacity(self.size);
            self.code = try memory.reallocate(self.allocator, self.code, new_n);
        }
        self.code[self.size] = byte;
        if (self.lines.items.len == 0 or self.lines.getLast().line != line) {
            try self.lines.append(.{ .line = line, .start = self.size });
        }
        self.size += 1;
    }

    pub fn add_constant(self: *Self, val: Value) Allocator.Error!usize {
        return try self.constants.write(val);
    }
};

pub const OpCode = enum(u8) {
    op_constant,
    op_return,
};

pub const LineInfo = struct {
    line: usize,
    start: usize,
};

test "write" {
    const expect = std.testing.expect;
    const allocator = std.testing.allocator;
    var chunk = Chunk.init(allocator);
    defer chunk.deinit();
    try expect(chunk.code.len == 0);
    try expect(chunk.size == 0);
    try chunk.write(@as(u8, OpCode.op_return));
    try expect(chunk.size == 1);
    try expect(chunk.code.len == 8);
    try expect(@as(OpCode, chunk.code[0]) == OpCode.op_return);
}
