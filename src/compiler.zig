const std = @import("std");
const Allocator = std.mem.Allocator;
const scanner = @import("scanner.zig");

pub fn compile(allocator: Allocator, source: []const u8) !void {
    scanner.init_scanner(source);
    var line: usize = 0;
    while (true) {
        const token = scanner.scan_token();
        if (token.line != line) {
            std.debug.print("{d: >4} ", .{token.line});
            line = token.line;
        } else {
            std.debug.print("   | ", .{});
        }
        std.debug.print("{d: >2} '{s}'\n", .{ @intFromEnum(token.kind), token.text });
        if (token.kind == scanner.TokenType.eof) break;
    }
    _ = allocator;
}
