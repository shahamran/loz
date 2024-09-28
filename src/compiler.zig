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
        const kind = std.enums.tagName(scanner.TokenType, token.kind) orelse unreachable;
        std.debug.print("{s: >12} '{s}'\n", .{ kind, token.text });
        if (token.kind == .eof) break;
    }
    _ = allocator;
}
