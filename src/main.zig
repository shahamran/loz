const std = @import("std");
const debug = @import("debug.zig");
const vm = @import("vm.zig");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        _ = gpa.deinit();
    }

    vm.init_vm();
    defer vm.deinit_vm();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len == 1) {
        try repl(allocator);
    } else if (args.len == 2) {
        try run_file(allocator, args.ptr[1]);
    } else {
        std.debug.print("Usage: loz [path]\n", .{});
        std.process.exit(64);
    }
}

fn repl(allocator: std.mem.Allocator) !void {
    var buf: [1024]u8 = undefined;
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    while (true) {
        try stdout.print("> ", .{});
        if (try stdin.readUntilDelimiterOrEof(buf[0..], '\n')) |line| {
            _ = try vm.interpret(allocator, line);
        } else {
            break;
        }
    }
    try stdout.print("\n", .{});
}

fn run_file(allocator: std.mem.Allocator, path: []const u8) !void {
    const file = std.fs.cwd().openFile(path, .{}) catch {
        std.debug.print("Could not open file \"{s}\".\n", .{path});
        std.process.exit(74);
    };
    defer file.close();

    const stat = try file.stat();
    const source = file.readToEndAlloc(allocator, stat.size) catch {
        std.debug.print("Could not read file \"{s}\".\n", .{path});
        std.process.exit(74);
    };
    defer allocator.free(source);

    switch (try vm.interpret(allocator, source)) {
        .ok => {},
        .compile_error => std.process.exit(65),
        .runtime_error => std.process.exit(70),
    }
}
