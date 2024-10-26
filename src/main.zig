const std = @import("std");
const config = @import("config");
const GcAllocator = @import("GcAllocator.zig");
const Vm = @import("Vm.zig");

pub const UINT8_COUNT = std.math.maxInt(u8) + 1;

var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
pub const allocator = gpa.allocator();

pub fn main() !void {
    defer std.debug.assert(gpa.deinit() == .ok);

    var vm: Vm = undefined;
    var gca = GcAllocator.init(&vm, allocator);
    defer gca.deinit();

    vm.init(gca.allocator());
    defer vm.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len == 1) {
        try repl(&vm);
    } else if (args.len == 2) {
        try run_file(&vm, args.ptr[1]);
    } else {
        std.debug.print("Usage: {s} [path]\n", .{args.ptr[0]});
        std.process.exit(64);
    }
}

fn repl(vm: *Vm) !void {
    var buf: [1024]u8 = undefined;
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    while (true) {
        try stdout.print("> ", .{});
        if (try stdin.readUntilDelimiterOrEof(buf[0..], '\n')) |line| {
            _ = try vm.interpret(line);
        } else {
            break;
        }
    }
    try stdout.print("\n", .{});
}

fn run_file(vm: *Vm, path: []const u8) !void {
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

    switch (try vm.interpret(source)) {
        .ok => {},
        .compile_error => std.process.exit(65),
        .runtime_error => std.process.exit(70),
    }
}

test {
    std.testing.refAllDecls(@This());
}
