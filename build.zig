const std = @import("std");

pub fn build(b: *std.Build) void {
    const print_code = b.option(
        bool,
        "print_code",
        "Print result bytecode after compilation",
    ) orelse false;
    const trace_execution = b.option(
        bool,
        "trace_execution",
        "Print each instruction before executing",
    ) orelse false;
    const stress_gc = b.option(
        bool,
        "stress_gc",
        "Run GC after each reallocation",
    ) orelse false;
    const log_gc = b.option(
        bool,
        "log_gc",
        "Print GC info",
    ) orelse false;
    const options = b.addOptions();
    options.addOption(bool, "print_code", print_code);
    options.addOption(bool, "trace_execution", trace_execution);
    options.addOption(bool, "stress_gc", stress_gc);
    options.addOption(bool, "log_gc", log_gc);

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const exe = b.addExecutable(.{
        .name = "loz",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe.root_module.addOptions("config", options);

    b.installArtifact(exe);
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe_unit_tests.root_module.addOptions("config", options);
    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);
}
