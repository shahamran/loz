const std = @import("std");
const config = @import("config");

const Chunk = @import("Chunk.zig");
const OpCode = Chunk.OpCode;
const Compiler = @import("Compiler.zig");
const List = @import("list.zig").List;
const Obj = @import("Obj.zig");
const Table = @import("Table.zig");
const Value = @import("value.zig").Value;

const UINT8_COUNT = @import("main.zig").UINT8_COUNT;
const FRAMES_MAX = 64;
const STACK_MAX = FRAMES_MAX * UINT8_COUNT;

const Vm = @This();

allocator: std.mem.Allocator,
out_writer: std.io.AnyWriter,
err_writer: std.io.AnyWriter,
compiler: Compiler,
frames: [FRAMES_MAX]CallFrame,
frame_count: u8,

stack: [STACK_MAX]Value,
stack_top: [*]Value,

global_names: Table, // maps global variable names to their indices in the globals list
global_values: List(Global),

strings: Table, // interned strings
init_string: ?*Obj.String, // literal "init"

open_upvalues: ?*Obj.Upvalue, // singly linked list of open upvalues
objects: ?*Obj, // singly linked list of all allocated objects

pub const Global = struct {
    name: *Obj.String,
    value: Value,
};

pub fn init(vm: *Vm, args: struct {
    allocator: std.mem.Allocator,
    out_writer: std.io.AnyWriter = std.io.getStdOut().writer().any(),
    err_writer: std.io.AnyWriter = std.io.getStdErr().writer().any(),
}) void {
    vm.resetStack();
    vm.compiler.init(vm);
    vm.allocator = args.allocator;
    vm.out_writer = args.out_writer;
    vm.err_writer = args.err_writer;
    vm.global_names = Table.init(args.allocator);
    vm.global_values = List(Global).init();
    vm.strings = Table.init(args.allocator);
    vm.objects = null;
    vm.init_string = null;
    vm.init_string = Obj.String.copy(vm, "init");
    const clock = Obj.Native.init(vm, 0, clockNative);
    _ = vm.defineGlobal("clock", clock.obj.value()) catch unreachable;
}

pub fn interpret(vm: *Vm, source: []const u8) !InterpretResult {
    var function = try vm.compiler.compile(source) orelse return .compile_error;
    var closure: *Obj.Closure = undefined;
    { // gc dance!
        vm.push(function.obj.value());
        defer _ = vm.pop();
        closure = Obj.Closure.init(vm, function);
    }
    vm.push(closure.obj.value());
    _ = vm.call(closure, 0);

    return try vm.run();
}

pub const InterpretResult = enum {
    ok,
    compile_error,
    runtime_error,
};

pub fn allocateObject(vm: *Vm, comptime T: type, args: anytype) *T {
    var ptr = vm.allocator.create(T) catch unreachable;
    ptr.obj = .{
        .kind = T.kind,
        .is_marked = false,
        .next = vm.objects,
    };
    inline for (@typeInfo(T).@"struct".fields) |field| {
        if (field.default_value) |opaque_ptr| {
            const default_ptr: *const field.type = @alignCast(@ptrCast(opaque_ptr));
            @field(ptr, field.name) = default_ptr.*;
        }
    }
    inline for (@typeInfo(@TypeOf(args)).@"struct".fields) |field| {
        @field(ptr, field.name) = @field(args, field.name);
    }
    vm.objects = &ptr.obj;
    if (config.log_gc) {
        std.debug.print(
            "0x{x} allocate {d} for {s}\n",
            .{ @intFromPtr(&ptr.obj), @sizeOf(T), @tagName(T.kind) },
        );
    }
    return ptr;
}

pub fn deinit(vm: *Vm) void {
    vm.init_string = null;
    vm.strings.deinit();
    vm.global_names.deinit();
    vm.global_values.deinit(vm.allocator);
    vm.freeObjects();
}

pub inline fn push(vm: *Vm, value: Value) void {
    vm.stack_top[0] = value;
    vm.stack_top += 1;
}

pub inline fn pop(vm: *Vm) Value {
    vm.stack_top -= 1;
    return vm.stack_top[0];
}

pub fn defineGlobal(vm: *Vm, name: []const u8, value: Value) !usize {
    vm.push(value);
    defer _ = vm.pop();
    vm.push((Obj.String.copy(vm, name)).obj.value());
    defer _ = vm.pop();

    const index = vm.global_values.items.len;
    const index_val = Value{ .number = @floatFromInt(index) };
    const ident = vm.peek(0).obj.as(Obj.String);
    _ = try vm.global_names.insert(ident, index_val);
    try vm.global_values.push(vm.allocator, .{ .name = ident, .value = value });
    return index;
}

fn freeObjects(vm: *Vm) void {
    var object = vm.objects;
    while (object) |o| {
        const next = o.next;
        o.deinit(vm);
        object = next;
    }
}

const CallFrame = struct {
    const Self = @This();

    closure: *Obj.Closure,
    ip: [*]u8,
    slots: [*]Value,

    inline fn readByte(self: *Self) u8 {
        const byte = self.ip[0];
        self.ip += 1;
        return byte;
    }

    inline fn readTwo(self: *Self) u16 {
        const byte1 = self.ip[0];
        const byte2 = self.ip[1];
        self.ip += 2;
        return (@as(u16, byte1) << 8) | @as(u16, byte2);
    }

    inline fn readConstant(self: *Self) Value {
        return self.constants()[self.readByte()];
    }

    inline fn readGlobal(self: *Self, vm: *Vm) *Global {
        return &vm.global_values.items[self.readByte()];
    }

    inline fn constants(self: *Self) []Value {
        return self.closure.function.chunk.constants.items;
    }

    inline fn code(self: *Self) []u8 {
        return self.closure.function.chunk.code.items;
    }

    inline fn offset(self: *Self) usize {
        return self.ip - self.code().ptr;
    }
};

fn run(vm: *Vm) !InterpretResult {
    var frame = &vm.frames[vm.frame_count - 1];
    while (true) {
        if (comptime config.trace_execution) {
            std.debug.print("          ", .{});
            var slot: [*]Value = &vm.stack;
            while (slot != vm.stack_top) : (slot += 1) {
                std.debug.print("[ {s} ]", .{slot[0]});
            }
            std.debug.print("\n", .{});
            _ = @import("debug.zig")
                .disassembleInstruction(&frame.closure.function.chunk, frame.offset());
        }
        const instruction: OpCode = @enumFromInt(frame.readByte());
        switch (instruction) {
            .op_constant => {
                const constant = frame.readConstant();
                vm.push(constant);
            },
            .op_nil => vm.push(Value{ .nil = {} }),
            .op_true => vm.push(Value{ .bool_ = true }),
            .op_false => vm.push(Value{ .bool_ = false }),
            .op_pop => _ = vm.pop(),
            .op_get_local => {
                const slot = frame.readByte();
                vm.push(frame.slots[slot]);
            },
            .op_set_local => {
                const slot = frame.readByte();
                frame.slots[slot] = vm.peek(0);
            },
            .op_get_global => {
                const global = frame.readGlobal(vm);
                if (global.value == .undefined_) {
                    vm.runtimeError("Undefined variable '{s}'.", .{global.name});
                    return .runtime_error;
                }
                vm.push(global.value);
            },
            .op_define_global => frame.readGlobal(vm).value = vm.pop(),
            .op_set_global => {
                const global = frame.readGlobal(vm);
                if (global.value == .undefined_) {
                    vm.runtimeError("Undefined variable '{s}'.", .{global.name});
                    return .runtime_error;
                }
                global.value = vm.peek(0);
            },
            .op_get_upvalue => {
                const slot = frame.readByte();
                vm.push(frame.closure.upvalues[slot].?.location.*);
            },
            .op_set_upvalue => {
                const slot = frame.readByte();
                frame.closure.upvalues[slot].?.location.* = vm.peek(0);
            },
            .op_get_property => {
                if (!vm.peek(0).is_obj(.instance)) {
                    vm.runtimeError("Only instances have properties.", .{});
                    return .runtime_error;
                }
                const instance = vm.peek(0).obj.as(Obj.Instance);
                const name = frame.readGlobal(vm).name;
                if (instance.fields.get(name)) |value| {
                    _ = vm.pop(); // instance
                    vm.push(value);
                } else if (!try vm.bind_method(instance.class, name)) {
                    return .runtime_error;
                }
            },
            .op_set_property => {
                if (!vm.peek(1).is_obj(.instance)) {
                    vm.runtimeError("Only instances have fields.", .{});
                    return .runtime_error;
                }
                const instance = vm.peek(1).obj.as(Obj.Instance);
                const name = frame.readGlobal(vm).name;
                _ = try instance.fields.insert(name, vm.peek(0));
                const value = vm.pop();
                _ = vm.pop();
                vm.push(value);
            },
            .op_get_super => {
                const name = frame.readGlobal(vm).name;
                const superclass = vm.pop().obj.as(Obj.Class);
                if (!try vm.bind_method(superclass, name)) {
                    return .runtime_error;
                }
            },
            .op_equal => {
                const b = vm.pop();
                const a = vm.pop();
                vm.push(Value{ .bool_ = a.eql(b) });
            },
            .op_add => {
                if (vm.peek(0).is_obj(.string) and vm.peek(1).is_obj(.string)) {
                    const b = vm.peek(0).obj.as(Obj.String);
                    const a = vm.peek(1).obj.as(Obj.String);
                    var result = try a.value.clone(vm.allocator);
                    try result.append(vm.allocator, b.value.asSlice());
                    const obj = Obj.String.take(vm, &result);
                    _ = vm.pop();
                    _ = vm.pop();
                    vm.push(obj.obj.value());
                } else if (vm.peek(0) == .number and vm.peek(1) == .number) {
                    const b = vm.pop().number;
                    const a = vm.pop().number;
                    vm.push(Value{ .number = a + b });
                } else {
                    vm.runtimeError("Operands must be two numbers or two strings.", .{});
                    return .runtime_error;
                }
            },
            .op_subtract => vm.binaryOp(f64, subtract) orelse return .runtime_error,
            .op_multiply => vm.binaryOp(f64, multiply) orelse return .runtime_error,
            .op_divide => vm.binaryOp(f64, divide) orelse return .runtime_error,
            .op_greater => vm.binaryOp(bool, greater) orelse return .runtime_error,
            .op_less => vm.binaryOp(bool, less) orelse return .runtime_error,
            .op_not => {
                const value = vm.pop();
                vm.push(Value{ .bool_ = isFalsey(value) });
            },
            .op_negate => {
                if (vm.peek(0) != .number) {
                    vm.runtimeError("Operand must be a number.", .{});
                    return .runtime_error;
                }
                vm.push(Value{ .number = -vm.pop().number });
            },
            .op_print => vm.out_writer.print("{s}\n", .{vm.pop()}) catch unreachable,
            .op_jump => {
                const offset = frame.readTwo();
                frame.ip += offset;
            },
            .op_jump_if_false => {
                const offset = frame.readTwo();
                if (isFalsey(vm.peek(0))) frame.ip += offset;
            },
            .op_loop => {
                const offset = frame.readTwo();
                frame.ip -= offset;
            },
            .op_call => {
                const arg_count = frame.readByte();
                if (!vm.callValue(vm.peek(arg_count), arg_count)) {
                    return .runtime_error;
                }
                frame = &vm.frames[vm.frame_count - 1];
            },
            .op_invoke => {
                const method = frame.readGlobal(vm).name;
                const arg_count = frame.readByte();
                if (!vm.invoke(method, arg_count)) {
                    return .runtime_error;
                }
                frame = &vm.frames[vm.frame_count - 1];
            },
            .op_super_invoke => {
                const method = frame.readGlobal(vm).name;
                const arg_count = frame.readByte();
                const superclass = vm.pop().obj.as(Obj.Class);
                if (!vm.invokeFromClass(superclass, method, arg_count)) {
                    return .runtime_error;
                }
                frame = &vm.frames[vm.frame_count - 1];
            },
            .op_closure => {
                const function = frame.readConstant().obj.as(Obj.Function);
                const closure = Obj.Closure.init(vm, function);
                vm.push(closure.obj.value());
                for (closure.upvalues) |*upvalue| {
                    const is_local = frame.readByte();
                    const index = frame.readByte();
                    if (is_local == 1) {
                        upvalue.* = try vm.captureUpvalue(&frame.slots[index]);
                    } else {
                        upvalue.* = frame.closure.upvalues[index];
                    }
                }
            },
            .op_close_upvalue => {
                vm.closeUpvalue(vm.stack_top - 1);
                _ = vm.pop();
            },
            .op_return => {
                const result = vm.pop();
                vm.closeUpvalue(frame.slots);
                vm.frame_count -= 1;
                if (vm.frame_count == 0) {
                    _ = vm.pop();
                    return .ok;
                }
                vm.stack_top = frame.slots;
                vm.push(result);
                frame = &vm.frames[vm.frame_count - 1];
            },
            .op_class => {
                const name = frame.readGlobal(vm).name;
                const class = Obj.Class.init(vm, name);
                vm.push(class.obj.value());
            },
            .op_inherit => {
                const superclass = vm.peek(1);
                if (!superclass.is_obj(.class)) {
                    vm.runtimeError("Superclass must be a class.", .{});
                    return .runtime_error;
                }
                const subclass = vm.peek(0).obj.as(Obj.Class);
                try superclass.obj.as(Obj.Class).methods.copyAll(&subclass.methods);
                _ = vm.pop();
            },
            .op_method => try vm.defineMethod(frame.readGlobal(vm).name),
        }
    }
}

fn resetStack(vm: *Vm) void {
    vm.stack_top = &vm.stack;
    vm.frame_count = 0;
    vm.open_upvalues = null;
}

inline fn peek(vm: *Vm, distance: usize) Value {
    return (vm.stack_top - 1 - distance)[0];
}

fn callValue(vm: *Vm, callee: Value, arg_count: u8) bool {
    switch (callee) {
        .obj => |o| switch (o.kind) {
            .bound_method => {
                const bound = o.as(Obj.BoundMethod);
                const ptr = vm.stack_top - arg_count - 1;
                ptr[0] = bound.receiver;
                return vm.call(bound.method, arg_count);
            },
            .class => {
                const class = o.as(Obj.Class);
                const ptr = vm.stack_top - arg_count - 1;
                ptr[0] = Obj.Instance.init(vm, class).obj.value();
                if (class.methods.get(vm.init_string.?)) |initializer| {
                    return vm.call(initializer.obj.as(Obj.Closure), arg_count);
                } else if (arg_count != 0) {
                    vm.runtimeError("Expected 0 arguments but got {d}.", .{arg_count});
                    return false;
                }
                return true;
            },
            .closure => return vm.call(o.as(Obj.Closure), arg_count),
            .native => {
                const native = o.as(Obj.Native);
                const arity = native.arity;
                if (arg_count != arity) {
                    vm.runtimeError("Expected {d} arguments but got {d}.", .{ arity, arg_count });
                    return false;
                }
                const result = native.function(arg_count, vm.stack_top - arg_count);
                vm.stack_top -= arg_count + 1;
                vm.push(result);
                return true;
            },
            else => {},
        },
        else => {},
    }
    vm.runtimeError("Can only call functions and classes.", .{});
    return false;
}

fn invoke(vm: *Vm, name: *Obj.String, arg_count: u8) bool {
    const receiver = vm.peek(arg_count);
    if (!receiver.is_obj(.instance)) {
        vm.runtimeError("Only instances have methods.", .{});
        return false;
    }
    const instance = receiver.obj.as(Obj.Instance);
    if (instance.fields.get(name)) |value| {
        (vm.stack_top - arg_count - 1)[0] = value;
        return vm.callValue(value, arg_count);
    }
    return vm.invokeFromClass(instance.class, name, arg_count);
}

fn invokeFromClass(vm: *Vm, class: *Obj.Class, name: *Obj.String, arg_count: u8) bool {
    const method = class.methods.get(name) orelse {
        vm.runtimeError("Undefined property '{s}'.", .{name});
        return false;
    };
    return vm.call(method.obj.as(Obj.Closure), arg_count);
}

fn captureUpvalue(vm: *Vm, local: *Value) !*Obj.Upvalue {
    var prev_upvalue: ?*Obj.Upvalue = null;
    var upvalue = vm.open_upvalues;
    while (upvalue != null and @intFromPtr(upvalue.?.location) > @intFromPtr(local)) {
        prev_upvalue = upvalue;
        upvalue = upvalue.?.next;
    }
    if (upvalue != null and upvalue.?.location == local) {
        return upvalue.?;
    }
    const created_upvalue = Obj.Upvalue.init(vm, .{ .location = local, .next = upvalue });
    if (prev_upvalue) |v| {
        v.next = created_upvalue;
    } else {
        vm.open_upvalues = created_upvalue;
    }
    return created_upvalue;
}

fn closeUpvalue(vm: *Vm, last: [*]Value) void {
    while (vm.open_upvalues) |upvalue| {
        // same as while condition having: upvalue.location >= last
        if (@intFromPtr(last) > @intFromPtr(upvalue.location)) break;
        upvalue.closed = upvalue.location.*;
        upvalue.location = &upvalue.closed;
        vm.open_upvalues = upvalue.next;
    }
}

fn call(vm: *Vm, closure: *Obj.Closure, arg_count: u8) bool {
    const arity = closure.function.arity;
    if (arg_count != arity) {
        vm.runtimeError("Expected {d} arguments but got {d}.", .{ arity, arg_count });
        return false;
    }
    if (vm.frame_count == Vm.FRAMES_MAX) {
        vm.runtimeError("Stack overflow.", .{});
        return false;
    }
    var frame = &vm.frames[vm.frame_count];
    vm.frame_count += 1;
    frame.closure = closure;
    frame.ip = frame.code().ptr;
    frame.slots = vm.stack_top - arg_count - 1;
    return true;
}

fn isFalsey(value: Value) bool {
    return switch (value) {
        .nil => true,
        .bool_ => |b| !b,
        else => false,
    };
}

fn binaryOp(vm: *Vm, comptime Ret: type, fun: *const fn (f64, f64) Ret) ?void {
    if (vm.peek(0) != .number or vm.peek(1) != .number) {
        vm.runtimeError("Operands must be numbers.", .{});
        return null;
    }
    const b = vm.pop().number;
    const a = vm.pop().number;
    vm.push(if (Ret == bool)
        Value{ .bool_ = fun(a, b) }
    else
        Value{ .number = fun(a, b) });
}

fn subtract(a: f64, b: f64) f64 {
    return a - b;
}

fn multiply(a: f64, b: f64) f64 {
    return a * b;
}

fn divide(a: f64, b: f64) f64 {
    return a / b;
}

fn greater(a: f64, b: f64) bool {
    return a > b;
}

fn less(a: f64, b: f64) bool {
    return a < b;
}

fn runtimeError(vm: *Vm, comptime fmt: []const u8, args: anytype) void {
    vm.err_writer.print(fmt, args) catch unreachable;
    vm.err_writer.print("\n", .{}) catch unreachable;

    var i = vm.frame_count;
    while (i > 0) {
        i -= 1;
        const frame = &vm.frames[i];
        const function = frame.closure.function;
        const instruction = frame.offset() - 1;
        const line = function.chunk.get_line(instruction);
        vm.err_writer.print("[line {d}] in ", .{line}) catch unreachable;
        if (function.name) |name| {
            vm.err_writer.print("{s}()\n", .{name.value.asSlice()}) catch unreachable;
        } else {
            vm.err_writer.print("script\n", .{}) catch unreachable;
        }
    }
    vm.resetStack();
}

fn clockNative(_: u8, _: [*]Value) Value {
    const time = std.time;
    const timestamp: f64 = @floatFromInt(time.microTimestamp());
    const us_per_s: f64 = @floatFromInt(time.us_per_s);
    return Value{ .number = timestamp / us_per_s };
}

fn defineMethod(vm: *Vm, name: *Obj.String) !void {
    const method = vm.peek(0);
    const class = vm.peek(1).obj.as(Obj.Class);
    _ = try class.methods.insert(name, method);
    _ = vm.pop();
}

fn bind_method(vm: *Vm, class: *Obj.Class, name: *Obj.String) !bool {
    const method = class.methods.get(name) orelse {
        vm.runtimeError("Undefined property '{s}'.", .{name});
        return false;
    };
    const bound = Obj.BoundMethod.init(vm, vm.peek(0), method.obj.as(Obj.Closure));
    _ = vm.pop();
    vm.push(bound.obj.value());
    return true;
}

pub usingnamespace if (@import("builtin").is_test)
    struct {
        const TestResult = struct {
            result: InterpretResult,
            output: std.ArrayList(u8),
            err: std.ArrayList(u8),

            fn deinit(self: *@This()) void {
                self.output.deinit();
                self.err.deinit();
            }
        };

        fn testVm(source: []const u8) !TestResult {
            const GcAllocator = @import("GcAllocator.zig");
            const allocator = std.testing.allocator;

            var vm: Vm = undefined;
            var gca = GcAllocator.init(&vm, allocator);
            var out = std.ArrayList(u8).init(allocator);
            var err = std.ArrayList(u8).init(allocator);
            defer gca.deinit();
            vm.init(.{
                .allocator = gca.allocator(),
                .out_writer = out.writer().any(),
                .err_writer = err.writer().any(),
            });
            defer vm.deinit();

            const result = try vm.interpret(source);
            return .{ .result = result, .output = out, .err = err };
        }
    }
else
    struct {};

const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

test "string equality" {
    var t = try Vm.testVm(
        \\ print "hello" == "world";
        \\ print "hello" == "hello";
        \\ var x = "hello";
        \\ print x == "hello";
    );
    defer t.deinit();
    try expectEqual(.ok, t.result);
    try expectEqualStrings("false\ntrue\ntrue\n", t.output.items);
}

test "class - nested this" {
    var t = try Vm.testVm(
        \\ class Nested {
        \\   method() {
        \\     fun function() {
        \\       print this;
        \\     }
        \\
        \\     function();
        \\   }
        \\ }
        \\ Nested().method();
    );
    defer t.deinit();
    try expectEqual(.ok, t.result);
    try expectEqualStrings("Nested instance\n", t.output.items);
}

test "class - invalid this" {
    {
        var t = try Vm.testVm("print this;");
        defer t.deinit();
        try expectEqual(.compile_error, t.result);
    }
    {
        var t = try Vm.testVm(
            \\ fun notMethod() {
            \\   print this;
            \\ }
        );
        defer t.deinit();
        try expectEqual(.compile_error, t.result);
    }
}

test "class - invoke field" {
    var t = try Vm.testVm(
        \\ class Oops {
        \\   init() {
        \\     fun f() {
        \\       print "not a method";
        \\     }
        \\
        \\     this.field = f;
        \\   }
        \\ }
        \\ var oops = Oops();
        \\ oops.field();
    );
    defer t.deinit();
    try expectEqual(.ok, t.result);
    try expectEqualStrings("not a method\n", t.output.items);
}

test "globals - function equality" {
    var t = try Vm.testVm(
        \\ fun uniq() { return 1; }
        \\
        \\ fun outer() {
        \\   fun uniq() { return 2; }
        \\   return uniq;
        \\ }
        \\
        \\ print uniq == outer();
        \\
        \\ var uv = uniq;
        \\ print uv == uniq;
    );
    defer t.deinit();
    try expectEqual(.ok, t.result);
    try expectEqualStrings("false\ntrue\n", t.output.items);
}

test "globals - class equality" {
    var t = try Vm.testVm(
        \\ class uniq { init(a) { this.a = a; } }
        \\
        \\ fun outer() {
        \\   class uniq { init(b) { this.b = b; } }
        \\   return uniq;
        \\ }
        \\
        \\ print uniq == outer();
        \\
        \\ var uv = uniq;
        \\ print uv == uniq;
    );
    defer t.deinit();
    try expectEqual(.ok, t.result);
    try expectEqualStrings("false\ntrue\n", t.output.items);
}

test "inheritance - method call" {
    var t = try Vm.testVm(
        \\ class Doughnut {
        \\   cook() {
        \\     print "Dunk in the fryer.";
        \\   }
        \\ }
        \\ class Cruller < Doughnut {
        \\   finish() {
        \\     print "Glaze with icing.";
        \\   }
        \\ }
        \\ var c = Cruller();
        \\ c.cook();
        \\ c.finish();
    );
    defer t.deinit();
    try expectEqual(.ok, t.result);
    try expectEqualStrings(
        "Dunk in the fryer.\n" ++
            "Glaze with icing.\n",
        t.output.items,
    );
}

test "inheritance - super" {
    var t = try Vm.testVm(
        \\ class A {
        \\   method() {
        \\     print "A method";
        \\   }
        \\ }
        \\ class B < A {
        \\   method() {
        \\     print "B method";
        \\   }
        \\
        \\   test() {
        \\     super.method();
        \\   }
        \\ }
        \\ class C < B {}
        \\ C().test();
    );
    defer t.deinit();
    try expectEqual(.ok, t.result);
    try expectEqualStrings("A method\n", t.output.items);
}

test "function - call field" {
    var t = try Vm.testVm(
        \\ class Foo {}
        \\ fun bar(a, b) {
        \\   print "bar";
        \\   print a;
        \\   print b;
        \\ }
        \\ var foo = Foo();
        \\ foo.bar = bar;
        \\ foo.bar(1, 2);
    );
    defer t.deinit();
    try expectEqual(.ok, t.result);
    try expectEqualStrings("bar\n1\n2\n", t.output.items);
}

test "class - recursive method" {
    var t = try Vm.testVm(
        \\ class Foo {
        \\   method() {
        \\     print method; // expect runtime error: Undefined variable 'method'.
        \\   }
        \\ }
        \\ Foo().method();
    );
    defer t.deinit();
    try expectEqual(.runtime_error, t.result);
}
