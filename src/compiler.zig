const std = @import("std");
const Allocator = std.mem.Allocator;
const scanner = @import("scanner.zig");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;

const DEBUG_PRINT_CODE = true;

const Parser = struct {
    current: scanner.Token,
    previous: scanner.Token,
    had_error: bool,
    panic_mode: bool,
    allocator: Allocator,
};

var parser: Parser = undefined;
var compiling_chunk: *Chunk = undefined;

pub fn compile(allocator: Allocator, source: []const u8, chunk: *Chunk) !bool {
    defer end_compiler();
    scanner.init_scanner(source);
    parser.allocator = allocator;
    compiling_chunk = chunk;
    parser.had_error = false;
    parser.panic_mode = false;
    advance();
    try expression();
    consume(.eof, "Expected end of expression.");
    return !parser.had_error;
}

const Precedence = enum {
    none,
    assignment, // =
    or_, // or
    and_, // and
    equality, // == !=
    comparison, // < > <= >=
    term, // + -
    factor, // * /
    unary, // ! -
    call, // . ()
    primary,
};

const ParseRule = struct {
    prefix: ?*const fn () Allocator.Error!void,
    infix: ?*const fn () Allocator.Error!void,
    precedence: Precedence,
};

fn expression() Allocator.Error!void {
    try parse_precedence(.assignment);
}

fn parse_precedence(precedence: Precedence) Allocator.Error!void {
    advance();
    const prefix_rule = rules.get(parser.previous.kind).prefix orelse {
        error_("Expected expression.");
        return;
    };
    try prefix_rule();
    while (@intFromEnum(precedence) <= @intFromEnum(rules.get(parser.current.kind).precedence)) {
        advance();
        const infix_rule = rules.get(parser.previous.kind).infix orelse {
            unreachable;
        };
        try infix_rule();
    }
}

fn number() Allocator.Error!void {
    const value = std.fmt.parseFloat(f64, parser.previous.text) catch unreachable;
    try emit_constant(value);
}

fn grouping() Allocator.Error!void {
    try expression();
    consume(.right_paren, "Expected ')' after expression.");
}

fn unary() Allocator.Error!void {
    const op_kind = parser.previous.kind;
    // compile the operand.
    try parse_precedence(.unary);
    // emit the operator instruction.
    try switch (op_kind) {
        .minus => emit_byte(@intFromEnum(OpCode.op_negate)),
        else => unreachable,
    };
}

fn binary() Allocator.Error!void {
    const op_kind = parser.previous.kind;
    const rule = rules.get(op_kind);
    try parse_precedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));
    try switch (op_kind) {
        .plus => emit_byte(@intFromEnum(OpCode.op_add)),
        .minus => emit_byte(@intFromEnum(OpCode.op_subtract)),
        .star => emit_byte(@intFromEnum(OpCode.op_multiply)),
        .slash => emit_byte(@intFromEnum(OpCode.op_divide)),
        else => unreachable,
    };
}

fn advance() void {
    parser.previous = parser.current;
    while (true) {
        parser.current = scanner.scan_token();
        if (parser.current.kind != .error_) {
            break;
        }
        error_at_current(parser.current.text);
    }
}

fn consume(kind: scanner.TokenType, message: []const u8) void {
    if (parser.current.kind == kind) {
        advance();
        return;
    }
    error_at_current(message);
}

fn emit_byte(byte: u8) Allocator.Error!void {
    try current_chunk().write(byte, parser.previous.line);
}

fn emit_return() !void {
    try emit_byte(@intFromEnum(OpCode.op_return));
}

fn emit_with_operand(op: OpCode, operand: u8) !void {
    try emit_byte(@intFromEnum(op));
    try emit_byte(operand);
}

fn emit_constant(value: f64) !void {
    const constant = try current_chunk().add_constant(value);
    if (constant > std.math.maxInt(u8)) {
        // TODO: use op_constan_long instead.
        error_("Too many constants in one chunk.");
        return;
    }
    try emit_with_operand(.op_constant, @intCast(constant));
}

fn error_(message: []const u8) void {
    error_at(&parser.previous, message);
}

fn error_at_current(message: []const u8) void {
    error_at(&parser.current, message);
}

fn error_at(token: *scanner.Token, message: []const u8) void {
    if (parser.panic_mode) return;
    parser.panic_mode = true;
    std.debug.print("[line {d}] Error", .{token.line});

    if (token.kind == .eof) {
        std.debug.print(" at end", .{});
    } else if (token.kind == .error_) {
        // Nothing.
    } else {
        std.debug.print(" at '{s}'", .{token.text});
    }
    std.debug.print(": {s}\n", .{message});
    parser.had_error = true;
}

fn end_compiler() void {
    emit_return() catch unreachable;
    if (DEBUG_PRINT_CODE) {
        if (!parser.had_error) {
            @import("debug.zig").disassemble_chunk(current_chunk(), "code");
        }
    }
}

fn current_chunk() *Chunk {
    return compiling_chunk;
}

const rules = std.EnumArray(scanner.TokenType, ParseRule).init(.{
    .left_paren = .{ .prefix = grouping, .infix = null, .precedence = .none },
    .right_paren = .{ .prefix = null, .infix = null, .precedence = .none },
    .left_brace = .{ .prefix = null, .infix = null, .precedence = .none },
    .right_brace = .{ .prefix = null, .infix = null, .precedence = .none },
    .comma = .{ .prefix = null, .infix = null, .precedence = .none },
    .dot = .{ .prefix = null, .infix = null, .precedence = .none },
    .minus = .{ .prefix = unary, .infix = binary, .precedence = .term },
    .plus = .{ .prefix = null, .infix = binary, .precedence = .term },
    .semicolon = .{ .prefix = null, .infix = null, .precedence = .none },
    .slash = .{ .prefix = null, .infix = binary, .precedence = .factor },
    .star = .{ .prefix = null, .infix = binary, .precedence = .factor },
    .bang = .{ .prefix = unary, .infix = null, .precedence = .none },
    .bang_equal = .{ .prefix = null, .infix = null, .precedence = .none },
    .equal = .{ .prefix = null, .infix = null, .precedence = .none },
    .equal_equal = .{ .prefix = null, .infix = null, .precedence = .none },
    .greater = .{ .prefix = null, .infix = null, .precedence = .none },
    .greater_equal = .{ .prefix = null, .infix = null, .precedence = .none },
    .less = .{ .prefix = null, .infix = null, .precedence = .none },
    .less_equal = .{ .prefix = null, .infix = null, .precedence = .none },
    .identifier = .{ .prefix = null, .infix = null, .precedence = .none },
    .string = .{ .prefix = null, .infix = null, .precedence = .none },
    .number = .{ .prefix = number, .infix = null, .precedence = .none },
    .and_ = .{ .prefix = null, .infix = null, .precedence = .none },
    .class = .{ .prefix = null, .infix = null, .precedence = .none },
    .else_ = .{ .prefix = null, .infix = null, .precedence = .none },
    .false_ = .{ .prefix = null, .infix = null, .precedence = .none },
    .for_ = .{ .prefix = null, .infix = null, .precedence = .none },
    .fun = .{ .prefix = null, .infix = null, .precedence = .none },
    .if_ = .{ .prefix = null, .infix = null, .precedence = .none },
    .nil = .{ .prefix = null, .infix = null, .precedence = .none },
    .or_ = .{ .prefix = null, .infix = null, .precedence = .none },
    .print = .{ .prefix = null, .infix = null, .precedence = .none },
    .return_ = .{ .prefix = null, .infix = null, .precedence = .none },
    .super = .{ .prefix = null, .infix = null, .precedence = .none },
    .this = .{ .prefix = null, .infix = null, .precedence = .none },
    .true_ = .{ .prefix = null, .infix = null, .precedence = .none },
    .var_ = .{ .prefix = null, .infix = null, .precedence = .none },
    .while_ = .{ .prefix = null, .infix = null, .precedence = .none },
    .error_ = .{ .prefix = null, .infix = null, .precedence = .none },
    .eof = .{ .prefix = null, .infix = null, .precedence = .none },
});
