const std = @import("std");
const Allocator = std.mem.Allocator;
const scanner = @import("scanner.zig");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const Table = @import("table.zig").Table;
const config = @import("config");
const object = @import("object.zig");

var parser: Parser = undefined;
var compiling_chunk: *Chunk = undefined;
var string_constants: Table = undefined;

const Parser = struct {
    current: scanner.Token,
    previous: scanner.Token,
    had_error: bool,
    panic_mode: bool,
};

const Precedence = enum {
    /// lowest precedence
    none,
    /// =
    assignment,
    or_,
    and_,
    /// ==, !=
    equality,
    /// <, >, <=, >=
    comparison,
    /// +, -
    term,
    /// *, /
    factor,
    /// !, -
    unary,
    /// ., ()
    call,
    /// highest precedence
    primary,

    const Self = @This();

    fn higher(self: *const Self) Self {
        return @enumFromInt(self.int() + 1);
    }

    fn int(self: *const Self) u8 {
        return @intFromEnum(self.*);
    }
};

pub fn compile(source: []const u8, chunk: *Chunk) !bool {
    defer end_compiler();
    scanner.init_scanner(source);
    compiling_chunk = chunk;
    parser.had_error = false;
    parser.panic_mode = false;
    string_constants = Table.init();
    defer string_constants.deinit();
    advance();
    while (!match(.eof)) {
        try declaration();
    }
    return !parser.had_error;
}

fn declaration() !void {
    if (match(.var_)) {
        try var_declaration();
    } else {
        try statement();
    }
    if (parser.panic_mode) synchronize();
}

fn var_declaration() !void {
    const global = try parse_variable("Expected variable name.");
    if (match(.equal)) {
        try expression();
    } else {
        try emit_byte(op_u8(.op_nil));
    }
    consume(.semicolon, "Expected ';' after variable declaration.");
    try define_variable(global);
}

fn statement() !void {
    if (match(.print)) {
        try print_statement();
    } else {
        try expression_statement();
    }
}

fn print_statement() !void {
    try expression();
    consume(.semicolon, "Expected ';' after value.");
    try emit_byte(op_u8(.op_print));
}

fn expression_statement() !void {
    try expression();
    consume(.semicolon, "Expected ';' after expression.");
    try emit_byte(op_u8(.op_pop));
}

fn expression() Allocator.Error!void {
    try parse_precedence(.assignment);
}

fn parse_precedence(precedence: Precedence) Allocator.Error!void {
    advance();
    const prefix_rule = rules.get(parser.previous.kind).prefix orelse {
        error_("Expected expression.");
        return;
    };
    const can_assign = precedence.int() <= Precedence.assignment.int();
    try prefix_rule(can_assign);
    while (precedence.int() <= rules.get(parser.current.kind).precedence.int()) {
        advance();
        const infix_rule = rules.get(parser.previous.kind).infix orelse {
            unreachable;
        };
        try infix_rule(can_assign);
    }
    if (can_assign and match(.equal)) {
        error_("Invalid assignment target.");
    }
}

fn define_variable(global: u8) !void {
    try emit_two(op_u8(.op_define_global), global);
}

fn parse_variable(error_message: []const u8) !u8 {
    consume(.identifier, error_message);
    return try identifier_constant(&parser.previous);
}

fn identifier_constant(name: *const scanner.Token) !u8 {
    const ident = try object.ObjString.copy(name.text);
    if (string_constants.get(ident)) |constant|
        return @intFromFloat(constant.number);
    const index: u8 = @intCast(try make_constant(Value{ .obj = ident.upcast() }));
    _ = try string_constants.insert(ident, Value{ .number = @floatFromInt(index) });
    return index;
}

fn named_variable(name: scanner.Token, can_assign: bool) !void {
    const arg = try identifier_constant(&name);
    if (can_assign and match(.equal)) {
        try expression();
        try emit_two(op_u8(.op_set_global), arg);
    } else {
        try emit_two(op_u8(.op_get_global), arg);
    }
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

fn match(kind: scanner.TokenType) bool {
    if (!check(kind)) return false;
    advance();
    return true;
}

fn check(kind: scanner.TokenType) bool {
    return parser.current.kind == kind;
}

fn emit_byte(byte: u8) !void {
    try current_chunk().write(byte, parser.previous.line);
}

fn emit_return() !void {
    try emit_byte(op_u8(.op_return));
}

fn emit_two(byte1: u8, byte2: u8) !void {
    try emit_byte(byte1);
    try emit_byte(byte2);
}

fn emit_constant(value: Value) !void {
    const constant = try make_constant(value);
    if (constant > std.math.maxInt(u8)) {
        // TODO: use op_constan_long instead.
        error_("Too many constants in one chunk.");
        return;
    }
    try emit_two(op_u8(.op_constant), @intCast(constant));
}

inline fn make_constant(value: Value) !usize {
    return try current_chunk().add_constant(value);
}

fn synchronize() void {
    parser.panic_mode = false;
    while (parser.current.kind != .eof) {
        if (parser.previous.kind == .semicolon) return;
        switch (parser.current.kind) {
            .class, .fun, .var_, .for_, .if_, .while_, .print, .return_ => return,
            else => {}, // Do nothing.
        }
        advance();
    }
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
    if (config.print_code) {
        if (!parser.had_error) {
            @import("debug.zig").disassemble_chunk(current_chunk(), "code");
        }
    }
}

fn current_chunk() *Chunk {
    return compiling_chunk;
}

inline fn op_u8(op: OpCode) u8 {
    return @intFromEnum(op);
}

/// Parse a number literal.
fn number(can_assign: bool) Allocator.Error!void {
    _ = can_assign;
    const value = std.fmt.parseFloat(f64, parser.previous.text) catch unreachable;
    try emit_constant(Value{ .number = value });
}

/// Parse booleans and nil.
fn literal(can_assign: bool) Allocator.Error!void {
    _ = can_assign;
    switch (parser.previous.kind) {
        .false_ => try emit_byte(op_u8(.op_false)),
        .true_ => try emit_byte(op_u8(.op_true)),
        .nil => try emit_byte(op_u8(.op_nil)),
        else => unreachable,
    }
}

/// Parse string literals.
fn string(can_assign: bool) !void {
    _ = can_assign;
    const end = parser.previous.text.len - 1;
    const chars = parser.previous.text[1..end]; // remove the quotes
    const s = try object.ObjString.copy(chars);
    try emit_constant(.{ .obj = s.upcast() });
}

/// Parse variable names.
fn variable(can_assign: bool) !void {
    try named_variable(parser.previous, can_assign);
}

/// Parse parenthesized expressions.
fn grouping(can_assign: bool) Allocator.Error!void {
    _ = can_assign;
    try expression();
    consume(.right_paren, "Expected ')' after expression.");
}

/// Parse unary operators.
fn unary(can_assign: bool) Allocator.Error!void {
    _ = can_assign;
    const op_kind = parser.previous.kind;
    // compile the operand.
    try parse_precedence(.unary);
    // emit the operator instruction.
    try switch (op_kind) {
        .bang => emit_byte(op_u8(.op_not)),
        .minus => emit_byte(op_u8(.op_negate)),
        else => unreachable,
    };
}

/// Parse binary operators.
fn binary(can_assign: bool) Allocator.Error!void {
    _ = can_assign;
    const op_kind = parser.previous.kind;
    const rule = rules.get(op_kind);
    try parse_precedence(rule.precedence.higher());
    try switch (op_kind) {
        .bang_equal => emit_two(op_u8(.op_equal), op_u8(.op_not)),
        .equal_equal => emit_byte(op_u8(.op_equal)),
        .greater => emit_byte(op_u8(.op_greater)),
        .greater_equal => emit_two(op_u8(.op_less), op_u8(.op_not)),
        .less => emit_byte(op_u8(.op_less)),
        .less_equal => emit_two(op_u8(.op_greater), op_u8(.op_not)),
        .plus => emit_byte(op_u8(.op_add)),
        .minus => emit_byte(op_u8(.op_subtract)),
        .star => emit_byte(op_u8(.op_multiply)),
        .slash => emit_byte(op_u8(.op_divide)),
        else => unreachable,
    };
}

/// Parse table entry.
const ParseRule = struct {
    prefix: ?*const fn (bool) Allocator.Error!void,
    infix: ?*const fn (bool) Allocator.Error!void,
    precedence: Precedence,
};

/// Parse table. Maps token types to parsing functions and precedence.
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
    .bang_equal = .{ .prefix = null, .infix = binary, .precedence = .equality },
    .equal = .{ .prefix = null, .infix = null, .precedence = .none },
    .equal_equal = .{ .prefix = null, .infix = binary, .precedence = .equality },
    .greater = .{ .prefix = null, .infix = binary, .precedence = .comparison },
    .greater_equal = .{ .prefix = null, .infix = binary, .precedence = .comparison },
    .less = .{ .prefix = null, .infix = binary, .precedence = .comparison },
    .less_equal = .{ .prefix = null, .infix = binary, .precedence = .comparison },
    .identifier = .{ .prefix = variable, .infix = null, .precedence = .none },
    .string = .{ .prefix = string, .infix = null, .precedence = .none },
    .number = .{ .prefix = number, .infix = null, .precedence = .none },
    .and_ = .{ .prefix = null, .infix = null, .precedence = .none },
    .class = .{ .prefix = null, .infix = null, .precedence = .none },
    .else_ = .{ .prefix = null, .infix = null, .precedence = .none },
    .false_ = .{ .prefix = literal, .infix = null, .precedence = .none },
    .for_ = .{ .prefix = null, .infix = null, .precedence = .none },
    .fun = .{ .prefix = null, .infix = null, .precedence = .none },
    .if_ = .{ .prefix = null, .infix = null, .precedence = .none },
    .nil = .{ .prefix = literal, .infix = null, .precedence = .none },
    .or_ = .{ .prefix = null, .infix = null, .precedence = .none },
    .print = .{ .prefix = null, .infix = null, .precedence = .none },
    .return_ = .{ .prefix = null, .infix = null, .precedence = .none },
    .super = .{ .prefix = null, .infix = null, .precedence = .none },
    .this = .{ .prefix = null, .infix = null, .precedence = .none },
    .true_ = .{ .prefix = literal, .infix = null, .precedence = .none },
    .var_ = .{ .prefix = null, .infix = null, .precedence = .none },
    .while_ = .{ .prefix = null, .infix = null, .precedence = .none },
    .error_ = .{ .prefix = null, .infix = null, .precedence = .none },
    .eof = .{ .prefix = null, .infix = null, .precedence = .none },
});
