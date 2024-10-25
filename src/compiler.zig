const std = @import("std");
const Allocator = std.mem.Allocator;
const Scanner = @import("Scanner.zig");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const Table = @import("Table.zig");
const Obj = @import("Obj.zig");
const config = @import("config");
const vm = @import("vm.zig");
const memory = @import("memory.zig");

const UINT8_COUNT = std.math.maxInt(u8) + 1;

var parser: Parser = undefined;
var current: *Compiler = undefined;
var compiling_chunk: *Chunk = undefined;
var string_constants: Table = undefined;
var scanner: Scanner = undefined;

const Parser = struct {
    current: Scanner.Token,
    previous: Scanner.Token,
    had_error: bool,
    panic_mode: bool,
};

const Compiler = struct {
    const Self = @This();

    enclosing: ?*Self,
    function: *Obj.Function,
    kind: FunctionKind,
    locals: [UINT8_COUNT]Local,
    local_count: u8,
    upvalues: [UINT8_COUNT]Upvalue,
    scope_depth: u8,

    fn init(self: *Self, kind: FunctionKind) !void {
        self.enclosing = current;
        self.function = undefined; // clox uses NULL, but I don't wanna have ?* type
        self.kind = kind;
        self.local_count = 0;
        self.scope_depth = 0;
        self.function = try Obj.Function.init();
        current = self;
        if (kind != .script) {
            current.function.name = try Obj.String.copy(parser.previous.text);
        }

        const local = &current.locals[current.local_count];
        current.local_count += 1;
        local.depth = 0;
        local.is_captured = false;
        local.name.text = "";
    }
};

const FunctionKind = enum {
    function,
    script,
};

const Local = struct {
    name: Scanner.Token,
    depth: ?u8,
    is_captured: bool,
};

const Upvalue = struct {
    index: u8,
    is_local: bool,
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

pub fn compile(source: []const u8) !?*Obj.Function {
    scanner.init(source);
    var compiler: Compiler = undefined;
    try compiler.init(.script);
    parser.had_error = false;
    parser.panic_mode = false;
    string_constants = Table.init();
    defer string_constants.deinit();
    advance();
    while (!match(.eof)) {
        try declaration();
    }
    const fun = end_compiler();
    return if (parser.had_error) null else fun;
}

pub fn mark_compiler_roots() void {
    var compiler: ?*Compiler = current;
    while (compiler) |c| {
        memory.mark_object(&c.function.obj);
        compiler = c.enclosing;
    }
}

fn declaration() !void {
    if (match(.fun)) {
        try fun_declaration();
    } else if (match(.var_)) {
        try var_declaration();
    } else {
        try statement();
    }
    if (parser.panic_mode) synchronize();
}

fn fun_declaration() !void {
    const global = try parse_variable("Expected function name.");
    mark_initialized();
    try function(.function);
    try define_variable(global);
}

fn var_declaration() !void {
    const slot = try parse_variable("Expected variable name.");
    if (match(.equal)) {
        try expression();
    } else {
        try emit_byte(op_u8(.op_nil));
    }
    consume(.semicolon, "Expected ';' after variable declaration.");
    try define_variable(slot);
}

fn statement() !void {
    if (match(.print)) {
        try print_statement();
    } else if (match(.for_)) {
        try for_statement();
    } else if (match(.if_)) {
        try if_statement();
    } else if (match(.return_)) {
        try return_statement();
    } else if (match(.while_)) {
        try while_statement();
    } else if (match(.left_brace)) {
        begin_scope();
        try block();
        try end_scope();
    } else {
        try expression_statement();
    }
}

fn print_statement() !void {
    try expression();
    consume(.semicolon, "Expected ';' after value.");
    try emit_byte(op_u8(.op_print));
}

fn for_statement() Allocator.Error!void {
    begin_scope();
    consume(.left_paren, "Expected '(' after 'for'.");
    if (match(.semicolon)) {
        // no initialier.
    } else if (match(.var_)) {
        try var_declaration();
    } else {
        try expression_statement();
    }

    var loop_start = current_chunk().code.items.len;
    var exit_jump: ?usize = null;
    if (!match(.semicolon)) {
        try expression();
        consume(.semicolon, "Expected ';' after loop condition.");
        // jump out of the loop if the condition is false.
        exit_jump = try emit_jump(.op_jump_if_false);
        try emit_byte(op_u8(.op_pop));
    }

    if (!match(.right_paren)) {
        const body_jump = try emit_jump(.op_jump);
        const increment_start = current_chunk().code.items.len;
        try expression();
        try emit_byte(op_u8(.op_pop));
        consume(.right_paren, "Expected ')' after 'for' clauses.");
        try emit_loop(loop_start);
        loop_start = increment_start;
        patch_jump(body_jump);
    }

    try statement();
    try emit_loop(loop_start);

    if (exit_jump) |offset| {
        patch_jump(offset);
        try emit_byte(op_u8(.op_pop)); // condition.
    }

    try end_scope();
}

fn if_statement() Allocator.Error!void {
    consume(.left_paren, "Expected '(' after 'if'.");
    try expression();
    consume(.right_paren, "Expected ')' after condition.");

    const then_jump = try emit_jump(.op_jump_if_false);
    try emit_byte(op_u8(.op_pop));
    try statement();

    const else_jump = try emit_jump(.op_jump);
    patch_jump(then_jump);
    try emit_byte(op_u8(.op_pop));

    if (match(.else_)) try statement();
    patch_jump(else_jump);
}

fn return_statement() Allocator.Error!void {
    if (current.kind == .script) {
        error_("Can't return from top-level code.");
    }
    if (match(.semicolon)) {
        try emit_return();
    } else {
        try expression();
        consume(.semicolon, "Expected ';' after return value.");
        try emit_byte(op_u8(.op_return));
    }
}

fn while_statement() Allocator.Error!void {
    const loop_start = current_chunk().code.items.len;
    consume(.left_paren, "Expected '(' after 'while'.");
    try expression();
    consume(.right_paren, "Expected ')' after condition.");
    const exit_jump = try emit_jump(.op_jump_if_false);
    try emit_byte(op_u8(.op_pop));
    try statement();
    try emit_loop(loop_start);
    patch_jump(exit_jump);
    try emit_byte(op_u8(.op_pop));
}

fn block() Allocator.Error!void {
    while (!check(.right_brace) and !check(.eof)) {
        try declaration();
    }
    consume(.right_brace, "Expected '}' after block.");
}

fn function(kind: FunctionKind) !void {
    var compiler: Compiler = undefined;
    try compiler.init(kind);
    begin_scope();

    consume(.left_paren, "Expected '(' after function name.");
    if (!check(.right_paren)) {
        while (true) {
            if (current.function.arity == 255) {
                error_at_current("Can't have more than 255 parameters.");
            }
            current.function.arity += 1;
            const constant = try parse_variable("Expected parameter name.");
            try define_variable(constant);
            if (!match(.comma)) break;
        }
    }
    consume(.right_paren, "Expected ')' after parameters.");
    consume(.left_brace, "Expected '{' before function body.");
    try block();

    const fun = end_compiler();
    const constant = try make_constant(fun.obj.value());
    try emit_two(op_u8(.op_closure), @intCast(constant));

    for (0..fun.upvalue_count) |i| {
        try emit_byte(if (compiler.upvalues[i].is_local) 1 else 0);
        try emit_byte(compiler.upvalues[i].index);
    }
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
    if (current.scope_depth > 0) {
        mark_initialized();
        return;
    }
    try emit_two(op_u8(.op_define_global), global);
}

fn argument_list() !u8 {
    var arg_count: u8 = 0;
    if (!check(.right_paren)) {
        while (true) {
            try expression();
            if (arg_count == 255) {
                error_("Can't have more than 255 arguments.");
            }
            arg_count += 1;
            if (!match(.comma)) break;
        }
    }
    consume(.right_paren, "Expected ')' after arguments.");
    return arg_count;
}

fn mark_initialized() void {
    if (current.scope_depth == 0) return;
    current.locals[current.local_count - 1].depth = current.scope_depth;
}

fn declare_variable() void {
    if (current.scope_depth == 0) return;
    const name = &parser.previous;
    var i: u8 = current.local_count;
    while (i > 0) {
        i -= 1;
        const local = &current.locals[i];
        if (local.depth != null and local.depth.? < current.scope_depth) {
            break;
        }
        if (identifiers_equal(name, &local.name)) {
            error_("Variable with this name already declared in this scope.");
        }
    }
    add_local(name.*);
}

fn add_local(name: Scanner.Token) void {
    if (current.local_count == std.math.maxInt(u8)) {
        error_("Too many local variables in function.");
        return;
    }
    const local = &current.locals[current.local_count];
    current.local_count += 1;
    local.name = name;
    local.depth = null;
    local.is_captured = false;
}

fn parse_variable(error_message: []const u8) !u8 {
    consume(.identifier, error_message);
    declare_variable();
    if (current.scope_depth > 0) return 0;
    return try identifier_constant(&parser.previous);
}

fn identifier_constant(name: *const Scanner.Token) !u8 {
    const ident = try Obj.String.copy(name.text);
    if (vm.vm.global_names.get(ident)) |index|
        return @intFromFloat(index.number);
    try vm.vm.global_values.push(Value.undefined_);
    const index = vm.vm.global_values.items.len - 1;
    _ = try vm.vm.global_names.insert(ident, Value{ .number = @floatFromInt(index) });
    return @intCast(index);
}

fn resolve_local(compiler: *const Compiler, name: *const Scanner.Token) ?u8 {
    var i = compiler.local_count;
    while (i > 0) {
        i -= 1;
        const local = &compiler.locals[i];
        if (identifiers_equal(name, &local.name)) {
            if (local.depth == null)
                error_("Cannot read local variable in its own initializer.");
            return i;
        }
    }
    return null;
}

fn resolve_upvalue(compiler: *Compiler, name: *const Scanner.Token) ?u8 {
    const enclosing = compiler.enclosing orelse return null;
    if (resolve_local(enclosing, name)) |local| {
        enclosing.locals[local].is_captured = true;
        return add_upvalue(compiler, local, true);
    }
    if (resolve_upvalue(enclosing, name)) |upvalue| {
        return add_upvalue(compiler, upvalue, false);
    }
    return null;
}

fn add_upvalue(compiler: *Compiler, index: u8, is_local: bool) u8 {
    const upvalue_count = compiler.function.upvalue_count;
    // see if there's an existing upvalue with the same values
    for (0..upvalue_count) |i| {
        const upvalue = &compiler.upvalues[i];
        if (upvalue.index == index and upvalue.is_local == is_local) {
            return @intCast(i);
        }
    }
    if (upvalue_count == std.math.maxInt(u8)) {
        error_("Too many closure variables in function.");
        return 0;
    }
    // create a new upvalue
    compiler.upvalues[upvalue_count].is_local = is_local;
    compiler.upvalues[upvalue_count].index = index;
    compiler.function.upvalue_count += 1;
    return upvalue_count;
}

fn named_variable(name: Scanner.Token, can_assign: bool) !void {
    var arg: u8 = undefined;
    var get_op: OpCode = undefined;
    var set_op: OpCode = undefined;
    if (resolve_local(current, &name)) |local| {
        arg = local;
        get_op = .op_get_local;
        set_op = .op_set_local;
    } else if (resolve_upvalue(current, &name)) |upvalue| {
        arg = upvalue;
        get_op = .op_get_upvalue;
        set_op = .op_set_upvalue;
    } else {
        arg = try identifier_constant(&name);
        get_op = .op_get_global;
        set_op = .op_set_global;
    }
    if (can_assign and match(.equal)) {
        try expression();
        try emit_two(op_u8(set_op), arg);
    } else {
        try emit_two(op_u8(get_op), arg);
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

fn consume(kind: Scanner.TokenType, message: []const u8) void {
    if (parser.current.kind == kind) {
        advance();
        return;
    }
    error_at_current(message);
}

fn match(kind: Scanner.TokenType) bool {
    if (!check(kind)) return false;
    advance();
    return true;
}

fn check(kind: Scanner.TokenType) bool {
    return parser.current.kind == kind;
}

fn emit_byte(byte: u8) !void {
    try current_chunk().write(byte, parser.previous.line);
}

fn emit_return() !void {
    try emit_byte(op_u8(.op_nil));
    try emit_byte(op_u8(.op_return));
}

fn emit_two(byte1: u8, byte2: u8) !void {
    try emit_byte(byte1);
    try emit_byte(byte2);
}

fn emit_jump(instruction: OpCode) !usize {
    try emit_byte(op_u8(instruction));
    try emit_two(0xff, 0xff);
    // the index of the first byte of the jump offset.
    return current_chunk().code.items.len - 2;
}

fn emit_loop(loop_start: usize) !void {
    try emit_byte(op_u8(.op_loop));
    const offset = current_chunk().code.items.len - loop_start + 2;
    if (offset > std.math.maxInt(u16)) error_("Loop body too large.");
    try emit_byte(@intCast((offset >> 8) & 0xff));
    try emit_byte(@intCast(offset & 0xff));
}

fn patch_jump(offset: usize) void {
    const jump = current_chunk().code.items.len - offset - 2;
    if (jump > std.math.maxInt(u16)) {
        error_("Too much code to jump over.");
    }
    current_chunk().code.items[offset] = @intCast((jump >> 8) & 0xff);
    current_chunk().code.items[offset + 1] = @intCast(jump & 0xff);
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

fn error_at(token: *Scanner.Token, message: []const u8) void {
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

fn end_compiler() *Obj.Function {
    emit_return() catch unreachable;
    const fun = current.function;
    if (config.print_code) {
        if (!parser.had_error) {
            @import("debug.zig").disassemble_chunk(current_chunk(), if (fun.name) |name|
                name.value.as_slice()
            else
                "<script>");
        }
    }
    if (current.enclosing) |compiler| current = compiler;
    return fun;
}

fn begin_scope() void {
    current.scope_depth += 1;
}

fn end_scope() !void {
    current.scope_depth -= 1;
    while (current.local_count > 0 and
        current.locals[current.local_count - 1].depth orelse 0 >
        current.scope_depth)
    {
        if (current.locals[current.local_count - 1].is_captured) {
            try emit_byte(op_u8(.op_close_upvalue));
        } else {
            try emit_byte(op_u8(.op_pop));
        }
        current.local_count -= 1;
    }
}

inline fn identifiers_equal(a: *const Scanner.Token, b: *const Scanner.Token) bool {
    return std.mem.eql(u8, a.text, b.text);
}

fn current_chunk() *Chunk {
    return &current.function.chunk;
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
    const s = try Obj.String.copy(chars);
    try emit_constant(s.obj.value());
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

fn call(can_assign: bool) Allocator.Error!void {
    _ = can_assign;
    const arg_count = try argument_list();
    try emit_two(op_u8(.op_call), arg_count);
}

/// Parse logical and as control flow.
fn and_(can_assign: bool) Allocator.Error!void {
    _ = can_assign;
    const end_jump = try emit_jump(.op_jump_if_false);
    try emit_byte(op_u8(.op_pop));
    try parse_precedence(.and_);
    patch_jump(end_jump);
}

fn or_(can_assign: bool) Allocator.Error!void {
    _ = can_assign;
    const else_jump = try emit_jump(.op_jump_if_false);
    const end_jump = try emit_jump(.op_jump);
    patch_jump(else_jump);
    try emit_byte(op_u8(.op_pop));
    try parse_precedence(.or_);
    patch_jump(end_jump);
}

/// Parse table entry.
const ParseRule = struct {
    prefix: ?*const fn (bool) Allocator.Error!void,
    infix: ?*const fn (bool) Allocator.Error!void,
    precedence: Precedence,
};

/// Parse table. Maps token types to parsing functions and precedence.
const rules = std.EnumArray(Scanner.TokenType, ParseRule).init(.{
    .left_paren = .{ .prefix = grouping, .infix = call, .precedence = .call },
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
    .and_ = .{ .prefix = null, .infix = and_, .precedence = .and_ },
    .class = .{ .prefix = null, .infix = null, .precedence = .none },
    .else_ = .{ .prefix = null, .infix = null, .precedence = .none },
    .false_ = .{ .prefix = literal, .infix = null, .precedence = .none },
    .for_ = .{ .prefix = null, .infix = null, .precedence = .none },
    .fun = .{ .prefix = null, .infix = null, .precedence = .none },
    .if_ = .{ .prefix = null, .infix = null, .precedence = .none },
    .nil = .{ .prefix = literal, .infix = null, .precedence = .none },
    .or_ = .{ .prefix = null, .infix = or_, .precedence = .or_ },
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
