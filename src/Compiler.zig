const std = @import("std");
const config = @import("config");
const Error = std.mem.Allocator.Error;
const Chunk = @import("Chunk.zig");
const OpCode = Chunk.OpCode;
const Obj = @import("Obj.zig");
const Scanner = @import("Scanner.zig");
const Table = @import("Table.zig");
const Value = @import("value.zig").Value;
const Vm = @import("Vm.zig");

const UINT8_COUNT = @import("main.zig").UINT8_COUNT;
const Compiler = @This();

vm: *Vm,
valid: bool,
parser: Parser,
current: *Node,
current_class: ?*Class,
compiling_chunk: *Chunk,
string_constants: Table,
scanner: Scanner,

pub fn init(self: *Compiler, vm: *Vm) void {
    self.vm = vm;
    self.valid = false;
}

pub fn compile(self: *Compiler, source: []const u8) !?*Obj.Function {
    defer self.valid = false;
    self.scanner.init(source);

    self.parser.had_error = false;
    self.parser.panic_mode = false;

    self.string_constants = Table.init(self.vm.allocator);
    defer self.string_constants.deinit();

    self.current_class = null;
    // each compiler node lives entirely on the stack
    var node: Node = undefined;
    try node.init(self, .script);
    self.valid = true;

    self.advance();
    while (!self.match(.eof)) {
        try self.declaration();
    }
    const fun = node.end();
    return if (self.parser.had_error) null else fun;
}

const Class = struct {
    enclosing: ?*Class,
};

const Parser = struct {
    current: Scanner.Token,
    previous: Scanner.Token,
    had_error: bool,
    panic_mode: bool,
};

pub const Node = struct {
    compiler: *Compiler,
    enclosing: ?*Node,
    function: *Obj.Function,
    kind: FunctionKind,
    locals: [UINT8_COUNT]Local,
    local_count: u8,
    upvalues: [UINT8_COUNT]Upvalue,
    scope_depth: u8,

    pub fn init(self: *Node, compiler: *Compiler, kind: FunctionKind) !void {
        self.compiler = compiler;
        self.enclosing = if (kind == .script) null else compiler.current;
        self.function = undefined; // clox uses NULL, but I don't wanna have ?* type
        self.kind = kind;
        self.local_count = 0;
        self.scope_depth = 0;
        self.function = try Obj.Function.init(compiler.vm);
        compiler.current = self;
        if (kind != .script) {
            self.function.name = try Obj.String.copy(compiler.vm, compiler.parser.previous.text);
        }

        const local = &self.locals[self.local_count];
        self.local_count += 1;
        local.depth = 0;
        local.is_captured = false;
        local.name.text = if (kind != .function) "this" else "";
    }

    pub fn end(self: *Node) *Obj.Function {
        self.compiler.emit_return() catch unreachable;
        const fun = self.function;
        if (config.print_code) {
            if (!self.compiler.parser.had_error) {
                @import("debug.zig").disassemble_chunk(
                    self.compiler.current_chunk(),
                    if (fun.name) |name|
                        name.value.as_slice()
                    else
                        "<script>",
                );
            }
        }
        if (self.enclosing) |n| self.compiler.current = n;
        return fun;
    }

    pub fn resolve_local(self: *const Node, name: *const Scanner.Token) ?u8 {
        var i = self.local_count;
        while (i > 0) {
            i -= 1;
            const local = &self.locals[i];
            if (name.eql(&local.name)) {
                if (local.depth == null)
                    self.compiler.error_("Cannot read local variable in its own initializer.");
                return i;
            }
        }
        return null;
    }

    pub fn resolve_upvalue(self: *Node, name: *const Scanner.Token) ?u8 {
        const enclosing = self.enclosing orelse return null;
        if (enclosing.resolve_local(name)) |local| {
            enclosing.locals[local].is_captured = true;
            return self.add_upvalue(local, true);
        }
        if (enclosing.resolve_upvalue(name)) |upvalue| {
            return self.add_upvalue(upvalue, false);
        }
        return null;
    }

    pub fn add_upvalue(self: *Node, index: u8, is_local: bool) u8 {
        const upvalue_count = self.function.upvalue_count;
        // see if there's an existing upvalue with the same values
        for (0..upvalue_count) |i| {
            const upvalue = &self.upvalues[i];
            if (upvalue.index == index and upvalue.is_local == is_local) {
                return @intCast(i);
            }
        }
        if (upvalue_count == std.math.maxInt(u8)) {
            self.compiler.error_("Too many closure variables in function.");
            return 0;
        }
        // create a new upvalue
        self.upvalues[upvalue_count].is_local = is_local;
        self.upvalues[upvalue_count].index = index;
        self.function.upvalue_count += 1;
        return upvalue_count;
    }
};

pub const FunctionKind = enum {
    function,
    initializer,
    method,
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

    fn higher(self: *const Precedence) Precedence {
        return @enumFromInt(self.int() + 1);
    }

    fn int(self: *const Precedence) u8 {
        return @intFromEnum(self.*);
    }
};

fn declaration(self: *Compiler) !void {
    if (self.match(.class)) {
        try self.class_declaration();
    } else if (self.match(.fun)) {
        try self.fun_declaration();
    } else if (self.match(.var_)) {
        try self.var_declaration();
    } else {
        try self.statement();
    }
    if (self.parser.panic_mode) self.synchronize();
}

fn class_declaration(self: *Compiler) !void {
    self.consume(.identifier, "Expected class name.");

    const class_name = self.parser.previous;
    var name: *Obj.String = undefined;
    const class_slot = try self.identifier_constant(&self.parser.previous, &name);
    var class_obj: *Obj.Class = undefined;
    // create the class object at compile time.
    {
        self.vm.push(name.obj.value());
        defer _ = self.vm.pop(); // gc dance
        class_obj = try Obj.Class.init(self.vm, name);
    }
    self.vm.global_values.items[class_slot] = class_obj.obj.value();

    self.declare_variable();
    try self.emit_two(op_u8(.op_class), class_slot);
    try self.define_variable(class_slot);

    var class = Class{ .enclosing = self.current_class };
    self.current_class = &class;
    defer self.current_class = class.enclosing;

    try self.named_variable(class_name, false);
    self.consume(.left_brace, "Expected '{' before class body.");
    while (!self.check(.right_brace) and !self.check(.eof)) {
        try self.method();
    }
    self.consume(.right_brace, "Expected '}' after class body.");
    try self.emit_byte(op_u8(.op_pop));
}

fn method(self: *Compiler) !void {
    self.consume(.identifier, "Expected method name.");
    var name: *Obj.String = undefined;
    const name_slot = try self.identifier_constant(&self.parser.previous, &name);
    self.vm.global_values.items[name_slot] = name.obj.value();
    const kind: FunctionKind =
        if (std.mem.eql(u8, "init", self.parser.previous.text)) .initializer else .method;
    try self.function(kind);
    try self.emit_two(op_u8(.op_method), name_slot);
}

fn fun_declaration(self: *Compiler) !void {
    const global = try self.parse_variable("Expected function name.");
    self.mark_initialized();
    try self.function(.function);
    try self.define_variable(global);
}

fn var_declaration(self: *Compiler) !void {
    const slot = try self.parse_variable("Expected variable name.");
    if (self.match(.equal)) {
        try self.expression();
    } else {
        try self.emit_byte(op_u8(.op_nil));
    }
    self.consume(.semicolon, "Expected ';' after variable declaration.");
    try self.define_variable(slot);
}

fn statement(self: *Compiler) !void {
    if (self.match(.print)) {
        try self.print_statement();
    } else if (self.match(.for_)) {
        try self.for_statement();
    } else if (self.match(.if_)) {
        try self.if_statement();
    } else if (self.match(.return_)) {
        try self.return_statement();
    } else if (self.match(.while_)) {
        try self.while_statement();
    } else if (self.match(.left_brace)) {
        self.begin_scope();
        try self.block();
        try self.end_scope();
    } else {
        try self.expression_statement();
    }
}

fn print_statement(self: *Compiler) !void {
    try self.expression();
    self.consume(.semicolon, "Expected ';' after value.");
    try self.emit_byte(op_u8(.op_print));
}

fn for_statement(self: *Compiler) Error!void {
    self.begin_scope();
    self.consume(.left_paren, "Expected '(' after 'for'.");
    if (self.match(.semicolon)) {
        // no initialier.
    } else if (self.match(.var_)) {
        try self.var_declaration();
    } else {
        try self.expression_statement();
    }

    var loop_start = self.current_chunk().code.items.len;
    var exit_jump: ?usize = null;
    if (!self.match(.semicolon)) {
        try self.expression();
        self.consume(.semicolon, "Expected ';' after loop condition.");
        // jump out of the loop if the condition is false.
        exit_jump = try self.emit_jump(.op_jump_if_false);
        try self.emit_byte(op_u8(.op_pop));
    }

    if (!self.match(.right_paren)) {
        const body_jump = try self.emit_jump(.op_jump);
        const increment_start = self.current_chunk().code.items.len;
        try self.expression();
        try self.emit_byte(op_u8(.op_pop));
        self.consume(.right_paren, "Expected ')' after 'for' clauses.");
        try self.emit_loop(loop_start);
        loop_start = increment_start;
        self.patch_jump(body_jump);
    }

    try self.statement();
    try self.emit_loop(loop_start);

    if (exit_jump) |offset| {
        self.patch_jump(offset);
        try self.emit_byte(op_u8(.op_pop)); // condition.
    }

    try self.end_scope();
}

fn if_statement(self: *Compiler) Error!void {
    self.consume(.left_paren, "Expected '(' after 'if'.");
    try self.expression();
    self.consume(.right_paren, "Expected ')' after condition.");

    const then_jump = try self.emit_jump(.op_jump_if_false);
    try self.emit_byte(op_u8(.op_pop));
    try self.statement();

    const else_jump = try self.emit_jump(.op_jump);
    self.patch_jump(then_jump);
    try self.emit_byte(op_u8(.op_pop));

    if (self.match(.else_)) try self.statement();
    self.patch_jump(else_jump);
}

fn return_statement(self: *Compiler) Error!void {
    if (self.current.kind == .script) {
        self.error_("Can't return from top-level code.");
    }
    if (self.match(.semicolon)) {
        try self.emit_return();
    } else {
        if (self.current.kind == .initializer and !self.check(.semicolon)) {
            self.error_("Can't return a value from an initializer.");
        }
        try self.expression();
        self.consume(.semicolon, "Expected ';' after return value.");
        try self.emit_byte(op_u8(.op_return));
    }
}

fn while_statement(self: *Compiler) Error!void {
    const loop_start = self.current_chunk().code.items.len;
    self.consume(.left_paren, "Expected '(' after 'while'.");
    try self.expression();
    self.consume(.right_paren, "Expected ')' after condition.");
    const exit_jump = try self.emit_jump(.op_jump_if_false);
    try self.emit_byte(op_u8(.op_pop));
    try self.statement();
    try self.emit_loop(loop_start);
    self.patch_jump(exit_jump);
    try self.emit_byte(op_u8(.op_pop));
}

fn block(self: *Compiler) Error!void {
    while (!self.check(.right_brace) and !self.check(.eof)) {
        try self.declaration();
    }
    self.consume(.right_brace, "Expected '}' after block.");
}

fn function(self: *Compiler, kind: FunctionKind) !void {
    var node: Node = undefined;
    try node.init(self, kind);
    self.begin_scope();

    self.consume(.left_paren, "Expected '(' after function name.");
    if (!self.check(.right_paren)) {
        while (true) {
            if (self.current.function.arity == 255) {
                self.error_at_current("Can't have more than 255 parameters.");
            }
            self.current.function.arity += 1;
            const constant = try self.parse_variable("Expected parameter name.");
            try self.define_variable(constant);
            if (!self.match(.comma)) break;
        }
    }
    self.consume(.right_paren, "Expected ')' after parameters.");
    self.consume(.left_brace, "Expected '{' before function body.");
    try self.block();

    const fun = node.end();
    const constant = try self.make_constant(fun.obj.value());
    try self.emit_two(op_u8(.op_closure), constant);

    for (0..fun.upvalue_count) |i| {
        try self.emit_byte(if (node.upvalues[i].is_local) 1 else 0);
        try self.emit_byte(node.upvalues[i].index);
    }
}

fn expression_statement(self: *Compiler) !void {
    try self.expression();
    self.consume(.semicolon, "Expected ';' after expression.");
    try self.emit_byte(op_u8(.op_pop));
}

inline fn expression(self: *Compiler) Error!void {
    try self.parse_precedence(.assignment);
}

fn parse_precedence(self: *Compiler, precedence: Precedence) Error!void {
    self.advance();
    const prefix_rule = rules.get(self.parser.previous.kind).prefix orelse {
        self.error_("Expected expression.");
        return;
    };
    const can_assign = precedence.int() <= Precedence.assignment.int();
    try prefix_rule(self, can_assign);
    while (precedence.int() <= rules.get(self.parser.current.kind).precedence.int()) {
        self.advance();
        const infix_rule = rules.get(self.parser.previous.kind).infix orelse {
            unreachable;
        };
        try infix_rule(self, can_assign);
    }
    if (can_assign and self.match(.equal)) {
        self.error_("Invalid assignment target.");
    }
}

fn define_variable(self: *Compiler, global: u8) !void {
    if (self.current.scope_depth > 0) {
        self.mark_initialized();
        return;
    }
    try self.emit_two(op_u8(.op_define_global), global);
}

fn argument_list(self: *Compiler) !u8 {
    var arg_count: u8 = 0;
    if (!self.check(.right_paren)) {
        while (true) {
            try self.expression();
            if (arg_count == 255) {
                self.error_("Can't have more than 255 arguments.");
            }
            arg_count += 1;
            if (!self.match(.comma)) break;
        }
    }
    self.consume(.right_paren, "Expected ')' after arguments.");
    return arg_count;
}

inline fn mark_initialized(self: *Compiler) void {
    if (self.current.scope_depth == 0) return;
    self.current.locals[self.current.local_count - 1].depth = self.current.scope_depth;
}

fn declare_variable(self: *Compiler) void {
    if (self.current.scope_depth == 0) return;
    const name = &self.parser.previous;
    var i: u8 = self.current.local_count;
    while (i > 0) {
        i -= 1;
        const local = &self.current.locals[i];
        if (local.depth != null and local.depth.? < self.current.scope_depth) {
            break;
        }
        if (name.eql(&local.name)) {
            self.error_("Variable with this name already declared in this scope.");
        }
    }
    self.add_local(name.*);
}

fn add_local(self: *Compiler, name: Scanner.Token) void {
    if (self.current.local_count == std.math.maxInt(u8)) {
        self.error_("Too many local variables in function.");
        return;
    }
    const local = &self.current.locals[self.current.local_count];
    self.current.local_count += 1;
    local.name = name;
    local.depth = null;
    local.is_captured = false;
}

fn parse_variable(self: *Compiler, error_message: []const u8) !u8 {
    self.consume(.identifier, error_message);
    self.declare_variable();
    if (self.current.scope_depth > 0) return 0;
    return try self.identifier_constant(&self.parser.previous, null);
}

fn identifier_constant(self: *Compiler, name: *const Scanner.Token, name_string: ?**Obj.String) !u8 {
    const ident = try Obj.String.copy(self.vm, name.text);
    if (name_string) |s| s.* = ident;
    if (self.vm.global_names.get(ident)) |index|
        return @intFromFloat(index.number);

    self.vm.push(ident.obj.value());
    defer _ = self.vm.pop(); // gc dance

    try self.vm.global_values.push(self.vm.allocator, Value.undefined_);
    const index = self.vm.global_values.items.len - 1;
    _ = try self.vm.global_names.insert(ident, Value{ .number = @floatFromInt(index) });
    return @intCast(index);
}

fn named_variable(self: *Compiler, name: Scanner.Token, can_assign: bool) !void {
    var arg: u8 = undefined;
    var get_op: OpCode = undefined;
    var set_op: OpCode = undefined;
    if (self.current.resolve_local(&name)) |local| {
        arg = local;
        get_op = .op_get_local;
        set_op = .op_set_local;
    } else if (self.current.resolve_upvalue(&name)) |upvalue| {
        arg = upvalue;
        get_op = .op_get_upvalue;
        set_op = .op_set_upvalue;
    } else {
        arg = try self.identifier_constant(&name, null);
        get_op = .op_get_global;
        set_op = .op_set_global;
    }
    if (can_assign and self.match(.equal)) {
        try self.expression();
        try self.emit_two(op_u8(set_op), arg);
    } else {
        try self.emit_two(op_u8(get_op), arg);
    }
}

fn advance(self: *Compiler) void {
    self.parser.previous = self.parser.current;
    while (true) {
        self.parser.current = self.scanner.scan_token();
        if (self.parser.current.kind != .error_) {
            break;
        }
        self.error_at_current(self.parser.current.text);
    }
}

inline fn consume(self: *Compiler, kind: Scanner.TokenType, message: []const u8) void {
    if (self.parser.current.kind == kind) {
        self.advance();
        return;
    }
    self.error_at_current(message);
}

inline fn match(self: *Compiler, kind: Scanner.TokenType) bool {
    if (!self.check(kind)) return false;
    self.advance();
    return true;
}

inline fn check(self: *Compiler, kind: Scanner.TokenType) bool {
    return self.parser.current.kind == kind;
}

inline fn emit_byte(self: *Compiler, byte: u8) !void {
    try self.current_chunk().write(self.vm.allocator, byte, self.parser.previous.line);
}

pub inline fn emit_return(self: *Compiler) !void {
    if (self.current.kind == .initializer) {
        try self.emit_two(op_u8(.op_get_local), 0);
    } else {
        try self.emit_byte(op_u8(.op_nil));
    }
    try self.emit_byte(op_u8(.op_return));
}

inline fn emit_two(self: *Compiler, byte1: u8, byte2: u8) !void {
    try self.emit_byte(byte1);
    try self.emit_byte(byte2);
}

fn emit_jump(self: *Compiler, instruction: OpCode) !usize {
    try self.emit_byte(op_u8(instruction));
    try self.emit_two(0xff, 0xff);
    // the index of the first byte of the jump offset.
    return self.current_chunk().code.items.len - 2;
}

fn emit_loop(self: *Compiler, loop_start: usize) !void {
    try self.emit_byte(op_u8(.op_loop));
    const offset = self.current_chunk().code.items.len - loop_start + 2;
    if (offset > std.math.maxInt(u16)) self.error_("Loop body too large.");
    try self.emit_byte(@intCast((offset >> 8) & 0xff));
    try self.emit_byte(@intCast(offset & 0xff));
}

fn patch_jump(self: *Compiler, offset: usize) void {
    const jump = self.current_chunk().code.items.len - offset - 2;
    if (jump > std.math.maxInt(u16)) {
        self.error_("Too much code to jump over.");
    }
    self.current_chunk().code.items[offset] = @intCast((jump >> 8) & 0xff);
    self.current_chunk().code.items[offset + 1] = @intCast(jump & 0xff);
}

inline fn emit_constant(self: *Compiler, value: Value) !void {
    try self.emit_two(op_u8(.op_constant), try self.make_constant(value));
}

fn make_constant(self: *Compiler, value: Value) !u8 {
    self.vm.push(value);
    defer _ = self.vm.pop();
    const index = try self.current_chunk().add_constant(self.vm.allocator, value);
    if (index > std.math.maxInt(u8)) {
        self.error_("Too many constants in one chunk.");
        return 0;
    }
    return @intCast(index);
}

fn synchronize(self: *Compiler) void {
    self.parser.panic_mode = false;
    while (self.parser.current.kind != .eof) {
        if (self.parser.previous.kind == .semicolon) return;
        switch (self.parser.current.kind) {
            .class, .fun, .var_, .for_, .if_, .while_, .print, .return_ => return,
            else => {}, // Do nothing.
        }
        self.advance();
    }
}

pub inline fn error_(self: *Compiler, message: []const u8) void {
    self.error_at(&self.parser.previous, message);
}

inline fn error_at_current(self: *Compiler, message: []const u8) void {
    self.error_at(&self.parser.current, message);
}

fn error_at(self: *Compiler, token: *Scanner.Token, message: []const u8) void {
    if (self.parser.panic_mode) return;
    self.parser.panic_mode = true;
    self.vm.out_writer.print("[line {d}] Error", .{token.line}) catch unreachable;

    if (token.kind == .eof) {
        self.vm.out_writer.print(" at end", .{}) catch unreachable;
    } else if (token.kind == .error_) {
        // Nothing.
    } else {
        self.vm.out_writer.print(" at '{s}'", .{token.text}) catch unreachable;
    }
    self.vm.out_writer.print(": {s}\n", .{message}) catch unreachable;
    self.parser.had_error = true;
}

inline fn begin_scope(self: *Compiler) void {
    self.current.scope_depth += 1;
}

fn end_scope(self: *Compiler) !void {
    self.current.scope_depth -= 1;
    while (self.current.local_count > 0 and
        self.current.locals[self.current.local_count - 1].depth orelse 0 >
        self.current.scope_depth)
    {
        if (self.current.locals[self.current.local_count - 1].is_captured) {
            try self.emit_byte(op_u8(.op_close_upvalue));
        } else {
            try self.emit_byte(op_u8(.op_pop));
        }
        self.current.local_count -= 1;
    }
}

pub inline fn current_chunk(self: *Compiler) *Chunk {
    return &self.current.function.chunk;
}

inline fn op_u8(op: OpCode) u8 {
    return @intFromEnum(op);
}

/// Parse a number literal.
fn number(self: *Compiler, can_assign: bool) Error!void {
    _ = can_assign;
    const value = std.fmt.parseFloat(f64, self.parser.previous.text) catch unreachable;
    try self.emit_constant(Value{ .number = value });
}

/// Parse booleans and nil.
fn literal(self: *Compiler, can_assign: bool) Error!void {
    _ = can_assign;
    switch (self.parser.previous.kind) {
        .false_ => try self.emit_byte(op_u8(.op_false)),
        .true_ => try self.emit_byte(op_u8(.op_true)),
        .nil => try self.emit_byte(op_u8(.op_nil)),
        else => unreachable,
    }
}

/// Parse string literals.
fn string(self: *Compiler, can_assign: bool) !void {
    _ = can_assign;
    const end = self.parser.previous.text.len - 1;
    const chars = self.parser.previous.text[1..end]; // remove the quotes
    const s = try Obj.String.copy(self.vm, chars);
    try self.emit_constant(s.obj.value());
}

/// Parse variable names.
fn variable(self: *Compiler, can_assign: bool) !void {
    try self.named_variable(self.parser.previous, can_assign);
}

/// Parse parenthesized expressions.
fn grouping(self: *Compiler, can_assign: bool) Error!void {
    _ = can_assign;
    try self.expression();
    self.consume(.right_paren, "Expected ')' after expression.");
}

/// Parse unary operators.
fn unary(self: *Compiler, can_assign: bool) Error!void {
    _ = can_assign;
    const op_kind = self.parser.previous.kind;
    // compile the operand.
    try self.parse_precedence(.unary);
    // emit the operator instruction.
    try switch (op_kind) {
        .bang => self.emit_byte(op_u8(.op_not)),
        .minus => self.emit_byte(op_u8(.op_negate)),
        else => unreachable,
    };
}

/// Parse binary operators.
fn binary(self: *Compiler, can_assign: bool) Error!void {
    _ = can_assign;
    const op_kind = self.parser.previous.kind;
    const rule = rules.get(op_kind);
    try self.parse_precedence(rule.precedence.higher());
    try switch (op_kind) {
        .bang_equal => self.emit_two(op_u8(.op_equal), op_u8(.op_not)),
        .equal_equal => self.emit_byte(op_u8(.op_equal)),
        .greater => self.emit_byte(op_u8(.op_greater)),
        .greater_equal => self.emit_two(op_u8(.op_less), op_u8(.op_not)),
        .less => self.emit_byte(op_u8(.op_less)),
        .less_equal => self.emit_two(op_u8(.op_greater), op_u8(.op_not)),
        .plus => self.emit_byte(op_u8(.op_add)),
        .minus => self.emit_byte(op_u8(.op_subtract)),
        .star => self.emit_byte(op_u8(.op_multiply)),
        .slash => self.emit_byte(op_u8(.op_divide)),
        else => unreachable,
    };
}

fn call(self: *Compiler, can_assign: bool) Error!void {
    _ = can_assign;
    const arg_count = try self.argument_list();
    try self.emit_two(op_u8(.op_call), arg_count);
}

/// Parse logical and as control flow.
fn and_(self: *Compiler, can_assign: bool) Error!void {
    _ = can_assign;
    const end_jump = try self.emit_jump(.op_jump_if_false);
    try self.emit_byte(op_u8(.op_pop));
    try self.parse_precedence(.and_);
    self.patch_jump(end_jump);
}

fn or_(self: *Compiler, can_assign: bool) Error!void {
    _ = can_assign;
    const else_jump = try self.emit_jump(.op_jump_if_false);
    const end_jump = try self.emit_jump(.op_jump);
    self.patch_jump(else_jump);
    try self.emit_byte(op_u8(.op_pop));
    try self.parse_precedence(.or_);
    self.patch_jump(end_jump);
}

fn dot(self: *Compiler, can_assign: bool) Error!void {
    self.consume(.identifier, "Expected property name after '.'.");
    var s: *Obj.String = undefined;
    const name = try self.identifier_constant(&self.parser.previous, &s);
    self.vm.global_values.items[name] = s.obj.value();
    if (can_assign and self.match(.equal)) {
        try self.expression();
        try self.emit_two(op_u8(.op_set_property), name);
    } else if (self.match(.left_paren)) {
        const arg_count = try self.argument_list();
        try self.emit_two(op_u8(.op_invoke), name);
        try self.emit_byte(arg_count);
    } else {
        try self.emit_two(op_u8(.op_get_property), name);
    }
}

fn this(self: *Compiler, can_assign: bool) Error!void {
    _ = can_assign;
    if (self.current_class == null) {
        self.error_("Can't use 'this' outside of a class.");
        return;
    }
    try self.variable(false);
}

/// Parse table entry.
const ParseRule = struct {
    prefix: ?*const fn (*Compiler, bool) Error!void,
    infix: ?*const fn (*Compiler, bool) Error!void,
    precedence: Precedence,
};

/// Parse table. Maps token types to parsing functions and precedence.
const rules = std.EnumArray(Scanner.TokenType, ParseRule).init(.{
    .left_paren = .{ .prefix = grouping, .infix = call, .precedence = .call },
    .right_paren = .{ .prefix = null, .infix = null, .precedence = .none },
    .left_brace = .{ .prefix = null, .infix = null, .precedence = .none },
    .right_brace = .{ .prefix = null, .infix = null, .precedence = .none },
    .comma = .{ .prefix = null, .infix = null, .precedence = .none },
    .dot = .{ .prefix = null, .infix = dot, .precedence = .call },
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
    .this = .{ .prefix = this, .infix = null, .precedence = .none },
    .true_ = .{ .prefix = literal, .infix = null, .precedence = .none },
    .var_ = .{ .prefix = null, .infix = null, .precedence = .none },
    .while_ = .{ .prefix = null, .infix = null, .precedence = .none },
    .error_ = .{ .prefix = null, .infix = null, .precedence = .none },
    .eof = .{ .prefix = null, .infix = null, .precedence = .none },
});
