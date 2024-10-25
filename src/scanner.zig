const std = @import("std");

const Scanner = @This();

source: []const u8,
current: usize,
line: usize,

pub const Token = struct {
    kind: TokenType,
    text: []const u8,
    line: usize,
};

pub const TokenType = enum {
    left_paren,
    right_paren,
    left_brace,
    right_brace,
    comma,
    dot,
    minus,
    plus,
    semicolon,
    slash,
    star,

    bang,
    bang_equal,
    equal,
    equal_equal,
    greater,
    greater_equal,
    less,
    less_equal,

    identifier,
    string,
    number,

    and_,
    class,
    else_,
    false_,
    for_,
    fun,
    if_,
    nil,
    or_,
    print,
    return_,
    super,
    this,
    true_,
    var_,
    while_,

    error_,
    eof,
};

pub fn init(self: *Scanner, source: []const u8) void {
    self.source = source;
    self.current = 0;
    self.line = 1;
}

pub fn scan_token(self: *Scanner) Token {
    self.skip_whitespace();
    self.source = self.source[self.current..];
    self.current = 0;

    if (self.is_at_end()) {
        return self.make_token(.eof);
    }
    const c = self.advance();
    if (is_alpha(c)) return self.identifier();
    if (is_digit(c)) return self.number();
    switch (c) {
        '(' => return self.make_token(.left_paren),
        ')' => return self.make_token(.right_paren),
        '{' => return self.make_token(.left_brace),
        '}' => return self.make_token(.right_brace),
        ';' => return self.make_token(.semicolon),
        ',' => return self.make_token(.comma),
        '.' => return self.make_token(.dot),
        '-' => return self.make_token(.minus),
        '+' => return self.make_token(.plus),
        '/' => return self.make_token(.slash),
        '*' => return self.make_token(.star),
        '!' => return self.make_token(if (self.match('=')) .bang_equal else .bang),
        '=' => return self.make_token(if (self.match('=')) .equal_equal else .equal),
        '<' => return self.make_token(if (self.match('=')) .less_equal else .less),
        '>' => return self.make_token(if (self.match('=')) .greater_equal else .greater),
        '"' => return self.string(),
        else => return self.error_token("Unexpected character."),
    }
}

fn string(self: *Scanner) Token {
    while (self.peek() != '"' and !self.is_at_end()) {
        if (self.peek() == '\n') self.line += 1;
        _ = self.advance();
    }
    if (self.is_at_end()) return self.error_token("Unterminated string.");
    _ = self.advance();
    return self.make_token(.string);
}

fn identifier(self: *Scanner) Token {
    while (is_alpha(self.peek()) or is_digit(self.peek()))
        _ = self.advance();
    return self.make_token(self.identifier_type());
}

fn identifier_type(self: *Scanner) TokenType {
    switch (self.source[0]) {
        'a' => return self.check_keyword(1, "nd", .and_),
        'c' => return self.check_keyword(1, "lass", .class),
        'e' => return self.check_keyword(1, "lse", .else_),
        'f' => if (self.current > 1) {
            switch (self.source[1]) {
                'a' => return self.check_keyword(2, "lse", .false_),
                'o' => return self.check_keyword(2, "r", .for_),
                'u' => return self.check_keyword(2, "n", .fun),
                else => {},
            }
        },
        'i' => return self.check_keyword(1, "f", .if_),
        'n' => return self.check_keyword(1, "il", .nil),
        'o' => return self.check_keyword(1, "r", .or_),
        'p' => return self.check_keyword(1, "rint", .print),
        'r' => return self.check_keyword(1, "eturn", .return_),
        's' => return self.check_keyword(1, "uper", .super),
        't' => if (self.current > 1) {
            switch (self.source[1]) {
                'h' => return self.check_keyword(2, "is", .this),
                'r' => return self.check_keyword(2, "ue", .true_),
                else => {},
            }
        },
        'v' => return self.check_keyword(1, "ar", .var_),
        'w' => return self.check_keyword(1, "hile", .while_),
        else => {},
    }
    return .identifier;
}

fn check_keyword(self: *Scanner, start: usize, rest: []const u8, kind: TokenType) TokenType {
    const current = self.source[start..self.current];
    if (std.mem.eql(u8, current, rest)) return kind;
    return .identifier;
}

fn number(self: *Scanner) Token {
    while (is_digit(self.peek()))
        _ = self.advance();
    if (self.peek() == '.' and is_digit(self.peek_next())) {
        _ = self.advance(); // consume the ".".
        while (is_digit(self.peek())) _ = self.advance();
    }
    return self.make_token(.number);
}

inline fn advance(self: *Scanner) u8 {
    self.current += 1;
    return self.source[self.current - 1];
}

inline fn peek(self: *const Scanner) u8 {
    if (self.is_at_end()) return 0;
    return self.source[self.current];
}

fn peek_next(self: *const Scanner) u8 {
    if (self.current >= self.source.len - 1) return 0;
    return self.source[self.current + 1];
}

fn match(self: *Scanner, expected: u8) bool {
    if (self.peek() != expected) return false;
    self.current += 1;
    return true;
}

fn skip_whitespace(self: *Scanner) void {
    while (true) {
        switch (self.peek()) {
            ' ', '\r', '\t' => _ = self.advance(),
            '\n' => {
                self.line += 1;
                _ = self.advance();
            },
            '/' => if (self.peek_next() == '/') {
                while (self.peek() != '\n' and !self.is_at_end())
                    _ = self.advance();
            } else {
                return;
            },
            else => return,
        }
    }
}

inline fn make_token(self: *const Scanner, kind: TokenType) Token {
    return Token{
        .kind = kind,
        .text = self.text(),
        .line = self.line,
    };
}

inline fn text(self: *const Scanner) []const u8 {
    return self.source[0..self.current];
}

inline fn error_token(self: *const Scanner, message: []const u8) Token {
    return Token{
        .kind = .error_,
        .text = message,
        .line = self.line,
    };
}

inline fn is_at_end(self: *const Scanner) bool {
    return self.current >= self.source.len;
}

inline fn is_digit(c: u8) bool {
    return c >= '0' and c <= '9';
}

inline fn is_alpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or
        (c >= 'A' and c <= 'Z') or
        c == '_';
}
