const std = @import("std");

const Scanner = @This();

source: []const u8,
current: usize,
line: usize,

pub const Token = struct {
    kind: TokenType,
    text: []const u8,
    line: usize,

    pub inline fn eql(self: *const Token, other: *const Token) bool {
        return std.mem.eql(u8, self.text, other.text);
    }
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

pub fn scanToken(self: *Scanner) Token {
    self.skipWhitespace();
    self.source = self.source[self.current..];
    self.current = 0;

    if (self.isAtEnd()) {
        return self.makeToken(.eof);
    }
    const c = self.advance();
    if (isAlpha(c)) return self.identifier();
    if (isDigit(c)) return self.number();
    switch (c) {
        '(' => return self.makeToken(.left_paren),
        ')' => return self.makeToken(.right_paren),
        '{' => return self.makeToken(.left_brace),
        '}' => return self.makeToken(.right_brace),
        ';' => return self.makeToken(.semicolon),
        ',' => return self.makeToken(.comma),
        '.' => return self.makeToken(.dot),
        '-' => return self.makeToken(.minus),
        '+' => return self.makeToken(.plus),
        '/' => return self.makeToken(.slash),
        '*' => return self.makeToken(.star),
        '!' => return self.makeToken(if (self.match('=')) .bang_equal else .bang),
        '=' => return self.makeToken(if (self.match('=')) .equal_equal else .equal),
        '<' => return self.makeToken(if (self.match('=')) .less_equal else .less),
        '>' => return self.makeToken(if (self.match('=')) .greater_equal else .greater),
        '"' => return self.string(),
        else => return self.errorToken("Unexpected character."),
    }
}

fn string(self: *Scanner) Token {
    while (self.peek() != '"' and !self.isAtEnd()) {
        if (self.peek() == '\n') self.line += 1;
        _ = self.advance();
    }
    if (self.isAtEnd()) return self.errorToken("Unterminated string.");
    _ = self.advance();
    return self.makeToken(.string);
}

fn identifier(self: *Scanner) Token {
    while (isAlpha(self.peek()) or isDigit(self.peek()))
        _ = self.advance();
    return self.makeToken(self.identifierType());
}

fn identifierType(self: *Scanner) TokenType {
    switch (self.source[0]) {
        'a' => return self.checkKeyword(1, "nd", .and_),
        'c' => return self.checkKeyword(1, "lass", .class),
        'e' => return self.checkKeyword(1, "lse", .else_),
        'f' => if (self.current > 1) {
            switch (self.source[1]) {
                'a' => return self.checkKeyword(2, "lse", .false_),
                'o' => return self.checkKeyword(2, "r", .for_),
                'u' => return self.checkKeyword(2, "n", .fun),
                else => {},
            }
        },
        'i' => return self.checkKeyword(1, "f", .if_),
        'n' => return self.checkKeyword(1, "il", .nil),
        'o' => return self.checkKeyword(1, "r", .or_),
        'p' => return self.checkKeyword(1, "rint", .print),
        'r' => return self.checkKeyword(1, "eturn", .return_),
        's' => return self.checkKeyword(1, "uper", .super),
        't' => if (self.current > 1) {
            switch (self.source[1]) {
                'h' => return self.checkKeyword(2, "is", .this),
                'r' => return self.checkKeyword(2, "ue", .true_),
                else => {},
            }
        },
        'v' => return self.checkKeyword(1, "ar", .var_),
        'w' => return self.checkKeyword(1, "hile", .while_),
        else => {},
    }
    return .identifier;
}

inline fn checkKeyword(self: *Scanner, start: usize, rest: []const u8, kind: TokenType) TokenType {
    const current = self.source[start..self.current];
    if (std.mem.eql(u8, current, rest)) return kind;
    return .identifier;
}

fn number(self: *Scanner) Token {
    while (isDigit(self.peek()))
        _ = self.advance();
    if (self.peek() == '.' and isDigit(self.peekNext())) {
        _ = self.advance(); // consume the ".".
        while (isDigit(self.peek())) _ = self.advance();
    }
    return self.makeToken(.number);
}

inline fn advance(self: *Scanner) u8 {
    self.current += 1;
    return self.source[self.current - 1];
}

inline fn peek(self: *const Scanner) u8 {
    if (self.isAtEnd()) return 0;
    return self.source[self.current];
}

inline fn peekNext(self: *const Scanner) u8 {
    if (self.current >= self.source.len - 1) return 0;
    return self.source[self.current + 1];
}

inline fn match(self: *Scanner, expected: u8) bool {
    if (self.peek() != expected) return false;
    self.current += 1;
    return true;
}

fn skipWhitespace(self: *Scanner) void {
    while (true) {
        switch (self.peek()) {
            ' ', '\r', '\t' => _ = self.advance(),
            '\n' => {
                self.line += 1;
                _ = self.advance();
            },
            '/' => if (self.peekNext() == '/') {
                while (self.peek() != '\n' and !self.isAtEnd())
                    _ = self.advance();
            } else {
                return;
            },
            else => return,
        }
    }
}

inline fn makeToken(self: *const Scanner, kind: TokenType) Token {
    return Token{
        .kind = kind,
        .text = self.text(),
        .line = self.line,
    };
}

inline fn text(self: *const Scanner) []const u8 {
    return self.source[0..self.current];
}

inline fn errorToken(self: *const Scanner, message: []const u8) Token {
    return Token{
        .kind = .error_,
        .text = message,
        .line = self.line,
    };
}

inline fn isAtEnd(self: *const Scanner) bool {
    return self.current >= self.source.len;
}

inline fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

inline fn isAlpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or
        (c >= 'A' and c <= 'Z') or
        c == '_';
}
