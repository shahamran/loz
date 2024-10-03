const std = @import("std");

pub fn init_scanner(source: []const u8) void {
    scanner.source = source;
    scanner.current = 0;
    scanner.line = 1;
}

pub fn scan_token() Token {
    skip_whitespace();
    scanner.source = scanner.source[scanner.current..];
    scanner.current = 0;

    if (is_at_end()) {
        return make_token(.eof);
    }
    const c = advance();
    if (is_alpha(c)) return identifier();
    if (is_digit(c)) return number();
    switch (c) {
        '(' => return make_token(.left_paren),
        ')' => return make_token(.right_paren),
        '{' => return make_token(.left_brace),
        '}' => return make_token(.right_brace),
        ';' => return make_token(.semicolon),
        ',' => return make_token(.comma),
        '.' => return make_token(.dot),
        '-' => return make_token(.minus),
        '+' => return make_token(.plus),
        '/' => return make_token(.slash),
        '*' => return make_token(.star),
        '!' => return make_token(if (match('=')) .bang_equal else .bang),
        '=' => return make_token(if (match('=')) .equal_equal else .equal),
        '<' => return make_token(if (match('=')) .less_equal else .less),
        '>' => return make_token(if (match('=')) .greater_equal else .greater),
        '"' => return string(),
        else => return error_token("Unexpected character."),
    }
}

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

const Scanner = struct {
    source: []const u8,
    current: usize,
    line: usize,
};

var scanner: Scanner = undefined;

fn string() Token {
    while (peek() != '"' and !is_at_end()) {
        if (peek() == '\n') scanner.line += 1;
        _ = advance();
    }
    if (is_at_end()) return error_token("Unterminated string.");
    _ = advance();
    return make_token(.string);
}

fn identifier() Token {
    while (is_alpha(peek()) or is_digit(peek())) _ = advance();
    return make_token(identifier_type());
}

fn identifier_type() TokenType {
    switch (scanner.source[0]) {
        'a' => return check_keyword(1, "nd", .and_),
        'c' => return check_keyword(1, "lass", .class),
        'e' => return check_keyword(1, "lse", .else_),
        'f' => if (scanner.current > 1) {
            switch (scanner.source[1]) {
                'a' => return check_keyword(2, "lse", .false_),
                'o' => return check_keyword(2, "r", .for_),
                'u' => return check_keyword(2, "n", .fun),
                else => {},
            }
        },
        'i' => return check_keyword(1, "f", .if_),
        'n' => return check_keyword(1, "il", .nil),
        'o' => return check_keyword(1, "r", .or_),
        'p' => return check_keyword(1, "rint", .print),
        'r' => return check_keyword(1, "eturn", .return_),
        's' => return check_keyword(1, "uper", .super),
        't' => if (scanner.current > 1) {
            switch (scanner.source[1]) {
                'h' => return check_keyword(2, "is", .this),
                'r' => return check_keyword(2, "ue", .true_),
                else => {},
            }
        },
        'v' => return check_keyword(1, "ar", .var_),
        'w' => return check_keyword(1, "hile", .while_),
        else => {},
    }
    return .identifier;
}

fn check_keyword(start: usize, rest: []const u8, kind: TokenType) TokenType {
    const current = scanner.source[start..scanner.current];
    if (std.mem.eql(u8, current, rest)) return kind;
    return .identifier;
}

fn number() Token {
    while (is_digit(peek())) _ = advance();
    if (peek() == '.' and is_digit(peek_next())) {
        _ = advance(); // consume the ".".
        while (is_digit(peek())) _ = advance();
    }
    return make_token(.number);
}

fn text() []const u8 {
    return scanner.source[0..scanner.current];
}

fn advance() u8 {
    scanner.current += 1;
    return scanner.source[scanner.current - 1];
}

fn peek() u8 {
    if (is_at_end()) return 0;
    return scanner.source[scanner.current];
}

fn peek_next() u8 {
    if (scanner.current >= scanner.source.len - 1) return 0;
    return scanner.source[scanner.current + 1];
}

fn match(expected: u8) bool {
    if (peek() != expected) return false;
    scanner.current += 1;
    return true;
}

fn skip_whitespace() void {
    while (true) {
        switch (peek()) {
            ' ', '\r', '\t' => _ = advance(),
            '\n' => {
                scanner.line += 1;
                _ = advance();
            },
            '/' => if (peek_next() == '/') {
                while (peek() != '\n' and !is_at_end())
                    _ = advance();
            } else {
                return;
            },
            else => return,
        }
    }
}

fn make_token(kind: TokenType) Token {
    return Token{
        .kind = kind,
        .text = text(),
        .line = scanner.line,
    };
}

fn error_token(message: []const u8) Token {
    return Token{
        .kind = .error_,
        .text = message,
        .line = scanner.line,
    };
}

fn is_at_end() bool {
    return scanner.current >= scanner.source.len;
}

fn is_digit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn is_alpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or
        (c >= 'A' and c <= 'Z') or
        c == '_';
}
