const std = @import("std");
const reportError = @import("main.zig").reportError;

// zig fmt: off
pub const TokenType = enum { 
    LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,

    DOT, COMMA, PLUS, MINUS, STAR, SLASH, SEMICOLON, EQUAL, EQUAL_EQUAL,
    BANG, BANG_EQUAL, LESS, LESS_EQUAL, GREATER, GREATER_EQUAL,

    STRING, NUMBER, IDENTIFIER,

    AND, CLASS, ELSE, FALSE, FOR, FUN, IF, NIL, OR,
    PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE,

    EOF,
};
// zig fmt: on

pub const Literal = union(enum) {
    string: []const u8,
    number: f64,
    boolean: bool,
    nil,

    // zig fmt: off
    pub fn format(
        self: Literal, 
        comptime fmt: []const u8, 
        options: std.fmt.FormatOptions, 
        writer: anytype
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            Literal.string => |str| try writer.print("{s}", .{ str }),
            Literal.number => |num| {
                if (@ceil(num) == num) {
                    try writer.print("{d}.0", .{ num });
                } else {
                    try writer.print("{d}", .{ num });
                }
            },
            Literal.boolean => |bln| {
                if (bln) {
                    try writer.print("true", .{});
                } else {
                    try writer.print("false", .{});
                }
            },
            Literal.nil => {
                try writer.print("nil", .{});
            }
        }
    }
    // zig fmt: on
};

pub const Token = struct {
    token_type: TokenType,
    lexeme: []const u8,
    literal: ?Literal,
    line: usize,

    // zig fmt: off
    pub fn format(
        self: Token, 
        comptime fmt: []const u8, 
        options: std.fmt.FormatOptions, 
        writer: anytype
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("{s} {s} {?}", .{ 
            @tagName(self.token_type), 
            self.lexeme,
            self.literal
        });
    }
    // zig fmt: on
};

const Scanner = @This();

const keywords_slice = [_]struct { []const u8, TokenType }{ .{ "and", .AND }, .{ "class", .CLASS }, .{ "else", .ELSE }, .{ "false", .FALSE }, .{ "for", .FOR }, .{ "fun", .FUN }, .{ "if", .IF }, .{ "nil", .NIL }, .{ "or", .OR }, .{ "print", .PRINT }, .{ "return", .RETURN }, .{ "super", .SUPER }, .{ "this", .THIS }, .{ "true", .TRUE }, .{ "var", .VAR }, .{ "while", .WHILE } };

const keywords = std.StaticStringMap(TokenType).initComptime(keywords_slice);

allocator: std.mem.Allocator,
source: []const u8,
tokens: std.ArrayList(Token),
start: usize = 0,
current: usize = 0,
line: usize = 1,

pub fn init(allocator: std.mem.Allocator, source: []const u8) !Scanner {
    // zig fmt: off
    return Scanner{ 
        .allocator = allocator, 
        .tokens = std.ArrayList(Token).init(allocator), 
        .source = source,
    };
    // zig fmt: on
}

pub fn deinit(self: *Scanner) void {
    self.tokens.deinit();
}

pub fn scanTokensToOwnedSlice(self: *Scanner) ![]Token {
    while (!self.isAtEnd()) {
        // we are at the beggining of the next lexeme (token)
        self.start = self.current;
        try self.scanToken();
    }

    try self.tokens.append(.{
        .token_type = .EOF,
        .lexeme = "",
        .literal = null,
        .line = self.line,
    });

    return self.tokens.toOwnedSlice();
}

fn scanToken(self: *Scanner) !void {
    const c = self.advance();
    switch (c) {
        '(' => try self.addToken(.LEFT_PAREN, null),
        ')' => try self.addToken(.RIGHT_PAREN, null),
        '{' => try self.addToken(.LEFT_BRACE, null),
        '}' => try self.addToken(.RIGHT_BRACE, null),
        '.' => try self.addToken(.DOT, null),
        ',' => try self.addToken(.COMMA, null),
        '+' => try self.addToken(.PLUS, null),
        '-' => try self.addToken(.MINUS, null),
        '*' => try self.addToken(.STAR, null),
        ';' => try self.addToken(.SEMICOLON, null),
        '=' => try self.addToken(if (self.match('=')) .EQUAL_EQUAL else .EQUAL, null),
        '!' => try self.addToken(if (self.match('=')) .BANG_EQUAL else .BANG, null),
        '<' => try self.addToken(if (self.match('=')) .LESS_EQUAL else .LESS, null),
        '>' => try self.addToken(if (self.match('=')) .GREATER_EQUAL else .GREATER, null),
        '/' => {
            // we are in a comment
            if (self.match('/')) {
                // we should consume characters until newline
                // this can be done with peek()
                while (self.peek() != '\n' and !self.isAtEnd()) _ = self.advance();
            } else try self.addToken(.SLASH, null);
        },
        '"' => try self.string(),
        '0'...'9' => try self.number(),
        'a'...'z', 'A'...'Z', '_' => try self.identifier(),
        // noop
        ' ', '\r', '\t' => {},
        '\n' => {
            self.line += 1;
        },
        else => {
            const slice = try std.fmt.allocPrint(self.allocator, "Unexpected character: {c}", .{c});
            defer self.allocator.free(slice);
            reportError(self.line, slice);
        },
    }
}

fn identifier(self: *Scanner) !void {
    while (isAlphanumeric(self.peek())) _ = self.advance();

    const current_lexeme = self.source[self.start..self.current];
    const token_type = keywords.get(current_lexeme) orelse .IDENTIFIER;
    try self.addToken(token_type, null);
}

fn number(self: *Scanner) !void {
    while (isDigit(self.peek())) {
        _ = self.advance();
    }

    if (self.peek() == '.' and isDigit(self.peekNext())) {
        _ = self.advance();

        while (isDigit(self.peek())) {
            _ = self.advance();
        }
    }

    // zig fmt: off
    const literal = try std.fmt.parseFloat(
        std.meta.FieldType(Literal, .number), 
        self.source[self.start..self.current]
    );
    // zig fmt: on
    try self.addToken(.NUMBER, .{ .number = literal });
}

fn string(self: *Scanner) !void {
    while (!self.isAtEnd()) {
        const next = self.advance();
        if (next == '"') {
            const literal = self.source[self.start + 1 .. self.current - 1];
            try self.addToken(.STRING, .{ .string = literal });
            return;
        }
    }

    reportError(self.line, "Unterminated string.");
}

fn isAlphanumeric(c: u8) bool {
    return isAlpha(c) or isDigit(c);
}

fn isAlpha(c: u8) bool {
    return std.ascii.isAlphabetic(c) or c == '_';
}

fn isDigit(c: u8) bool {
    return std.ascii.isDigit(c);
}

// take a look at the next char without moving
fn peek(self: *Scanner) u8 {
    if (self.isAtEnd()) return 0;
    return self.source[self.current];
}

// take a look at the next chars next char without moving
fn peekNext(self: *Scanner) u8 {
    if (self.current + 1 >= self.source.len) return 0;
    return self.source[self.current + 1];
}

// move current forward once
fn advance(self: *Scanner) u8 {
    self.current += 1;
    return self.source[self.current - 1];
}

// return true if next char is of expected and advance 1
fn match(self: *Scanner, expected: u8) bool {
    if (self.isAtEnd()) return false;
    if (self.source[self.current] != expected) return false;

    self.current += 1;
    return true;
}

fn addToken(self: *Scanner, token_type: TokenType, literal: ?Literal) !void {
    const text = self.source[self.start..self.current];
    try self.tokens.append(.{
        .token_type = token_type,
        .lexeme = text,
        .literal = literal,
        .line = self.line,
    });
}

fn isAtEnd(self: *Scanner) bool {
    return self.current >= self.source.len;
}
