const std = @import("std");
const Token = @import("Scanner.zig").Token;
const TokenType = @import("Scanner.zig").TokenType;
const Expr = @import("expr.zig").Expr;

const Parser = @This();

tokens: []Token,
allocator: std.mem.Allocator,
current: usize,

// expression    → equality
// equality      → comparison ( ( "!=" | "==" ) comparison )*
// comparison    → term ( ( ">" | ">=" | "<" | "<=" ) term )*
// term          → factor ( ( "-" | "+" ) factor )*
// factor        → unary ( ( "/" | "*" ) unary )*
// unary         → ("!"|"-") unary
//               | primary
// primary       → NUMBER | STRING | "true" | "false" | "nil"
//               | "(" expression ")"

pub fn init(allocator: std.mem.Allocator, tokens: []Token) Parser {
    return .{ .allocator = allocator, .tokens = tokens, .current = 0 };
}

pub fn deinit(self: *Parser) void {
    _ = self;
}

pub fn parseToOwnedSlice(self: *Parser) !*Expr {
    return self.expression();
}

// expression    → equality
fn expression(self: *Parser) !*Expr {
    return self.equality();
}

// equality      → comparison ( ( "!=" | "==" ) comparison )*
fn equality(self: *Parser) !*Expr {
    var expr = try self.comparison();

    while (self.match(.{ TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL })) {
        const operator = self.previous();
        const right = try self.comparison();
        const bin = try self.allocator.create(Expr);
        // zig fmt: off
        bin.* = .{
            .Binary = .{
                .l = expr,
                .o = operator,
                .r = right
            }
        };
        // zig fmt: on
        expr = bin;
    }

    return expr;
}

// comparison    → term ( ( ">" | ">=" | "<" | "<=" ) term )*
fn comparison(self: *Parser) !*Expr {
    var expr = try self.term();

    while (self.match(.{ TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL })) {
        const operator = self.previous();
        const right = try self.term();
        const bin = try self.allocator.create(Expr);
        // zig fmt: off
        bin.* = .{
            .Binary = .{
                .l = expr,
                .o = operator,
                .r = right
            }
        };
        // zig fmt: on
        expr = bin;
    }

    return expr;
}

// term          → factor ( ( "-" | "+" ) factor )*
fn term(self: *Parser) !*Expr {
    var expr = try self.factor();

    while (self.match(.{ TokenType.MINUS, TokenType.PLUS })) {
        const operator = self.previous();
        const right = try self.factor();
        const bin = try self.allocator.create(Expr);
        // zig fmt: off
        bin.* = .{
            .Binary = .{
                .l = expr,
                .o = operator,
                .r = right
            }
        };
        // zig fmt: on
        expr = bin;
    }

    return expr;
}

// factor        → unary ( ( "/" | "*" ) unary )*
fn factor(self: *Parser) !*Expr {
    var expr = try self.unary();

    while (self.match(.{ TokenType.SLASH, TokenType.STAR })) {
        const operator = self.previous();
        const right = try self.unary();
        const bin = try self.allocator.create(Expr);
        // zig fmt: off
        bin.* = .{
            .Binary = .{
                .l = expr,
                .o = operator,
                .r = right
            }
        };
        // zig fmt: on
        expr = bin;
    }

    return expr;
}

// unary         → ("!"|"-") unary
//               | primary
fn unary(self: *Parser) !*Expr {
    if (self.match(.{ TokenType.BANG, TokenType.MINUS })) {
        const operator = self.previous();
        const right = try self.unary();
        const un = try self.allocator.create(Expr);
        // zig fmt: off
        un.* = .{
            .Unary = .{
                .o = operator,
                .r = right
            }
        };
        // zig fmt: on
        return un;
    }

    return self.primary();
}

// primary       → NUMBER | STRING | "true" | "false" | "nil"
//               | "(" expression ")"
fn primary(self: *Parser) !*Expr {
    const new = try self.allocator.create(Expr);
    const next = self.advance();
    switch (next.token_type) {
        .NUMBER, .STRING => {
            new.* = .{ .Literal = .{ .v = next.literal.? } };
        },
        .TRUE => {
            new.* = .{ .Literal = .{ .v = .{ .boolean = true } } };
        },
        .FALSE => {
            new.* = .{ .Literal = .{ .v = .{ .boolean = false } } };
        },
        .NIL => {
            new.* = .{ .Literal = .{ .v = .nil } };
        },
        .LEFT_PAREN => {
            // todo
        },
        else => unreachable,
    }
    return new;
}

fn match(self: *Parser, types: anytype) bool {
    inline for (std.meta.fields(@TypeOf(types))) |t| {
        if (t.type != TokenType) {
            @compileError("Expected TokenType, received " ++ @typeName(t.type));
        }
        const t_value = @field(types, t.name);

        if (self.check(t_value)) {
            _ = self.advance();
            return true;
        }
    }
    return false;
}

fn check(self: *Parser, token_type: TokenType) bool {
    if (self.isAtEnd()) return false;
    return self.peek().token_type == token_type;
}

fn advance(self: *Parser) Token {
    if (!self.isAtEnd()) self.current += 1;
    return self.previous();
}

fn isAtEnd(self: *Parser) bool {
    return self.peek().token_type == .EOF;
}

fn peek(self: *Parser) Token {
    return self.tokens[self.current];
}

fn previous(self: *Parser) Token {
    return self.tokens[self.current - 1];
}
