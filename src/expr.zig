const std = @import("std");
const Token = @import("Scanner.zig").Token;
const TokenType = @import("Scanner.zig").TokenType;
const Literal = @import("Scanner.zig").Literal;

const RuntimeError = error{OperandMustBeANumber};

pub const Object = union(enum) {
    string: []const u8,
    number: f64,
    boolean: bool,
    nil,

    // zig fmt: off
    pub fn format(
        self: Object, 
        comptime fmt: []const u8, 
        options: std.fmt.FormatOptions, 
        writer: anytype
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .string => |str| try writer.print("{s}", .{ str }),
            .number => |num| {
                try writer.print("{d}", .{ num });
            },
            .boolean => |bln| {
                if (bln) {
                    try writer.print("true", .{});
                } else {
                    try writer.print("false", .{});
                }
            },
            .nil => {
                try writer.print("nil", .{});
            }
        }
    }
    // zig fmt: on

    pub fn fromLiteral(literal: Literal) Object {
        return switch (literal) {
            .string => |str| .{ .string = str },
            .number => |num| .{ .number = num },
            .boolean => |bln| .{ .boolean = bln },
            .nil => .nil,
        };
    }

    pub fn isTruthy(object: Object) bool {
        if (object == .nil) return false;
        return switch (object) {
            .boolean => |bln| bln,
            else => true,
        };
    }
};

// zig fmt: off
pub const Expr = union(enum) { 
    Binary: struct { l: *Expr, o: Token, r: *Expr },
    Grouping: struct { e: *Expr },
    Literal: struct  { v: Literal },
    Unary: struct  { o: Token, r: *Expr },

    pub fn format(
        self: Expr, 
        comptime fmt: []const u8, 
        options: std.fmt.FormatOptions, 
        writer: anytype
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .Binary => |binary| 
                try writer.print("({s} {} {})", .{
                    binary.o.lexeme,
                    binary.l.*,
                    binary.r.*
                }),
            .Grouping => |grouping| 
                try writer.print("(group {})", .{grouping.e.*}),
            .Literal => |literal| 
                try writer.print("{}", .{literal.v}),
            .Unary => |unary| 
                try writer.print("({s} {})", .{
                    unary.o.lexeme,
                    unary.r.* 
                })
        }
    }

    pub fn evaluate(self: Expr, allocator: std.mem.Allocator) !Object {
        return switch (self) {
            .Literal => |lit| Object.fromLiteral(lit.v),
            .Grouping => |grp| grp.e.evaluate(allocator),
            .Unary => |un| {
                const right = try un.r.evaluate(allocator);

                return switch (un.o.token_type) {
                    .MINUS => {
                        try checkNumberOperand(right);
                        return .{ .number = -right.number };
                    },
                    .BANG => .{ .boolean = !right.isTruthy() },
                    else => .nil
                };

            },
            .Binary => |bin| {
                const left = try bin.l.evaluate(allocator);
                const right = try bin.r.evaluate(allocator);

                return switch (bin.o.token_type) {
                    .GREATER => {
                        try checkNumberOperands(left, right);
                        return .{ .boolean = left.number > right.number };
                    },
                    .GREATER_EQUAL => {
                        try checkNumberOperands(left, right);
                        return .{ .boolean = left.number >= right.number };
                    },

                    .LESS => {
                        try checkNumberOperands(left, right);
                        return .{ .boolean = left.number < right.number };
                    },
                    .LESS_EQUAL => {
                        try checkNumberOperands(left, right);
                        return .{ .boolean = left.number <= right.number };
                    },
                    .MINUS => {
                        try checkNumberOperands(left, right);
                        return .{ .number = left.number - right.number };
                    },
                    .SLASH => {
                        try checkNumberOperands(left, right);
                        return .{ .number = left.number / right.number };
                    },
                    .STAR => {
                        try checkNumberOperands(left, right);
                        return .{ .number = left.number * right.number };
                    },
                    .PLUS => {
                        // TODO: add string support

                        if (left == .string and right == .string) {
                            const concat = try std.fmt.allocPrint(allocator, "{s}{s}", .{left.string, right.string});
                            return .{
                                .string = concat
                            };

                        } else if (left == .number and right == .number) {
                            return .{ .number = left.number + right.number };
                        }
                        
                        return .nil;
                    },
                    else => .nil
                };
            },
        };
    }

    fn checkNumberOperand(operand: Object) !void {
        switch (operand) {
            .number => return,
            else => return RuntimeError.OperandMustBeANumber
        }
    }

    fn checkNumberOperands(left: Object, right: Object) !void {
        try checkNumberOperand(left);
        try checkNumberOperand(right);
    }
};
// zig fmt: on

test "expression print" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // zig fmt: off
    const lit = try allocator.create(Expr);
    lit.* = .{ 
        .Literal = .{
            .v = .{ .number = 123 }
        }
    };
    const lit2 = try allocator.create(Expr);
    lit2.* = .{ 
        .Literal = .{
            .v = .{ .number = 45.67 }
        }
    };
    const un = try allocator.create(Expr);
    un.* = .{ 
        .Unary = .{
            .o = .{
                .line = 1,
                .lexeme = "-",
                .literal = null,
                .token_type = TokenType.MINUS
            },
            .r = lit
        }
    };
    const grp = try allocator.create(Expr);
    grp.* = .{ 
        .Grouping = .{
            .e = lit2
        }
    };
    const bin = try allocator.create(Expr);
    bin.* = .{
        .Binary = .{
            .l = un,
            .o = .{
                .line = 1,
                .lexeme = "*",
                .literal = null,
                .token_type = TokenType.STAR
            },
            .r = grp
        }
    };
    // zig fmt: on
    std.debug.print("{}\n", .{bin});
    const actual = try std.fmt.allocPrint(allocator, "{}", .{bin});
    try std.testing.expectEqualStrings("(* (- 123.0) (group 45.67))", actual);
}
