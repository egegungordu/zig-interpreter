const std = @import("std");
const Token = @import("Scanner.zig").Token;
const TokenType = @import("Scanner.zig").TokenType;
const Literal = @import("Scanner.zig").Literal;

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
