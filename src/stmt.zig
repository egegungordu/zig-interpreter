const std = @import("std");
const Expr = @import("expr.zig").Expr;

pub const Stmt = union(enum) {
    Expression: struct { e: *Expr },
    Print: struct { e: *Expr },

    pub fn execute(self: Stmt, allocator: std.mem.Allocator) !void {
        switch (self) {
            .Expression => |expr| _ = try expr.e.evaluate(allocator),
            .Print => |prt| {
                const obj = try prt.e.evaluate(allocator);
                try std.io.getStdOut().writer().print("{}\n", .{obj});
            },
        }
    }
};
