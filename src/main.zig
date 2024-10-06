const std = @import("std");
const Scanner = @import("Scanner.zig");
const Parser = @import("Parser.zig");
const Token = Scanner.Token;
const TokenType = Scanner.TokenType;
const Expr = @import("expr.zig").Expr;

var had_error = false;

fn report(line: usize, where: []const u8, message: []const u8) void {
    std.debug.print("[line {d}] Error{s}: {s}\n", .{ line, where, message });
    had_error = true;
}

pub fn reportError(line: usize, message: []const u8) void {
    report(line, "", message);
}

pub fn error_(token: Token, message: []const u8) !void {
    if (token.token_type == TokenType.EOF) {
        report(token.line, " at end", message);
    } else {
        const buf: []u8 = undefined;
        const where = try std.fmt.bufPrint(buf, " at '{s}'", .{token.lexeme});
        report(token.line, where, message);
    }
}

// returns owned token slice
fn allocTokenizeFile(allocator: std.mem.Allocator, file_contents: []u8) ![]Token {
    var scanner = try Scanner.init(allocator, file_contents);
    defer scanner.deinit();
    return try scanner.scanTokensToOwnedSlice();
}

// return owned parse tree slice
fn allocParseTokens(allocator: std.mem.Allocator, tokens: []Token) !*Expr {
    var parser = Parser.init(allocator, tokens);
    defer parser.deinit();
    return try parser.parseToOwnedSlice();
}

const commands = [_][]const u8{ "tokenize", "parse" };

const Command = enum {
    tokenize,
    parse,
    evaluate,

    fn fromString(str: []const u8) ?Command {
        const fields = std.meta.fields(Command);
        inline for (fields) |field| {
            if (std.mem.eql(u8, field.name, str)) {
                return @enumFromInt(field.value);
            }
        }
        unreachable;
    }
};

pub fn main() !void {
    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer std.process.argsFree(std.heap.page_allocator, args);

    if (args.len < 3) {
        std.debug.print("Usage: ./your_program.sh tokenize <filename>\n", .{});
        std.process.exit(1);
    }

    const commandStr = args[1];
    const filename = args[2];

    const command = Command.fromString(commandStr);

    if (command == null) {
        std.debug.print("Unknown command: {s}\n", .{commandStr});
        std.process.exit(1);
    }

    const file_contents = try std.fs.cwd().readFileAlloc(std.heap.page_allocator, filename, std.math.maxInt(usize));
    defer std.heap.page_allocator.free(file_contents);

    std.debug.print("File contents: {s}\n", .{file_contents});

    const stdout = std.io.getStdOut().writer();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    switch (command.?) {
        Command.tokenize => {
            const tokens = try allocTokenizeFile(allocator, file_contents);
            defer allocator.free(tokens);

            for (tokens) |token| {
                try stdout.print("{}\n", .{token});
            }

            if (had_error) {
                std.process.exit(65);
            }
        },
        Command.parse => {
            const tokens = try allocTokenizeFile(allocator, file_contents);
            defer allocator.free(tokens);

            if (had_error) {
                std.process.exit(65);
            }

            var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            defer arena.deinit();

            const arena_allocator = arena.allocator();
            const parse_tree = allocParseTokens(arena_allocator, tokens) catch {
                std.process.exit(65);
            };

            try stdout.print("{}\n", .{parse_tree});
        },
        Command.evaluate => {},
    }
}
