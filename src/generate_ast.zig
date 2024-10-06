const std = @import("std");

pub fn main() !void {
    const filename = "Test.zig";

    var file = try std.fs.cwd().createFile(filename, .{});
    defer file.close();
    var bw = std.io.bufferedWriter(file.writer());
    const writer = bw.writer();

    const content =
        \\ pub const a = 1;
    ;

    try writer.writeAll(content);
}
