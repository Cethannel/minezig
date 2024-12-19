const std = @import("std");
const zigimg = @import("zigimg");

const math = @import("math.zig");

pub fn loadImage(path: []const u8, allocator: std.mem.Allocator) [][3]u8 {
    zigimg.Image.fromFilePath(allocator, path);
}

const imageSize = 32;

const Color = packed struct {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
};

pub fn createAtlas(textures: []const []const u8, allocator: std.mem.Allocator) ![]u32 {
    var out = try std.ArrayList(u32).initCapacity(allocator, textures.len * 32 * 32);
    defer out.deinit();

    for (textures) |textFile| {
        var image = try zigimg.Image.fromFilePath(allocator, textFile);
        try image.convert(.rgba32);

        for (image.pixels.rgba32) |pixel| {
            const color: Color = .{
                .a = pixel.a,
                .r = pixel.r,
                .g = pixel.g,
                .b = pixel.b,
            };

            try out.append(@bitCast(color));
        }

        image.deinit();
    }

    return out.toOwnedSlice();
}
