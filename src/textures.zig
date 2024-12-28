const std = @import("std");
const zigimg = @import("zigimg");

const root = @import("main.zig");

const blocks = @import("blocks.zig");

const zlm = @import("zlm");

const math = @import("math.zig");

const state = &root.state;

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

pub fn registerBlocks(blocksToRegister: []blocks.Block) ![]const []const u8 {
    var hashMap = std.StringHashMap(struct {}).init(state.allocator);
    defer hashMap.deinit();
    const basePath = "assets/textures/";

    for (blocksToRegister, 0..) |block, i| {
        std.log.info("Block[{}]: `{s}`", .{ i, block.blockName });
        const names = try block.get_textures_names(state.allocator);
        defer state.allocator.free(names);

        for (names) |name| {
            defer state.allocator.free(name);
            if (!hashMap.contains(name)) {
                const newName = try state.allocator.alloc(u8, basePath.len + name.len);
                @memcpy(newName[0..basePath.len], basePath);
                @memcpy(newName[basePath.len..], name);
                try hashMap.put(newName, .{});
            }
        }
    }

    var out = std.ArrayList([]const u8).init(state.allocator);
    defer out.deinit();

    var keyIter = hashMap.keyIterator();

    while (keyIter.next()) |key| {
        try out.append(key.*);
    }

    return out.toOwnedSlice();
}
