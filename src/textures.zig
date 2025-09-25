const std = @import("std");
const zignal = @import("zignal");

const root = @import("main.zig");

const blocks = @import("blocks.zig");

const zlm = @import("zlm");

const math = @import("math.zig");

const state = &root.state;

const imageSize = 32;

const Color = packed struct {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
};

pub fn createAtlas(textures: []const []const u8, allocator: std.mem.Allocator) ![]u8 {
    var out = try zignal.Image(zignal.Rgba).init(allocator, 32 * textures.len, 32);
    errdefer out.deinit(allocator);

    var top: usize = 0;
    for (textures) |textFile| {
        std.log.info("Loading image {s}", .{textFile});
        var image = try zignal.png.load(zignal.Rgba, allocator, textFile);

        out.insert(image, .{
            .l = 0,
            .t = @floatFromInt(top),
            .b = @floatFromInt(top + 31),
            .r = 31,
        }, 0.0, .nearest_neighbor);

        image.deinit(allocator);
        top += 32;
    }

    return out.asBytes();
}

const basePath = "assets/textures/";

pub fn registerBlocks(blocksToRegister: []blocks.Block) ![]const []const u8 {
    const alloc = state.texturesArena.allocator();
    var out = std.ArrayList([]const u8).empty;
    defer out.deinit(alloc);

    for (blocksToRegister, 0..) |block, i| {
        std.log.info("Block[{}]: `{s}`", .{ i, block.blockName.* });
        if (i == 0) {
            continue;
        }
        const names = try block.get_textures_names(alloc);
        defer alloc.free(names);

        for (names) |name| {
            defer alloc.free(name);
            if (!hasTextureName(out, name)) {
                std.log.info("Adding texture: {s}", .{name});
                const newName = try alloc.alloc(u8, basePath.len + name.len);
                @memcpy(newName[0..basePath.len], basePath);
                @memcpy(newName[basePath.len..], name);
                try out.append(alloc, newName);
            }
        }
    }

    return out.toOwnedSlice(alloc);
}

fn hasTextureName(arr: std.ArrayList([]const u8), name: []const u8) bool {
    for (arr.items) |value| {
        if (std.mem.eql(u8, value[basePath.len..], name)) {
            return true;
        }
    }

    return false;
}
