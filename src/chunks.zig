const std = @import("std");
const root = @import("root");

pub const chunkWidth = 16;
pub const chunkHeight = 256;

pub const BlockId = enum(u32) {
    Air = 0,
    _,
};

pub const Block = struct {
    id: BlockId,
};

const baseVertices = [_]root.Vertex{
    .{ .x = 0.0, .y = 0.0, .z = 0.0, .u = 0, .v = 1, .nx = 0.0, .ny = 0.0, .nz = -1.0 },
    .{ .x = 1.0, .y = 0.0, .z = 0.0, .u = 1, .v = 1, .nx = 0.0, .ny = 0.0, .nz = -1.0 },
    .{ .x = 1.0, .y = 1.0, .z = 0.0, .u = 1, .v = 0, .nx = 0.0, .ny = 0.0, .nz = -1.0 },
    .{ .x = 0.0, .y = 1.0, .z = 0.0, .u = 0, .v = 0, .nx = 0.0, .ny = 0.0, .nz = -1.0 },

    .{ .x = 0.0, .y = 0.0, .z = 1.0, .u = 0, .v = 1, .nx = 0.0, .ny = 0.0, .nz = 1.0 },
    .{ .x = 1.0, .y = 0.0, .z = 1.0, .u = 1, .v = 1, .nx = 0.0, .ny = 0.0, .nz = 1.0 },
    .{ .x = 1.0, .y = 1.0, .z = 1.0, .u = 1, .v = 0, .nx = 0.0, .ny = 0.0, .nz = 1.0 },
    .{ .x = 0.0, .y = 1.0, .z = 1.0, .u = 0, .v = 0, .nx = 0.0, .ny = 0.0, .nz = 1.0 },

    .{ .x = 0.0, .y = 0.0, .z = 0.0, .u = 1, .v = 1, .nx = -1.0, .ny = 0.0, .nz = 0.0 },
    .{ .x = 0.0, .y = 1.0, .z = 0.0, .u = 1, .v = 0, .nx = -1.0, .ny = 0.0, .nz = 0.0 },
    .{ .x = 0.0, .y = 1.0, .z = 1.0, .u = 0, .v = 0, .nx = -1.0, .ny = 0.0, .nz = 0.0 },
    .{ .x = 0.0, .y = 0.0, .z = 1.0, .u = 0, .v = 1, .nx = -1.0, .ny = 0.0, .nz = 0.0 },

    .{ .x = 1.0, .y = 0.0, .z = 0.0, .u = 0, .v = 1, .nx = 1.0, .ny = 0.0, .nz = 0.0 },
    .{ .x = 1.0, .y = 1.0, .z = 0.0, .u = 0, .v = 0, .nx = 1.0, .ny = 0.0, .nz = 0.0 },
    .{ .x = 1.0, .y = 1.0, .z = 1.0, .u = 1, .v = 0, .nx = 1.0, .ny = 0.0, .nz = 0.0 },
    .{ .x = 1.0, .y = 0.0, .z = 1.0, .u = 1, .v = 1, .nx = 1.0, .ny = 0.0, .nz = 0.0 },

    .{ .x = 0.0, .y = 0.0, .z = 0.0, .u = 0, .v = 0, .nx = 0.0, .ny = -1.0, .nz = 0.0 },
    .{ .x = 0.0, .y = 0.0, .z = 1.0, .u = 1, .v = 0, .nx = 0.0, .ny = -1.0, .nz = 0.0 },
    .{ .x = 1.0, .y = 0.0, .z = 1.0, .u = 1, .v = 1, .nx = 0.0, .ny = -1.0, .nz = 0.0 },
    .{ .x = 1.0, .y = 0.0, .z = 0.0, .u = 0, .v = 1, .nx = 0.0, .ny = -1.0, .nz = 0.0 },

    .{ .x = 0.0, .y = 1.0, .z = 0.0, .u = 0, .v = 0, .nx = 0.0, .ny = 1.0, .nz = 0.0 },
    .{ .x = 0.0, .y = 1.0, .z = 1.0, .u = 1, .v = 0, .nx = 0.0, .ny = 1.0, .nz = 0.0 },
    .{ .x = 1.0, .y = 1.0, .z = 1.0, .u = 1, .v = 1, .nx = 0.0, .ny = 1.0, .nz = 0.0 },
    .{ .x = 1.0, .y = 1.0, .z = 0.0, .u = 0, .v = 1, .nx = 0.0, .ny = 1.0, .nz = 0.0 },
};

const baseIndices = [_]u32{
    0, 1, 2, 0, 2, 3, // 0,  1,  2,  0,  2,  3,
    2, 1, 0, 3, 2, 0, // 6,  5,  4,  7,  6,  4,
    0, 1, 2, 0, 2, 3, // 8,  9,  10, 8,  10, 11,
    2, 1, 0, 3, 2, 0, // 14, 13, 12, 15, 14, 12,
    0, 1, 2, 0, 2, 3, // 16, 17, 18, 16, 18, 19,
    2, 1, 0, 3, 2, 0, // 22, 21, 20, 23, 22, 20,
};

pub const Chunk = struct {
    blocks: [chunkWidth][chunkHeight][chunkWidth]Block,

    pub fn gen_solid_chunk() @This() {
        const idThing: u32 = 1;
        const blocks = .{.{.{Block{ .id = @enumFromInt(idThing) }} ** chunkWidth} ** chunkHeight} ** chunkWidth;
        return .{
            .blocks = blocks,
        };
    }

    pub fn gen_mesh(self: *const @This(), allocator: std.mem.Allocator) !struct {
        vertices: std.ArrayList(root.Vertex),
        indices: std.ArrayList(u32),
    } {
        var maxOffset: u32 = 0;

        var vertices = std.ArrayList(root.Vertex).init(allocator);
        var indices = std.ArrayList(u32).init(allocator);

        for (self.blocks, 0..) |slice, x| {
            for (slice, 0..) |col, y| {
                for (col, 0..) |block, z| {
                    if (block.id != .Air) {
                        face: for (0..6) |index1| {
                            if (index1 == 0) {
                                if (z != 0) {
                                    if (self.blocks[x][y][z - 1].id != .Air) {
                                        continue :face;
                                    }
                                }
                            }

                            if (index1 == 1) {
                                if (z != 15) {
                                    if (self.blocks[x][y][z + 1].id != .Air) {
                                        continue :face;
                                    }
                                }
                            }

                            if (index1 == 2) {
                                if (x != 0) {
                                    if (self.blocks[x - 1][y][z].id != .Air) {
                                        continue :face;
                                    }
                                }
                            }

                            if (index1 == 3) {
                                if (x != 15) {
                                    if (self.blocks[x + 1][y][z].id != .Air) {
                                        continue :face;
                                    }
                                }
                            }

                            if (index1 == 4) {
                                if (y != 0) {
                                    if (self.blocks[x][y - 1][z].id != .Air) {
                                        continue :face;
                                    }
                                }
                            }

                            if (index1 == 5) {
                                if (y != 255) {
                                    if (self.blocks[x][y + 1][z].id != .Air) {
                                        continue :face;
                                    }
                                }
                            }

                            const indexOffset = maxOffset;
                            for (0..6) |index2| {
                                var i = index1 * 4 + index2;

                                if (index2 < 4) {
                                    var newVertex = baseVertices[i];
                                    newVertex.x += @floatFromInt(x);
                                    newVertex.y += @floatFromInt(y);
                                    newVertex.z += @floatFromInt(z);

                                    newVertex.v /= 2.0;

                                    try vertices.append(newVertex);
                                }

                                i = index1 * 6 + index2;

                                const index = baseIndices[i];
                                const newIndex = index + indexOffset;
                                try indices.append(newIndex);

                                if (newIndex > maxOffset) {
                                    maxOffset = newIndex;
                                }
                            }
                            maxOffset += 1;
                        }
                    }
                }
            }
        }

        return .{
            .vertices = vertices,
            .indices = indices,
        };
    }
};

test "Convert Int to float" {
    const f: f32 = @floatFromInt(1);

    try std.testing.expectEqual(1.0, f);
}
