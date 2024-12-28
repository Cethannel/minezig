const std = @import("std");
const root = @import("main.zig");

const sokol = @import("sokol");
const sapp = sokol.app;
const sg = sokol.gfx;
const sglue = sokol.glue;
const slog = sokol.log;
const sdtx = sokol.debugtext;

const zlm = @import("zlm");

const IZlm = zlm.SpecializeOn(i64);
pub const IVec3 = IZlm.Vec3;

const state = &root.state;

pub const chunkWidth = 16;
pub const chunkHeight = 256;

pub const BlockId = enum(u32) {
    Air = 0,
    _,
};

pub const Block = struct {
    id: BlockId,

    pub const Air = .{
        .id = .Air,
    };
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
    sides: Sides,

    pub fn gen_solid_chunk() @This() {
        const idThing: u32 = 1;
        const blocks = .{.{.{Block{ .id = @enumFromInt(idThing) }} ** chunkWidth} ** chunkHeight} ** chunkWidth;
        var tmp: @This() = .{
            .blocks = blocks,
            .sides = undefined,
        };

        tmp.sides = tmp.gen_sides();

        return tmp;
    }

    pub fn gen_half_solid_chunk() @This() {
        @setEvalBranchQuota(chunkWidth * chunkHeight * chunkWidth * 2);
        const idThing: u32 = 1;
        var blocks: [chunkWidth][chunkHeight][chunkWidth]Block = .{.{.{Block{ .id = .Air }} ** chunkWidth} ** chunkHeight} ** chunkWidth;
        for (0..chunkWidth) |x| {
            for (0..chunkHeight / 2) |y| {
                for (0..chunkWidth) |z| {
                    blocks[x][y][z].id = @enumFromInt(idThing);
                }
            }
        }
        var tmp: @This() = .{
            .blocks = blocks,
            .sides = undefined,
        };

        tmp.sides = tmp.gen_sides();

        return tmp;
    }

    fn genAll2() @This() {
        const idThing: u32 = 2;
        const blocks = .{.{.{Block{ .id = @enumFromInt(idThing) }} ** chunkWidth} ** chunkHeight} ** chunkWidth;
        var tmp: @This() = .{
            .blocks = blocks,
            .sides = undefined,
        };

        tmp.sides = tmp.gen_sides();

        return tmp;
    }

    pub fn gen_mesh(self: *const @This(), neighbor_sides: Sides, allocator: std.mem.Allocator) !struct {
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
                            var neighborBlock: Block = Block.Air;
                            switch (index1) {
                                0 => {
                                    if (z != 0) {
                                        neighborBlock = self.blocks[x][y][z - 1];
                                    } else {
                                        neighborBlock = neighbor_sides.neg_z[x][y];
                                    }
                                },
                                1 => {
                                    if (z != 15) {
                                        neighborBlock = self.blocks[x][y][z + 1];
                                    } else {
                                        neighborBlock = neighbor_sides.z[x][y];
                                    }
                                },
                                2 => {
                                    if (x != 0) {
                                        neighborBlock = self.blocks[x - 1][y][z];
                                    } else {
                                        neighborBlock = neighbor_sides.neg_x[z][y];
                                    }
                                },
                                3 => {
                                    if (x != 15) {
                                        neighborBlock = self.blocks[x + 1][y][z];
                                    } else {
                                        neighborBlock = neighbor_sides.x[z][y];
                                    }
                                },
                                4 => {
                                    if (y != 0) {
                                        neighborBlock = self.blocks[x][y - 1][z];
                                    }
                                },
                                5 => {
                                    if (y != 255) {
                                        neighborBlock = self.blocks[x][y + 1][z];
                                    }
                                },
                                else => @panic("Index should not excede 5"),
                            }

                            if (neighborBlock.id != .Air) {
                                continue :face;
                            }

                            const indexOffset = maxOffset;
                            const vert = try state.blocksArr.items[@intFromEnum(block.id)].gen_vertices_sides(
                                index1,
                                zlm.vec3(@floatFromInt(x), @floatFromInt(y), @floatFromInt(z)),
                            );
                            for (0..6) |index2| {
                                var i = index1 * 4 + index2;

                                if (index2 < 4) {
                                    try vertices.append(vert[index2]);
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

    pub fn gen_sides(self: *const @This()) Sides {
        var out: Sides = undefined;

        for (0..chunkWidth) |val| {
            for (0..chunkHeight) |y| {
                out.x[val][y] = self.blocks[chunkWidth - 1][y][val];
                out.neg_x[val][y] = self.blocks[0][y][val];

                out.z[val][y] = self.blocks[val][y][chunkWidth - 1];
                out.neg_z[val][y] = self.blocks[val][y][0];
            }
        }

        return out;
    }
};

const Sides = struct {
    x: [chunkWidth][chunkHeight]Block,
    neg_x: [chunkWidth][chunkHeight]Block,
    z: [chunkWidth][chunkHeight]Block,
    neg_z: [chunkWidth][chunkHeight]Block,

    const AllAir = Sides{
        .x = .{.{Block.Air} ** chunkHeight} ** chunkWidth,
        .neg_x = .{.{Block.Air} ** chunkHeight} ** chunkWidth,
        .z = .{.{Block.Air} ** chunkHeight} ** chunkWidth,
        .neg_z = .{.{Block.Air} ** chunkHeight} ** chunkWidth,
    };
};

pub const ChunkMap = struct {
    map: std.AutoHashMap(IVec3, RenderChunk),
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .map = std.AutoHashMap(IVec3, RenderChunk).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn get(self: *@This(), pos: IVec3) ?RenderChunk {
        return self.map.get(pos);
    }

    pub fn getPtr(self: *@This(), pos: IVec3) ?*RenderChunk {
        return self.map.getPtr(pos);
    }

    pub fn genChunk(self: *Self, chunkPos: IVec3) !void {
        const chunk = comptime Chunk.gen_half_solid_chunk();

        const rchunk = RenderChunk{
            .chunk = chunk,
            .mesh = null,
        };

        try self.map.put(chunkPos, rchunk);

        inline for (0..2) |ix| {
            inline for (0..2) |iz| {
                const x: i64 = @as(i64, @intCast(ix)) - 1;
                const z: i64 = @as(i64, @intCast(iz)) - 1;
                if (@abs(x) == @abs(z)) {
                    continue;
                }

                const offset = IVec3.new(x, 0, z);

                if (self.map.get(chunkPos.add(offset))) |rChunk| {
                    if (rChunk.mesh != null) {
                        try self.genMesh(chunkPos.add(offset));
                    }
                }
            }
        }
    }

    pub fn contains(self: *const Self, pos: IVec3) bool {
        return self.map.contains(pos);
    }

    pub fn genMesh(self: *Self, chunkPos: IVec3) !void {
        std.log.info("Generating chunk at: {}", .{chunkPos});
        if (self.map.getPtr(chunkPos)) |rChunk| {
            if (rChunk.mesh) |*mesh| {
                mesh.deinit();
            }

            const neighbors = self.genNeigbors(chunkPos);

            const meshData = try rChunk.chunk.gen_mesh(neighbors, self.allocator);
            errdefer meshData.indices.deinit();
            errdefer meshData.vertices.deinit();

            const vertexBuffer = sg.makeBuffer(.{
                .data = sg.asRange(meshData.vertices.items),
            });
            const indexBuffer = sg.makeBuffer(.{
                .type = .INDEXBUFFER,
                .data = sg.asRange(meshData.indices.items),
            });

            const mesh = Mesh{
                .vertices = meshData.vertices,
                .indices = meshData.indices,
                .vertexBuffer = vertexBuffer,
                .indexBuffer = indexBuffer,
            };

            rChunk.mesh = mesh;
        } else {
            return error.ChunkNotFound;
        }
    }

    pub fn deinit(self: *@This()) void {
        var iter = self.map.iterator();

        while (iter.next()) |val| {
            const rChunk = val.value_ptr;
            if (rChunk.mesh) |*mesh| {
                mesh.deinit();
            }
        }

        self.map.deinit();
    }

    fn genNeigbors(self: *const @This(), chunkPos: IVec3) Sides {
        var out: Sides = Sides.AllAir;

        inline for ([_][]const u8{ "x", "z" }) |dir| {
            inline for ([_]i64{ 1, -1 }) |offset| {
                var offsetVec = IVec3.zero;
                @field(offsetVec, dir) = offset;

                if (self.map.getPtr(chunkPos.add(offsetVec))) |neibor| {
                    if (offset == 1) {
                        @field(out, dir) = @field(neibor.chunk.sides, "neg_" ++ dir);
                    } else if (offset == -1) {
                        @field(out, "neg_" ++ dir) = @field(neibor.chunk.sides, dir);
                    }
                }
            }
        }

        return out;
    }
};

pub const RenderChunk = struct {
    chunk: Chunk,
    mesh: ?Mesh,
};

pub const Mesh = struct {
    vertices: std.ArrayList(root.Vertex),
    indices: std.ArrayList(u32),
    vertexBuffer: sg.Buffer,
    indexBuffer: sg.Buffer,

    pub fn deinit(self: *@This()) void {
        sg.destroyBuffer(self.vertexBuffer);
        sg.destroyBuffer(self.indexBuffer);

        self.vertices.deinit();
        self.indices.deinit();
    }
};

fn chunkPosFromPlayerPos(playerPos: zlm.Vec3) IVec3 {
    const div = zlm.Vec3{
        .x = chunkWidth,
        .y = chunkHeight,
        .z = chunkWidth,
    };

    const out = playerPos.div(div);

    return .{
        .x = @intFromFloat(out.x),
        .y = @intFromFloat(out.y),
        .z = @intFromFloat(out.z),
    };
}

fn getPlayerChunkPos() IVec3 {
    return chunkPosFromPlayerPos(state.cameraPos);
}

fn inRangeGen(chunkPos: IVec3, toGenPos: IVec3, dist2: u32) !void {
    if (toGenPos.distance2(chunkPos) > dist2) {
        return;
    }

    if (state.chunkMap.get(toGenPos)) |chunk| {
        if (chunk.mesh == null) {
            try state.genChunkMeshQueue.add(toGenPos);
        }

        return;
    }

    try state.genChunkQueue.add(toGenPos);
}

fn outRangeDel(chunkPos: IVec3, toGenPos: IVec3, dist2: u32) !void {
    if (toGenPos.distance2(chunkPos) <= dist2) {
        return;
    }

    if (state.chunkMap.getPtr(toGenPos)) |chunk| {
        if (chunk.mesh) |*mesh| {
            mesh.deinit();
            chunk.mesh = null;
        }
    }
}

/// Removes meshes outside chunk range
pub fn renderDistanceGen() !void {
    const chunkPos: IVec3 = getPlayerChunkPos();
    const dist2: u32 = @as(u32, @intCast(state.renderDistance)) * state.renderDistance;

    for (0..(state.renderDistance + 4) * 2) |dx| {
        for (0..(state.renderDistance + 4) * 2) |dz| {
            const toGenPos = IVec3.new(
                @as(i64, @intCast(dx)) - state.renderDistance + chunkPos.x,
                0,
                @as(i64, @intCast(dz)) - state.renderDistance + chunkPos.z,
            );

            try inRangeGen(chunkPos, toGenPos, dist2);
            try outRangeDel(chunkPos, toGenPos, dist2);
        }
    }
}

fn getNumberTextures() usize {
    return state.atlas.len / 32;
}

test "Convert Int to float" {
    const f: f32 = @floatFromInt(1);

    try std.testing.expectEqual(1.0, f);
}

test "GenSides" {
    const chunk = Chunk.genAll2();

    const expectedSides = Sides{
        .x = .{.{.{ .id = @as(BlockId, @enumFromInt(2)) }} ** chunkHeight} ** chunkWidth,
        .neg_x = .{.{.{ .id = @as(BlockId, @enumFromInt(2)) }} ** chunkHeight} ** chunkWidth,
        .z = .{.{.{ .id = @as(BlockId, @enumFromInt(2)) }} ** chunkHeight} ** chunkWidth,
        .neg_z = .{.{.{ .id = @as(BlockId, @enumFromInt(2)) }} ** chunkHeight} ** chunkWidth,
    };

    try std.testing.expectEqualDeep(expectedSides, chunk.sides);
}
