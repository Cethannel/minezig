const std = @import("std");
const root = @import("main.zig");
const shd = @import("shaders/cube.glsl.zig");

const config = @import("config");

const uuid = @import("uuid");

const fastnoise = @import("fastnoise.zig");

const Blocks = @import("blocks.zig");

const utils = @import("utils.zig");

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
const fChunkHeight: comptime_float = @floatFromInt(chunkHeight);
const fHalfChunkHeight: comptime_float = fChunkHeight / 2.0;

pub const mesh_variants = &.{ "solid", "transparent" };

pub var vertexCount: usize = 0;
pub var chunkCount: usize = 0;

pub const BlockId = enum(u32) {
    Air = 0,
    _,
};

pub const BlockVariant = u8;

pub const Block = extern struct {
    id: BlockId,
    variant: BlockVariant = 0,

    const Self = @This();

    pub const Air: Self = .{
        .id = .Air,
    };

    pub fn eql(self: *const @This(), other: *const @This()) bool {
        return self.id == other.id;
    }
};

pub const NeighborBlock = extern struct {
    x: Block = .Air,
    neg_x: Block = .Air,
    y: Block = .Air,
    neg_y: Block = .Air,
    z: Block = .Air,
    neg_z: Block = .Air,

    const Self = @This();
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
    uuid: uuid.Uuid,

    pub const MeshData = struct {
        vertices: std.ArrayList(root.Vertex),
        indices: std.ArrayList(u32),
    };

    pub fn eql(self: *const @This(), other: *const @This()) bool {
        if (self == other) {
            return true;
        }

        for (0..chunkWidth) |x| {
            for (0..chunkHeight) |y| {
                for (0..chunkWidth) |z| {
                    if (!self.blocks[x][y][z].eql(
                        &other.blocks[x][y][z],
                    )) {
                        return false;
                    }
                }
            }
        }

        return true;
    }

    const Solid = gen_solid_chunk();

    const Self = @This();

    pub fn AllAir() @This() {
        return Self{
            .blocks = @splat(@splat(@splat(.Air))),
            .uuid = uuid.v4.new(),
        };
    }

    pub fn gen_solid_chunk() @This() {
        const idThing: u32 = 1;
        const blocks: @FieldType(@This(), "blocks") = @splat(@splat(@splat(Block{ .id = @enumFromInt(idThing) })));
        const tmp: @This() = .{
            .blocks = blocks,
        };

        return tmp;
    }

    pub fn gen_half_solid_chunk() @This() {
        @setEvalBranchQuota(chunkWidth * chunkHeight * chunkWidth * 2);
        //var stone = Blocks.getBlockId("stone").?;
        const idThing: u32 = 1;
        var blocks: [chunkWidth][chunkHeight][chunkWidth]Block = @splat(@splat(@splat(Block{ .id = .Air })));
        for (0..chunkWidth) |x| {
            for (0..chunkHeight / 2) |y| {
                for (0..chunkWidth) |z| {
                    blocks[x][y][z].id = @enumFromInt(idThing);
                }
            }
        }
        const tmp: @This() = .{
            .blocks = blocks,
        };

        return tmp;
    }

    fn genAll2() @This() {
        const idThing: u32 = 2;
        const blocks: @FieldType(@This(), "blocks") = @splat(@splat(@splat(Block{ .id = @enumFromInt(idThing) })));
        const tmp: @This() = .{
            .blocks = blocks,
        };

        return tmp;
    }

    pub noinline fn gen_mesh(
        self: *const @This(),
        neighbor_sides: Sides,
        allocator: std.mem.Allocator,
    ) !struct {
        solid: MeshData,
        transparent: MeshData,
    } {
        var solid_maxOffset: u32 = 0;
        var transparent_maxOffset: u32 = 0;

        const initial_len = 10240;
        var solid_vertices = try std.ArrayList(root.Vertex).initCapacity(allocator, initial_len);
        var solid_indices = try std.ArrayList(u32).initCapacity(allocator, initial_len);

        var transparent_vertices = try std.ArrayList(root.Vertex).initCapacity(allocator, initial_len);
        var transparent_indices = try std.ArrayList(u32).initCapacity(allocator, initial_len);

        for (self.blocks, 0..) |slice, x| {
            for (slice, 0..) |col, y| {
                for (col, 0..) |block, z| {
                    if (block.id != .Air) {
                        var neighors: NeighborBlock = .{};
                        if (x != 0) {
                            neighors.neg_x = self.blocks[x - 1][y][z];
                        } else {
                            neighors.neg_x = neighbor_sides.neg_x[z][y];
                        }
                        if (x != 15) {
                            neighors.x = self.blocks[x + 1][y][z];
                        } else {
                            neighors.x = neighbor_sides.x[z][y];
                        }
                        if (y != 0) {
                            neighors.neg_y = self.blocks[x][y - 1][z];
                        }
                        if (y != chunkHeight) {
                            neighors.y = self.blocks[x][y + 1][z];
                        }
                        if (z != 0) {
                            neighors.neg_z = self.blocks[x][y][z - 1];
                        } else {
                            neighors.neg_z = neighbor_sides.neg_z[x][y];
                        }
                        if (z != 15) {
                            neighors.z = self.blocks[x][y][z + 1];
                        } else {
                            neighors.z = neighbor_sides.z[x][y];
                        }
                        face: for (0..6) |index1| {
                            var neighborBlock: *const Block = &Block.Air;
                            switch (index1) {
                                0 => {
                                    neighborBlock = &neighors.neg_z;
                                },
                                1 => {
                                    neighborBlock = &neighors.z;
                                },
                                2 => {
                                    neighborBlock = &neighors.neg_x;
                                },
                                3 => {
                                    neighborBlock = &neighors.x;
                                },
                                4 => {
                                    neighborBlock = &neighors.neg_y;
                                },
                                5 => {
                                    neighborBlock = &neighors.y;
                                },
                                else => @panic("Index should not excede 5"),
                            }

                            if (should_skip_side(&block, neighborBlock, @enumFromInt(index1))) {
                                continue :face;
                            }

                            const blockStruct = Blocks.getBlockOrUnkownFromId(block.id);

                            const indexOffset = if (blockStruct.transparent) transparent_maxOffset else solid_maxOffset;
                            const vert = try blockStruct.gen_vertices_sides(
                                .{
                                    .side = @enumFromInt(index1),
                                    .pos = zlm.vec3(
                                        @floatFromInt(x),
                                        @floatFromInt(y),
                                        @floatFromInt(z),
                                    ),
                                    .neighbors = neighors,
                                    .selfBlock = block,
                                },
                            );
                            for (0..6) |index2| {
                                var i = index1 * 4 + index2;

                                if (index2 < 4) {
                                    if (blockStruct.transparent) {
                                        try transparent_vertices.append(vert[index2]);
                                    } else {
                                        try solid_vertices.append(vert[index2]);
                                    }
                                }

                                i = index1 * 6 + index2;

                                const index = baseIndices[i];
                                const newIndex: u32 = index + indexOffset;
                                if (blockStruct.transparent) {
                                    try transparent_indices.append(newIndex);
                                    if (newIndex > transparent_maxOffset) {
                                        transparent_maxOffset = newIndex;
                                    }
                                } else {
                                    try solid_indices.append(newIndex);
                                    if (newIndex > solid_maxOffset) {
                                        solid_maxOffset = newIndex;
                                    }
                                }
                            }
                            if (blockStruct.transparent) {
                                transparent_maxOffset += 1;
                            } else {
                                solid_maxOffset += 1;
                            }
                        }
                    }
                }
            }
        }

        inline for (.{
            &solid_vertices,
            &solid_indices,
            &transparent_vertices,
            &transparent_indices,
        }) |value| {
            {
                if (value.items.len < initial_len - 1024) {
                    var new: @typeInfo(@TypeOf(value)).pointer.child = //
                        try .initCapacity(
                            allocator,
                            value.items.len,
                        );
                    errdefer new.deinit();
                    new.appendSliceAssumeCapacity(value.items);
                    value.deinit();
                    value.* = new;
                }
            }
        }

        vertexCount += solid_vertices.items.len;
        chunkCount += 1;

        return .{
            .solid = MeshData{
                .indices = solid_indices,
                .vertices = solid_vertices,
            },
            .transparent = MeshData{
                .indices = transparent_indices,
                .vertices = transparent_vertices,
            },
        };
    }

    pub fn gen_sides(self: *const @This()) Sides {
        var out: Sides = .{
            .neg_z = @splat(@splat(@enumFromInt(0xFF))),
            .neg_x = @splat(@splat(@enumFromInt(0xFF))),
            .z = @splat(@splat(@enumFromInt(0xFF))),
            .x = @splat(@splat(@enumFromInt(0xFF))),
        };

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
        .x = @splat(@splat(Block.Air)),
        .neg_x = @splat(@splat(Block.Air)),
        .z = @splat(@splat(Block.Air)),
        .neg_z = @splat(@splat(Block.Air)),
    };
};

pub const ChunkGenFunc = *const fn (chunk: *Chunk, pos: IVec3) callconv(.C) void;

pub fn add_builtin_gen_funcs() !void {
    try state.chunkGenFuncs.append(&genTerrain);
    try state.chunkGenFuncs.append(&genWater);
    try state.chunkGenFuncs.append(&propGrass);
}

pub const ChunkMap = std.AutoHashMap(IVec3, Chunk);

pub const OldChunkMap = struct {
    map: std.AutoHashMap(IVec3, RenderChunk),
    chunkGenFuncs: std.ArrayList(ChunkGenFunc),
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) !Self {
        var chunkGenFuncs: std.ArrayList(ChunkGenFunc) = .init(allocator);
        errdefer chunkGenFuncs.deinit();

        try chunkGenFuncs.append(&genTerrain);
        try chunkGenFuncs.append(&genWater);
        try chunkGenFuncs.append(&propGrass);

        return .{
            .map = std.AutoHashMap(IVec3, RenderChunk).init(allocator),
            .chunkGenFuncs = chunkGenFuncs,
            .allocator = allocator,
        };
    }

    pub fn get(self: *const @This(), pos: IVec3) ?RenderChunk {
        return self.map.get(pos);
    }

    pub fn getPtr(self: *const @This(), pos: IVec3) ?*RenderChunk {
        return self.map.getPtr(pos);
    }

    pub fn put(self: *@This(), pos: IVec3, chunk: Chunk) !void {
        var rchunk = RenderChunk{
            .chunk = try self.allocator.create(Chunk),
            .regenMesh = false,
            .allocator = self.allocator,
            .solid_mesh = null,
            .transparent_mesh = null,
        };
        rchunk.chunk.* = chunk;

        if (self.map.getPtr(pos)) |c| {
            inline for (mesh_variants) |varName| {
                @field(rchunk, varName ++ "_mesh") = //
                    @field(c, varName ++ "_mesh");
            }
        }

        try self.map.put(pos, rchunk);
    }

    pub fn contains(self: *const Self, pos: IVec3) bool {
        return self.map.contains(pos);
    }

    pub fn setRegen(self: *Self, pos: IVec3) void {
        if (self.getPtr(pos)) |chunk| {
            chunk.regenMesh = true;
        }
    }

    pub noinline fn genMesh(self: *Self, chunkPos: IVec3) !void {
        if (config.chunkGenLog) {
            std.log.info("Generating chunk at: {}", .{chunkPos});
        }
        if (self.map.getPtr(chunkPos)) |rChunk| {
            rChunk.clear_meshes();

            const neighbors = self.genNeigbors(chunkPos);

            const meshData = try rChunk.chunk.gen_mesh(
                neighbors,
                self.allocator,
            );

            inline for (mesh_variants) |variant| {
                const data: Chunk.MeshData = @field(meshData, variant);
                errdefer data.indices.deinit();
                errdefer data.vertices.deinit();

                if (data.indices.items.len == 0 or data.vertices.items.len == 0) {
                    data.indices.deinit();
                    data.vertices.deinit();
                } else {
                    const vertexBuffer = sg.makeBuffer(.{
                        .data = sg.asRange(data.vertices.items),
                    });
                    const indexBuffer = sg.makeBuffer(.{
                        .type = .INDEXBUFFER,
                        .data = sg.asRange(data.indices.items),
                    });

                    const mesh = Mesh{
                        .vertices = data.vertices,
                        .indices = data.indices,
                        .vertexBuffer = vertexBuffer,
                        .indexBuffer = indexBuffer,
                    };

                    @field(rChunk, variant ++ "_mesh") = mesh;
                }
            }
        } else {
            return error.ChunkNotFound;
        }
    }

    pub fn deinit(self: *@This()) void {
        var iter = self.map.iterator();

        while (iter.next()) |val| {
            const rChunk = val.value_ptr;
            rChunk.deinit();
        }

        self.map.deinit();
        self.chunkGenFuncs.deinit();
    }

    noinline fn genNeigbors(self: *const @This(), chunkPos: IVec3) Sides {
        var out: Sides = .AllAir;

        inline for ([_][]const u8{ "x", "z" }) |dir| {
            inline for ([_]i64{ 1, -1 }) |offset| {
                var offsetVec = IVec3.zero;
                @field(offsetVec, dir) = offset;
                var inChunkOffsetVec = zlm.SpecializeOn(usize).Vec3.zero;
                @field(inChunkOffsetVec, dir) = @abs((offset - 1) / 2) * (chunkWidth - 1);
                var dirMulti = zlm.SpecializeOn(usize).Vec3.one;
                @field(dirMulti, dir) = 0;

                if (self.map.getPtr(chunkPos.add(offsetVec))) |neibor| {
                    var side = &@field(out, dir);
                    if (offset == -1) {
                        side = &@field(out, "neg_" ++ dir);
                    }
                    for (0..chunkWidth) |i| {
                        for (0..chunkHeight) |y| {
                            side[i][y] = neibor.chunk.blocks //
                            [dirMulti.x * i + inChunkOffsetVec.x] //
                                [dirMulti.y * y + inChunkOffsetVec.y] //
                                [dirMulti.z * i + inChunkOffsetVec.z];
                        }
                    }
                } else {
                    if (offset == 1) {
                        @field(out, dir) = @field(Sides.AllAir, "neg_" ++ dir);
                    } else if (offset == -1) {
                        @field(out, "neg_" ++ dir) = @field(Sides.AllAir, dir);
                    }
                }
            }
        }

        return out;
    }
};

pub fn set_block(chunkMap: *std.AutoHashMap(IVec3, Chunk), pos: IVec3, block: Block) !void {
    const poss = worldToChunkPos(utils.ivec3ToVec3(pos));

    std.log.info("Setting block in chunk: {}", .{poss.chunkPos});
    std.log.info("Setting block in chunk pos: {}", .{poss.inChunkPos});

    const chunk = chunkMap.getPtr(poss.chunkPos) orelse return error.ChunkNotFound;

    chunk.blocks[@intCast(poss.inChunkPos.x)] //
    [@intCast(poss.inChunkPos.y)][@intCast(poss.inChunkPos.z)] = block;
}

pub fn genChunk(map: *std.AutoHashMap(IVec3, Chunk), chunkPos: IVec3) !void {
    var chunk = Chunk.AllAir();

    for (state.chunkGenFuncs.items) |func| {
        func(&chunk, chunkPos);
    }

    try map.put(chunkPos, chunk);
}

pub fn getBlockPtr(self: *const std.AutoHashMap(IVec3, Chunk), pos: IVec3) ?*Block {
    const chunkPos = iWorldToChunkPos(pos);

    const chunk = self.getPtr(chunkPos.chunkPos) orelse return null;
    return &chunk.blocks //
    [@intCast(chunkPos.inChunkPos.x)] //
    [@intCast(chunkPos.inChunkPos.y)] //
    [@intCast(chunkPos.inChunkPos.z)];
}

pub fn regenNeighborMeshes(chunkPos: IVec3) !void {
    inline for ([_][]const u8{ "x", "z" }) |dir| {
        inline for ([_]i64{ 1, -1 }) |offset| {
            var offsetVec = IVec3.zero;
            @field(offsetVec, dir) = offset;

            const newPos = chunkPos.add(offsetVec);
            if (state.solidMeshMap.contains(newPos) or state.transparentMeshMap.contains(newPos)) {
                try mark_chunk_for_regen(newPos);
            }
        }
    }
}

pub fn mark_chunk_for_regen(pos: IVec3) !void {
    if (state.chunkMap.contains(pos)) {
        _ = try state.chunksToRegen.add(pos);
    }
}

pub fn chunkData(comptime T: type) type {
    return struct {
        inner: T,
        uuid: uuid.Uuid,

        const Self = @This();

        pub usingnamespace if (@hasDecl(T, "deinit")) struct {
            pub fn deinit(self: *Self) void {
                self.inner.deinit();
            }
        } else struct {};
    };
}

pub fn chunkDataMap(comptime T: type) type {
    return std.AutoHashMap(IVec3, chunkData(T));
}

fn self_or_inner(comptime T: type) type {
    const tInfo = @typeInfo(T);
    switch (tInfo) {
        .pointer => |pt| {
            return pt.child;
        },
        else => return T,
    }
}

pub fn deinitDataMap(input: anytype) void {
    var iter = input.valueIterator();
    while (iter.next()) |val| {
        val.deinit();
    }

    input.deinit();
}

pub const RenderChunk = struct {
    chunk: *Chunk,
    allocator: std.mem.Allocator,
    regenMesh: bool,
    solid_mesh: ?Mesh,
    transparent_mesh: ?Mesh,

    pub fn clear_meshes(self: *@This()) void {
        inline for (mesh_variants) |variant| {
            if (@field(self, variant ++ "_mesh")) |*mesh| {
                mesh.deinit();
            }

            @field(self, variant ++ "_mesh") = null;
        }
    }

    pub fn deinit(self: *@This()) void {
        self.allocator.destroy(self.chunk);
        self.clear_meshes();
    }

    pub fn render(self: *const @This(), pos: *const IVec3) void {
        const cMesh: ?Mesh = self.solid_mesh;
        if (cMesh) |mesh| {
            if (mesh.buffers) |buffs| {
                state.bind.vertex_buffers[0] = buffs.vertexBuffer;
                state.bind.index_buffer = buffs.indexBuffer;
                sg.applyBindings(state.bind);

                const vs_params = shd.VsParams{
                    .mvp = root.computeVsParams(
                        @floatFromInt(pos.x * 16),
                        @floatFromInt(pos.y * 16),
                        @floatFromInt(pos.z * 16),
                    ),
                };
                sg.applyUniforms(shd.UB_vs_params, sg.asRange(&vs_params));
                sg.draw(0, @intCast(mesh.indices.items.len), 1);
            }
        }
    }

    pub fn renderTransparent(self: *const @This(), pos: *const IVec3) void {
        const cMesh: ?Mesh = self.transparent_mesh;
        if (cMesh) |mesh| {
            if (mesh.buffers) |buffs| {
                state.bind.vertex_buffers[0] = buffs.vertexBuffer;
                state.bind.index_buffer = buffs.indexBuffer;
                sg.applyBindings(state.bind);

                const vs_params = shd.VsParams{
                    .mvp = root.computeVsParams(
                        @floatFromInt(pos.x * 16),
                        @floatFromInt(pos.y * 16),
                        @floatFromInt(pos.z * 16),
                    ),
                };
                sg.applyUniforms(shd.UB_vs_params, sg.asRange(&vs_params));
                sg.draw(0, @intCast(mesh.indices.items.len), 1);
            }
        }
    }

    pub fn hookupBuffers(self: *@This()) void {
        inline for (mesh_variants) |value| {
            if (@field(self, value ++ "_mesh")) |*msh| {
                msh.hookupBuffers();
            }
        }
    }
};

const emptyBuf: [0]u8 = .{};
var emptyAllocBuf = std.heap.FixedBufferAllocator.init(&emptyBuf);
const emptyAlloc = emptyAllocBuf.allocator();

pub const Mesh = struct {
    vertices: std.ArrayList(root.Vertex) = .init(emptyAlloc),
    indices: std.ArrayList(u32) = .init(emptyAlloc),
    buffers: ?struct {
        vertexBuffer: sg.Buffer,
        indexBuffer: sg.Buffer,
    } = null,

    pub fn deinit(self: *@This()) void {
        if (self.buffers) |buffs| {
            sg.destroyBuffer(buffs.vertexBuffer);
            sg.destroyBuffer(buffs.indexBuffer);
        }

        self.vertices.deinit();
        self.indices.deinit();
    }

    pub fn hookupBuffers(self: *@This()) void {
        if (self.vertices.items.len == 0 or self.indices.items.len == 0) {
            return;
        }
        if (self.buffers) |buffs| {
            std.log.info("Destroying buffer: {any}", .{buffs.indexBuffer});
            sg.destroyBuffer(buffs.indexBuffer);
            std.log.info("Destroying buffer: {any}", .{buffs.vertexBuffer});
            sg.destroyBuffer(buffs.vertexBuffer);
        }
        const vertexBuffer = sg.makeBuffer(.{
            .data = sg.asRange(self.vertices.items),
        });
        const indexBuffer = sg.makeBuffer(.{
            .type = .INDEXBUFFER,
            .data = sg.asRange(self.indices.items),
        });

        self.buffers = .{
            .vertexBuffer = vertexBuffer,
            .indexBuffer = indexBuffer,
        };
    }

    pub fn swapInplace(self: *@This(), other: @This()) void {
        var otherMut = other;
        inline for (.{ "vertices", "indices" }) |field| {
            std.mem.swap(@FieldType(@This(), field), &@field(self, field), &@field(otherMut, field));
        }
        otherMut.deinit();
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

    if (state.chunkMap.contains(toGenPos)) {
        if (!state.solidMeshMap.contains(toGenPos) and !state.transparentMeshMap.contains(toGenPos)) {
            try mark_chunk_for_regen(toGenPos);
        }

        return;
    }

    if (state.chunksInFlightSet.get(toGenPos) == null) {
        //std.log.info("Generating chunk in range at: {}", .{toGenPos});
        try state.sendWorkerThreadQueue.enqueue(.{
            .GetChunk = toGenPos,
        });

        try state.chunksInFlightSet.put(toGenPos, .{});
    }
}

fn outRangeDel(chunkPos: IVec3, toGenPos: IVec3, dist2: u32) !void {
    if (toGenPos.distance2(chunkPos) <= dist2) {
        return;
    }

    if (state.chunkMap.fetchRemove(toGenPos)) |kv| {
        _ = kv;
        if (@hasDecl(Chunk, "deinit")) {
            @compileError("Add deinit here you dummy");
        }
    }

    if (state.solidMeshMap.fetchRemove(toGenPos)) |kv| {
        var chunk = kv.value;
        chunk.deinit();
    }

    if (state.transparentMeshMap.fetchRemove(toGenPos)) |kv| {
        var chunk = kv.value;
        chunk.deinit();
    }
}

/// Removes meshes outside chunk range
pub fn renderDistanceGen() !void {
    const chunkPos: IVec3 = getPlayerChunkPos();
    const dist2: u32 = @as(u32, @intCast(state.renderDistance)) * state.renderDistance;

    var chunkIter = state.chunkMap.keyIterator();
    while (chunkIter.next()) |toGenPos| {
        try outRangeDel(chunkPos, toGenPos.*, dist2);
    }

    for (0..(state.renderDistance + 2) * 2) |dx| {
        for (0..(state.renderDistance + 2) * 2) |dz| {
            const toGenPos = IVec3.new(
                @as(i64, @intCast(dx)) - state.renderDistance + chunkPos.x,
                0,
                @as(i64, @intCast(dz)) - state.renderDistance + chunkPos.z,
            );

            try inRangeGen(chunkPos, toGenPos, dist2);
        }
    }
}

fn getNumberTextures() usize {
    return state.atlas.len / 32;
}

pub fn chunkToWorldPos(chunkPos: IVec3) zlm.Vec3 {
    return .{
        .x = @as(f32, @floatFromInt(chunkPos.x)) * 16.0,
        .y = @as(f32, @floatFromInt(chunkPos.y)),
        .z = @as(f32, @floatFromInt(chunkPos.z)) * 16.0,
    };
}

pub const chunkAndWorldPos = struct {
    chunkPos: IVec3,
    inChunkPos: IVec3,
};

pub fn worldToChunkPos(worldPos: zlm.Vec3) chunkAndWorldPos {
    const iWorldPos: IVec3 = .{
        .x = @intFromFloat(worldPos.x),
        .y = @intFromFloat(worldPos.y),
        .z = @intFromFloat(worldPos.z),
    };

    return iWorldToChunkPos(iWorldPos);
}

pub fn iWorldToChunkPos(iWorldPos: utils.IVec3) chunkAndWorldPos {
    const chunkPos = acount_for_negatives(iWorldPos);
    const inChunkPos = convert_to_inchunk_coords(iWorldPos);

    return .{
        .chunkPos = chunkPos,
        .inChunkPos = inChunkPos,
    };
}

/// Converts worldspace coordinates to coordinates within a chunk
fn convert_to_inchunk_coords(global_coords: IVec3) IVec3 {
    return IVec3.new(
        convert_single_coord(global_coords.x),
        @mod(global_coords.y, chunkHeight),
        convert_single_coord(global_coords.z),
    );
}

// FIXME: Remove if check
fn convert_single_coord(input: i64) i64 {
    if (@mod(input, chunkHeight) == 0) {
        return 0;
    } else {
        const out = if (@rem(input, chunkWidth) == 0) blk: {
            break :blk 0;
        } else blk: {
            break :blk (if (input < 0) @as(i64, 16) else @as(i64, 0)) + @rem(input, chunkWidth);
        };

        var buf: [128:0]u8 = undefined;
        var msg = std.fmt.bufPrint(&buf, "expected < 16 got: {}", .{out}) catch unreachable;
        utils.assert(out < 16, msg);
        msg = std.fmt.bufPrint(&buf, "expected >= 0 got: {}", .{out}) catch unreachable;
        utils.assert(out >= 0, msg);

        return out;
    }
}

fn convert_single(input: i64, size: i64) i64 {
    return @divTrunc((if (input < 0) input + 1 else input), size) - @as(i64, if (input < 0) 1 else 0);
}

/// Converts worldspace coordinates to coordinates of a chunk
fn acount_for_negatives(input: IVec3) IVec3 {
    return IVec3.new(
        convert_single(input.x, chunkWidth),
        convert_single(input.y, chunkHeight),
        convert_single(input.z, chunkWidth),
    );
}

fn should_skip_side(
    block: *const Block,
    neighbor: *const Block,
    side: Blocks.SideEnum,
) bool {
    if (neighbor.id == .Air) {
        return false;
    }

    const blockStruct = state.blocksArr.items[@intFromEnum(block.id)];
    const neighborStruct = state.blocksArr.items[@intFromEnum(neighbor.id)];

    if (blockStruct.transparent != neighborStruct.transparent) {
        return false;
    }

    if (blockStruct.should_generate_side(.{
        .neighbor = neighbor.*,
        .selfBlock = block.*,
        .side = side,
    })) |shouldGen| {
        return !shouldGen;
    }

    const blockBounds = blockStruct.bounds(.{ .selfBlock = block.* });
    const neighborBounds = neighborStruct.bounds(.{ .selfBlock = neighbor.* });

    switch (side) {
        .Z, .NegZ, .X, .NegX => {
            if (blockBounds.eqlDir(neighborBounds, .y)) {
                return true;
            } else {
                std.log.info("Block: {any}", .{blockBounds});
                std.log.info("Neighbor: {any}", .{neighborBounds});
                return false;
            }
        },
        .NegY => {
            if (neighborBounds.max.y < 1.0) {
                return false;
            }
        },
        else => {},
    }

    return true;
}

test "Convert Int to float" {
    const f: f32 = @floatFromInt(1);

    try std.testing.expectEqual(1.0, f);
}

test "GenNeighbors" {
    var map = ChunkMap.init(std.testing.allocator);
    defer map.deinit();

    const full = Chunk.Solid;
    const empty = Chunk.AllAir;

    try map.put(IVec3.zero, full);
    try map.put(IVec3.unitX, full);
    try map.put(IVec3.unitX.neg(), empty);
    try map.put(IVec3.zero, full);
    try map.put(IVec3.unitZ, full);
    try map.put(IVec3.unitZ.neg(), empty);

    const neihbors = map.genNeigbors(IVec3.zero);

    const expected = Sides{
        .x = @splat(@splat(.{ .id = @enumFromInt(1) })),
        .neg_x = @splat(@splat(.Air)),
        .z = @splat(@splat(.{ .id = @enumFromInt(1) })),
        .neg_z = @splat(@splat(.Air)),
    };

    try std.testing.expectEqualDeep(expected, neihbors);
}

fn genTerrain(chunk: *Chunk, chunkPos: IVec3) callconv(.C) void {
    const noise = fastnoise.Noise(f32){
        .seed = state.seed,
        .noise_type = .perlin,
    };

    const stoneId = Blocks.getBlockId("stone").?;

    const chunkGlobalPos = chunkToWorldPos(chunkPos);

    for (0..chunkWidth) |x| {
        for (0..chunkWidth) |z| {
            const fx: f32 = @floatFromInt(x);
            const fz: f32 = @floatFromInt(z);
            const height = noise.genNoise2D(chunkGlobalPos.x + fx, chunkGlobalPos.z + fz);
            for (0..chunkHeight) |y| {
                const fy: f32 = @floatFromInt(y);

                const ny: f32 = (fy - fHalfChunkHeight) / fHalfChunkHeight;

                const dy = ny - height;

                if (dy < -0.1) {
                    chunk.blocks[x][y][z].id = stoneId;
                }
            }
        }
    }
}

const waterHeight = 128;

comptime {
    if (waterHeight > chunkHeight) {
        @compileError("WaterHeight should be less than chunkHeight");
    }
}

fn genWater(chunk: *Chunk, chunkPos: IVec3) callconv(.C) void {
    _ = chunkPos;

    const waterId = Blocks.getBlockId("water").?;

    for (0..chunkWidth) |x| {
        for (0..chunkWidth) |z| {
            for (1..waterHeight + 1) |neg_y| {
                const y = waterHeight - neg_y;
                if (chunk.blocks[x][y][z].id == .Air) {
                    chunk.blocks[x][y][z] = .{
                        .id = waterId,
                    };
                } else {
                    break;
                }
            }
        }
    }
}

fn propGrass(chunk: *Chunk, chunkPos: IVec3) callconv(.C) void {
    _ = chunkPos;

    const grassId = Blocks.getBlockId("grass").?;
    const dirtId = Blocks.getBlockId("dirt").?;
    const stoneId = Blocks.getBlockId("stone").?;

    const dirtDepth = 3;

    for (0..chunkWidth) |x| {
        for (0..chunkWidth) |z| {
            for (1..chunkHeight + 1) |neg_y| {
                const y = chunkHeight - neg_y;
                const block = &chunk.blocks[x][y][z];
                if (block.id != .Air and block.id != stoneId) {
                    break;
                }

                if (block.id == stoneId) {
                    block.* = .{ .id = grassId };

                    for (1..dirtDepth + 1) |dy| {
                        if (y - dy < 0) {
                            break;
                        }
                        chunk.blocks[x][y - dy][z] = .{ .id = dirtId };
                    }
                    break;
                }
            }
        }
    }
}

pub const NeighborChunks = struct {
    x: ?*Chunk = null,
    neg_x: ?*Chunk = null,
    z: ?*Chunk = null,
    neg_z: ?*Chunk = null,
};

pub fn genMeshSides(
    pos: IVec3,
    neighbors: NeighborChunks,
) !void {
    var out: Sides = Sides.AllAir;
    var chunk = state.chunkMap.get(pos) orelse return;

    inline for ([_][]const u8{ "x", "z" }) |dir| {
        inline for ([_]i64{ 1, -1 }) |offset| {
            var offsetVec = IVec3.zero;
            @field(offsetVec, dir) = offset;
            var inChunkOffsetVec = zlm.SpecializeOn(usize).Vec3.zero;
            @field(inChunkOffsetVec, dir) = @abs((offset - 1) / 2) * (chunkWidth - 1);
            var dirMulti = zlm.SpecializeOn(usize).Vec3.one;
            @field(dirMulti, dir) = 0;

            const start = if (offset == 1) "" else "neg_";

            if (@field(neighbors, start ++ dir)) |neibor| {
                var side = &@field(out, dir);
                if (offset == -1) {
                    side = &@field(out, "neg_" ++ dir);
                }
                for (0..chunkWidth) |i| {
                    for (0..chunkHeight) |y| {
                        side[i][y] = neibor.blocks //
                        [dirMulti.x * i + inChunkOffsetVec.x] //
                            [dirMulti.y * y + inChunkOffsetVec.y] //
                            [dirMulti.z * i + inChunkOffsetVec.z];
                    }
                }
            } else {
                if (offset == 1) {
                    @field(out, dir) = @field(Sides.AllAir, "neg_" ++ dir);
                } else if (offset == -1) {
                    @field(out, "neg_" ++ dir) = @field(Sides.AllAir, dir);
                }
            }
        }
    }

    const meshData = try chunk.gen_mesh(out, state.allocator);

    var rc: @FieldType(state.recvChunkMeshQueue.innerT(), "rc") = .{
        .uuid = chunk.uuid,
        .solid = .{},
        .transparent = .{},
    };

    inline for (mesh_variants) |variant| {
        const data: Chunk.MeshData = @field(meshData, variant);
        errdefer data.indices.deinit();
        errdefer data.vertices.deinit();

        if (data.indices.items.len == 0 or data.vertices.items.len == 0) {
            data.indices.deinit();
            data.vertices.deinit();
        } else {
            const mesh = Mesh{
                .vertices = data.vertices,
                .indices = data.indices,
                .buffers = null,
            };

            @field(rc, variant) = mesh;
        }
    }

    try state.recvChunkMeshQueue.enqueue(.{
        .pos = pos,
        .rc = rc,
    });
}
