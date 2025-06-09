const std = @import("std");
pub const shd = @import("shaders/selector.glsl.zig");

const util = @import("utils.zig");

const sokol = @import("sokol");
const sg = sokol.gfx;
const sglue = sokol.glue;

const root = @import("main.zig");
const state = &root.state;

const chunks = @import("chunks.zig");

const zlm = @import("zlm");

const izlm = zlm.SpecializeOn(i64);

const blocks = @import("blocks.zig");

pub const IVec3 = izlm.Vec3;

pub const Vertex = extern struct {
    pos: zlm.Vec3,
    u: f32,
    v: f32,
};

pub const Selector = struct {
    pos: IVec3 = IVec3.zero,
    place_pos: IVec3 = IVec3.zero,
    vertices: std.ArrayList(Vertex) = undefined,
    indices: std.ArrayList(u32) = undefined,
    allocator: std.mem.Allocator = undefined,
    bind: sg.Bindings = .{},
    pip: sg.Pipeline = .{},
    pass_action: sg.PassAction = .{},

    vertexBuffer: sg.Buffer = .{},
    indexBuffer: sg.Buffer = .{},

    t: f32 = std.math.floatMax(f32),
    last_t: f32 = std.math.floatMax(f32),

    last_pos: zlm.Vec3 = zlm.Vec3.zero,
    last_look: zlm.Vec3 = zlm.Vec3.zero,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) !Self {
        var out: Self = .{};

        out.allocator = allocator;
        out.pos = IVec3.zero;

        out.vertices = try std.ArrayList(Vertex).initCapacity(allocator, baseVertices.len);
        out.vertices.appendSliceAssumeCapacity(&baseVertices);

        out.vertexBuffer = sg.makeBuffer(.{
            .data = sg.asRange(out.vertices.items),
        });

        out.indices = try std.ArrayList(u32).initCapacity(allocator, baseIndices.len);
        out.indices.appendSliceAssumeCapacity(&baseIndices);

        out.indexBuffer = sg.makeBuffer(.{
            .usage = .{ .index_buffer = true },
            .data = sg.asRange(out.indices.items),
        });

        var pip_desc: sg.PipelineDesc = .{
            .index_type = .UINT32,
            .shader = sg.makeShader(shd.selectorShaderDesc(sg.queryBackend())),
            .depth = .{
                .compare = .LESS_EQUAL,
                .write_enabled = true,
            },
            .cull_mode = .BACK,
            .alpha_to_coverage_enabled = true,
            .primitive_type = .LINES,
        };

        pip_desc.layout.attrs[shd.ATTR_selector_pos].format = .FLOAT3;
        pip_desc.layout.attrs[shd.ATTR_selector_texcoord0].format = .FLOAT2;
        out.pip = sg.makePipeline(pip_desc);

        out.pass_action.colors[0] = .{
            .load_action = .LOAD,
        };

        return out;
    }

    pub fn render(self: *Self) void {
        sg.beginPass(.{ .action = self.pass_action, .swapchain = sglue.swapchain() });

        self.bind.vertex_buffers[0] = self.vertexBuffer;
        self.bind.index_buffer = self.indexBuffer;

        inline for (.{ self.pos, self.place_pos }) |pos| {
            const vs_params = shd.VsParams{ .mvp = root.computeVsParams(
                @floatFromInt(pos.x),
                @floatFromInt(pos.y),
                @floatFromInt(pos.z),
            ) };

            sg.applyPipeline(self.pip);
            sg.applyBindings(self.bind);

            sg.applyUniforms(shd.UB_vs_params, sg.asRange(&vs_params));
            sg.draw(0, @intCast(self.indices.items.len), 1);
        }
        sg.endPass();
    }

    pub fn deinit(self: *@This()) void {
        sg.destroyPipeline(self.pip);

        sg.destroyBuffer(self.vertexBuffer);
        sg.destroyBuffer(self.indexBuffer);

        self.vertices.deinit();
        self.indices.deinit();
    }

    pub fn getVertices(self: *const Self) sg.Buffer {
        return self.vertices.items;
    }

    pub fn getIndices(self: *const Self) []const u32 {
        return self.indices.items;
    }

    pub noinline fn calcPos(self: *Self) void {
        if (!self.last_pos.eql(state.cameraPos) or !self.last_look.eql(state.cameraFront)) {
            self.t = std.math.floatMax(f32);
            self.last_t = std.math.floatMax(f32);
            self.last_pos = state.cameraPos;
            self.last_look = state.cameraPos;
        }

        var dirfrac = zlm.Vec3.zero;

        inline for (std.meta.fields(zlm.Vec3)) |field| {
            @field(dirfrac, field.name) = 1.0 / @field(state.cameraFront, field.name);
        }

        var boundsSel: blocks.Bounds = .{ .min = .zero, .max = .one };

        inline for ([3]comptime_int{ 0, -1, 1 }) |chunkX| {
            inline for ([3]comptime_int{ 0, -1, 1 }) |chunkZ| {
                const pos = chunks.worldToChunkPos(state.cameraPos.add(zlm.Vec3.new(16 * chunkX, 0, 16 * chunkZ)));
                var found = false;
                if (state.chunkMap.get(pos.chunkPos)) |chunk| {
                    for (chunk.blocks, 0..) |slice, x| {
                        for (slice, 0..) |column, y| {
                            for (column, 0..) |block, z| {
                                if (block.id != .Air) {
                                    const b = blocks.getBlockFromId(block.id).?;

                                    const bounds = b.bounds(.{
                                        .selfBlock = block,
                                    });
                                    var blockPos = zlm.vec3(
                                        @floatFromInt(x),
                                        @floatFromInt(y),
                                        @floatFromInt(z),
                                    ).add(chunks.chunkToWorldPos(pos.chunkPos))
                                        .add(bounds.min);

                                    // FIXME: I don't think this works for negative chunks

                                    const blockMaxPos = blockPos.add(bounds.max);

                                    const intersects = self.intersectAABB(state.cameraPos, dirfrac, blockPos, blockMaxPos);

                                    if (intersects) {
                                        if (self.t < self.last_t) {
                                            found = true;
                                            self.last_t = self.t;
                                            self.pos = iVecFromVec3(blockPos);
                                            boundsSel = bounds;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                if (found and chunkX == 0 and chunkZ == 0) {
                    break;
                }
            }
        }

        var last_t = std.math.floatMax(f32);
        defer self.t = self.last_t;

        inline for ([3]comptime_int{ 0, -1, 1 }) |x| {
            inline for ([3]comptime_int{ 0, -1, 1 }) |y| {
                inline for ([3]comptime_int{ 0, -1, 1 }) |z| {
                    if (@abs(x) + @abs(y) + @abs(z) != 1) {
                        continue;
                    }
                    const blockPos = vecFromIVec3(self.pos).add(zlm.vec3(x, y, z).mul(boundsSel.max));
                    const blockMaxPos = blockPos.add(zlm.Vec3.one);

                    const intersects = self.intersectAABB(
                        state.cameraPos,
                        dirfrac,
                        blockPos,
                        blockMaxPos,
                    );

                    if (intersects) {
                        if (self.t < last_t) {
                            last_t = self.t;
                            self.place_pos = iVecFromVec3(blockPos);
                        }
                    }
                }
            }
        }
    }

    fn intersectAABB(self: *Self, rayOrigin: zlm.Vec3, dirfrac: zlm.Vec3, lb: zlm.Vec3, rt: zlm.Vec3) bool {
        const t1 = (lb.x - rayOrigin.x) * dirfrac.x;
        const t2 = (rt.x - rayOrigin.x) * dirfrac.x;
        const t3 = (lb.y - rayOrigin.y) * dirfrac.y;
        const t4 = (rt.y - rayOrigin.y) * dirfrac.y;
        const t5 = (lb.z - rayOrigin.z) * dirfrac.z;
        const t6 = (rt.z - rayOrigin.z) * dirfrac.z;

        const tmin = @max(@max(@min(t1, t2), @min(t3, t4)), @min(t5, t6));
        const tmax = @min(@min(@max(t1, t2), @max(t3, t4)), @max(t5, t6));

        if (tmax < 0) {
            self.t = tmax;
            return false;
        }

        // if tmin > tmax, ray doesn't intersect AABB
        if (tmin > tmax) {
            self.t = tmax;
            return false;
        }

        self.t = tmin;
        return true;
    }
};

const baseIndices = [_]u32{
    // Bottom face edges
    0, 1, // Edge between (0,0,0) and (1,0,0)
    1, 2, // Edge between (1,0,0) and (1,1,0)
    2, 3, // Edge between (1,1,0) and (0,1,0)
    3, 0, // Edge between (0,1,0) and (0,0,0)

    // Top face edges
    4, 5, // Edge between (0,0,1) and (1,0,1)
    5, 6, // Edge between (1,0,1) and (1,1,1)
    6, 7, // Edge between (1,1,1) and (0,1,1)
    7, 4, // Edge between (0,1,1) and (0,0,1)

    // Vertical edges
    0, 4, // Edge between (0,0,0) and (0,0,1)
    1, 5, // Edge between (1,0,0) and (1,0,1)
    2, 6, // Edge between (1,1,0) and (1,1,1)
    3, 7, // Edge between (0,1,0) and (0,1,1)
};

const baseVertices = [_]Vertex{
    // 0: (0,0,0)
    .{
        .pos = .{ .x = 0.0, .y = 0.0, .z = 0.0 },
        .u = 0,
        .v = 0,
    },
    // 1: (1,0,0)
    .{
        .pos = .{ .x = 1.0, .y = 0.0, .z = 0.0 },
        .u = 1,
        .v = 0,
    },
    // 2: (1,1,0)
    .{
        .pos = .{ .x = 1.0, .y = 1.0, .z = 0.0 },
        .u = 1,
        .v = 1,
    },
    // 3: (0,1,0)
    .{
        .pos = .{ .x = 0.0, .y = 1.0, .z = 0.0 },
        .u = 0,
        .v = 1,
    },
    // 4: (0,0,1)
    .{
        .pos = .{ .x = 0.0, .y = 0.0, .z = 1.0 },
        .u = 0,
        .v = 0,
    },
    // 5: (1,0,1)
    .{
        .pos = .{ .x = 1.0, .y = 0.0, .z = 1.0 },
        .u = 1,
        .v = 0,
    },
    // 6: (1,1,1)
    .{
        .pos = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
        .u = 1,
        .v = 1,
    },
    // 7: (0,1,1)
    .{
        .pos = .{ .x = 0.0, .y = 1.0, .z = 1.0 },
        .u = 0,
        .v = 1,
    },
};

fn intersectAABB(rayOrigin: zlm.Vec3, rayDir: zlm.Vec3, lb: zlm.Vec3, rt: zlm.Vec3) bool {
    var dirfrac = zlm.Vec3.zero;

    inline for (std.meta.fields(zlm.Vec3)) |field| {
        @field(dirfrac, field.name) = 1.0 / @field(rayDir, field.name);
    }

    const t1 = (lb.x - rayOrigin.x) * dirfrac.x;
    const t2 = (rt.x - rayOrigin.x) * dirfrac.x;
    const t3 = (lb.y - rayOrigin.y) * dirfrac.y;
    const t4 = (rt.y - rayOrigin.y) * dirfrac.y;
    const t5 = (lb.z - rayOrigin.z) * dirfrac.z;
    const t6 = (rt.z - rayOrigin.z) * dirfrac.z;

    const tmin = @max(@max(@min(t1, t2), @min(t3, t4)), @min(t5, t6));
    const tmax = @min(@min(@max(t1, t2), @max(t3, t4)), @max(t5, t6));

    if (tmax < 0) {
        return false;
    }

    // if tmin > tmax, ray doesn't intersect AABB
    if (tmin > tmax) {
        return false;
    }

    return true;
}

pub fn vecToArr(input: zlm.Vec3) [3]f64 {
    return .{
        input.x,
        input.y,
        input.z,
    };
}

pub fn vecFromIVec3(input: IVec3) zlm.Vec3 {
    return .{
        .x = @floatFromInt(input.x),
        .y = @floatFromInt(input.y),
        .z = @floatFromInt(input.z),
    };
}

pub fn iVecFromVec3(input: zlm.Vec3) IVec3 {
    return .{
        .x = @intFromFloat(input.x),
        .y = @intFromFloat(input.y),
        .z = @intFromFloat(input.z),
    };
}

//test "Intersecct AABB" {
//    const blockOrigin = zlm.vec3(0.0, 0.0, 0.0);
//    const blockMax = zlm.vec3(1.0, 1.0, 1.0);
//
//    const rayOrigin = zlm.vec3(2.0, 0.0, 0.0);
//    const rayDir = zlm.vec3(-1.0, 0.0, 0.0);
//
//    const out = intersectAABB(rayOrigin, rayDir, blockOrigin, blockMax);
//
//    std.debug.print("Out: {any}", .{out});
//
//    try std.testing.expect(out.x < out.y);
//}
