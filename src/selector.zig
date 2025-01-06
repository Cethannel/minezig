const std = @import("std");
pub const shd = @import("shaders/selector.glsl.zig");

const sokol = @import("sokol");
const sapp = sokol.app;
const simgui = sokol.imgui;
const sg = sokol.gfx;
const sglue = sokol.glue;
const slog = sokol.log;
const sdtx = sokol.debugtext;

const root = @import("main.zig");
const state = &root.state;

const chunks = @import("chunks.zig");

const zlm = @import("zlm");

const izlm = zlm.SpecializeOn(i64);

pub const IVec3 = izlm.Vec3;

pub const Vertex = extern struct {
    pos: zlm.Vec3,
    u: f32,
    v: f32,
};

pub const Selector = struct {
    pos: IVec3 = IVec3.zero,
    vertices: std.ArrayList(Vertex) = undefined,
    indices: std.ArrayList(u32) = undefined,
    allocator: std.mem.Allocator = undefined,
    bind: sg.Bindings = .{},
    pip: sg.Pipeline = .{},
    pass_action: sg.PassAction = undefined,

    vertexBuffer: sg.Buffer = undefined,
    indexBuffer: sg.Buffer = undefined,

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
            .type = .INDEXBUFFER,
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

    pub fn calcPos(self: *Self) void {
        const pos = chunks.worldToChunkPos(state.cameraPos);

        if (state.chunkMap.get(pos.chunkPos)) |chunk| {
            for (chunk.chunk.blocks, 0..) |slice, x| {
                for (slice, 0..) |column, y| {
                    for (column, 0..) |block, z| {
                        if (block.id != .Air) {
                            const blockPos = zlm.vec3(
                                @floatFromInt(x),
                                @floatFromInt(y),
                                @floatFromInt(z),
                            );

                            const blockMaxPos = blockPos.add(zlm.Vec3.one);

                            const fInChunkPos = vecFromIVec3(pos.inChunkPos);

                            const out = intersectAABB(fInChunkPos, state.cameraFront, blockPos, blockMaxPos);

                            if (out.x < out.y) {
                                std.log.info("Found intersection: {any}", .{blockPos});
                                self.pos = iVecFromVec3(blockPos);
                            }
                        }
                    }
                }
            }
        }
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

fn intersectAABB(rayOrigin: zlm.Vec3, rayDir: zlm.Vec3, boxMin: zlm.Vec3, boxMax: zlm.Vec3) zlm.Vec2 {
    const tMin = boxMin.sub(rayOrigin).div(rayDir);
    const tMax = boxMax.sub(rayOrigin).div(rayDir);
    const t1 = tMin.componentMin(tMax);
    const t2 = tMin.componentMin(tMax);
    const tNear = @max(@max(t1.x, t1.y), t1.z);
    const tFar = @min(@min(t2.x, t2.y), t2.z);
    return zlm.vec2(tNear, tFar);
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

test "Intersecct AABB" {
    const blockOrigin = zlm.vec3(0.0, 0.0, 0.0);
    const blockMax = zlm.vec3(1.0, 1.0, 1.0);

    const rayOrigin = zlm.vec3(2.0, 0.0, 0.0);
    const rayDir = zlm.vec3(-1.0, 0.0, 0.0);

    const out = intersectAABB(rayOrigin, rayDir, blockOrigin, blockMax);

    std.debug.print("Out: {any}", .{out});

    try std.testing.expect(out.x < out.y);
}
