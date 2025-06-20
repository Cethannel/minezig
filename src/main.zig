const std = @import("std");
pub const sokol = @import("sokol");
const ig = @import("cimgui");
const sapp = sokol.app;
const simgui = sokol.imgui;
const sg = sokol.gfx;
const sglue = sokol.glue;
const slog = sokol.log;
const sdtx = sokol.debugtext;

const clayRender = @import("clay_render.zig");

const util = @import("utils.zig");
const workerThread = @import("workerThread.zig");

const zuuid = @import("uuid");

const config = @import("config");

const clay = @import("zclay");

const zset = @import("ziglangSet");

const c = if (config.controllerSupport) @cImport(
    @cInclude("Gamepad.h"),
) else struct {};

const Thread = std.Thread;

const IZlm = zlm.SpecializeOn(i64);
pub const IVec3 = IZlm.Vec3;

const chunks = @import("chunks.zig");

const textures = @import("textures.zig");

const math = @import("math.zig");

const zlm = @import("zlm");
const Vec3 = zlm.Vec3;
const mat4 = zlm.Mat4;

//const shd = @import("shaders/triangle.glsl.zig");
const shd = @import("cube.glsl");
const selectorShd = @import("shaders/selector.glsl.zig");

const blocks = @import("blocks.zig");

const KC854 = 0;
const C64 = 1;
const ORIC = 2;

const window_w = 1920;
const window_h = 1080;

const selector = @import("selector.zig");
const crosshair = @import("crosshair.zig");

const Color = struct { r: u8, g: u8, b: u8 };

pub const std_options: std.Options = .{
    .log_level = .info,
};

pub const Mesh = struct {
    vertexBuffer: sg.Buffer,
    indexBuffer: sg.Buffer,
    offset: Vec3,
    numIndices: u32,
};

const State = struct {
    dx: f32 = 0.0,
    dy: f32 = 0.0,
    dz: f32 = 0.0,
    pip: sg.Pipeline = .{},
    bind: sg.Bindings = .{},
    pass_action: sg.PassAction = .{},
    numIndices: u32 = 0,
    allocator: std.mem.Allocator = undefined,
    lockedMouse: bool = false,
    mouseX: f32 = 0.0,
    mouseY: f32 = 0.0,
    controllerMouseX: f32 = 0.0,
    controllerMouseY: f32 = 0.0,
    text_pass_action: sg.PassAction = .{},
    atlas: []u32 = undefined,

    chunkMap: std.AutoHashMap(IVec3, chunks.Chunk) = undefined,
    solidMeshMap: chunks.chunkDataMap(chunks.Mesh) = undefined,
    transparentMeshMap: chunks.chunkDataMap(chunks.Mesh) = undefined,
    chunksToRegen: zset.ArraySetManaged(IVec3) = undefined,

    chunkGenFuncs: std.ArrayList(chunks.ChunkGenFunc) = undefined,

    cameraPos: Vec3 = Vec3.new(0.0, 129.0, 3.0),
    prevCameraPos: Vec3 = Vec3.new(0.0, 0.0, 0.0),
    cameraFront: Vec3 = Vec3.new(0.0, 0.0, -1.0),
    prevCameraFront: Vec3 = Vec3.new(0.0, 0.0, -1.0),
    cameraUp: Vec3 = Vec3.new(0.0, 1.0, 0.0),
    prevCameraUp: Vec3 = Vec3.new(0.0, 1.0, 0.0),

    pitch: f32 = -180,
    yaw: f32 = 90.0,

    fov: f32 = 60.0,

    initialMouse: bool = true,

    enable_frustum_culling: bool = true,

    sensitivity: f32 = 0.1,

    genChunkMeshQueue: genChunkQueueT = undefined,

    sendWorkerThreadQueue: util.mspc(workerThread.toWorkerThreadMessage) = undefined,
    recvWorkerThreadQueue: util.mspc(workerThread.fromWorkerThreadMessage) = undefined,

    recvChunkMeshQueue: util.mspc(struct { rc: struct {
        solid: chunks.Mesh,
        transparent: chunks.Mesh,
        uuid: zuuid.Uuid,

        const Self = @This();

        pub fn deinit(self: *Self) void {
            self.solid.deinit();
            self.transparent.deinit();
        }
    }, pos: IVec3 }) = undefined,

    chunksInFlightSet: chunksInFlightT = undefined,

    blocksArr: std.ArrayList(blocks.Block) = undefined,
    blocksNameArr: std.ArrayList(u8) = undefined,

    selectedBlock: c_int = 0,

    colors: [3]Color = .{
        .{ .r = 0xf4, .g = 0x43, .b = 0x36 },
        .{ .r = 0x21, .g = 0x96, .b = 0xf3 },
        .{ .r = 0x4c, .g = 0xaf, .b = 0x50 },
    },

    gpa: GPA = undefined,

    renderDistance: u8 = 16,

    textureMap: std.StringHashMap(u32) = undefined,

    seed: i32 = 1337,

    selector: selector.Selector = undefined,
    crosshair: crosshair.Crosshair = .{},

    close: std.atomic.Value(bool) = std.atomic.Value(bool).init(false),

    workerThreadHandle: std.Thread = undefined,

    const GPA = std.heap.GeneralPurposeAllocator(.{
        .enable_memory_limit = true,
    });

    const genChunkContext = struct {
        playerPos: Vec3,
    };

    pub const chunksInFlightT = std.AutoHashMap(IVec3, struct {});
    pub const genChunkQueueT = util.mspc(IVec3);

    fn compChunks(ctx: genChunkContext, a: IVec3, b: IVec3) std.math.Order {
        _ = ctx;
        const ia = ivec3ToVec3(a);
        const ib = ivec3ToVec3(b);

        const ad = state.cameraPos.distance(ia);
        const bd = state.cameraPos.distance(ib);

        return std.math.order(ad, bd);
    }
};

pub var state: State = .{};

pub const Vertex = extern struct {
    pos: zlm.Vec3,
    u: f32,
    v: f32,
    normal: zlm.Vec3,
    modifierColor: zlm.Vec3,
};

pub fn main() !void {
    var gpa = State.GPA{
        .requested_memory_limit = 8 * 1024 * 1024 * 1024,
    };
    state.allocator = gpa.allocator();
    state.gpa = gpa;

    sapp.run(.{
        .init_cb = init,
        .frame_cb = frame,
        .cleanup_cb = cleanup,
        .event_cb = event_cb,
        .width = window_w,
        .height = window_h,
        .icon = .{ .sokol_default = true },
        .window_title = "minezig",
        .sample_count = 4,
        .logger = .{ .func = slog.func },
        .swap_interval = 0,
    });

    std.log.info("Total memory requested: {}", .{
        gpa.total_requested_bytes,
    });

    std.log.info("Chunks vertex average: {}", .{
        chunks.vertexCount / chunks.chunkCount,
    });

    if (gpa.deinit() == .leak) {
        std.log.err("Memory leak", .{});
    }
}

fn init() callconv(.C) void {
    if (config.controllerSupport) {
        c.Gamepad_init();

        c.Gamepad_buttonDownFunc(gamepad_buttonDownFunc, null);
        c.Gamepad_buttonUpFunc(gamepad_buttonUpFunc, null);
        c.Gamepad_axisMoveFunc(gamepad_axisMovedFunc, null);
    }

    sg.setup(.{
        .environment = sglue.environment(),
        .logger = .{ .func = slog.func },
        .buffer_pool_size = 1024 * 8,
    });

    simgui.setup(.{
        .logger = .{ .func = slog.func },
    });

    state.selector = selector.Selector.init(state.allocator) catch unreachable;
    state.crosshair = crosshair.Crosshair.init();

    state.textureMap = std.StringHashMap(u32).init(state.allocator);

    state.blocksArr = std.ArrayList(blocks.Block).init(state.allocator);
    state.blocksNameArr = std.ArrayList(u8).init(state.allocator);

    state.blocksArr.append(blocks.AirBlock) catch unreachable;

    defaultBlocks() catch unreachable;

    for (state.blocksArr.items) |block| {
        state.blocksNameArr.appendSlice(block.blockName.*) catch unreachable;
        state.blocksNameArr.append(0) catch unreachable;
    }
    state.blocksNameArr.append(0) catch unreachable;

    const blockTextures = textures.registerBlocks(state.blocksArr.items) catch unreachable;

    defer state.allocator.free(blockTextures);

    registerBlockUpdates();

    state.atlas = textures.createAtlas(blockTextures, state.allocator) catch unreachable;

    for (blockTextures, 0..) |blkName, i| {
        const basePath = "assets/textures/";
        const name = state.allocator.alloc(u8, blkName.len - "assets/textures/".len) catch unreachable;
        @memcpy(name, blkName[basePath.len..]);
        std.log.info("Adding texture name: {s}", .{name});
        state.textureMap.put(name, @intCast(i)) catch unreachable;
        state.allocator.free(blkName);
    }

    var img_desc: sg.ImageDesc = .{
        .width = 32,
        .height = @intCast(state.atlas.len / 32),
    };
    img_desc.data.subimage[0][0] = sg.asRange(state.atlas);
    state.bind.images[shd.IMG_tex] = sg.makeImage(img_desc);

    state.bind.samplers[shd.SMP_smp] = sg.makeSampler(.{});

    state.genChunkMeshQueue = State.genChunkQueueT.init(state.allocator, 64 * 64) catch unreachable;

    state.sendWorkerThreadQueue = util.mspc(workerThread.toWorkerThreadMessage) //
        .init(state.allocator, 1024) catch unreachable;
    state.recvWorkerThreadQueue = util.mspc(workerThread.fromWorkerThreadMessage) //
        .init(state.allocator, 1024) catch unreachable;

    state.recvChunkMeshQueue = @TypeOf(state.recvChunkMeshQueue).init(state.allocator, 64 * 64) catch unreachable;

    state.chunksInFlightSet = State.chunksInFlightT.init(state.allocator);

    state.chunkMap = std.AutoHashMap(IVec3, chunks.Chunk).init(state.allocator);
    state.chunkMap.ensureTotalCapacity(32 * 32) catch unreachable;

    state.solidMeshMap = chunks.chunkDataMap(chunks.Mesh).init(state.allocator);
    state.solidMeshMap.ensureTotalCapacity(32 * 32) catch unreachable;

    state.transparentMeshMap = chunks.chunkDataMap(chunks.Mesh).init(state.allocator);
    state.transparentMeshMap.ensureTotalCapacity(32 * 32) catch unreachable;

    state.chunksToRegen = zset.ArraySetManaged(IVec3).init(state.allocator);

    state.chunkGenFuncs = std.ArrayList(chunks.ChunkGenFunc).init(state.allocator);
    chunks.add_builtin_gen_funcs() catch unreachable;

    state.pass_action.colors[0] = .{
        .load_action = .CLEAR,
        .clear_value = .{ .r = 0.25, .g = 0.5, .b = 0.75, .a = 1 },
    };

    // create a shader and pipeline object
    var pip_desc: sg.PipelineDesc = .{
        .index_type = .UINT32,
        .shader = sg.makeShader(shd.texcubeShaderDesc(sg.queryBackend())),
        .depth = .{
            .compare = .LESS_EQUAL,
            .write_enabled = true,
        },
        .cull_mode = .BACK,
        .colors = colors: {
            var out: [4]sg.ColorTargetState = [_]sg.ColorTargetState{.{}} ** 4;

            out[0] = sg.ColorTargetState{
                .blend = .{
                    .enabled = true,
                    .src_factor_rgb = .SRC_ALPHA,
                    .dst_factor_rgb = .ONE_MINUS_SRC_ALPHA,
                    .src_factor_alpha = .ONE,
                    .dst_factor_alpha = .ZERO,
                },
            };

            break :colors out;
        },
    };
    pip_desc.layout.attrs[shd.ATTR_texcube_pos].format = .FLOAT3;
    pip_desc.layout.attrs[shd.ATTR_texcube_texcoord0].format = .FLOAT2;
    pip_desc.layout.attrs[shd.ATTR_texcube_normal0].format = .FLOAT3;
    pip_desc.layout.attrs[shd.ATTR_texcube_modifierColor0].format = .FLOAT3;
    state.pip = sg.makePipeline(pip_desc);

    var sdtx_desc: sdtx.Desc = .{ .logger = .{ .func = slog.func } };
    sdtx_desc.fonts[KC854] = sdtx.fontKc854();
    sdtx_desc.fonts[C64] = sdtx.fontC64();
    sdtx_desc.fonts[ORIC] = sdtx.fontOric();
    sdtx.setup(sdtx_desc);

    state.text_pass_action.colors[0] = .{
        .load_action = .LOAD,
    };

    state.workerThreadHandle = std.Thread.spawn(
        .{},
        workerThread.workerThread,
        .{},
    ) catch unreachable;
}

fn frame() callconv(.C) void {
    var frameAlloc = std.heap.ArenaAllocator.init(state.allocator);
    defer frameAlloc.deinit();
    const alloc = frameAlloc.allocator();
    _ = &alloc;

    if (config.controllerSupport) {
        c.Gamepad_processEvents();
    }
    simgui.newFrame(.{
        .width = sapp.width(),
        .height = sapp.height(),
        .delta_time = sapp.frameDuration(),
        .dpi_scale = sapp.dpiScale(),
    });

    playerMovement() catch |err| {
        std.log.err(
            "Error when calculating player movement: {s}",
            .{
                @errorName(err),
            },
        );
    };

    eventQueue() catch |err| {
        std.log.err(
            "Error in processing the event queue: {s}",
            .{@errorName(err)},
        );
    };

    imguiPass() catch |err| {
        std.log.err(
            "Error while drawing debug ui: {s}",
            .{@errorName(err)},
        );
    };

    worldRender() catch |err| {
        std.log.err(
            "Error while rendering the world: {s}",
            .{@errorName(err)},
        );
    };

    state.selector.render();
    state.crosshair.render();

    sdtx.print("Second\n", .{});

    inline for (.{ "x", "y", "z" }) |dir| {
        sdtx.print("{s}: {d:.2}\n", .{ dir, @field(state.cameraPos, dir) });
    }

    inline for (.{ KC854, C64, ORIC }) |font| {
        const color = state.colors[font];
        sdtx.font(font);
        sdtx.color3b(color.r, color.g, color.b);
        sdtx.print("Hello '{s}'!\n", .{"there"});
    }

    sdtx.print("Currently using: {}", .{state.gpa.total_requested_bytes});
    sdtx.font(KC854);
    sdtx.color3b(255, 128, 0);

    uiRender() catch |err| {
        std.log.err(
            "Error while rendering the UI: {s}",
            .{@errorName(err)},
        );
    };
}

noinline fn uiRender() !void {
    sg.beginPass(.{ .action = state.text_pass_action, .swapchain = sglue.swapchain() });
    sdtx.draw();
    sg.endPass();

    sg.beginPass(.{ .action = state.text_pass_action, .swapchain = sglue.swapchain() });
    simgui.render();
    sg.endPass();

    sg.commit();
}

noinline fn recvWorker() !void {
    while (state.recvWorkerThreadQueue.dequeue()) |msg| {
        switch (msg) {
            .NewChunk => |nc| {
                _ = state.chunksInFlightSet.remove(nc.pos);
                try state.chunkMap.put(nc.pos, nc.chunk);
                try chunks.regenNeighborMeshes(nc.pos);
                try chunks.mark_chunk_for_regen(nc.pos);
            },
        }
    }
}

fn clear_meshes(pos: IVec3) void {
    if (state.solidMeshMap.fetchRemove(pos)) |kv| {
        var mesh = kv.value;
        mesh.deinit();
    }
    if (state.transparentMeshMap.fetchRemove(pos)) |kv| {
        var mesh = kv.value;
        mesh.deinit();
    }
}

noinline fn genMeshes() !void {
    while (state.recvChunkMeshQueue.dequeue()) |rchunkthing| {
        var rChunk = rchunkthing.rc;
        if (state.chunkMap.getPtr(rchunkthing.pos)) |chunk| {
            if (chunk.uuid != rChunk.uuid) {
                rChunk.deinit();
                continue;
            }
        }
        inline for (.{ "solid", "transparent" }) |field| {
            if (@field(state, field ++ "MeshMap").getPtr(rchunkthing.pos)) |data| {
                data.deinit();
            }
            var mesh = @field(rChunk, field);
            mesh.hookupBuffers();
            try @field(state, field ++ "MeshMap").put(rchunkthing.pos, .{
                .inner = mesh,
                .uuid = rChunk.uuid,
            });
        }
    }

    var chunksIter = state.chunksToRegen.iterator();

    while (chunksIter.next()) |entry| {
        var neighbors: chunks.NeighborChunks = .{};
        var chunkPos = entry.key_ptr.*;
        defer _ = state.chunksToRegen.remove(chunkPos);
        inline for ([_][]const u8{ "x", "z" }) |dir| {
            inline for ([_]i64{ 1, -1 }) |offset| {
                var offsetVec = IVec3.zero;
                @field(offsetVec, dir) = offset;
                const chunk = state.chunkMap.getPtr(chunkPos.add(offsetVec));
                if (offset == 1) {
                    @field(neighbors, dir) = chunk;
                }
                if (offset == -1) {
                    @field(neighbors, "neg_" ++ dir) = chunk;
                }
            }
        }
        const thread = try std.Thread.spawn(.{}, chunks.genMeshSides, .{
            chunkPos,
            neighbors,
        });
        thread.detach();
    }
}

noinline fn eventQueue() !void {
    recvWorker() catch |err| {
        std.log.err("Error in recvWorker", .{});
        return err;
    };

    try genMeshes();

    chunks.renderDistanceGen() catch |err| {
        std.log.err("Error in renderDistanceGen", .{});
        return err;
    };
}

noinline fn worldRender() !void {
    sg.endPass();
    sg.beginPass(.{ .action = state.pass_action, .swapchain = sglue.swapchain() });
    sg.applyPipeline(state.pip);
    var chunkIter = state.solidMeshMap.iterator();
    while (chunkIter.next()) |entry| {
        try renderMesh(entry.value_ptr, entry.key_ptr);
    }
    chunkIter = state.transparentMeshMap.iterator();
    while (chunkIter.next()) |entry| {
        try renderMesh(entry.value_ptr, entry.key_ptr);
    }
    sg.endPass();
}

inline fn renderMesh(mesh: *const chunks.chunkData(chunks.Mesh), pos: *const IVec3) !void {
    if (state.chunkMap.getPtr(pos.*)) |chunk| {
        if (chunk.uuid != mesh.uuid) {
            clear_meshes(pos.*);
            return;
        }
    } else {
        clear_meshes(pos.*);
        return;
    }
    if (mesh.inner.buffers) |buffs| {
        state.bind.vertex_buffers[0] = buffs.vertexBuffer;
        state.bind.index_buffer = buffs.indexBuffer;
        sg.applyBindings(state.bind);

        const worldPos = chunks.chunkToWorldPos(pos.*);

        const mvp = computeVsParams(
            worldPos.x,
            worldPos.y,
            worldPos.z,
        );

        if (state.enable_frustum_culling //
        and worldPos.swizzle("xz").distance2(state.cameraPos.swizzle("xz")) //
            > comptime (32 * 32))
        {
            const aabb = util.AABB{
                .min = worldPos,
                .max = worldPos.add(.{
                    .x = chunks.chunkWidth,
                    .y = chunks.chunkHeight,
                    .z = chunks.chunkWidth,
                }),
            };
            const tan_fov = @tan(0.5 * zlm.toRadians(state.fov + 10));
            const aspect = sapp.widthf() / sapp.heightf();
            const frustrum: util.CullingFrustum = .{
                .near_right = aspect * near * tan_fov,
                .near_top = near * tan_fov,
                .near_plane = -near,
                .far_plane = -far,
            };
            const view = mat4.createLookAt(
                state.cameraPos,
                state.cameraPos.add(state.cameraFront),
                state.cameraUp,
            );
            const model = mat4.createTranslationXYZ(worldPos.x, worldPos.y, worldPos.z);
            const transform = model.mul(view);
            if (!util.SATVisibilityTest(frustrum, transform, aabb)) {
                return;
            }
        }

        const vs_params = shd.VsParams{
            .mvp = mvp,
        };
        sg.applyUniforms(shd.UB_vs_params, sg.asRange(&vs_params));
        sg.draw(0, @intCast(mesh.inner.indices.items.len), 1);
    }
}

noinline fn imguiPass() !void {
    sg.beginPass(.{ .action = state.pass_action, .swapchain = sglue.swapchain() });
    sg.applyPipeline(state.pip);

    ig.igSetNextWindowPos(.{ .x = 400, .y = 10 }, ig.ImGuiCond_Once);
    ig.igSetNextWindowSize(.{ .x = 400, .y = 100 }, ig.ImGuiCond_Once);
    _ = ig.igBegin("Hello Dear ImGui!", 0, ig.ImGuiWindowFlags_None);
    ig.igText("Position:");
    const dirs = [_][]const u8{ "x", "y", "z" };
    inline for (dirs) |name| {
        var thing: c_int = @intCast(@field(state.selector.pos, name));
        _ = ig.igDragInt(name ++ ": ", &thing);
        @field(state.selector.pos, name) = thing;
    }

    _ = ig.igCombo("Block select", &state.selectedBlock, state.blocksNameArr.items.ptr);

    if (ig.igButton("Place block")) {
        state.sendWorkerThreadQueue.enqueue(.{
            .SetBlock = .{
                .pos = state.selector.pos,
                .block = .{
                    .id = @enumFromInt(state.selectedBlock),
                },
            },
        }) catch unreachable;
    }

    _ = ig.igCheckbox("Toggle culling: ", &state.enable_frustum_culling);

    ig.igEnd();
}

noinline fn playerMovement() !void {
    const dt: f32 = @floatCast(sapp.frameDuration() * 60);

    state.pitch += state.controllerMouseY * dt;
    state.yaw += state.controllerMouseX * dt;

    if (state.pitch > 89.0) {
        state.pitch = 89.0;
    }
    if (state.pitch > -89.0) {
        state.pitch = -89.0;
    }

    var direction: Vec3 = undefined;
    direction.x = @cos(zlm.toRadians(state.yaw)) * @cos(zlm.toRadians(state.pitch));
    direction.y = @sin(zlm.toRadians(state.pitch));
    direction.z = @sin(zlm.toRadians(state.yaw)) * @cos(zlm.toRadians(state.pitch));
    state.cameraFront = direction.normalize();

    const nonY = Vec3.new(1.0, 0.0, 1.0);

    //const lookY = mat4.createAngleAxis(Vec3.unitX, state.mouseY);
    state.cameraPos = state.cameraPos.add(state.cameraFront.scale(state.dz * dt).mul(nonY));
    state.cameraPos = state.cameraPos.sub(
        state.cameraFront.cross(state.cameraUp).normalize().scale(state.dx * dt).mul(nonY),
    );

    state.cameraPos = state.cameraPos.add(Vec3.new(0.0, state.dy * dt, 0.0));

    if (!state.cameraPos.eql(state.prevCameraPos) or !state.cameraFront.eql(state.prevCameraFront) or !state.cameraUp.eql(state.prevCameraUp)) {
        state.selector.calcPos();
        state.prevCameraPos = state.cameraPos;
        state.prevCameraFront = state.cameraFront;
        state.prevCameraUp = state.cameraUp;
    }
}

fn cleanup() callconv(.C) void {
    state.close.store(true, .release);
    state.workerThreadHandle.join();

    var tMapIter = state.textureMap.keyIterator();

    while (tMapIter.next()) |key| {
        std.log.info("Freeing: {s}", .{key.*});
        state.allocator.free(key.*);
    }

    state.textureMap.deinit();

    for (state.blocksArr.items) |*blk| {
        blk.deinit();
    }

    state.blocksArr.deinit();
    state.blocksNameArr.deinit();

    state.selector.deinit();

    state.sendWorkerThreadQueue.deinit();
    state.recvWorkerThreadQueue.deinit();
    state.recvChunkMeshQueue.deinit();
    state.chunksInFlightSet.deinit();
    state.genChunkMeshQueue.deinit();
    state.chunkMap.deinit();
    chunks.deinitDataMap(&state.solidMeshMap);
    chunks.deinitDataMap(&state.transparentMeshMap);
    state.chunksToRegen.deinit();
    state.chunkGenFuncs.deinit();
    state.allocator.free(state.atlas);
    sg.shutdown();

    if (config.controllerSupport) {
        c.Gamepad_shutdown();
    }
}

// Frustum near and far.
pub const near = 0.01;
pub const far = 1000;

pub fn computeVsParams(rx: f32, ry: f32, rz: f32) zlm.Mat4 {
    const view = mat4.createLookAt(
        state.cameraPos,
        state.cameraPos.add(state.cameraFront),
        state.cameraUp,
    );

    const model = mat4.createTranslationXYZ(rx, ry, rz);
    const aspect = sapp.widthf() / sapp.heightf();
    const proj = mat4.createPerspective(zlm.toRadians(state.fov), aspect, 0.01, 1000.0);
    const mvp = model.mul(view).mul(proj);
    return mvp;
}

fn event_cb(event_arr: [*c]const sapp.Event) callconv(.C) void {
    const event = event_arr[0];

    _ = simgui.handleEvent(event);

    switch (event.type) {
        .KEY_DOWN, .KEY_UP => {
            if (event.key_repeat) {
                return;
            }
            const change: f32 = switch (event.type) {
                .KEY_DOWN => 1.0,
                .KEY_UP => -1.0,
                else => unreachable,
            };
            switch (event.key_code) {
                .A => {
                    state.dx += change;
                },
                .D => {
                    state.dx -= change;
                },
                .W => {
                    state.dz += change;
                },
                .S => {
                    state.dz -= change;
                },
                .SPACE => {
                    state.dy += change;
                },
                .LEFT_SHIFT => {
                    state.dy -= change;
                },
                .ESCAPE => {
                    if (event.type == .KEY_DOWN) {
                        sapp.lockMouse(!sapp.mouseLocked());
                    }
                },
                else => {},
            }
        },
        .MOUSE_DOWN => {
            if (sapp.mouseLocked()) {
                switch (event.mouse_button) {
                    .LEFT => {
                        state.sendWorkerThreadQueue.enqueue(.{
                            .SetBlock = .{
                                .pos = state.selector.pos,
                                .block = .{
                                    .id = .Air,
                                },
                            },
                        }) catch {
                            std.log.err("Failed to set block to air", .{});
                        };
                    },
                    .RIGHT => {
                        state.sendWorkerThreadQueue.enqueue(.{
                            .SetBlock = .{
                                .pos = state.selector.place_pos,
                                .block = .{
                                    .id = @enumFromInt(state.selectedBlock),
                                },
                            },
                        }) catch {
                            std.log.err("Failed to set block ", .{});
                        };
                    },
                    else => {},
                }
            }
        },
        .MOUSE_MOVE => {
            if (sapp.mouseLocked()) {
                state.mouseX += event.mouse_dx * state.sensitivity;
                state.mouseY += event.mouse_dy * state.sensitivity;

                if (state.initialMouse) {
                    state.initialMouse = false;
                    return;
                }

                state.yaw += event.mouse_dx * state.sensitivity;
                state.pitch += event.mouse_dy * state.sensitivity;
            }
        },
        else => {},
    }
}

fn gamepad_buttonDownFunc(
    dev: [*c]c.Gamepad_device,
    buttonId: c_uint,
    timestamp: f64,
    context: ?*anyopaque,
) callconv(.C) void {
    _ = &dev;
    _ = &context;
    _ = &timestamp;

    switch (buttonId) {
        0 => {
            state.dy += 1.0;
        },
        1 => {
            state.dy -= 1.0;
        },
        else => {},
    }
}

fn gamepad_buttonUpFunc(
    dev: [*c]c.Gamepad_device,
    buttonId: c_uint,
    timestamp: f64,
    context: ?*anyopaque,
) callconv(.C) void {
    _ = &dev;
    _ = &context;
    _ = &timestamp;

    switch (buttonId) {
        0 => {
            state.dy -= 1.0;
        },
        1 => {
            state.dy += 1.0;
        },
        else => {},
    }
}

fn gamepad_axisMovedFunc(
    dev: [*c]c.Gamepad_device,
    axisId: c_uint,
    value: f32,
    lastValue: f32,
    timestamp: f64,
    context: ?*anyopaque,
) callconv(.C) void {
    _ = &dev;
    _ = &context;
    _ = &timestamp;
    _ = &lastValue;

    switch (axisId) {
        0 => {
            state.dx = -value;
        },
        1 => {
            state.dz = -value;
        },
        3 => {
            state.mouseX += value;
            state.controllerMouseX = value;
        },
        4 => {
            state.mouseY += value;
            state.controllerMouseY = value;
        },
        else => {},
    }
}

fn calcPos(pitch: f32, yaw: f32, offset: f32) Vec3 {
    const pitchRadian = pitch; // zlm.toRadians(pitch);
    const yawRadian = yaw; // zlm.toRadians(yaw);

    const newPosX = offset * @sin(yawRadian) * @cos(pitchRadian);
    const newPosY = offset * -@sin(pitchRadian);
    const newPosZ = offset * @cos(yawRadian) * @cos(pitchRadian);

    return .{
        .x = newPosX,
        .y = newPosY,
        .z = newPosZ,
    };
}

fn defaultBlocks() !void {
    try state.blocksArr.appendSlice(
        &.{
            try (try blocks.Cube.init_all(
                state.allocator,
                .{ .file = "bricks.png" },
            )).to_block("bricks"),
            try (try blocks.Cube.init_all(
                state.allocator,
                .{ .file = "dirt.png" },
            )).to_block("dirt"),
            try (try blocks.Cube.init_all(
                state.allocator,
                .{ .file = "stone.png" },
            )).to_block("stone"),
            try (try blocks.Cube.init_sides(
                state.allocator,
                .{
                    .TopBotOthers = .{
                        .top = .{
                            .file = "grass_top.png",
                            .colorOveride = .{
                                .x = 124.0 / 256.0,
                                .y = 189.0 / 256.0,
                                .z = 107.0 / 256.0,
                            },
                        },
                        .other = .{ .file = "grass_side.png" },
                        .bot = .{ .file = "dirt.png" },
                    },
                },
            )).to_block("grass"),
            try (try blocks.Slab.init_sides(state.allocator, .{
                .TopBotOthers = .{
                    .top = .{ .file = "stone_slab_top.png" },
                    .other = .{ .file = "stone_slab_side.png" },
                    .bot = .{ .file = "stone_slab_top.png" },
                },
            })).to_block("stone_slab"),
            try (try blocks.Cube.init_all(
                state.allocator,
                .{
                    .file = "glass.png",
                },
            )).to_block_transparent("glass"),
            try (try blocks.Fluid.init_all(
                state.allocator,
                .{
                    .file = "water_still.png",
                },
            )).to_block_transparent("water"),
            try (try blocks.Cube.init_all(state.allocator, .{
                .file = "Unkown.png",
            })).to_block_transparent("unkown"),
        },
    );
}

fn registerBlockUpdates() void {
    registerBlockUpdate("water", @ptrCast(&water_update));
}

fn registerBlockUpdate(blockName: []const u8, callback: blocks.Block.blockUpdate) void {
    const blockId = blocks.getBlockId(blockName) orelse {
        std.log.err("Failed to find block with name: {s}", .{blockName});
        return;
    };
    if (blocks.getBlockFromIdPtr(blockId)) |block| {
        block.inner_block_update = callback;
    }
}

pub fn ivec3ToVec3(input: IVec3) Vec3 {
    return .{
        .x = @floatFromInt(input.x),
        .y = @floatFromInt(input.y),
        .z = @floatFromInt(input.z),
    };
}

test {
    _ = @import("blocks.zig");
    _ = @import("utils.zig");

    @import("std").testing.refAllDecls(@This());
}

test "cross test" {
    const v1 = IVec3.new(0, 3, 4);
    const v2 = IVec3.new(2, 2, 2);

    const out = v1.cross(v2);

    try std.testing.expectEqualDeep(IVec3.new(-2, 8, -6), out);
}

fn water_update(
    self: *const blocks.Fluid,
    args: *const blocks.BlockUpdateParams,
) void {
    _ = self;
    std.log.info("Updating water at: {}", .{args.pos});
    for ([_]i64{ -1, 1 }) |x| {
        for ([_]i64{ -1, 1 }) |z| {
            const neighborpos = args.pos.add(util.IVec3.new(x, 0, z));
            const neighbor = chunks.getBlockPtr(args.chunksMap, neighborpos) orelse continue;
            if (neighbor.id == .Air) {
                waterProp(args.chunksMap, neighborpos, args.setblockCallback, args.blockUpdateCallback);
            }
        }
    }
}

fn waterProp(
    chunkMap: *const chunks.ChunkMap,
    pos: IVec3,
    blockSetCallBack: blocks.BlockUpdateParams.setBlockCallbackT,
    blockUpdateCallback: blocks.BlockUpdateParams.blockUpdateCallbackT,
) void {
    const waterId = blocks.getBlockId("water") orelse {
        std.log.err("Failed to find water id", .{});
        return;
    };
    var neighborWater: u8 = 0;
    outer: for ([_]i64{ -1, 1 }) |x| {
        inline for (.{ "x", "z" }) |field| {
            var offset = util.IVec3.zero;
            @field(offset, field) = x;
            const neighborpos = pos.add(offset);
            if (chunks.getBlockPtr(chunkMap, neighborpos)) |neighbor| {
                std.log.info("Checking neighbor with block: {s}", .{blocks.getBlockFromId(neighbor.id).?.blockName.*});
                if (neighbor.id == waterId) {
                    neighborWater += 1;
                }
                if (neighborWater >= 2) {
                    break :outer;
                }
            }
        }
    }

    if (neighborWater >= 2) {
        std.log.info("Propegating water: {}", .{pos});
        blockSetCallBack(&pos, &chunks.Block{
            .id = waterId,
        });

        inline for (.{ -1, 1 }) |x| {
            inline for (.{ "x", "z" }) |field| {
                var offset = util.IVec3.zero;
                @field(offset, field) = x;
                const neighborpos = pos.add(offset);
                blockUpdateCallback(&neighborpos);
            }
        }
    }
}

fn posLessThan(ctx: Vec3, a: IVec3, b: IVec3) bool {
    const dista = ctx.distance2(chunks.chunkToWorldPos(a));
    const distb = ctx.distance2(chunks.chunkToWorldPos(b));

    return dista < distb;
}
