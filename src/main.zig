const std = @import("std");
const sokol = @import("sokol");
const ig = @import("cimgui");
const sapp = sokol.app;
const simgui = sokol.imgui;
const sg = sokol.gfx;
const sglue = sokol.glue;
const slog = sokol.log;
const sdtx = sokol.debugtext;

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
const shd = @import("shaders/cube.glsl.zig");

const blocks = @import("blocks.zig");

const KC854 = 0;
const C64 = 1;
const ORIC = 2;

const window_w = 1280;
const window_h = 720;

const Color = struct { r: u8, g: u8, b: u8 };

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
    text_pass_action: sg.PassAction = .{},
    atlas: []u32 = undefined,

    chunkMap: chunks.ChunkMap = undefined,

    cameraPos: Vec3 = Vec3.new(0.0, 129.0, 3.0),
    cameraFront: Vec3 = Vec3.new(0.0, 0.0, -1.0),
    cameraUp: Vec3 = Vec3.new(0.0, 1.0, 0.0),

    pitch: f32 = -180,
    yaw: f32 = 90.0,

    initialMouse: bool = true,

    sensitivity: f32 = 0.1,

    genChunkQueue: genChunkQueueT = undefined,
    genChunkMeshQueue: genChunkQueueT = undefined,

    blocksArr: std.ArrayList(blocks.Block) = undefined,

    colors: [3]Color = .{
        .{ .r = 0xf4, .g = 0x43, .b = 0x36 },
        .{ .r = 0x21, .g = 0x96, .b = 0xf3 },
        .{ .r = 0x4c, .g = 0xaf, .b = 0x50 },
    },

    gpa: GPA = undefined,

    renderDistance: u8 = 16,

    textureMap: std.StringHashMap(u32) = undefined,

    const GPA = std.heap.GeneralPurposeAllocator(.{
        .enable_memory_limit = true,
    });

    const genChunkContext = struct {
        playerPos: Vec3,
    };

    pub const genChunkQueueT = std.PriorityQueue(IVec3, genChunkContext, compChunks);

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
    x: f32,
    y: f32,
    z: f32,
    u: f32,
    v: f32,
    nx: f32,
    ny: f32,
    nz: f32,
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
        .window_title = "triangle.zig",
        .sample_count = 4,
        .logger = .{ .func = slog.func },
        .swap_interval = 0,
    });

    std.log.info("Total memory requested: {}", .{
        gpa.total_requested_bytes,
    });

    if (gpa.deinit() == .leak) {
        std.log.err("Memory leak", .{});
    }
}

fn init() callconv(.C) void {
    sg.setup(.{
        .environment = sglue.environment(),
        .logger = .{ .func = slog.func },
        .buffer_pool_size = 1024 * 4,
    });

    simgui.setup(.{
        .logger = .{ .func = slog.func },
    });

    state.textureMap = std.StringHashMap(u32).init(state.allocator);

    state.blocksArr = std.ArrayList(blocks.Block).init(state.allocator);

    defaultBlocks() catch unreachable;

    const blockTextures = textures.registerBlocks(state.blocksArr.items) catch unreachable;

    defer state.allocator.free(blockTextures);

    for (blockTextures, 0..) |blkName, i| {
        const name = blkName["assets/textures/".len..];
        std.log.info("Adding texture name: {s}", .{name});
        state.textureMap.put(name, @intCast(i)) catch unreachable;
    }

    state.atlas = textures.createAtlas(blockTextures, state.allocator) catch unreachable;

    var img_desc: sg.ImageDesc = .{
        .width = 32,
        .height = @intCast(state.atlas.len / 32),
    };
    img_desc.data.subimage[0][0] = sg.asRange(state.atlas);
    state.bind.images[shd.IMG_tex] = sg.makeImage(img_desc);

    state.bind.samplers[shd.SMP_smp] = sg.makeSampler(.{});

    state.genChunkQueue = State.genChunkQueueT.init(state.allocator, .{
        .playerPos = state.cameraPos,
    });

    state.genChunkMeshQueue = State.genChunkQueueT.init(state.allocator, .{
        .playerPos = state.cameraPos,
    });

    state.chunkMap = chunks.ChunkMap.init(state.allocator);

    for (0..4) |x| {
        for (0..4) |z| {
            const sx: i64 = @intCast(x);
            const sz: i64 = @intCast(z);
            state.genChunkQueue.add(IVec3.new(sx - 2, 0, sz - 2)) catch unreachable;
        }
    }

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
    };
    pip_desc.layout.attrs[shd.ATTR_texcube_pos].format = .FLOAT3;
    pip_desc.layout.attrs[shd.ATTR_texcube_texcoord0].format = .FLOAT2;
    pip_desc.layout.attrs[shd.ATTR_texcube_normal0].format = .FLOAT3;
    state.pip = sg.makePipeline(pip_desc);

    var sdtx_desc: sdtx.Desc = .{ .logger = .{ .func = slog.func } };
    sdtx_desc.fonts[KC854] = sdtx.fontKc854();
    sdtx_desc.fonts[C64] = sdtx.fontC64();
    sdtx_desc.fonts[ORIC] = sdtx.fontOric();
    sdtx.setup(sdtx_desc);

    state.text_pass_action.colors[0] = .{
        .load_action = .LOAD,
    };
}

fn frame() callconv(.C) void {
    simgui.newFrame(.{
        .width = sapp.width(),
        .height = sapp.height(),
        .delta_time = sapp.frameDuration(),
        .dpi_scale = sapp.dpiScale(),
    });

    const dt: f32 = @floatCast(sapp.frameDuration() * 60);

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

    sdtx.print("First\n", .{});

    sdtx.print("Dt is: {d:.2}\n", .{1.0 / sapp.frameDuration()});

    while (state.genChunkQueue.removeOrNull()) |chunkPos| {
        state.chunkMap.genChunk(chunkPos) catch unreachable;
        state.genChunkMeshQueue.add(chunkPos) catch unreachable;
    }

    while (state.genChunkMeshQueue.removeOrNull()) |chunkPos| {
        state.chunkMap.genMesh(chunkPos) catch unreachable;
    }

    if (1.0 / sapp.frameDuration() > 60.0) {
        chunks.renderDistanceGen() catch unreachable;
    }

    sg.beginPass(.{ .action = state.pass_action, .swapchain = sglue.swapchain() });
    sg.applyPipeline(state.pip);

    ig.igSetNextWindowPos(.{ .x = 400, .y = 10 }, ig.ImGuiCond_Once);
    ig.igSetNextWindowSize(.{ .x = 400, .y = 100 }, ig.ImGuiCond_Once);
    _ = ig.igBegin("Hello Dear ImGui!", 0, ig.ImGuiWindowFlags_None);
    _ = ig.igColorEdit3("Background", &state.pass_action.colors[0].clear_value.r, ig.ImGuiColorEditFlags_None);
    ig.igEnd();

    var chunkIter = state.chunkMap.map.iterator();

    while (chunkIter.next()) |entry| {
        const pos = entry.key_ptr;
        const chunk = entry.value_ptr;
        if (chunk.mesh) |mesh| {
            state.bind.vertex_buffers[0] = mesh.vertexBuffer;
            state.bind.index_buffer = mesh.indexBuffer;

            sg.applyBindings(state.bind);
            const vs_params = computeVsParams(
                @floatFromInt(pos.x * 16),
                @floatFromInt(pos.y * 16),
                @floatFromInt(pos.z * 16),
            );
            sg.applyUniforms(shd.UB_vs_params, sg.asRange(&vs_params));
            sg.draw(0, @intCast(mesh.indices.items.len), 1);
        }
    }
    sg.endPass();

    sdtx.print("Second\n", .{});

    inline for (.{ KC854, C64, ORIC }) |font| {
        const color = state.colors[font];
        sdtx.font(font);
        sdtx.color3b(color.r, color.g, color.b);
        sdtx.print("Hello '{s}'!\n", .{"there"});
    }

    sdtx.print("Currently using: {}", .{state.gpa.total_requested_bytes});
    sdtx.font(KC854);
    sdtx.color3b(255, 128, 0);

    sg.beginPass(.{ .action = state.text_pass_action, .swapchain = sglue.swapchain() });
    sdtx.draw();
    sg.endPass();

    sg.beginPass(.{ .action = state.text_pass_action, .swapchain = sglue.swapchain() });
    simgui.render();
    sg.endPass();

    sg.commit();
}

fn cleanup() callconv(.C) void {
    for (state.blocksArr.items) |*blk| {
        blk.deinit();
    }

    state.blocksArr.deinit();

    state.genChunkQueue.deinit();
    state.genChunkMeshQueue.deinit();
    state.chunkMap.deinit();
    state.allocator.free(state.atlas);
    sg.shutdown();
}

fn computeVsParams(rx: f32, ry: f32, rz: f32) shd.VsParams {
    const color = state.colors[KC854];
    sdtx.font(KC854);
    sdtx.color3b(color.r, color.g, color.b);

    const view = mat4.createLookAt(
        state.cameraPos,
        state.cameraPos.add(state.cameraFront),
        state.cameraUp,
    );

    const model = mat4.createTranslationXYZ(rx, ry, rz);
    const aspect = sapp.widthf() / sapp.heightf();
    const proj = mat4.createPerspective(zlm.toRadians(60.0), aspect, 0.01, 1000.0);
    const mvp = model.mul(view).mul(proj);
    //return shd.VsParams{ .mvp = mat4.mul(mat4.mul(proj, newView), model) };
    return shd.VsParams{ .mvp = math.Mat4.fromZlm(mvp) };
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

fn createMesh(offset: Vec3) Mesh {
    var mesh: Mesh = .{
        .offset = offset,
        .numIndices = 0,
        .indexBuffer = undefined,
        .vertexBuffer = undefined,
    };

    const chunk = chunks.Chunk.gen_solid_chunk().gen_mesh(state.allocator) catch unreachable;

    const verticesToRender = chunk.vertices.items;

    // create vertex buffer with triangle vertices
    mesh.vertexBuffer = sg.makeBuffer(.{
        .data = sg.asRange(verticesToRender),
    });

    const indices = chunk.indices.items;

    mesh.numIndices = @intCast(indices.len);

    mesh.indexBuffer = sg.makeBuffer(.{
        .type = .INDEXBUFFER,
        .data = sg.asRange(indices),
    });

    return mesh;
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
                "bricks.png",
            )).to_block("bricks"),
            try (try blocks.Cube.init_all(
                state.allocator,
                "dirt.png",
            )).to_block("dirt"),
        },
    );
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

    @import("std").testing.refAllDecls(@This());
}
