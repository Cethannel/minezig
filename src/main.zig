const std = @import("std");
const sokol = @import("sokol");
const sapp = sokol.app;
const sg = sokol.gfx;
const sglue = sokol.glue;
const slog = sokol.log;
const sdtx = sokol.debugtext;

const textures = @import("textures.zig");

const math = @import("math.zig");

const zlm = @import("zlm");
const Vec3 = zlm.Vec3;
const mat4 = zlm.Mat4;

//const shd = @import("shaders/triangle.glsl.zig");
const shd = @import("shaders/cube.glsl.zig");

const KC854 = 0;
const C64 = 1;
const ORIC = 2;

const window_w = 1280;
const window_h = 720;

const Color = struct { r: u8, g: u8, b: u8 };

const Mesh = struct {
    bind: sg.Bindings = .{},
    offset: Vec3,
    numIndices: u32,
};

var state: struct {
    rx: f32 = 0.0,
    ry: f32 = 0.0,
    rz: f32 = 0.0,
    dx: f32 = 0.0,
    dy: f32 = 0.0,
    dz: f32 = 0.0,
    pip: sg.Pipeline = .{},
    bind: sg.Bindings = .{},
    pass_action: sg.PassAction = .{},
    //view: mat4 = mat4.createLookAt(.{ .x = 0.0, .y = 1.5, .z = 6.0 }, Vec3.zero, Vec3.unitY),
    view: mat4 = mat4.createLook(Vec3{ .x = -2.0, .y = 0.0, .z = 0.0 }, Vec3.unitX, Vec3.unitY),
    numIndices: u32 = 0,
    allocator: std.mem.Allocator = undefined,
    lockedMouse: bool = false,
    mouseX: f32 = 0.0,
    mouseY: f32 = 0.0,
    text_pass_action: sg.PassAction = .{},
    meshes: std.ArrayList(Mesh) = undefined,
    atlas: []u32 = undefined,

    sensitivity: f32 = 0.001,

    colors: [3]Color = .{
        .{ .r = 0xf4, .g = 0x43, .b = 0x36 },
        .{ .r = 0x21, .g = 0x96, .b = 0xf3 },
        .{ .r = 0x4c, .g = 0xaf, .b = 0x50 },
    },
} = .{};

const Vertex = extern struct {
    x: f32,
    y: f32,
    z: f32,
    color: u32,
    u: f32,
    v: f32,
};

const vertices = [_]Vertex{
    .{ .x = 0.0, .y = 0.0, .z = 0.0, .color = 0xFF0000FF, .u = 0, .v = 0 },
    .{ .x = 1.0, .y = 0.0, .z = 0.0, .color = 0xFF0000FF, .u = 1, .v = 0 },
    .{ .x = 1.0, .y = 1.0, .z = 0.0, .color = 0xFF0000FF, .u = 1, .v = 1 },
    .{ .x = 0.0, .y = 1.0, .z = 0.0, .color = 0xFF0000FF, .u = 0, .v = 1 },

    .{ .x = 0.0, .y = 0.0, .z = 1.0, .color = 0xFF00FF00, .u = 0, .v = 0 },
    .{ .x = 1.0, .y = 0.0, .z = 1.0, .color = 0xFF00FF00, .u = 1, .v = 0 },
    .{ .x = 1.0, .y = 1.0, .z = 1.0, .color = 0xFF00FF00, .u = 1, .v = 1 },
    .{ .x = 0.0, .y = 1.0, .z = 1.0, .color = 0xFF00FF00, .u = 0, .v = 1 },

    .{ .x = 0.0, .y = 0.0, .z = 0.0, .color = 0xFFFF0000, .u = 0, .v = 0 },
    .{ .x = 0.0, .y = 1.0, .z = 0.0, .color = 0xFFFF0000, .u = 1, .v = 0 },
    .{ .x = 0.0, .y = 1.0, .z = 1.0, .color = 0xFFFF0000, .u = 1, .v = 1 },
    .{ .x = 0.0, .y = 0.0, .z = 1.0, .color = 0xFFFF0000, .u = 0, .v = 1 },

    .{ .x = 1.0, .y = 0.0, .z = 0.0, .color = 0xFFFF007F, .u = 0, .v = 0 },
    .{ .x = 1.0, .y = 1.0, .z = 0.0, .color = 0xFFFF007F, .u = 1, .v = 0 },
    .{ .x = 1.0, .y = 1.0, .z = 1.0, .color = 0xFFFF007F, .u = 1, .v = 1 },
    .{ .x = 1.0, .y = 0.0, .z = 1.0, .color = 0xFFFF007F, .u = 0, .v = 1 },

    .{ .x = 0.0, .y = 0.0, .z = 0.0, .color = 0xFFFF7F00, .u = 0, .v = 0 },
    .{ .x = 0.0, .y = 0.0, .z = 1.0, .color = 0xFFFF7F00, .u = 1, .v = 0 },
    .{ .x = 1.0, .y = 0.0, .z = 1.0, .color = 0xFFFF7F00, .u = 1, .v = 1 },
    .{ .x = 1.0, .y = 0.0, .z = 0.0, .color = 0xFFFF7F00, .u = 0, .v = 1 },

    .{ .x = 0.0, .y = 1.0, .z = 0.0, .color = 0xFF007FFF, .u = 0, .v = 0 },
    .{ .x = 0.0, .y = 1.0, .z = 1.0, .color = 0xFF007FFF, .u = 1, .v = 0 },
    .{ .x = 1.0, .y = 1.0, .z = 1.0, .color = 0xFF007FFF, .u = 1, .v = 1 },
    .{ .x = 1.0, .y = 1.0, .z = 0.0, .color = 0xFF007FFF, .u = 0, .v = 1 },
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    state.allocator = gpa.allocator();

    sapp.run(.{
        .init_cb = init,
        .frame_cb = frame,
        .cleanup_cb = cleanup,
        .event_cb = event_cb,
        .width = 640,
        .height = 480,
        .icon = .{ .sokol_default = true },
        .window_title = "triangle.zig",
        .sample_count = 4,
        .logger = .{ .func = slog.func },
    });

    if (gpa.deinit() == .leak) {
        std.log.err("Memory leak", .{});
    }
}

fn init() callconv(.C) void {
    sg.setup(.{
        .environment = sglue.environment(),
        .logger = .{ .func = slog.func },
    });

    state.atlas = textures.createAtlas(&.{
        "assets/textures/bricks.png",
        "assets/textures/dirt.png",
    }, state.allocator) catch unreachable;

    state.meshes = std.ArrayList(Mesh).init(state.allocator);

    state.meshes.append(createMesh(Vec3.zero)) catch unreachable;
    state.meshes.append(createMesh(Vec3.unitY)) catch unreachable;

    state.pass_action.colors[0] = .{
        .load_action = .CLEAR,
        .clear_value = .{ .r = 0.25, .g = 0.5, .b = 0.75, .a = 1 },
    };

    // create a shader and pipeline object
    var pip_desc: sg.PipelineDesc = .{
        .index_type = .UINT16,
        .shader = sg.makeShader(shd.texcubeShaderDesc(sg.queryBackend())),
        .depth = .{
            .compare = .LESS_EQUAL,
            .write_enabled = true,
        },
        .cull_mode = .NONE,
    };
    pip_desc.layout.attrs[shd.ATTR_texcube_pos].format = .FLOAT3;
    pip_desc.layout.attrs[shd.ATTR_texcube_color0].format = .UBYTE4N;
    pip_desc.layout.attrs[shd.ATTR_texcube_texcoord0].format = .FLOAT2;
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
    const dt: f32 = @floatCast(sapp.frameDuration() * 60);

    const pitch = state.mouseX;
    const yaw = state.mouseY;

    const offset = calcPos(pitch, yaw, state.dz);
    state.rx -= offset.x * dt;
    state.ry -= offset.y * dt;
    state.rz -= offset.z * dt;

    sdtx.print("First\n", .{});

    sg.beginPass(.{ .action = state.pass_action, .swapchain = sglue.swapchain() });
    sg.applyPipeline(state.pip);
    for (state.meshes.items) |mesh| {
        sg.applyBindings(mesh.bind);
        const vs_params = computeVsParams(
            mesh.offset.x,
            mesh.offset.y,
            mesh.offset.z,
        );
        sg.applyUniforms(shd.UB_vs_params, sg.asRange(&vs_params));
        sg.draw(0, mesh.numIndices, 1);
    }
    sg.endPass();

    sdtx.print("Second\n", .{});

    inline for (.{ KC854, C64, ORIC }) |font| {
        const color = state.colors[font];
        sdtx.font(font);
        sdtx.color3b(color.r, color.g, color.b);
        sdtx.print("Hello '{s}'!\n", .{"there"});
    }
    sdtx.font(KC854);
    sdtx.color3b(255, 128, 0);

    sg.beginPass(.{ .action = state.text_pass_action, .swapchain = sglue.swapchain() });
    sdtx.draw();
    sg.endPass();
    sg.commit();
}

fn cleanup() callconv(.C) void {
    state.meshes.deinit();
    state.allocator.free(state.atlas);
    sg.shutdown();
}

fn computeVsParams(rx: f32, ry: f32, rz: f32) shd.VsParams {
    const lookX = mat4.createAngleAxis(Vec3.unitY, state.mouseX * state.sensitivity);
    const lookY = mat4.createAngleAxis(Vec3.unitX, state.mouseY * state.sensitivity);

    const color = state.colors[KC854];
    sdtx.font(KC854);
    sdtx.color3b(color.r, color.g, color.b);

    const translationVec3 = Vec3{ .x = state.rx, .y = state.ry, .z = state.rz };

    const thing = translationVec3;

    const translation = mat4.createTranslation(thing);

    const model = mat4.createTranslationXYZ(rx, ry, rz).mul(translation);
    const aspect = sapp.widthf() / sapp.heightf();
    const proj = mat4.createPerspective(zlm.toRadians(60.0), aspect, 0.01, 1000.0);
    const view = state.view.mul(lookX).mul(lookY);
    const mvp = model.mul(view).mul(proj);
    //return shd.VsParams{ .mvp = mat4.mul(mat4.mul(proj, newView), model) };
    return shd.VsParams{ .mvp = math.Mat4.fromZlm(mvp) };
}

fn event_cb(event_arr: [*c]const sapp.Event) callconv(.C) void {
    const event = event_arr[0];

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
                    state.dy -= change;
                },
                .LEFT_SHIFT => {
                    state.dy += change;
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
                state.mouseX += event.mouse_dx;
                state.mouseY += event.mouse_dy;
            }
        },
        else => {},
    }
}

fn genCube(offset: Vec3, indexOffset: u16) struct {
    vertices: [6 * 4]Vertex,
    indices: [6 * 6]u16,
    maxIndex: u16,
} {
    var outVert = vertices;

    for (&outVert) |*vertex| {
        vertex.x += offset.x;
        vertex.y += offset.y;
        vertex.z += offset.z;

        if (!offset.eql(Vec3.zero)) {
            vertex.color = 0xFFFFFFFF;
        }

        const numTexs: f32 = @floatFromInt(state.atlas.len / 32 / 32);

        vertex.v /= numTexs;

        if (!offset.eql(Vec3.zero)) {
            vertex.v += 1.0 / numTexs;
        }
    }

    std.log.info("Starting at index {}", .{indexOffset});

    var outIndex = [_]u16{
        0,  1,  2,  0,  2,  3,
        6,  5,  4,  7,  6,  4,
        8,  9,  10, 8,  10, 11,
        14, 13, 12, 15, 14, 12,
        16, 17, 18, 16, 18, 19,
        22, 21, 20, 23, 22, 20,
    };

    var maxIndex: u16 = 0;

    for (&outIndex) |*index| {
        index.* += indexOffset;
        if (index.* > maxIndex) {
            maxIndex = index.*;
        }
    }

    maxIndex += 1;

    return .{
        .vertices = outVert,
        .indices = outIndex,
        .maxIndex = maxIndex,
    };
}

fn genChunk() struct { vertices: std.ArrayList(Vertex), indices: std.ArrayList(u16) } {}

fn createMesh(offset: Vec3) Mesh {
    var mesh: Mesh = .{
        .offset = offset,
        .numIndices = 0,
    };

    var img_desc: sg.ImageDesc = .{
        .width = 32,
        .height = @intCast(state.atlas.len / 32),
    };
    img_desc.data.subimage[0][0] = sg.asRange(state.atlas);
    mesh.bind.images[shd.IMG_tex] = sg.makeImage(img_desc);

    mesh.bind.samplers[shd.SMP_smp] = sg.makeSampler(.{});

    const cube1 = genCube(Vec3.zero, 0);
    std.debug.print("Cube1: {any}", .{cube1});
    const cube2 = genCube(Vec3.new(2.0, 0.0, 0.0), @intCast(cube1.maxIndex));

    const verticesToRender = cube1.vertices ++ cube2.vertices;

    std.log.info("Has num verts: {}", .{verticesToRender.len});

    const len = (@sizeOf(Vertex) * verticesToRender.len / 4);

    std.log.info(
        "Vertices: {any}",
        .{@as(*const [len]f32, @ptrCast(&verticesToRender))},
    );

    // create vertex buffer with triangle vertices
    mesh.bind.vertex_buffers[0] = sg.makeBuffer(.{
        .data = sg.asRange(&verticesToRender),
    });

    const indices = cube1.indices ++ cube2.indices;

    mesh.numIndices = @intCast(indices.len);

    std.log.info("Indices ({}): {any}", .{ indices.len, indices });

    mesh.bind.index_buffer = sg.makeBuffer(.{
        .type = .INDEXBUFFER,
        .data = sg.asRange(&indices),
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
