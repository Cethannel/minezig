const std = @import("std");
const sokol = @import("sokol");
const sapp = sokol.app;
const sg = sokol.gfx;
const sglue = sokol.glue;
const slog = sokol.log;
const sdtx = sokol.debugtext;

const chunks = @import("chunks.zig");

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

pub const Mesh = struct {
    bind: sg.Bindings = .{},
    offset: Vec3,
    numIndices: u32,
};

var state: struct {
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
    meshes: std.ArrayList(Mesh) = undefined,
    atlas: []u32 = undefined,

    cameraPos: Vec3 = Vec3.new(0.0, 0.0, 3.0),
    cameraFront: Vec3 = Vec3.new(0.0, 0.0, -1.0),
    cameraUp: Vec3 = Vec3.new(0.0, 1.0, 0.0),

    pitch: f32 = -180,
    yaw: f32 = 90.0,

    initialMouse: bool = true,

    sensitivity: f32 = 0.1,

    colors: [3]Color = .{
        .{ .r = 0xf4, .g = 0x43, .b = 0x36 },
        .{ .r = 0x21, .g = 0x96, .b = 0xf3 },
        .{ .r = 0x4c, .g = 0xaf, .b = 0x50 },
    },
} = .{};

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

const vertices = [_]Vertex{
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

    .{ .x = 1.0, .y = 0.0, .z = 0.0, .u = 0, .v = 0, .nx = 1.0, .ny = 0.0, .nz = 0.0 },
    .{ .x = 1.0, .y = 1.0, .z = 0.0, .u = 1, .v = 0, .nx = 1.0, .ny = 0.0, .nz = 0.0 },
    .{ .x = 1.0, .y = 1.0, .z = 1.0, .u = 1, .v = 1, .nx = 1.0, .ny = 0.0, .nz = 0.0 },
    .{ .x = 1.0, .y = 0.0, .z = 1.0, .u = 0, .v = 1, .nx = 1.0, .ny = 0.0, .nz = 0.0 },

    .{ .x = 0.0, .y = 0.0, .z = 0.0, .u = 0, .v = 0, .nx = 0.0, .ny = -1.0, .nz = 0.0 },
    .{ .x = 0.0, .y = 0.0, .z = 1.0, .u = 1, .v = 0, .nx = 0.0, .ny = -1.0, .nz = 0.0 },
    .{ .x = 1.0, .y = 0.0, .z = 1.0, .u = 1, .v = 1, .nx = 0.0, .ny = -1.0, .nz = 0.0 },
    .{ .x = 1.0, .y = 0.0, .z = 0.0, .u = 0, .v = 1, .nx = 0.0, .ny = -1.0, .nz = 0.0 },

    .{ .x = 0.0, .y = 1.0, .z = 0.0, .u = 0, .v = 0, .nx = 0.0, .ny = 1.0, .nz = 0.0 },
    .{ .x = 0.0, .y = 1.0, .z = 1.0, .u = 1, .v = 0, .nx = 0.0, .ny = 1.0, .nz = 0.0 },
    .{ .x = 1.0, .y = 1.0, .z = 1.0, .u = 1, .v = 1, .nx = 0.0, .ny = 1.0, .nz = 0.0 },
    .{ .x = 1.0, .y = 1.0, .z = 0.0, .u = 0, .v = 1, .nx = 0.0, .ny = 1.0, .nz = 0.0 },
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
    state.meshes.append(createMesh(Vec3.new(16.0, 0.0, 0.0))) catch unreachable;

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

    //const lookY = mat4.createAngleAxis(Vec3.unitX, state.mouseY);
    state.cameraPos = state.cameraPos.add(state.cameraFront.scale(state.dz * dt));
    state.cameraPos = state.cameraPos.sub(
        state.cameraFront.cross(state.cameraUp).normalize().scale(state.dx * dt),
    );

    sdtx.print("First\n", .{});

    sdtx.print("Dt is: {d}\n", .{1.0 / sapp.frameDuration()});

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
    };

    var img_desc: sg.ImageDesc = .{
        .width = 32,
        .height = @intCast(state.atlas.len / 32),
    };
    img_desc.data.subimage[0][0] = sg.asRange(state.atlas);
    mesh.bind.images[shd.IMG_tex] = sg.makeImage(img_desc);

    mesh.bind.samplers[shd.SMP_smp] = sg.makeSampler(.{});

    const chunk = chunks.Chunk.gen_solid_chunk().gen_mesh(state.allocator) catch unreachable;

    const verticesToRender = chunk.vertices.items;

    std.log.info("Has num verts: {}", .{verticesToRender.len});

    // create vertex buffer with triangle vertices
    mesh.bind.vertex_buffers[0] = sg.makeBuffer(.{
        .data = sg.asRange(verticesToRender),
    });

    const indices = chunk.indices.items;

    mesh.numIndices = @intCast(indices.len);

    mesh.bind.index_buffer = sg.makeBuffer(.{
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
