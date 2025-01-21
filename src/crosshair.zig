const shd = @import("shaders/crosshair.glsl.zig");

const sokol = @import("sokol");
const sg = sokol.gfx;
const sglue = sokol.glue;

const zlm = @import("zlm");

const Vertex = extern struct {
    pos: zlm.Vec3,
    u: f32,
    v: f32,
};

pub const Crosshair = struct {
    vertices: [4]Vertex = .{
        .{
            .pos = zlm.Vec3.new(-0.01, -0.01, 0.01),
            .u = 0.0,
            .v = 0.0,
        },
        .{
            .pos = zlm.Vec3.new(0.01, -0.01, 0.01),
            .u = 1.0,
            .v = 0.0,
        },
        .{
            .pos = zlm.Vec3.new(0.01, 0.01, 0.01),
            .u = 1.0,
            .v = 1.0,
        },
        .{
            .pos = zlm.Vec3.new(-0.01, 0.01, 0.01),
            .u = 0.0,
            .v = 1.0,
        },
    },
    indices: [6]u32 = .{
        0, 1, 3, //
        1, 2, 3, //
    },
    bind: sg.Bindings = .{},
    pass_action: sg.PassAction = .{},
    pip: sg.Pipeline = .{},

    vertexBuffer: sg.Buffer = .{},
    indexBuffer: sg.Buffer = .{},

    const Self = @This();

    pub fn init() Self {
        var out: Self = .{};

        out.vertexBuffer = sg.makeBuffer(.{
            .data = sg.asRange(out.vertices[0..]),
        });

        out.indexBuffer = sg.makeBuffer(.{
            .type = .INDEXBUFFER,
            .data = sg.asRange(out.indices[0..]),
        });

        var pip_desc: sg.PipelineDesc = .{
            .index_type = .UINT32,
            .shader = sg.makeShader(shd.crosshairShaderDesc(sg.queryBackend())),
            .depth = .{
                .compare = .LESS_EQUAL,
                .write_enabled = true,
            },
            .cull_mode = .NONE,
            .alpha_to_coverage_enabled = true,
            .primitive_type = .TRIANGLES,
        };

        pip_desc.layout.attrs[shd.ATTR_crosshair_pos].format = .FLOAT3;
        pip_desc.layout.attrs[shd.ATTR_crosshair_texcoord0].format = .FLOAT2;
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

        sg.applyPipeline(self.pip);
        sg.applyBindings(self.bind);

        sg.draw(0, @intCast(self.indices.len), 1);
        sg.endPass();
    }

    pub fn deinit(self: *Self) void {
        sg.destroyPipeline(self.pip);

        sg.destroyBuffer(self.vertexBuffer);
        sg.destroyBuffer(self.indexBuffer);
    }
};
