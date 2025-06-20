const std = @import("std");

const clay = @import("zclay");

const sokol = @import("sokol");
const sdtx = sokol.debugtext;
const shape = sokol.shape;
const sg = sokol.gfx;

const zlm = @import("zlm");

pub fn measureText(
    text: []const u8,
    config: *clay.TextElementConfig,
    context: void,
) clay.Dimensions {
    _ = context;
    return clay.Dimensions{
        .w = @floatFromInt(text.len * config.font_size),
        .h = @floatFromInt(config.font_size),
    };
}

pub fn clayRaylibRender(
    render_commands: *clay.ClayArray(clay.RenderCommand),
    allocator: std.mem.Allocator,
) void {
    var i: usize = 0;
    _ = allocator;
    while (i < render_commands.length) : (i += 1) {
        const render_command = clay.renderCommandArrayGet(render_commands, @intCast(i));
        const bounding_box = render_command.bounding_box;
        switch (render_command.command_type) {
            .none => {},
            .text => {
                const textData = render_command.render_data.text;
                sdtx.pos(bounding_box.x, bounding_box.y);
                sdtx.color4f(
                    textData.text_color[0],
                    textData.text_color[1],
                    textData.text_color[2],
                    textData.text_color[3],
                );
                sdtx.font(textData.font_id);
                const text: [*:0]const u8 = @ptrCast(textData.string_contents.base_chars);
                sdtx.print("{s}", .{text});
            },
            .rectangle => {
                const rectData = render_command.render_data.rectangle;
                const transform = zlm.Mat4.createTranslation(.{
                    .x = bounding_box.x,
                    .y = bounding_box.y,
                    .z = 0,
                });
                const color = floatColorTou32(rectData.background_color);
                const box: shape.Box = .{
                    .width = bounding_box.width,
                    .height = bounding_box.height,
                    .transform = .{
                        .m = transform.fields,
                    },
                    .color = color,
                };

                const vertices: [128]shape.Vertex = @splat(.{});
                const indices: [16]u16 = undefined;
                var buf = shape.Buffer{
                    .valid = true,
                    .vertices = .{ .buffer = shape.asRange(&vertices) },
                    .indices = .{ .buffer = shape.asRange(&indices) },
                };

                buf = shape.buildBox(buf, box);
                const boxS = shape.elementRange(buf);

                sg.draw(boxS.base_element, boxS.num_elements, 1);
            },
            else => {
                std.log.err("Unsupported ui element: {s}", .{@tagName(render_command.command_type)});
            },
        }
    }
}

fn floatColorTou32(input: [4]f32) u32 {
    var out: [4]u8 = undefined;

    for (0..4) |i| {
        out[i] = @intFromFloat(255 / input[i]);
    }

    return @bitCast(out);
}
