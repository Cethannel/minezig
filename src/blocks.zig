const std = @import("std");

const zlm = @import("zlm");

const root = @import("main.zig");

const state = &root.state;

pub const Block = struct {
    const getTextureNames = *const fn (
        self: *const anyopaque,
        allocator: std.mem.Allocator,
    ) anyerror![][]const u8;

    const deinitFn = *const fn (
        self: *anyopaque,
    ) void;

    const genVerticesSidesFn = *const fn (
        self: *const anyopaque,
        side: usize,
        pos: zlm.Vec3,
    ) anyerror![4]root.Vertex;

    inner: *anyopaque,
    allocator: std.mem.Allocator,

    blockName: []const u8,

    free_inner: *const fn (allocator: std.mem.Allocator, inner: *anyopaque) void,
    inner_get_textures_names: getTextureNames,
    inner_gen_vertices_sides: genVerticesSidesFn,
    inner_deinit: deinitFn,

    pub fn get_textures_names(self: *const @This(), allocator: std.mem.Allocator) ![][]const u8 {
        return self.inner_get_textures_names(self.inner, allocator);
    }

    pub fn gen_vertices_sides(self: *const @This(), side: usize, pos: zlm.Vec3) ![4]root.Vertex {
        return self.inner_gen_vertices_sides(self.inner, side, pos);
    }

    pub fn deinit(self: *@This()) void {
        if (std.mem.eql(u8, "air", self.blockName)) {
            return;
        }

        self.inner_deinit(self.inner);
        self.allocator.free(self.blockName);
        self.free_inner(self.allocator, self.inner);
    }
};

pub const SideInfo = struct {
    file: []const u8,
    colorOveride: zlm.Vec3 = zlm.Vec3.all(1.0),

    pub fn dupe(self: @This(), allocator: std.mem.Allocator) !@This() {
        return .{
            .file = try allocator.dupe(u8, self.file),
            .colorOveride = self.colorOveride,
        };
    }

    pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
        allocator.free(self.file);
    }
};

pub const Sides = union(enum) {
    All: SideInfo,
    TopOthers: struct {
        top: SideInfo,
        other: SideInfo,
    },
    TopBotOthers: struct {
        top: SideInfo,
        other: SideInfo,
        bot: SideInfo,
    },

    pub fn dupe(self: @This(), allocator: std.mem.Allocator) !Sides {
        return switch (self) {
            .All => |all| .{
                .All = try all.dupe(allocator),
            },
            .TopOthers => |topOthers| Sides{ .TopOthers = .{
                .top = try topOthers.top.dupe(allocator),
                .other = try topOthers.other.dupe(allocator),
            } },
            .TopBotOthers => |topOthers| Sides{ .TopBotOthers = .{
                .top = try topOthers.top.dupe(allocator),
                .other = try topOthers.other.dupe(allocator),
                .bot = try topOthers.bot.dupe(allocator),
            } },
        };
    }
};

fn freeGenerice(t: type) type {
    return struct {
        fn free(allocator: std.mem.Allocator, inner: *anyopaque) void {
            const thing: *t = @alignCast(@ptrCast(inner));
            allocator.destroy(thing);
        }
    };
}

pub fn toBlock(inner: anytype, allocator: std.mem.Allocator, name: []const u8) !Block {
    const bsInfo = @typeInfo(Block).@"struct";
    const iType = @TypeOf(inner);

    var out: Block = undefined;

    const innerPtr: *iType = try allocator.create(iType);
    errdefer allocator.destroy(innerPtr);
    innerPtr.* = inner;

    out.inner = innerPtr;
    out.allocator = allocator;

    out.blockName = try allocator.dupe(u8, name);

    out.free_inner = &freeGenerice(iType).free;

    inline for (bsInfo.fields) |field| {
        if (!comptime std.mem.startsWith(u8, field.name, "inner_")) {
            continue;
        }

        const other_field_name = field.name["inner_".len..];

        if (!@hasDecl(iType, other_field_name)) {
            @compileError(std.fmt.comptimePrint("{} does not have required function `{s}`: {}", .{
                iType,
                other_field_name,
                field.type,
            }));
        }

        @field(out, field.name) = @ptrCast(&@field(iType, other_field_name));
    }

    return out;
}

pub const Cube = struct {
    allocator: std.mem.Allocator,
    sides: Sides,

    const Self = @This();

    pub fn init_all(allocator: std.mem.Allocator, all: SideInfo) !Self {
        const sides = try all.dupe(allocator);

        return Self{
            .allocator = allocator,
            .sides = .{
                .All = sides,
            },
        };
    }

    pub fn init_sides(allocator: std.mem.Allocator, sides: Sides) !Self {
        const newSides = try sides.dupe(allocator);

        return Self{
            .allocator = allocator,
            .sides = newSides,
        };
    }

    pub fn get_textures_names(self: *const Self, allocator: std.mem.Allocator) anyerror![][]const u8 {
        switch (self.sides) {
            .All => |sides| {
                const out = try allocator.alloc([]const u8, 1);
                errdefer allocator.free(out);

                out[0] = try allocator.dupe(u8, sides.file);

                return out;
            },
            .TopOthers => |sides| {
                const out = try allocator.alloc([]const u8, 2);
                errdefer allocator.free(out);

                out[0] = try allocator.dupe(u8, sides.top.file);
                out[1] = try allocator.dupe(u8, sides.other.file);

                return out;
            },
            .TopBotOthers => |sides| {
                const out = try allocator.alloc([]const u8, 3);
                errdefer allocator.free(out);

                out[0] = try allocator.dupe(u8, sides.top.file);
                out[1] = try allocator.dupe(u8, sides.other.file);
                out[2] = try allocator.dupe(u8, sides.bot.file);

                return out;
            },
        }
    }

    pub fn gen_vertices_sides(
        self: *const Self,
        side: usize,
        pos: zlm.Vec3,
    ) ![4]root.Vertex {
        var out: [4]root.Vertex = undefined;
        const numIndices = getNumberTextures();

        for (0..4) |i| {
            var newVertex = baseVertices[side * 4 + i];
            newVertex.pos.x += pos.x;
            newVertex.pos.y += pos.y;
            newVertex.pos.z += pos.z;

            const texInfo = swi: switch (self.sides) {
                .All => |all| all,
                .TopOthers => |topOthers| {
                    var texName = topOthers.other;
                    if (side == 5) {
                        texName = topOthers.top;
                    }
                    break :swi texName;
                },
                .TopBotOthers => |topOthers| {
                    var texName = topOthers.other;
                    if (side == 5) {
                        texName = topOthers.top;
                    }
                    if (side == 4) {
                        texName = topOthers.bot;
                    }
                    break :swi texName;
                },
            };
            const textureIndex = state.textureMap.get(texInfo.file).?;

            newVertex.modifierColor = texInfo.colorOveride;

            newVertex.v /= @as(f32, @floatFromInt(numIndices));
            newVertex.v += @as(f32, @floatFromInt(textureIndex)) / //
                @as(f32, @floatFromInt(numIndices));

            out[i] = newVertex;
        }

        return out;
    }

    pub fn deinit(self: *Self) void {
        switch (self.sides) {
            .All => |sides| {
                sides.deinit(self.allocator);
            },
            .TopOthers => |sides| {
                sides.top.deinit(self.allocator);
                sides.other.deinit(self.allocator);
            },
            .TopBotOthers => |sides| {
                sides.top.deinit(self.allocator);
                sides.other.deinit(self.allocator);
                sides.bot.deinit(self.allocator);
            },
        }
    }

    pub fn to_block(self: *const Self, name: []const u8) !Block {
        return toBlock(self.*, self.allocator, name);
    }
};

pub const Slab = struct {
    allocator: std.mem.Allocator,
    sides: Sides,

    const Self = @This();

    pub fn init_all(allocator: std.mem.Allocator, all: SideInfo) !Self {
        const sides = try all.dupe(allocator);

        return Self{
            .allocator = allocator,
            .sides = .{
                .All = sides,
            },
        };
    }

    pub fn init_sides(allocator: std.mem.Allocator, sides: Sides) !Self {
        const newSides = try sides.dupe(allocator);

        return Self{
            .allocator = allocator,
            .sides = newSides,
        };
    }

    pub fn get_textures_names(self: *const Self, allocator: std.mem.Allocator) anyerror![][]const u8 {
        switch (self.sides) {
            .All => |sides| {
                const out = try allocator.alloc([]const u8, 1);
                errdefer allocator.free(out);

                out[0] = try allocator.dupe(u8, sides.file);

                return out;
            },
            .TopOthers => |sides| {
                const out = try allocator.alloc([]const u8, 2);
                errdefer allocator.free(out);

                out[0] = try allocator.dupe(u8, sides.top.file);
                out[1] = try allocator.dupe(u8, sides.other.file);

                return out;
            },
            .TopBotOthers => |sides| {
                const out = try allocator.alloc([]const u8, 3);
                errdefer allocator.free(out);

                out[0] = try allocator.dupe(u8, sides.top.file);
                out[1] = try allocator.dupe(u8, sides.other.file);
                out[2] = try allocator.dupe(u8, sides.bot.file);

                return out;
            },
        }
    }

    pub fn gen_vertices_sides(
        self: *const Self,
        side: usize,
        pos: zlm.Vec3,
    ) ![4]root.Vertex {
        var out: [4]root.Vertex = undefined;
        const numIndices = getNumberTextures();

        for (0..4) |i| {
            var newVertex = baseVertices[side * 4 + i];
            newVertex.pos.y /= 2;
            newVertex.pos.x += pos.x;
            newVertex.pos.y += pos.y;
            newVertex.pos.z += pos.z;

            const texInfo = swi: switch (self.sides) {
                .All => |all| all,
                .TopOthers => |topOthers| {
                    var texName = topOthers.other;
                    if (side == 5) {
                        texName = topOthers.top;
                    }
                    break :swi texName;
                },
                .TopBotOthers => |topOthers| {
                    var texName = topOthers.other;
                    if (side == 5) {
                        texName = topOthers.top;
                    }
                    if (side == 4) {
                        texName = topOthers.bot;
                    }
                    break :swi texName;
                },
            };
            const textureIndex = state.textureMap.get(texInfo.file).?;

            newVertex.modifierColor = texInfo.colorOveride;

            newVertex.v /= @as(f32, @floatFromInt(numIndices)) * 2;
            newVertex.v += @as(f32, @floatFromInt(textureIndex)) / //
                @as(f32, @floatFromInt(numIndices));

            out[i] = newVertex;
        }

        return out;
    }

    pub fn deinit(self: *Self) void {
        switch (self.sides) {
            .All => |sides| {
                sides.deinit(self.allocator);
            },
            .TopOthers => |sides| {
                sides.top.deinit(self.allocator);
                sides.other.deinit(self.allocator);
            },
            .TopBotOthers => |sides| {
                sides.top.deinit(self.allocator);
                sides.other.deinit(self.allocator);
                sides.bot.deinit(self.allocator);
            },
        }
    }

    pub fn to_block(self: *const Self, name: []const u8) !Block {
        return toBlock(self.*, self.allocator, name);
    }
};

fn getNumberTextures() usize {
    return state.atlas.len / 32 / 32;
}

// How I should be able to define a block
//       Cube!(
//           "bricks",
//           all "bricks.png"
//       ),

const baseVertices = [_]root.Vertex{
    .{
        .pos = .{ .x = 0.0, .y = 0.0, .z = 0.0 },
        .u = 0,
        .v = 1,
        .normal = .{ .x = 0.0, .y = 0.0, .z = -1.0 },
        .modifierColor = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
    },
    .{
        .pos = .{ .x = 1.0, .y = 0.0, .z = 0.0 },
        .u = 1,
        .v = 1,
        .normal = .{ .x = 0.0, .y = 0.0, .z = -1.0 },
        .modifierColor = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
    },
    .{
        .pos = .{ .x = 1.0, .y = 1.0, .z = 0.0 },
        .u = 1,
        .v = 0,
        .normal = .{ .x = 0.0, .y = 0.0, .z = -1.0 },
        .modifierColor = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
    },
    .{
        .pos = .{ .x = 0.0, .y = 1.0, .z = 0.0 },
        .u = 0,
        .v = 0,
        .normal = .{ .x = 0.0, .y = 0.0, .z = -1.0 },
        .modifierColor = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
    },

    .{
        .pos = .{ .x = 0.0, .y = 0.0, .z = 1.0 },
        .u = 0,
        .v = 1,
        .normal = .{ .x = 0.0, .y = 0.0, .z = 1.0 },
        .modifierColor = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
    },
    .{
        .pos = .{ .x = 1.0, .y = 0.0, .z = 1.0 },
        .u = 1,
        .v = 1,
        .normal = .{ .x = 0.0, .y = 0.0, .z = 1.0 },
        .modifierColor = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
    },
    .{
        .pos = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
        .u = 1,
        .v = 0,
        .normal = .{ .x = 0.0, .y = 0.0, .z = 1.0 },
        .modifierColor = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
    },
    .{
        .pos = .{ .x = 0.0, .y = 1.0, .z = 1.0 },
        .u = 0,
        .v = 0,
        .normal = .{ .x = 0.0, .y = 0.0, .z = 1.0 },
        .modifierColor = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
    },

    .{
        .pos = .{ .x = 0.0, .y = 0.0, .z = 0.0 },
        .u = 1,
        .v = 1,
        .normal = .{ .x = -1.0, .y = 0.0, .z = 0.0 },
        .modifierColor = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
    },
    .{
        .pos = .{ .x = 0.0, .y = 1.0, .z = 0.0 },
        .u = 1,
        .v = 0,
        .normal = .{ .x = -1.0, .y = 0.0, .z = 0.0 },
        .modifierColor = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
    },
    .{
        .pos = .{ .x = 0.0, .y = 1.0, .z = 1.0 },
        .u = 0,
        .v = 0,
        .normal = .{ .x = -1.0, .y = 0.0, .z = 0.0 },
        .modifierColor = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
    },
    .{
        .pos = .{ .x = 0.0, .y = 0.0, .z = 1.0 },
        .u = 0,
        .v = 1,
        .normal = .{ .x = -1.0, .y = 0.0, .z = 0.0 },
        .modifierColor = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
    },

    .{
        .pos = .{ .x = 1.0, .y = 0.0, .z = 0.0 },
        .u = 0,
        .v = 1,
        .normal = .{ .x = 1.0, .y = 0.0, .z = 0.0 },
        .modifierColor = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
    },
    .{
        .pos = .{ .x = 1.0, .y = 1.0, .z = 0.0 },
        .u = 0,
        .v = 0,
        .normal = .{ .x = 1.0, .y = 0.0, .z = 0.0 },
        .modifierColor = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
    },
    .{
        .pos = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
        .u = 1,
        .v = 0,
        .normal = .{ .x = 1.0, .y = 0.0, .z = 0.0 },
        .modifierColor = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
    },
    .{
        .pos = .{ .x = 1.0, .y = 0.0, .z = 1.0 },
        .u = 1,
        .v = 1,
        .normal = .{ .x = 1.0, .y = 0.0, .z = 0.0 },
        .modifierColor = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
    },

    .{
        .pos = .{ .x = 0.0, .y = 0.0, .z = 0.0 },
        .u = 0,
        .v = 0,
        .normal = .{ .x = 0.0, .y = -1.0, .z = 0.0 },
        .modifierColor = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
    },
    .{
        .pos = .{ .x = 0.0, .y = 0.0, .z = 1.0 },
        .u = 1,
        .v = 0,
        .normal = .{ .x = 0.0, .y = -1.0, .z = 0.0 },
        .modifierColor = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
    },
    .{
        .pos = .{ .x = 1.0, .y = 0.0, .z = 1.0 },
        .u = 1,
        .v = 1,
        .normal = .{ .x = 0.0, .y = -1.0, .z = 0.0 },
        .modifierColor = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
    },
    .{
        .pos = .{ .x = 1.0, .y = 0.0, .z = 0.0 },
        .u = 0,
        .v = 1,
        .normal = .{ .x = 0.0, .y = -1.0, .z = 0.0 },
        .modifierColor = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
    },

    .{
        .pos = .{ .x = 0.0, .y = 1.0, .z = 0.0 },
        .u = 0,
        .v = 0,
        .normal = .{ .x = 0.0, .y = 1.0, .z = 0.0 },
        .modifierColor = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
    },
    .{
        .pos = .{ .x = 0.0, .y = 1.0, .z = 1.0 },
        .u = 1,
        .v = 0,
        .normal = .{ .x = 0.0, .y = 1.0, .z = 0.0 },
        .modifierColor = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
    },
    .{
        .pos = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
        .u = 1,
        .v = 1,
        .normal = .{ .x = 0.0, .y = 1.0, .z = 0.0 },
        .modifierColor = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
    },
    .{
        .pos = .{ .x = 1.0, .y = 1.0, .z = 0.0 },
        .u = 0,
        .v = 1,
        .normal = .{ .x = 0.0, .y = 1.0, .z = 0.0 },
        .modifierColor = .{ .x = 1.0, .y = 1.0, .z = 1.0 },
    },
};

pub const Air = struct {
    const Self = @This();

    pub fn get_textures_names(self: *const Self, allocator: std.mem.Allocator) anyerror![][]const u8 {
        _ = self;
        _ = allocator;
        @panic("Called get texture names for air");
    }

    pub fn gen_vertices_sides(
        self: *const Self,
        side: usize,
        pos: zlm.Vec3,
    ) ![4]root.Vertex {
        _ = self;
        _ = side;
        _ = pos;
        @panic("Called get texture names for air");
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    pub fn to_block(self: *const Self, name: []const u8) !Block {
        return toBlock(self.*, self.allocator, name);
    }
};

const air = Air{};
pub const AirBlock = Block{
    .inner = @constCast(@ptrCast(&air)),
    .allocator = undefined,
    .blockName = "air",
    .free_inner = undefined,
    .inner_deinit = undefined,
    .inner_get_textures_names = @ptrCast(&Air.get_textures_names),
    .inner_gen_vertices_sides = @ptrCast(&Air.gen_vertices_sides),
};

pub fn getBlockId(blockName: []const u8) ?u32 {
    for (state.blocksArr.items, 0..) |name, i| {
        if (std.mem.eql(u8, name.blockName, blockName)) {
            return @intCast(i);
        }
    }

    return null;
}

test "Cube Block" {
    const allocator = std.testing.allocator;

    const cube = try Cube.init_all(allocator, .{ .file = "thing" });

    const out = try cube.get_textures_names(allocator);
    defer allocator.free(out);
    defer allocator.free(out[0]);

    try std.testing.expectEqual(1, out.len);
    try std.testing.expectEqualStrings("thing", out[0]);

    var block = try toBlock(cube, allocator, "thing");
    block.deinit();
}
