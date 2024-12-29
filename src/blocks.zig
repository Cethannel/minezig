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

pub const Sides = union(enum) {
    All: []const u8,
    TopOthers: struct {
        top: []const u8,
        other: []const u8,
    },

    pub fn dupe(self: @This(), allocator: std.mem.Allocator) !Sides {
        return switch (self) {
            .All => |all| .{ .All = try allocator.dupe(u8, all) },
            .TopOthers => |topOthers| Sides{ .TopOthers = .{
                .top = try allocator.dupe(u8, topOthers.top),
                .other = try allocator.dupe(u8, topOthers.other),
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

    pub fn init_all(allocator: std.mem.Allocator, all: []const u8) !Self {
        const sidesName = try allocator.dupe(u8, all);

        return Self{
            .allocator = allocator,
            .sides = .{
                .All = sidesName,
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

                out[0] = try allocator.dupe(u8, sides);

                return out;
            },
            .TopOthers => |sides| {
                const out = try allocator.alloc([]const u8, 2);
                errdefer allocator.free(out);

                out[0] = try allocator.dupe(u8, sides.top);
                out[1] = try allocator.dupe(u8, sides.other);

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
            newVertex.x += pos.x;
            newVertex.y += pos.y;
            newVertex.z += pos.z;

            const texName = swi: switch (self.sides) {
                .All => |all| all,
                .TopOthers => |topOthers| {
                    var texName = topOthers.other;
                    if (side == 5) {
                        texName = topOthers.top;
                    }
                    break :swi texName;
                },
            };
            const textureIndex = state.textureMap.get(texName).?;

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
                self.allocator.free(sides);
            },
            .TopOthers => |sides| {
                self.allocator.free(sides.top);
                self.allocator.free(sides.other);
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

    const cube = try Cube.init_all(allocator, "thing");

    const out = try cube.get_textures_names(allocator);
    defer allocator.free(out);
    defer allocator.free(out[0]);

    try std.testing.expectEqual(1, out.len);
    try std.testing.expectEqualStrings("thing", out[0]);

    var block = try toBlock(cube, allocator, "thing");
    block.deinit();
}
