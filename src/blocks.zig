const std = @import("std");

const zlm = @import("zlm");

const root = @import("main.zig");

const utils = @import("utils.zig");

const state = &root.state;

pub const Block = extern struct {
    const getTextureNames = *const fn (
        self: *const anyopaque,
        allocator: *const std.mem.Allocator,
    ) callconv(.C) ?*[][]const u8;

    const deinitFn = *const fn (
        self: *anyopaque,
    ) callconv(.C) void;

    const genVerticesSidesFn = *const fn (
        self: *const anyopaque,
        side: usize,
        pos: zlm.Vec3,
        out: *[4]root.Vertex,
    ) callconv(.C) bool;

    inner: *anyopaque,
    allocator: *const std.mem.Allocator,

    blockName: *const []const u8,
    transparent: bool = false,

    free_inner: *const fn (allocator: *const std.mem.Allocator, inner: *anyopaque) callconv(.C) void,
    inner_get_textures_names: getTextureNames,
    inner_gen_vertices_sides: genVerticesSidesFn,
    inner_deinit: deinitFn,

    pub fn get_textures_names(self: *const @This(), allocator: std.mem.Allocator) ![][]const u8 {
        const innerOut = self.inner_get_textures_names(self.inner, &allocator);

        if (innerOut) |out| {
            defer allocator.destroy(out);
            return out.*;
        } else {
            return error.GetTextureNames;
        }
    }

    pub inline fn gen_vertices_sides(self: *const @This(), side: usize, pos: zlm.Vec3) ![4]root.Vertex {
        var out: [4]root.Vertex = undefined;

        if (self.inner_gen_vertices_sides(self.inner, side, pos, &out)) {
            return out;
        } else {
            return error.GenVerticesSides;
        }
    }

    pub fn deinit(self: *@This()) void {
        if (std.mem.eql(u8, "air", self.blockName.*)) {
            return;
        }

        self.inner_deinit(self.inner);
        self.allocator.free(self.blockName.*);
        self.allocator.destroy(self.blockName);
        self.free_inner(self.allocator, self.inner);
        const alocClone = self.allocator.*;
        alocClone.destroy(self.allocator);
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
        fn free(allocator: *const std.mem.Allocator, inner: *anyopaque) callconv(.C) void {
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
    const allocPtr = try allocator.create(std.mem.Allocator);
    errdefer allocator.destroy(allocPtr);
    allocPtr.* = allocator;
    out.allocator = allocPtr;
    out.transparent = false;

    const dupedName = try allocator.dupe(u8, name);
    errdefer allocator.free(dupedName);

    const namePtr = try allocator.create([]u8);
    errdefer allocator.destroy(namePtr);
    namePtr.* = dupedName;

    out.blockName = namePtr;

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

        const otherFieldType = @TypeOf(@field(iType, other_field_name));
        const otherFieldTypeInfo = @typeInfo(otherFieldType);

        const genFieldTypeInfo = @typeInfo(field.type);

        switch (genFieldTypeInfo) {
            .pointer => |ptrInfo| {
                const childTypeInfo: std.builtin.Type = @typeInfo(ptrInfo.child);
                switch (childTypeInfo) {
                    .@"fn" => |fnInfo| {
                        if (otherFieldTypeInfo != .@"fn") {
                            @compileError("FixME");
                        }
                        const fnInfoActual = @as(std.builtin.Type.Fn, fnInfo);
                        const iFnInfoActual = @as(std.builtin.Type.Fn, otherFieldTypeInfo.@"fn");

                        if (fnInfoActual.calling_convention != iFnInfoActual.calling_convention) {
                            @compileError(std.fmt.comptimePrint(
                                "For function: {s}.{s}\n" ++ //
                                    "Expected calling convention: `{s}` but" ++ //
                                    " found convention: `{s}`",
                                .{
                                    @typeName(iType),
                                    other_field_name,
                                    @tagName(fnInfoActual.calling_convention),
                                    @tagName(iFnInfoActual.calling_convention),
                                },
                            ));
                        }

                        if (fnInfoActual.return_type != iFnInfoActual.return_type) {
                            @compileError(std.fmt.comptimePrint(
                                \\ For function: {s}.{s}
                                \\ Expected return type to be:
                                \\ {s}
                                \\ but found type:
                                \\ {s}
                            ,
                                .{
                                    @typeName(iType),
                                    other_field_name,
                                    utils.optionalTypeName(fnInfoActual.return_type),
                                    utils.optionalTypeName(iFnInfoActual.return_type),
                                },
                            ));
                        }

                        inline for (iFnInfoActual.params, 0..) |iParam, i| {
                            const param = fnInfoActual.params[i];
                            const paramTInfo: std.builtin.Type = @typeInfo(param.type.?);
                            const iParamTInfo: std.builtin.Type = @typeInfo(iParam.type.?);

                            if (paramTInfo == .pointer and iParamTInfo == .pointer) {
                                if (iParamTInfo.pointer.child == iType and paramTInfo.pointer.child == anyopaque) {
                                    continue;
                                }
                            }

                            if (param.type != iParam.type) {
                                @compileError(std.fmt.comptimePrint(
                                    \\ For function: {s}.{s}
                                    \\ Expected param[{}] to be of type:
                                    \\ {s}
                                    \\ but found type:
                                    \\ {s}
                                ,
                                    .{
                                        @typeName(iType),
                                        other_field_name,
                                        i,
                                        @typeName(param.type.?),
                                        @typeName(iParam.type.?),
                                    },
                                ));
                            }
                        }
                    },
                    else => {},
                }
            },
            else => {},
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

    pub fn get_textures_names(
        self: *const Self,
        allocator: *const std.mem.Allocator,
    ) callconv(.C) ?*[][]const u8 {
        return generic_get_textures_names(self, allocator) catch return null;
    }

    pub fn gen_vertices_sides(
        self: *const Self,
        side: usize,
        pos: zlm.Vec3,
        out: *[4]root.Vertex,
    ) callconv(.C) bool {
        //var out: [4]root.Vertex = undefined;
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
            const textureIndex = state.textureMap.get(texInfo.file) orelse return false;

            newVertex.modifierColor = texInfo.colorOveride;

            newVertex.v /= @as(f32, @floatFromInt(numIndices));
            newVertex.v += @as(f32, @floatFromInt(textureIndex)) / //
                @as(f32, @floatFromInt(numIndices));

            out[i] = newVertex;
        }

        return true;
    }

    pub fn deinit(self: *Self) callconv(.C) void {
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

    pub fn to_block_transparent(self: *const Self, name: []const u8) !Block {
        var block = try toBlock(self.*, self.allocator, name);
        block.transparent = true;
        return block;
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

    pub fn get_textures_names(self: *const Self, allocator: *const std.mem.Allocator) callconv(.C) ?*[][]const u8 {
        return generic_get_textures_names(self, allocator) catch return null;
    }

    pub fn gen_vertices_sides(
        self: *const Self,
        side: usize,
        pos: zlm.Vec3,
        out: *[4]root.Vertex,
    ) callconv(.C) bool {
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
            const textureIndex = state.textureMap.get(texInfo.file) orelse return false;

            newVertex.modifierColor = texInfo.colorOveride;

            newVertex.v /= @as(f32, @floatFromInt(numIndices)) * 2;
            newVertex.v += @as(f32, @floatFromInt(textureIndex)) / //
                @as(f32, @floatFromInt(numIndices));

            out[i] = newVertex;
        }

        return true;
    }

    pub fn deinit(self: *Self) callconv(.C) void {
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

    pub fn to_block_transparent(self: *const Self, name: []const u8) !Block {
        var block = try toBlock(self.*, self.allocator, name);
        block.transparent = true;
        return block;
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
    .blockName = &"air",
    .free_inner = undefined,
    .inner_deinit = undefined,
    .inner_get_textures_names = @ptrCast(&Air.get_textures_names),
    .inner_gen_vertices_sides = @ptrCast(&Air.gen_vertices_sides),
};

pub const Fluid = struct {
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

    pub fn get_textures_names(self: *const Self, allocator: *const std.mem.Allocator) callconv(.C) ?*[][]const u8 {
        return generic_get_textures_names(self, allocator) catch return null;
    }

    pub fn gen_vertices_sides(
        self: *const Self,
        side: usize,
        pos: zlm.Vec3,
        out: *[4]root.Vertex,
    ) callconv(.C) bool {
        const numIndices = getNumberTextures();

        for (0..4) |i| {
            var newVertex = baseVertices[side * 4 + i];
            newVertex.pos.y *= (15.0 / 16.0);
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
            const textureIndex = state.textureMap.get(texInfo.file) orelse return false;

            newVertex.modifierColor = texInfo.colorOveride;

            newVertex.v /= @as(f32, @floatFromInt(numIndices)) * 2;
            newVertex.v += @as(f32, @floatFromInt(textureIndex)) / //
                @as(f32, @floatFromInt(numIndices));

            out[i] = newVertex;
        }

        return true;
    }

    pub fn deinit(self: *Self) callconv(.C) void {
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

    pub fn to_block_transparent(self: *const Self, name: []const u8) !Block {
        var block = try toBlock(self.*, self.allocator, name);
        block.transparent = true;
        return block;
    }
};

pub fn getBlockId(blockName: []const u8) ?u32 {
    for (state.blocksArr.items, 0..) |name, i| {
        if (std.mem.eql(u8, name.blockName.*, blockName)) {
            return @intCast(i);
        }
    }

    return null;
}

fn generic_get_textures_names(self: anytype, allocator: *const std.mem.Allocator) !*[][]const u8 {
    const outPtr = try allocator.create([][]const u8);
    errdefer allocator.destroy(outPtr);
    switch (self.sides) {
        .All => |sides| {
            const out = try allocator.alloc([]const u8, 1);
            errdefer allocator.free(out);

            out[0] = try allocator.dupe(u8, sides.file);

            outPtr.* = out;
        },
        .TopOthers => |sides| {
            const out = try allocator.alloc([]const u8, 2);
            errdefer allocator.free(out);

            out[0] = try allocator.dupe(u8, sides.top.file);
            out[1] = try allocator.dupe(u8, sides.other.file);

            outPtr.* = out;
        },
        .TopBotOthers => |sides| {
            const out = try allocator.alloc([]const u8, 3);
            errdefer allocator.free(out);

            out[0] = try allocator.dupe(u8, sides.top.file);
            out[1] = try allocator.dupe(u8, sides.other.file);
            out[2] = try allocator.dupe(u8, sides.bot.file);

            outPtr.* = out;
        },
    }

    return outPtr;
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
