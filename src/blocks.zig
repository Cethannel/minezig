const std = @import("std");

const zlm = @import("zlm");

const root = @import("main.zig");

const utils = @import("utils.zig");

const chunks = @import("chunks.zig");

const state = &root.state;

pub const BlockUpdateParams = extern struct {
    pub const setBlockCallbackT = *const fn (
        pos: *const utils.IVec3,
        block: *const chunks.Block,
    ) callconv(.C) void;
    pub const blockUpdateCallbackT = *const fn (
        pos: *const utils.IVec3,
    ) callconv(.C) void;

    chunksMap: *const chunks.ChunkMap,
    pos: *const utils.IVec3,
    setblockCallback: setBlockCallbackT,
    blockUpdateCallback: blockUpdateCallbackT,
};

pub const GenVerticesSidesParams = extern struct {
    side: SideEnum,
    pos: zlm.Vec3,
    selfBlock: chunks.Block,
    neighbors: chunks.NeighborBlock,
};

pub const ShouldGenerateSidePrams = extern struct {
    selfBlock: chunks.Block,
    neighbor: chunks.Block,
    side: SideEnum,
};

pub const SideEnum = enum(usize) {
    NegZ = 0,
    Z = 1,
    NegX = 2,
    X = 3,
    NegY = 4,
    Y = 5,
};

pub const Bounds = extern struct {
    min: zlm.Vec3,
    max: zlm.Vec3,

    const Self = @This();

    pub fn eql(self: Self, other: Self) bool {
        inline for (@typeInfo(Self).@"struct".fields) |field| {
            if (!@field(self, field.name).eql(@field(other, field.name))) {
                return false;
            }
        }
        return true;
    }

    pub fn eqlDir(self: Self, other: Self, comptime field: utils.getFieldEnum(zlm.Vec3)) bool {
        return @field(self.min, @tagName(field)) == @field(other.min, @tagName(field)) and
            @field(self.max, @tagName(field)) == @field(other.max, @tagName(field));
    }
};

pub const BoundsParams = extern struct {
    selfBlock: chunks.Block,
};

pub const Block = extern struct {
    pub const getTextureNames = *const fn (
        self: *const anyopaque,
        allocator: *const std.mem.Allocator,
    ) callconv(.C) ?*[][]const u8;

    pub const deinitFn = *const fn (
        self: *anyopaque,
    ) callconv(.C) void;

    pub const genVerticesSidesFn = *const fn (
        self: *const anyopaque,
        params: *const GenVerticesSidesParams,
        out: *utils.MultiArray(root.Vertex, 4),
    ) callconv(.C) bool;

    pub const blockUpdate = *const fn (
        self: *const anyopaque,
        params: *const BlockUpdateParams,
    ) callconv(.C) void;

    pub const shouldGenerateSide = *const fn (
        self: *const anyopaque,
        params: *const ShouldGenerateSidePrams,
    ) callconv(.C) bool;

    pub const boundsFn = *const fn (
        self: *const anyopaque,
        params: *const BoundsParams,
        bound_out: *Bounds,
    ) callconv(.C) void;

    inner: *anyopaque,
    allocator: *const std.mem.Allocator,

    blockName: *const []const u8,
    transparent: bool = false,

    free_inner: *const fn (allocator: *const std.mem.Allocator, inner: *anyopaque) callconv(.C) void,
    inner_get_textures_names: getTextureNames,
    inner_gen_vertices_sides: genVerticesSidesFn,
    inner_block_update: ?blockUpdate,
    inner_should_generate_side: ?shouldGenerateSide,
    inner_bounds: boundsFn,
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

    pub inline fn gen_vertices_sides(
        self: *const @This(),
        params: GenVerticesSidesParams,
    ) !utils.MultiArray(root.Vertex, 4) {
        var out = utils.MultiArray(root.Vertex, 4).empty;

        if (self.inner_gen_vertices_sides(self.inner, &params, &out)) {
            return out;
        } else {
            return error.GenVerticesSides;
        }
    }

    pub inline fn block_update(self: *const @This(), params: *const BlockUpdateParams) void {
        if (self.inner_block_update) |ibu| {
            ibu(self.inner, params);
        }
    }

    pub inline fn bounds(self: *const @This(), params: BoundsParams) Bounds {
        var out: Bounds = undefined;
        self.inner_bounds(self.inner, &params, &out);
        return out;
    }

    pub inline fn should_generate_side(
        self: *const @This(),
        params: ShouldGenerateSidePrams,
    ) ?bool {
        const fun = self.inner_should_generate_side orelse return null;
        return fun(self.inner, &params);
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

        const genFieldTypeInfo = @typeInfo(field.type);

        switch (comptime checkType(genFieldTypeInfo, other_field_name, iType, field)) {
            .Default => {
                if (!@hasDecl(iType, other_field_name)) {
                    @compileError(std.fmt.comptimePrint("{} does not have required function `{s}`: {}", .{
                        iType,
                        other_field_name,
                        field.type,
                    }));
                }

                @field(out, field.name) = @ptrCast(&@field(iType, other_field_name));
            },
            .Continue => continue,
            .Null => {
                @field(out, field.name) = null;
            },
        }
    }

    return out;
}

const typeRes = enum {
    Default,
    Continue,
    Null,
};

fn checkType(
    genFieldTypeInfo: std.builtin.Type,
    other_field_name: [:0]const u8,
    iType: type,
    field: std.builtin.Type.StructField,
) typeRes {
    switch (genFieldTypeInfo) {
        .optional => |opt| {
            if (@hasDecl(iType, other_field_name)) {
                return checkType(@typeInfo(opt.child), other_field_name, iType, field);
            } else {
                return .Null;
            }
        },
        .pointer => |ptrInfo| {
            const childTypeInfo: std.builtin.Type = @typeInfo(ptrInfo.child);
            switch (childTypeInfo) {
                .@"fn" => |fnInfo| {
                    const otherFieldType = @TypeOf(@field(iType, other_field_name));
                    const otherFieldTypeInfo = @typeInfo(otherFieldType);
                    if (otherFieldTypeInfo != .@"fn") {
                        @compileError("FixME");
                    }
                    const fnInfoActual = @as(std.builtin.Type.Fn, fnInfo);
                    const iFnInfoActual = @as(std.builtin.Type.Fn, otherFieldTypeInfo.@"fn");

                    if (!fnInfoActual.calling_convention.eql(iFnInfoActual.calling_convention)) {
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

    return .Default;
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

    pub fn bounds(
        self: *const Self,
        params: *const BoundsParams,
        bound_out: *Bounds,
    ) callconv(.C) void {
        _ = self;
        _ = params;
        bound_out.min = zlm.Vec3.zero;
        bound_out.max = zlm.Vec3.one;
    }

    pub fn gen_vertices_sides(
        self: *const Self,
        params: *const GenVerticesSidesParams,
        out: *utils.MultiArray(root.Vertex, 4),
    ) callconv(.C) bool {
        const texInfo = swi: switch (self.sides) {
            .All => |all| all,
            .TopOthers => |topOthers| {
                var texName = topOthers.other;
                if (params.side == .Y) {
                    texName = topOthers.top;
                }
                break :swi texName;
            },
            .TopBotOthers => |topOthers| {
                var texName = topOthers.other;
                if (params.side == .Y) {
                    texName = topOthers.top;
                }
                if (params.side == .NegY) {
                    texName = topOthers.bot;
                }
                break :swi texName;
            },
        };

        return gen_vertices_sides_from_base(
            params.side,
            params.pos,
            out,
            baseVertices[0..],
            &texInfo,
            zlm.Vec3.one,
        );
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

fn gen_vertices_sides_from_base(
    side: SideEnum,
    pos: zlm.Vec3,
    out: *utils.MultiArray(root.Vertex, 4),
    baseVerts: []const root.Vertex,
    texInfo: *const SideInfo,
    sideScale: zlm.Vec3,
) bool {
    const numIndices = getNumberTextures();
    const texture = state.textureMap.get(texInfo.file) orelse return false;

    const invNumIndices = 1.0 / @as(f32, @floatFromInt(numIndices));
    const textureOffset = @as(f32, @floatFromInt(texture)) * invNumIndices;

    for (0..4) |i| {
        const base = &baseVerts[@intFromEnum(side) * 4 + i];
        var newVertex: root.Vertex = undefined;
        newVertex.pos = base.pos.mul(sideScale).add(pos);

        newVertex.modifierColor = texInfo.colorOveride;

        newVertex.u = base.u;
        newVertex.v = base.v * invNumIndices;
        if (side != .Y and side != .NegY) {
            newVertex.v *= sideScale.y;
        }
        newVertex.v += textureOffset;

        newVertex.modifierColor = texInfo.colorOveride;

        out.set(i, newVertex);
    }

    return true;
}

pub const Slab = struct {
    allocator: std.mem.Allocator,
    sides: Sides,

    const innerBounds: Bounds = .{
        .min = zlm.Vec3.zero,
        .max = zlm.Vec3.new(1.0, 0.5, 1.0),
    };

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

    pub fn bounds(
        self: *const Self,
        params: *const BoundsParams,
        bound_out: *Bounds,
    ) callconv(.C) void {
        _ = self;
        bound_out.* = innerBounds;
        if (params.selfBlock.variant == 1) {
            bound_out.min.y += 0.5;
            bound_out.max.y += 0.5;
        }
    }

    pub fn gen_vertices_sides(
        self: *const Self,
        params: *const GenVerticesSidesParams,
        out: *utils.MultiArray(root.Vertex, 4),
    ) callconv(.C) bool {
        const texInfo = swi: switch (self.sides) {
            .All => |all| all,
            .TopOthers => |topOthers| {
                var texName = topOthers.other;
                if (params.side == .Y) {
                    texName = topOthers.top;
                }
                break :swi texName;
            },
            .TopBotOthers => |topOthers| {
                var texName = topOthers.other;
                if (params.side == .Y) {
                    texName = topOthers.top;
                }
                if (params.side == .NegY) {
                    texName = topOthers.bot;
                }
                break :swi texName;
            },
        };

        return gen_vertices_sides_from_base(
            params.side,
            params.pos,
            out,
            baseVertices[0..],
            &texInfo,
            zlm.Vec3.new(1.0, 0.5, 1.0),
        );
    }

    pub fn should_generate_side(
        self: *const Self,
        params: *const ShouldGenerateSidePrams,
    ) callconv(.C) bool {
        const neighborBlock = getBlockFromId(params.neighbor.id);
        switch (params.side) {
            .Y => {
                return true;
            },
            .NegY => {
                if (neighborBlock) |nb| {
                    return nb.bounds(.{ .selfBlock = params.neighbor }).max.y != 1.0;
                }
            },
            else => {
                if (params.neighbor.id == .Air) {
                    return true;
                }
                if (neighborBlock) |nb| {
                    var selfBound: Bounds = undefined;
                    self.bounds(&.{
                        .selfBlock = params.selfBlock,
                    }, &selfBound);
                    if (nb.bounds(.{ .selfBlock = params.neighbor }).eql(selfBound)) {
                        return false;
                    }
                }
            },
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
        params: *const GenVerticesSidesParams,
        out: *utils.MultiArray(root.Vertex, 4),
    ) ![4]root.Vertex {
        _ = self;
        _ = params;
        _ = out;
        @panic("Called get texture names for air");
    }

    pub fn bounds(
        self: *const Self,
        params: *const BoundsParams,
        out_bounds: *Bounds,
    ) callconv(.C) void {
        _ = params;
        _ = self;
        _ = out_bounds;
        @panic("Called bounds for air");
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
    .inner_bounds = undefined,
    .inner_block_update = null,
    .inner_should_generate_side = null,
};

pub const Fluid = struct {
    allocator: std.mem.Allocator,
    sides: Sides,

    const Self = @This();

    const innerBounds: Bounds = .{
        .min = zlm.Vec3.zero,
        .max = zlm.Vec3.new(1.0, 15.0 / 16.0, 1.0),
    };

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

    pub fn bounds(
        self: *const Self,
        params: *const BoundsParams,
        bound_out: *Bounds,
    ) callconv(.C) void {
        _ = params;
        _ = self;
        bound_out.* = innerBounds;
    }

    pub fn gen_vertices_sides(
        self: *const Self,
        params: *const GenVerticesSidesParams,
        out: *utils.MultiArray(root.Vertex, 4),
    ) callconv(.C) bool {
        const texInfo = swi: switch (self.sides) {
            .All => |all| all,
            .TopOthers => |topOthers| {
                var texName = topOthers.other;
                if (params.side == .Y) {
                    texName = topOthers.top;
                }
                break :swi texName;
            },
            .TopBotOthers => |topOthers| {
                var texName = topOthers.other;
                if (params.side == .Y) {
                    texName = topOthers.top;
                }
                if (params.side == .NegY) {
                    texName = topOthers.bot;
                }
                break :swi texName;
            },
        };

        var sideScale =
            zlm.Vec3.new(1.0, comptime 15.0 / 16.0, 1.0);

        if (params.selfBlock.id == params.neighbors.y.id) {
            sideScale = zlm.Vec3.one;
        }

        return gen_vertices_sides_from_base(
            params.side,
            params.pos,
            out,
            baseVertices[0..],
            &texInfo,
            sideScale,
        );
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

pub fn getBlockId(blockName: []const u8) ?chunks.BlockId {
    for (state.blocksArr.items, 0..) |name, i| {
        if (std.mem.eql(u8, name.blockName.*, blockName)) {
            return @enumFromInt(i);
        }
    }

    return null;
}

pub fn getBlockFromId(id: chunks.BlockId) ?Block {
    if (state.blocksArr.items.len <= @intFromEnum(id)) {
        return null;
    }

    return state.blocksArr.items[@intFromEnum(id)];
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

    const out = cube.get_textures_names(&allocator) orelse unreachable;
    defer allocator.destroy(out);
    defer allocator.free(out.*);
    defer allocator.free(out.*[0]);

    try std.testing.expectEqual(1, out.len);
    try std.testing.expectEqualStrings("thing", out.*[0]);

    var block = try toBlock(cube, allocator, "thing");
    block.deinit();
}
