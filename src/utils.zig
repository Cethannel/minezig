const std = @import("std");

const zlm = @import("zlm");

pub const IVec3 = zlm.SpecializeOn(i64).Vec3;

pub fn mspc(T: type) type {
    const atomicsUsize = std.atomic.Value(usize);

    return struct {
        count: atomicsUsize,
        head: atomicsUsize,
        tail: usize,
        max: usize,
        buffer: []std.atomic.Value(?*T),
        allocator: std.mem.Allocator,

        const Self = @This();

        pub fn innerT(_: *const Self) type {
            return T;
        }

        pub fn init(allocator: std.mem.Allocator, capacity: usize) !Self {
            var buffer = try allocator.alloc(std.atomic.Value(?*T), capacity);
            errdefer allocator.free(buffer);
            @memset(buffer[0..], std.atomic.Value(?*T).init(null));
            return Self{
                .count = atomicsUsize.init(0),
                .head = atomicsUsize.init(0),
                .tail = 0,
                .buffer = buffer,
                .allocator = allocator,
                .max = capacity,
            };
        }

        pub fn deinit(self: *Self) void {
            while (self.dequeue()) |val| {
                _ = val;
            }

            self.allocator.free(self.buffer);
        }

        pub fn enqueue(self: *Self, value: T) !void {
            const count = self.count.fetchAdd(1, .acquire);
            if (count >= self.max) {
                _ = self.count.fetchSub(1, .release);
                return error.Full;
            }

            const obj = try self.allocator.create(T);
            obj.* = value;

            const head = self.head.fetchAdd(1, .acquire);
            std.debug.assert(self.buffer[head % self.max].load(.acquire) == null);
            const rv = self.buffer[head % self.max].rmw(.Xchg, obj, .release);
            std.debug.assert(rv == null);
            return;
        }

        pub fn dequeue(self: *Self) ?T {
            const retPtr = self.buffer[self.tail].rmw(.Xchg, null, .acquire);
            if (retPtr == null) {
                return null;
            }
            defer self.allocator.destroy(retPtr.?);

            self.tail += 1;
            if (self.tail >= self.max) {
                self.tail = 0;
            }

            const r = self.count.fetchSub(1, .release);
            std.debug.assert(r > 0);

            return retPtr.?.*;
        }
    };
}

pub fn assert(condition: bool, message: ?[]const u8) void {
    if (!condition) {
        const src = @src();
        std.log.err("Failed assert: {s} at {s}:[{}]", .{
            src.fn_name,
            src.file,
            src.line,
        });
        if (message) |msg| {
            @panic(msg);
        } else {
            @panic("");
        }
    }
}

const dataType = struct {
    a: i32,
    b: []const u8,
};

const values = [_]dataType{
    .{
        .a = 1,
        .b = "a",
    },
    .{
        .a = 2,
        .b = "ab",
    },
    .{
        .a = 3,
        .b = "abs",
    },
};

test "Single thread test" {
    const alloc = std.testing.allocator;

    var queue = try mspc(dataType).init(alloc, 5);
    defer queue.deinit();

    for (values) |value| {
        try queue.enqueue(value);
    }

    for (values) |value| {
        const out = queue.dequeue();

        try std.testing.expectEqualDeep(out.?, value);
    }
}

var otherDone = false;

pub fn optionalTypeName(comptime T: ?type) [:0]const u8 {
    if (T) |t| {
        return @typeName(t);
    } else {
        return @typeName(void);
    }
}

pub fn ivec3ToVec3(input: zlm.SpecializeOn(i64).Vec3) zlm.Vec3 {
    var out = zlm.Vec3.zero;

    inline for (std.meta.fields(zlm.Vec3)) |field| {
        @field(out, field.name) = @floatFromInt(@field(input, field.name));
    }

    return out;
}

pub fn vec3ToIVec3(input: zlm.Vec3) zlm.SpecializeOn(i64).Vec3 {
    var out = zlm.SpecializeOn(i64).Vec3.zero;

    inline for (std.meta.fields(zlm.Vec3)) |field| {
        @field(out, field.name) = @floatFromInt(@field(input, field.name));
    }

    return out;
}

fn otherThread(data: []const dataType, queue: *mspc(dataType)) void {
    for (data) |value| {
        queue.enqueue(value) catch unreachable;
    }
    otherDone = true;
}

pub fn binaryInsertAssumeCapacity(
    comptime T: type,
    arr: *std.ArrayList(T),
    context: anytype,
    comptime lessThanFn: fn (@TypeOf(context), lhs: T, rhs: T) bool,
    value: T,
) void {
    var left: usize = 0;
    var right: usize = arr.items.len;

    while (left < right) {
        const mid = left + (right - left) / 2;
        if (lessThanFn(context, arr.items[mid], value)) {
            left = mid + 1;
        } else {
            right = mid;
        }
    }

    arr.insertAssumeCapacity(left, value);
}

test "Multi thread test" {
    const alloc = std.testing.allocator;

    var queue = try mspc(dataType).init(alloc, 5);
    defer queue.deinit();

    var thread = try std.Thread.spawn(.{}, otherThread, .{ @as([]const dataType, values[0..]), &queue });

    var outVals = try std.ArrayList(dataType).initCapacity(alloc, values.len);
    defer outVals.deinit();

    while (!otherDone) {
        while (queue.dequeue()) |val| {
            try outVals.append(val);
        }
    }

    thread.join();

    for (0..values.len) |i| {
        try std.testing.expectEqualDeep(values[i], outVals.items[i]);
    }
}

const meta = std.meta;
const mem = std.mem;

pub fn MultiArray(comptime T: type, comptime len: usize) type {
    return extern struct {
        bytes: [len * @sizeOf(T)]u8 = @splat(0),

        const Self = @This();

        pub const empty: Self = .{
            .bytes = @splat(0),
        };

        const Elem = @typeInfo(T).@"struct";

        const Field = meta.FieldEnum(T);

        fn FieldType(comptime field: Field) type {
            return meta.fieldInfo(T, field).type;
        }

        const fields = @typeInfo(T).@"struct".fields;

        fn caclOffsetField(
            comptime field: Field,
        ) usize {
            const fieldI = @intFromEnum(field);
            var offset: usize = 0;
            inline for (Elem.fields, 0..) |f, i| {
                if (i >= fieldI) {
                    break;
                }
                offset += @sizeOf(f.type) * len;
            }
            return offset;
        }

        fn caclOffset(
            comptime field: Field,
            index: usize,
        ) usize {
            const fieldI = @intFromEnum(field);
            const fieldSize = @sizeOf(fields[fieldI].type);
            return caclOffsetField(field) + fieldSize * index;
        }

        fn getFieldPtr(
            self: *const Self,
            comptime field: Field,
            index: usize,
        ) *FieldType(field) {
            var arr: [*]const u8 = self.bytes[0..].ptr;
            arr += caclOffset(field, index);
            return @constCast(@alignCast(@ptrCast(arr)));
        }

        pub fn getField(
            self: *const Self,
            comptime field: Field,
            index: usize,
        ) FieldType(field) {
            return self.getFieldPtr(field, index).*;
        }

        pub fn setField(
            self: *const Self,
            comptime field: Field,
            index: usize,
            value: FieldType(field),
        ) void {
            self.getFieldPtr(field, index).* = value;
        }

        pub fn get(self: *const Self, index: usize) T {
            var out: T = undefined;
            inline for (fields, 0..) |field, i| {
                @field(out, field.name) = self.getField(@enumFromInt(i), index);
            }
            return out;
        }

        pub fn set(self: *Self, index: usize, value: T) void {
            inline for (fields, 0..) |field, i| {
                self.setField(@enumFromInt(i), index, @field(value, field.name));
            }
        }
    };
}

pub fn getFieldOrNull(
    value: anytype,
    comptime field: std.meta.FieldEnum(@TypeOf(value)),
) ?@FieldType(@TypeOf(value), @tagName(field)) {
    if (value) |iV| {
        return @field(iV, @tagName(field));
    }
    return null;
}

test "MultiArray" {
    var mArr = MultiArray(struct {
        x: u32,
        y: u32,
    }, 5).empty;

    for (0..5) |value| {
        mArr.set(value, .{
            .x = @intCast(value),
            .y = @intCast(value),
        });
    }

    for (0..5) |i| {
        try std.testing.expectEqual(i, mArr.get(i).x);
        try std.testing.expectEqual(i, mArr.get(i).y);
    }
}

pub const CullingFrustum = struct {
    near_right: f32,
    near_top: f32,
    near_plane: f32,
    far_plane: f32,
};

pub const AABB = struct {
    min: zlm.Vec3,
    max: zlm.Vec3,
};

pub const OBB = struct {
    center: zlm.Vec3,
    extents: zlm.Vec3,
    axes: [3]zlm.Vec3,
};

pub fn SATVisibilityTest(
    frustum: CullingFrustum,
    vs_transform: zlm.Mat4,
    aabb: AABB,
) bool {
    const z_near = frustum.near_plane;
    const z_far = frustum.far_plane;
    const x_near = frustum.near_right;
    const y_near = frustum.near_top;

    var corners = [_]zlm.Vec3{
        .{ .x = aabb.min.x, .y = aabb.min.y, .z = aabb.min.z },
        .{ .x = aabb.max.x, .y = aabb.min.y, .z = aabb.min.z },
        .{ .x = aabb.min.x, .y = aabb.max.y, .z = aabb.min.z },
        .{ .x = aabb.min.x, .y = aabb.min.y, .z = aabb.max.z },
    };

    for (&corners) |*corner| {
        corner.* = (corner.toAffinePosition().transform(vs_transform)).swizzle("xyz");
    }

    var obb = OBB{
        .axes = .{
            corners[1].sub(corners[0]),
            corners[2].sub(corners[0]),
            corners[3].sub(corners[0]),
        },
        .center = undefined,
        .extents = undefined,
    };

    obb.center = corners[0].add(obb.axes[0].add(obb.axes[1]).add(obb.axes[2]).scale(0.5));
    obb.extents = zlm.vec3(obb.axes[0].length(), obb.axes[1].length(), obb.axes[2].length());
    obb.axes[0] = obb.axes[0].scale(1 / obb.extents.x);
    obb.axes[1] = obb.axes[1].scale(1 / obb.extents.y);
    obb.axes[2] = obb.axes[2].scale(1 / obb.extents.z);
    obb.extents = obb.extents.scale(0.5);

    {
        //const M = zlm.Vec3.unitZ;
        //const MoX = 0.0;
        //const MoY = 0.0;
        //const MoZ = M.z;

        const MoC = obb.center.z;
        var radius: f32 = 0.0;
        inline for (std.meta.fields(zlm.Vec3), 0..) |field, i| {
            radius += @abs(obb.axes[i].z) * @field(obb.extents, field.name);
        }
        const obb_min = MoC - radius;
        const obb_max = MoC + radius;

        const m0 = z_far;
        const m1 = z_near;

        if (obb_min > m1 or obb_max < m0) {
            return false;
        }
    }

    {
        const M = [_]zlm.Vec3{
            .{ .x = 0.0, .y = -z_near, .z = y_near }, // Top plane
            .{ .x = 0.0, .y = z_near, .z = y_near }, // Bottom plane
            .{ .x = -z_near, .y = 0.0, .z = x_near }, // Right plane
            .{ .x = z_near, .y = 0.0, .z = x_near }, // Left Plane
        };

        for (M) |m| {
            const MoX = @abs(m.x);
            const MoY = @abs(m.y);
            const MoZ = m.z;
            const MoC = m.dot(obb.center);

            var obb_radius: f32 = 0.0;
            inline for (std.meta.fields(zlm.Vec3), 0..) |field, i| {
                obb_radius += @abs(m.dot(obb.axes[i])) * @field(obb.extents, field.name);
            }

            const obb_min = MoC - obb_radius;
            const obb_max = MoC + obb_radius;

            const p = x_near * MoX + y_near * MoY;

            var tau_0 = z_near * MoZ - p;
            var tau_1 = z_near * MoZ + p;

            if (tau_0 < 0.0) {
                tau_0 *= z_far / z_near;
            }
            if (tau_1 > 0.0) {
                tau_1 *= z_far / z_near;
            }

            if (obb_min > tau_1 or obb_max < tau_0) {
                return false;
            }
        }
    }

    const field_names = .{
        "x",
        "y",
        "z",
    };

    // OBB Axes
    {
        inline for (obb.axes, 0..) |M, i| {
            const MoX = @abs(M.x);
            const MoY = @abs(M.y);
            const MoZ = M.z;
            const MoC = M.dot(obb.center);

            const obb_radius: f32 = @field(obb.extents, field_names[i]);

            const obb_min = MoC - obb_radius;
            const obb_max = MoC + obb_radius;

            const p = x_near + MoX + y_near * MoY;
            var tau_0 = z_near * MoZ - p;
            var tau_1 = z_near * MoZ + p;
            if (tau_0 < 0.0) {
                tau_0 *= z_far / z_near;
            }
            if (tau_1 > 0.0) {
                tau_1 *= z_far / z_near;
            }

            if (obb_min > tau_1 or obb_max < tau_0) {
                return false;
            }
        }
    }

    // Now let's perform each of the cross products between the edges
    // First R x A_i
    {
        for (obb.axes) |axis| {
            const M: zlm.Vec3 = .{ .x = 0.0, .y = -axis.z, .z = axis.y };
            const MoX = 0.0;
            const MoY = @abs(M.y);
            const MoZ = M.z;
            const MoC = M.y * obb.center.y + M.z * obb.center.z;

            var obb_radius: f32 = 0.0;
            inline for (std.meta.fields(zlm.Vec3), 0..) |field, i| {
                obb_radius += @abs(M.dot(obb.axes[i])) * @field(obb.extents, field.name);
            }

            const obb_min = MoC - obb_radius;
            const obb_max = MoC + obb_radius;

            // Frustum projection
            const p = x_near * MoX + y_near * MoY;
            var tau_0 = z_near * MoZ - p;
            var tau_1 = z_near * MoZ + p;
            if (tau_0 < 0.0) {
                tau_0 *= z_far / z_near;
            }
            if (tau_1 > 0.0) {
                tau_1 *= z_far / z_near;
            }

            if (obb_min > tau_1 or obb_max < tau_0) {
                return false;
            }
        }
    }

    // U x A_i
    {
        for (obb.axes) |axis| {
            const M: zlm.Vec3 = .{ .x = axis.z, .y = 0.0, .z = -axis.x };
            const MoX = @abs(M.x);
            const MoY = 0.0;
            const MoZ = M.z;
            const MoC = M.x * obb.center.x + M.z * obb.center.z;

            var obb_radius: f32 = 0.0;
            inline for (std.meta.fields(zlm.Vec3), 0..) |field, i| {
                obb_radius += @abs(axis.dot(obb.axes[i])) * @field(obb.extents, field.name);
            }

            const obb_min = MoC - obb_radius;
            const obb_max = MoC + obb_radius;

            // Frustum projection
            const p = x_near * MoX + y_near * MoY;
            var tau_0 = z_near * MoZ - p;
            var tau_1 = z_near * MoZ + p;
            if (tau_0 < 0.0) {
                tau_0 *= z_far / z_near;
            }
            if (tau_1 > 0.0) {
                tau_1 *= z_far / z_near;
            }

            if (obb_min > tau_1 or obb_max < tau_0) {
                return false;
            }
        }
    }

    // Frustum Edges X Ai
    {
        for (obb.axes) |axis| {
            const M = [_]zlm.Vec3{
                zlm.Vec3.cross(.{ .x = -x_near, .y = 0.0, .z = z_near }, axis), // Left Plane
                zlm.Vec3.cross(.{ .x = x_near, .y = 0.0, .z = z_near }, axis), // Right plane
                zlm.Vec3.cross(.{ .x = 0.0, .y = y_near, .z = z_near }, axis), // Top plane
                zlm.Vec3.cross(.{ .x = 0.0, .y = -y_near, .z = z_near }, axis), // Bottom plane
            };

            for (M, 0..) |a, m| {
                const MoX = @abs(a.x);
                const MoY = @abs(a.y);
                const MoZ = a.z;

                const epsilon = 1e-4;
                if (MoX < epsilon and MoY < epsilon and @abs(MoZ) < epsilon) continue;

                const MoC = M[m].dot(obb.center);

                var obb_radius: f32 = 0.0;
                inline for (std.meta.fields(zlm.Vec3), 0..) |field, i| {
                    obb_radius += @abs(a.dot(obb.axes[i])) * @field(obb.extents, field.name);
                }

                const obb_min = MoC - obb_radius;
                const obb_max = MoC + obb_radius;

                // Frustum projection
                const p = x_near * MoX + y_near * MoY;
                var tau_0 = z_near * MoZ - p;
                var tau_1 = z_near * MoZ + p;
                if (tau_0 < 0.0) {
                    tau_0 *= z_far / z_near;
                }
                if (tau_1 > 0.0) {
                    tau_1 *= z_far / z_near;
                }

                if (obb_min > tau_1 or obb_max < tau_0) {
                    return false;
                }
            }
        }
    }

    // No intersections detected
    return true;
}

pub fn testAABBAgainstFrustum(mvp: zlm.Mat4, aabb: *const AABB) bool {
    inline for (std.meta.fields(zlm.Vec3)) |field| {
        const min = @field(aabb.min, field.name);
        const max = @field(aabb.max, field.name);
        if (min > max) {
            std.debug.panic("Field {s} is wrong min ({}) > max ({})", .{
                field.name,
                min,
                max,
            });
        }
    }

    const corners: [8]zlm.Vec4 = .{
        .{ .x = aabb.min.x, .y = aabb.min.y, .z = aabb.min.z, .w = 1.0 }, // x y z
        .{ .x = aabb.max.x, .y = aabb.min.y, .z = aabb.min.z, .w = 1.0 }, // X y z
        .{ .x = aabb.min.x, .y = aabb.max.y, .z = aabb.min.z, .w = 1.0 }, // x Y z
        .{ .x = aabb.max.x, .y = aabb.max.y, .z = aabb.min.z, .w = 1.0 }, // X Y z
        .{ .x = aabb.min.x, .y = aabb.min.y, .z = aabb.max.z, .w = 1.0 }, // x y Z
        .{ .x = aabb.max.x, .y = aabb.min.y, .z = aabb.max.z, .w = 1.0 }, // X y Z
        .{ .x = aabb.min.x, .y = aabb.max.y, .z = aabb.max.z, .w = 1.0 }, // x Y Z
        .{ .x = aabb.max.x, .y = aabb.max.y, .z = aabb.max.z, .w = 1.0 }, // X Y Z
    };
    _ = &mvp;

    var inside = false;

    for (corners) |corner| {
        const transformed_corner = corner.transform(mvp);

        inside = inside or
            within(-transformed_corner.w, transformed_corner.x, transformed_corner.w) and
                within(-transformed_corner.w, transformed_corner.y, transformed_corner.w) and
                within(0.0, transformed_corner.z, transformed_corner.w);
    }

    return inside;
}

fn within(lower: f32, point: f32, upper: f32) bool {
    return lower <= point and point <= upper;
}
