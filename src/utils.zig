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
