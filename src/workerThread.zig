const std = @import("std");
const root = @import("main.zig");
const chunks = @import("chunks.zig");
const zlm = @import("zlm");

const IVec3 = zlm.SpecializeOn(i64).Vec3;

const state = &root.state;

pub const toWorkerThreadMessage = union(enum) {
    GetChunk: IVec3,
    SetPlayerPos: zlm.Vec3,
};

pub const fromWorkerThreadMessage = union(enum) {
    NewChunk: struct {
        chunk: chunks.Chunk,
        pos: IVec3,
    },
};

pub fn workerThread() void {
    var chunkMap = chunks.ChunkMap.init(state.allocator);
    var playerPos: ?zlm.Vec3 = null;
    defer chunkMap.deinit();

    while (!state.close.load(.acquire)) {
        while (state.sendWorkerThreadQueue.dequeue()) |message| {
            switch (message) {
                .GetChunk => |pos| getChunk(&chunkMap, pos) catch unreachable,
                .SetPlayerPos => |pos| playerPos = pos,
            }
        }
    }
}

fn getChunk(chunkMap: *chunks.ChunkMap, pos: IVec3) !void {
    if (chunkMap.get(pos) == null) {
        try chunkMap.genChunk(pos);
    }
    try state.recvWorkerThreadQueue.enqueue(.{
        .NewChunk = .{
            .pos = pos,
            .chunk = chunkMap.get(pos).?.chunk,
        },
    });
}
