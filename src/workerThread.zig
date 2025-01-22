const std = @import("std");
const root = @import("main.zig");
const chunks = @import("chunks.zig");
const zlm = @import("zlm");

const utils = @import("utils.zig");

const IVec3 = zlm.SpecializeOn(i64).Vec3;

const state = &root.state;

pub const toWorkerThreadMessage = union(enum) {
    GetChunk: IVec3,
    SetPlayerPos: zlm.Vec3,
    SetBlock: struct {
        pos: IVec3,
        block: chunks.Block,
    },
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

    const frameTime = std.time.ns_per_s / 20;

    while (!state.close.load(.acquire)) {
        const start = std.time.nanoTimestamp();
        while (state.sendWorkerThreadQueue.dequeue()) |message| {
            switch (message) {
                .GetChunk => |pos| getChunk(&chunkMap, pos) catch unreachable,
                .SetPlayerPos => |pos| playerPos = pos,
                .SetBlock => |sbData| {
                    std.log.info("Setting block at: {}", .{sbData.pos});
                    chunkMap.set_block(sbData.pos, sbData.block) catch {
                        std.log.err("Failed to set block at: {}", .{sbData.pos});
                    };
                    const cpos = chunks.worldToChunkPos(utils.ivec3ToVec3(sbData.pos));
                    chunkMap.getPtr(cpos.chunkPos).?.chunk.regen_sides();
                    getChunk(&chunkMap, cpos.chunkPos) catch unreachable;
                },
            }
        }
        const now = std.time.nanoTimestamp();
        const took = (now - start);
        const diff = frameTime - took;
        if (diff > 0) {
            std.Thread.sleep(@intCast(diff));
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
