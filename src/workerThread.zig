const std = @import("std");
const root = @import("main.zig");
const chunks = @import("chunks.zig");
const zlm = @import("zlm");

const utils = @import("utils.zig");
const blocks = @import("blocks.zig");

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

var blockUpdateQueue: utils.mspc(utils.IVec3) = undefined;

pub fn workerThread() void {
    var chunkMap = chunks.ChunkMap.init(state.allocator);
    var playerPos: ?zlm.Vec3 = null;
    defer chunkMap.deinit();
    blockUpdateQueue = utils.mspc(utils.IVec3).init(state.allocator, 128) catch unreachable;
    defer blockUpdateQueue.deinit();

    const frameTime = std.time.ns_per_s / 20;

    var updates = std.ArrayList(utils.IVec3).init(state.allocator);
    defer updates.clearAndFree();

    while (!state.close.load(.acquire)) {
        const start = std.time.nanoTimestamp();
        while (blockUpdateQueue.dequeue()) |update| {
            updates.insert(0, update) catch unreachable;
        }

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
                    blockUpdateCallback(&sbData.pos);
                    inline for (.{ -1, 1 }) |x| {
                        inline for (.{ -1, 1 }) |z| {
                            const neighborpos = sbData.pos.add(utils.IVec3.new(x, 0, z));
                            blockUpdateCallback(&neighborpos);
                        }
                    }
                },
            }
        }

        while (updates.popOrNull()) |update| {
            std.log.info("Got block update at: {}", .{update});
            if (chunkMap.getBlockPtr(update)) |block| {
                if (blocks.getBlockFromId(block.id)) |blk| {
                    blk.block_update(&.{
                        .pos = &update,
                        .chunksMap = &chunkMap,
                        .setblockCallback = &setBlockCallback,
                        .blockUpdateCallback = &blockUpdateCallback,
                    });
                } else {
                    std.log.err(
                        "Failed to get block with id: {}",
                        .{@intFromEnum(block.id)},
                    );
                }
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

fn blockUpdateCallback(pos: *const utils.IVec3) callconv(.C) void {
    blockUpdateQueue.enqueue(pos.*) catch {
        std.log.err("Failed to trigger block update at: {}", .{pos.*});
    };
}

fn setBlockCallback(
    pos: *const utils.IVec3,
    block: *const chunks.Block,
) callconv(.C) void {
    state.sendWorkerThreadQueue.enqueue(.{
        .SetBlock = .{
            .block = block.*,
            .pos = pos.*,
        },
    }) catch {
        std.log.err("Failed to set block at: {}", .{pos.*});
    };
}
