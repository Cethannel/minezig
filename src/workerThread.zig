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

var blockUpdateQueue: utils.MSPC(utils.IVec3) = undefined;

const ChunkMap = std.AutoHashMap(IVec3, chunks.Chunk);

pub fn workerThread() void {
    const dalloc = std.heap.page_allocator;
    var arena = std.heap.ArenaAllocator.init(dalloc);
    defer arena.deinit();
    const allocator = arena.allocator();
    var chunkMap = ChunkMap.init(allocator);
    var playerPos: ?zlm.Vec3 = null;
    defer chunkMap.deinit();
    blockUpdateQueue = utils.MSPC(utils.IVec3).init(allocator, 128) catch |err| {
        std.log.err("Failed to initialize block update queue: {t}", .{err});
        unreachable;
    };
    defer blockUpdateQueue.deinit();

    const frameTime = std.time.ns_per_s / 20;

    var updates = std.array_list.Managed(utils.IVec3).init(allocator);
    defer updates.clearAndFree();

    std.log.info("Entering worker thread", .{});
    while (!state.close.load(.acquire)) {
        const start = std.time.nanoTimestamp();
        while (blockUpdateQueue.dequeue()) |update| {
            updates.insert(0, update) catch |err| {
                std.log.err("Failed to insert block update: {t}", .{err});
                return;
            };
        }

        while (state.sendWorkerThreadQueue.dequeue()) |message| : ({
            std.Thread.yield() catch {};
        }) {
            switch (message) {
                .GetChunk => |pos| getChunk(&chunkMap, pos) catch |err| {
                    std.log.err("Failed to get chunk at {f}: {t}", .{ pos, err });
                    continue;
                },
                .SetPlayerPos => |pos| playerPos = pos,
                .SetBlock => |sbData| {
                    std.log.info("Setting block at: {f}", .{sbData.pos});
                    chunks.set_block(&chunkMap, sbData.pos, sbData.block) catch {
                        std.log.err("Failed to set block at: {f}", .{sbData.pos});
                    };
                    const cpos = chunks.worldToChunkPos(utils.ivec3ToVec3(sbData.pos));
                    getChunk(&chunkMap, cpos.chunkPos) catch |err| {
                        std.log.err("Failed to get chunk at {f}: {t}", .{ cpos.chunkPos, err });
                        continue;
                    };
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

        while (updates.pop()) |update| {
            std.log.info("Got block update at: {f}", .{update});
            if (chunks.getBlockPtr(&chunkMap, update)) |block| {
                if (blocks.getBlockFromId(block.id)) |blk| {
                    blk.block_update(&.{
                        .pos = &update,
                        .chunksMap = &chunkMap,
                        .setblockCallback = &setBlockCallback,
                        .blockUpdateCallback = &blockUpdateCallback,
                    });
                } else {
                    std.log.err(
                        "Failed to get block with id: {d}",
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

    std.log.info("Laving worker thread", .{});
}

fn getChunk(chunkMap: *ChunkMap, pos: IVec3) !void {
    if (chunkMap.get(pos) == null) {
        try chunks.genChunk(chunkMap, pos);
    }
    try state.recvWorkerThreadQueue.enqueue(.{
        .NewChunk = .{
            .pos = pos,
            .chunk = chunkMap.get(pos).?,
        },
    });
}

fn blockUpdateCallback(pos: *const utils.IVec3) callconv(.c) void {
    blockUpdateQueue.enqueue(pos.*) catch {
        std.log.err("Failed to trigger block update at: {f}", .{pos.*});
    };
}

fn setBlockCallback(
    pos: *const utils.IVec3,
    block: *const chunks.Block,
) callconv(.c) void {
    std.log.info("Setting block at: {f}", .{pos.*});
    state.sendWorkerThreadQueue.enqueue(.{
        .SetBlock = .{
            .block = block.*,
            .pos = pos.*,
        },
    }) catch {
        std.log.err("Failed to set block at: {f}", .{pos.*});
    };
}
