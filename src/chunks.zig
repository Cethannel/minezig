const std = @import("std");

pub const chunkWidth = 16;
pub const chunkHeight = 256;

pub const BlockId = enum(u32) {
    Air = 0,
};

pub const Block = struct {
    id: BlockId,
};

pub const Chunk = struct {
    blocks: [chunkWidth][chunkHeight][chunkWidth]Block,

    fn gen() !void {}
};
