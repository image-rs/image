//! Chunk types and functions
#![allow(dead_code)]
#![allow(non_upper_case_globals)]

pub type ChunkType = [u8; 4];

// -- Critical chunks --

/// Image header
pub const IHDR: &'static ChunkType = b"IHDR";
/// Palette
pub const PLTE: &'static ChunkType = b"PLTE";
/// Image data
pub const IDAT: &'static ChunkType = b"IDAT";
/// Image trailer
pub const IEND: &'static ChunkType = b"IEND";

// -- Ancillary chunks --

/// Transparency
pub const tRNS: &'static ChunkType = b"tRNS";
/// Background colour
pub const bKGD: &'static ChunkType = b"bKGD";
/// Image last-modification time
pub const tIME: &'static ChunkType = b"tIME";

// -- Extension chunks --

/// Animation control
pub const acTL: &'static ChunkType = b"acTL";
/// Frame control
pub const fcTL: &'static ChunkType = b"fcTL";
/// Frame data
pub const fdAT: &'static ChunkType = b"fdAT";

// -- Chunk type determination --

/// Returns true if the chunk is critical.
pub fn is_critical(type_: ChunkType) -> bool {
    type_[0] & 32 == 0
}

/// Returns true if the chunk is private.
pub fn is_private(type_: ChunkType) -> bool {
    type_[1] & 32 != 0
}

/// Checks whether the reserved bit of the chunk name is set.
/// If it is set the chunk name is invalid.
pub fn reserved_set(type_: ChunkType) -> bool {
    type_[2] & 32 != 0
}

/// Returns true if the chunk is safe to copy if unknown.
pub fn save_to_copy(type_: ChunkType) -> bool {
    type_[3] & 32 != 0
}