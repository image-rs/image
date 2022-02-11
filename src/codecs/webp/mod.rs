//! Decoding of WebP Images

pub use self::decoder::WebPDecoder;

mod decoder;
mod loop_filter;
mod transform;

mod huffman;
mod lossless;
mod lossless_transform;

mod extended;

pub mod vp8;
