//! Decoding and Encoding of WebP Images

pub use self::encoder::WebPEncoder;

mod encoder;

pub use self::decoder::WebPDecoder;

mod decoder;
mod extended;
mod huffman;
mod loop_filter;
mod lossless;
mod lossless_transform;
mod transform;

pub mod vp8;
