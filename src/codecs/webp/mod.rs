//! Decoding of WebP Images

pub use self::decoder::WebPDecoder;

mod decoder;
mod transform;
mod lossless;

pub mod vp8;
