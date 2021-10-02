//! Decoding of WebP Images

pub use self::decoder::WebPDecoder;

mod decoder;
mod transform;
mod loop_filter;

pub mod vp8;
