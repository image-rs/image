//! Decoding/Encoding of WebP Images

pub use self::decoder::WebPDecoder;
pub use self::encoder::WebPEncoder;

mod decoder;
mod encoder;

pub mod vp8;
