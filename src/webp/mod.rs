//! Decoding of Webp Images

pub use WebpDecoder = self::decoder::WebpDecoder;

mod decoder;
mod transform;

pub mod vp8;