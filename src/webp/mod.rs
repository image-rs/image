//! Decoding of Webp Images

pub use self::decoder::WebpDecoder as WebpDecoder;

mod decoder;
mod transform;

pub mod vp8;