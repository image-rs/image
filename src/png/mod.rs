//! Decoding and Encoding of PNG Images
//!
//! PNG (Portable Network Graphics) is an image format that supports lossless compression.
//!
//! # Related Links
//! * http://www.w3.org/TR/PNG/ - The PNG Specification
//!

pub use self::decoder::PNGDecoder;
pub use self::encoder::PNGEncoder;

mod filter;
mod decoder;
mod encoder;
pub mod zlib;
pub mod inflate;
pub mod hash;