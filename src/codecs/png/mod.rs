//! Decoding and Encoding of PNG Images
//!
//! PNG (Portable Network Graphics) is an image format that supports lossless compression.
//!
//! # Related Links
//! * <http://www.w3.org/TR/PNG/> - The PNG Specification
mod decoder;
mod encoder;

pub use decoder::*;
pub use encoder::*;

// http://www.w3.org/TR/PNG-Structure.html
// The first eight bytes of a PNG file always contain the following (decimal) values:
pub(crate) const PNG_SIGNATURE: [u8; 8] = [137, 80, 78, 71, 13, 10, 26, 10];

const XMP_KEY: &str = "XML:com.adobe.xmp";
