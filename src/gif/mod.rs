//!  Decoding of GIF Images
//!
//!  GIF (Graphics Interchange Format) is an image format that supports lossless compression.
//!
//!  # Related Links
//!  * http://www.w3.org/Graphics/GIF/spec-gif89a.txt - The GIF Specification
//!

pub use self::decoder::GIFDecoder;

mod decoder;
mod bits;
pub mod lzw;