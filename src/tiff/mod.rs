//!  Decoding and Encoding of TIFF Images
//!
//!  TIFF (Tagged Image File Format) is a versatile image format that supports 
//!  lossless and lossy compression.
//!
//!  # Related Links
//!  * http://partners.adobe.com/public/developer/tiff/index.html - The TIFF specification
//!

pub use self::decoder::TIFFDecoder;
pub use self::stream::ByteOrder;

mod decoder;
mod ifd;
mod stream;