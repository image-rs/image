//!  Decoding of ICO files
//!
//!  A decoder for ICO (Windows Icon) image container files
//!
//!  # Related Links
//!  * https://msdn.microsoft.com/en-us/library/ms997538.aspx
//!  * https://en.wikipedia.org/wiki/ICO_%28file_format%29

pub use self::decoder::ICODecoder;

mod decoder;
