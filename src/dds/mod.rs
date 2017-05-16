//!  Decoding of DDS Files
//!
//!  A decoder for DDS (DirectDraw Surface) images
//!
//!  # Related Links
//!  * https://msdn.microsoft.com/en-us/library/bb204842(v=vs.85).aspx - DXT compression format
//!  * https://msdn.microsoft.com/en-us/library/windows/desktop/bb943991(v=vs.85).aspx - DDS specifications

pub use self::decoder::DDSDecoder;

mod decoder;
