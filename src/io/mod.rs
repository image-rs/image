//! Input and output of images.
mod reader;
mod limits;
pub(crate) mod free_functions;

pub use self::reader::Reader;
pub use self::limits::DecodingLimits;
