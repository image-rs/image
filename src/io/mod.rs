//! Input and output of images.
mod metagram;
mod reader;
pub(crate) mod free_functions;

pub use self::reader::Reader;
pub use self::metagram::{Metagram, Recorder, SharedRecorder};
