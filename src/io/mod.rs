//! Input and output of images.
mod metadata;
mod reader;
pub(crate) mod free_functions;

pub use self::reader::Reader;
pub use self::metadata::{DatumRequested, MetadataContainer, Recorder, RecorderConfig, SharedRecorder};
