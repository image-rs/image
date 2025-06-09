//! An internal module for grouping all forms of image buffers.
pub(crate) mod buffer;
#[cfg(feature = "rayon")]
pub(crate) mod buffer_par;
pub(crate) mod dynimage;
pub mod flat;
pub(crate) mod sub_image;
