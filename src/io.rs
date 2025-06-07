//! Input and output of images.

pub(crate) mod free_functions;
pub(crate) mod image_reader_type;
pub(crate) mod limits;

#[deprecated(note = "this type has been moved and renamed to image::ImageReader")]
/// Deprecated re-export of `ImageReader` as `Reader`
pub type Reader<R> = ImageReader<R>;
#[deprecated(note = "this type has been moved to image::Limits")]
/// Deprecated re-export of `Limits`
pub type Limits = limits::Limits;
#[deprecated(note = "this type has been moved to image::LimitSupport")]
/// Deprecated re-export of `LimitSupport`
pub type LimitSupport = limits::LimitSupport;

pub(crate) use self::image_reader_type::ImageReader;
