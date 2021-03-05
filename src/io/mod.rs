//! Input and output of images.

use crate::{error, ImageError, ImageResult};

mod reader;
pub(crate) mod free_functions;

pub use self::reader::Reader;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
/// Set of supported strict limits for a decoder
pub struct LimitSupport {
    /// This field indicates whether the strict limit on the number of bytes is
    /// supported.
    pub max_alloc_strict: bool,
    _non_exhaustive: (),
}

impl Default for LimitSupport {
    fn default() -> LimitSupport {
        LimitSupport {
            max_alloc_strict: false,
            _non_exhaustive: (),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
/// Resource limits for decoding.
///
/// Limits can be either *strict* or *non-strict*. Non-strict limits are best-effort
/// limits where the library does not guarantee that limit wont be exceeded. Do note 
/// that it is still considered a bug if a non-strict limit is exceeded, however as 
/// some of the underlying decoders do . For strict
/// limits the library makes a stronger guarantee that the limit will not be exceeded.
/// Exceeding a strict limit is considered a critical bug. If a decoder cannot 
/// guaranree that it will uphold a strict limit it *must* fail with 
/// `image::error::LimitErrorKind::Unsupported`.
///
/// The limit check should only ever fail if a limit will be exceeded or an unsupported strict
/// limit is used.
pub struct Limits {
    /// The maximum allowed image width. This limit is strict. The default is no limit.
    pub max_image_width: Option<u32>,
    /// The maximum allowed image height. This limit is strict. The default is no limit.
    pub max_image_height: Option<u32>,
    /// The maximum allowed amount of memory to be allocated by the 
    /// decoder at any one time. This limit is non-strict by default,
    /// set `max_alloc_strict` to make this limit strict. The default 
    /// is 512MiB. Note that limits below 1MiB are not supported, this 
    /// is also the case if this limit is strict.
    pub max_alloc: Option<u64>,
    /// If `max_alloc` should be a strict limit.
    pub max_alloc_strict: bool,
    _non_exhaustive: (),
}

impl Default for Limits {
    fn default() -> Limits {
        Limits {
            max_image_width: None,
            max_image_height: None,
            max_alloc: Some(512*1024*1024),
            max_alloc_strict: false,
            _non_exhaustive: (),
        }
    }
}

impl Limits {
    /// Disable all limits
    pub fn no_limits() -> Limits {
        Limits {
            max_image_width: None,
            max_image_height: None,
            max_alloc: None,
            max_alloc_strict: false,
            _non_exhaustive: (),
        }
    }

    /// Check if the current limits given a decoder and a set of supported strict limits.
    ///
    /// This function checks that all currently set strict limits are supported. It also
    /// checks the `max_image_width` and `max_image_height` limits using the dimensions
    /// from the decoder.
    pub fn check_support<'a, D: crate::ImageDecoder<'a>>(&self, decoder: &mut D, supported: LimitSupport) -> ImageResult<()> {
        if self.max_alloc.is_some() && self.max_alloc_strict && !supported.max_alloc_strict {
            return Err(ImageError::Limits(error::LimitError::from_kind(
                error::LimitErrorKind::Unsupported {
                    limits: *self,
                    supported: supported,
                })))
        }

        let (width, height) = decoder.dimensions();
        if let Some(max_width) = self.max_image_width {
            if width > max_width {
                return Err(ImageError::Limits(crate::error::LimitError::from_kind(
                    crate::error::LimitErrorKind::DimensionError)))
            }
        }

        if let Some(max_height) = self.max_image_height {
            if height > max_height {
                return Err(ImageError::Limits(crate::error::LimitError::from_kind(
                    crate::error::LimitErrorKind::DimensionError)))
            }
        }

        Ok(())
    }
}
