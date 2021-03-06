//! Input and output of images.

use crate::{error, ImageError, ImageResult};

mod reader;
pub(crate) mod free_functions;

pub use self::reader::Reader;

/// Set of supported strict limits for a decoder
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
#[allow(missing_copy_implementations)]
pub struct LimitSupport {
    _non_exhaustive: (),
}

impl Default for LimitSupport {
    fn default() -> LimitSupport {
        LimitSupport {
            _non_exhaustive: (),
        }
    }
}

/// Resource limits for decoding.
///
/// Limits can be either *strict* or *non-strict*. Non-strict limits are best-effort
/// limits where the library does not guarantee that limit wont be exceeded. Do note 
/// that it is still considered a bug if a non-strict limit is exceeded, however as 
/// some of the underlying decoders do not support not support such limits one cannot 
/// rely on these limits being supported. For stric limits the library makes a stronger 
/// guarantee that the limit will not be exceeded. Exceeding a strict limit is considered 
/// a critical bug. If a decoder cannot guaranree that it will uphold a strict limit it 
/// *must* fail with `image::error::LimitErrorKind::Unsupported`.
///
/// The limit check should only ever fail if a limit will be exceeded or an unsupported
/// strict limit is used.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
#[allow(missing_copy_implementations)]
pub struct Limits {
    /// The maximum allowed image width. This limit is strict. The default is no limit.
    pub max_image_width: Option<u32>,
    /// The maximum allowed image height. This limit is strict. The default is no limit.
    pub max_image_height: Option<u32>,
    /// The maximum allowed amount of memory to be allocated by the 
    /// decoder at any one time. This limit is non-strict and some 
    /// decoders may ignore it. The default is 512MiB.
    pub max_alloc: Option<u64>,
    _non_exhaustive: (),
}

impl Default for Limits {
    fn default() -> Limits {
        Limits {
            max_image_width: None,
            max_image_height: None,
            max_alloc: Some(512*1024*1024),
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
            _non_exhaustive: (),
        }
    }

    /// This function checks that all currently set strict limits are supported.
    pub fn check_support(&self, _supported: &LimitSupport) -> ImageResult<()> {
        Ok(())
    }

    /// This function checks the `max_image_width` and `max_image_height` limits given
    /// the image width and height.
    pub fn check_dimensions(&self, width: u32, height: u32) -> ImageResult<()> {
        if let Some(max_width) = self.max_image_width {
            if width > max_width {
                return Err(ImageError::Limits(error::LimitError::from_kind(
                    error::LimitErrorKind::DimensionError)))
            }
        }

        if let Some(max_height) = self.max_image_height {
            if height > max_height {
                return Err(ImageError::Limits(error::LimitError::from_kind(
                    error::LimitErrorKind::DimensionError)))
            }
        }

        Ok(())
    }

    /// This function checks that the current limit allows for reserving the set amount
    /// of bytes, it then reduces the limit accordingly.
    pub fn reserve(&mut self, amount: u64) -> ImageResult<()> {
        if let Some(max_alloc) = self.max_alloc.as_mut() {
            if *max_alloc < amount {
                return Err(ImageError::Limits(error::LimitError::from_kind(
                    error::LimitErrorKind::InsufficientMemory)))
            }

            *max_alloc -= amount;
        }

        Ok(())
    }

    /// This function increases the `max_alloc` limit with amount. Should only be used
    /// togheter with [`reserve`].
    ///
    /// [`reserve`]: #method.reserve
    pub fn free(&mut self, amount: u64) {
        if let Some(max_alloc) = self.max_alloc.as_mut() {
            *max_alloc = max_alloc.saturating_add(amount);
        }
    }
}
