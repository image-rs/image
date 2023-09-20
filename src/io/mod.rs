//! Input and output of images.

use std::convert::TryFrom;

use crate::{error, ImageError, ImageResult};

pub(crate) mod free_functions;
mod reader;

pub use self::reader::Reader;

/// Set of supported strict limits for a decoder.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
#[allow(missing_copy_implementations)]
#[allow(clippy::manual_non_exhaustive)]
pub struct LimitSupport {
    _non_exhaustive: (),
}

#[allow(clippy::derivable_impls)]
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
/// limits where the library does not guarantee that limit will not be exceeded. Do note
/// that it is still considered a bug if a non-strict limit is exceeded, however as
/// some of the underlying decoders do not support not support such limits one cannot
/// rely on these limits being supported. For stric limits the library makes a stronger
/// guarantee that the limit will not be exceeded. Exceeding a strict limit is considered
/// a critical bug. If a decoder cannot guarantee that it will uphold a strict limit it
/// *must* fail with `image::error::LimitErrorKind::Unsupported`.
///
/// Currently the only strict limits supported are the `max_image_width` and `max_image_height`
/// limits, however more will be added in the future. [`LimitSupport`] will default to support
/// being false and decoders should enable support for the limits they support in
/// [`ImageDecoder::set_limits`].
///
/// The limit check should only ever fail if a limit will be exceeded or an unsupported strict
/// limit is used.
///
/// [`LimitSupport`]: ./struct.LimitSupport.html
/// [`ImageDecoder::set_limits`]: ../trait.ImageDecoder.html#method.set_limits
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
#[allow(missing_copy_implementations)]
#[allow(clippy::manual_non_exhaustive)]
pub struct Limits {
    /// The maximum allowed image width. This limit is strict. The default is no limit.
    pub max_image_width: Option<u32>,
    /// The maximum allowed image height. This limit is strict. The default is no limit.
    pub max_image_height: Option<u32>,
    /// The maximum allowed sum of allocations allocated by the decoder at any one time excluding
    /// allocator overhead. This limit is non-strict by default and some decoders may ignore it.
    /// The default is 512MiB.
    pub max_alloc: Option<u64>,
    _non_exhaustive: (),
}

impl Default for Limits {
    fn default() -> Limits {
        Limits {
            max_image_width: None,
            max_image_height: None,
            max_alloc: Some(512 * 1024 * 1024),
            _non_exhaustive: (),
        }
    }
}

impl Limits {
    /// Disable all limits.
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
                    error::LimitErrorKind::DimensionError,
                )));
            }
        }

        if let Some(max_height) = self.max_image_height {
            if height > max_height {
                return Err(ImageError::Limits(error::LimitError::from_kind(
                    error::LimitErrorKind::DimensionError,
                )));
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
                    error::LimitErrorKind::InsufficientMemory,
                )));
            }

            *max_alloc -= amount;
        }

        Ok(())
    }

    /// This function acts identically to [`reserve`], but takes a `usize` for convenience.
    pub fn reserve_usize(&mut self, amount: usize) -> ImageResult<()> {
        match u64::try_from(amount) {
            Ok(n) => self.reserve(n),
            Err(_) if self.max_alloc.is_some() => Err(ImageError::Limits(
                error::LimitError::from_kind(error::LimitErrorKind::InsufficientMemory),
            )),
            Err(_) => {
                // Out of bounds, but we weren't asked to consider any limit.
                Ok(())
            }
        }
    }

    /// This function increases the `max_alloc` limit with amount. Should only be used
    /// together with [`reserve`].
    ///
    /// [`reserve`]: #method.reserve
    pub fn free(&mut self, amount: u64) {
        if let Some(max_alloc) = self.max_alloc.as_mut() {
            *max_alloc = max_alloc.saturating_add(amount);
        }
    }

    /// This function acts identically to [`free`], but takes a `usize` for convenience.
    pub fn free_usize(&mut self, amount: usize) {
        match u64::try_from(amount) {
            Ok(n) => self.free(n),
            Err(_) if self.max_alloc.is_some() => {
                panic!("max_alloc is set, we should have exited earlier when the reserve failed");
            }
            Err(_) => {
                // Out of bounds, but we weren't asked to consider any limit.
            }
        }
    }
}
