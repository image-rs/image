use crate::{error, ColorType, ImageError, ImageResult};

use std::sync::{
    atomic::{AtomicU64, Ordering},
    Arc,
};

/// Set of supported strict limits for a decoder.
#[derive(Clone, Debug, Default, Eq, PartialEq, Hash)]
#[allow(missing_copy_implementations)]
#[non_exhaustive]
pub struct LimitSupport {}

/// Resource limits for decoding.
///
/// Limits can be either *strict* or *non-strict*. Non-strict limits are best-effort
/// limits where the library does not guarantee that limit will not be exceeded. Do note
/// that it is still considered a bug if a non-strict limit is exceeded.
/// Some of the underlying decoders do not support such limits, so one cannot
/// rely on these limits being supported. For strict limits, the library makes a stronger
/// guarantee that the limit will not be exceeded. Exceeding a strict limit is considered
/// a critical bug. If a decoder cannot guarantee that it will uphold a strict limit, it
/// *must* fail with [`error::LimitErrorKind::Unsupported`].
///
/// The only currently supported strict limits are the `max_image_width` and `max_image_height`
/// limits, but more will be added in the future. [`LimitSupport`] will default to support
/// being false, and decoders should enable support for the limits they support in
/// [`ImageDecoder::set_limits`].
///
/// The limit check should only ever fail if a limit will be exceeded or an unsupported strict
/// limit is used.
///
/// [`LimitSupport`]: ./struct.LimitSupport.html
/// [`ImageDecoder::set_limits`]: ../trait.ImageDecoder.html#method.set_limits
#[derive(Clone, Debug)]
#[allow(missing_copy_implementations)]
#[non_exhaustive]
pub struct Limits {
    /// The maximum allowed image width. This limit is strict. The default is no limit.
    pub max_image_width: Option<u32>,
    /// The maximum allowed image height. This limit is strict. The default is no limit.
    pub max_image_height: Option<u32>,
    /// The maximum allowed sum of allocations allocated by the decoder at any one time excluding
    /// allocator overhead. This limit is non-strict by default and some decoders may ignore it.
    /// The bytes required to store the output image count towards this value. The default is
    /// 512MiB.
    pub max_alloc: Option<u64>,
    /// A budget of allocations shared between all values from the same `Limits`.
    ///
    /// This can be cloned outside the library, too. For usage however consider _only_ atomically
    /// decreasing this value while it is shared. Also keep in mind that a `fetch_sub` uses
    /// overflowing semantics and may wrap to an absurdly high budget if you do not use it
    /// carefully with external synchronization. Prefer a `fetch_update` loop instead.
    ///
    /// Also this usage will result in a busy update loop. Try to fetch resources early and as
    /// large as you know you will need them to avoid contention. See [`Self::privatize`] and its
    /// implementation for reference.
    ///
    /// Accordingly, the local [`Self::max_alloc`] budget will be forfeit when the limits struct is
    /// dropped.
    pub shared_alloc: Option<Arc<AtomicU64>>,
}

/// Add some reasonable limits.
///
/// **Note**: This is not equivalent to _not_ adding limits. This may be changed in future major
/// version increases.
impl Default for Limits {
    fn default() -> Limits {
        Limits {
            max_image_width: None,
            max_image_height: None,
            max_alloc: Some(512 * 1024 * 1024),
            shared_alloc: None,
        }
    }
}

impl Limits {
    /// Disable all limits.
    #[must_use]
    pub fn no_limits() -> Limits {
        Limits {
            max_image_width: None,
            max_image_height: None,
            max_alloc: None,
            shared_alloc: None,
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
        // Try to grab any missing budget from the shared allocation.
        if self.shared_alloc.is_some() {
            if let Some(missing @ 1..) = self.max_alloc.and_then(|v| amount.checked_sub(v)) {
                // Ignore failure, we return the actual result below then.
                let _ = self.privatize(missing);
            }
        }

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
    ///
    /// [`reserve`]: #method.reserve
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

    /// This function acts identically to [`reserve`], but accepts the width, height and color type
    /// used to create an [`ImageBuffer`] and does all the math for you.
    ///
    /// [`ImageBuffer`]: crate::ImageBuffer
    /// [`reserve`]: #method.reserve
    pub fn reserve_buffer(
        &mut self,
        width: u32,
        height: u32,
        color_type: ColorType,
    ) -> ImageResult<()> {
        self.check_dimensions(width, height)?;
        let in_memory_size = u64::from(width)
            .saturating_mul(u64::from(height))
            .saturating_mul(color_type.bytes_per_pixel().into());
        self.reserve(in_memory_size)?;
        Ok(())
    }

    /// This function increases the `max_alloc` limit with amount. Should only be used
    /// together with [`reserve`].
    ///
    /// [`reserve`]: #method.reserve
    pub fn free(&mut self, amount: u64) {
        // We _never_ free into a shared budget, see its usage notes.
        if let Some(max_alloc) = self.max_alloc.as_mut() {
            *max_alloc = max_alloc.saturating_add(amount);
        }
    }

    /// This function acts identically to [`free`], but takes a `usize` for convenience.
    ///
    /// [`free`]: #method.free
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

    /// Attempt to take part of the share allocation into our private budget.
    ///
    /// Returns the available amount on error, however that value is sporadic and may have
    /// decreased by the time you try again from concurrent threads consuming the same shared
    /// budget.
    ///
    /// Please take a look into this function's implementation if you clone the reference counted
    /// [`Self::shared_alloc`] for yourself outside the limits struct.
    pub fn privatize(&mut self, amount: u64) -> Result<(), u64> {
        if amount == 0 {
            return Ok(());
        }

        // If we have no limit, we take what we want out of nowhere.
        let Some(private_free) = self.max_alloc else {
            return Ok(());
        };

        let Some(shared) = &self.shared_alloc else {
            // Nothing to grab from.
            return Err(0);
        };

        let _pre_update = shared.fetch_update(Ordering::Relaxed, Ordering::Relaxed, |current| {
            current.checked_sub(amount)
        })?;

        self.max_alloc = Some(private_free.saturating_add(amount));
        Ok(())
    }
}

#[test]
fn limits_reserve_free() {
    let mut limits = Limits {
        max_image_width: Some(100),
        max_image_height: Some(100),
        max_alloc: Some(1024),
        shared_alloc: None,
    };

    assert!(limits.reserve(512).is_ok());
    assert_eq!(limits.max_alloc, Some(512));

    assert!(limits.reserve(600).is_err());
    assert_eq!(limits.max_alloc, Some(512));

    limits.free(256);
    assert_eq!(limits.max_alloc, Some(768));
}

#[test]
fn shared_limits() {
    let shared = Arc::new(AtomicU64::new(1024));

    let mut limits_a = Limits {
        max_image_width: Some(100),
        max_image_height: Some(100),
        max_alloc: Some(512),
        shared_alloc: Some(shared.clone()),
    };

    let mut limits_b = Limits {
        max_image_width: Some(100),
        max_image_height: Some(100),
        max_alloc: Some(256),
        shared_alloc: Some(shared.clone()),
    };

    assert!(limits_a.reserve(512).is_ok());
    assert_eq!(limits_a.max_alloc, Some(0));
    assert_eq!(shared.load(Ordering::Relaxed), 1024);

    assert!(limits_a.reserve(1025).is_err());
    assert_eq!(limits_a.max_alloc, Some(0));
    assert_eq!(shared.load(Ordering::Relaxed), 1024);

    // b does not have enough itself but can grab from shared.
    assert!(limits_b.reserve(512).is_ok());
    assert_eq!(limits_b.max_alloc, Some(0));
    assert_eq!(shared.load(Ordering::Relaxed), 768);

    assert!(limits_b.reserve(1024).is_err());
    assert_eq!(limits_b.max_alloc, Some(0));
    assert_eq!(shared.load(Ordering::Relaxed), 768);
}
