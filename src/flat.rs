use std::marker::PhantomData;

/// A flat buffer over a (multi channel) image.
///
/// Note that the strides need not conform to the assumption that constructed
/// indices actually refer inside the underlying buffer but return values
/// of library functions will always guarantee this. To manually make this
/// check use `check_index_validities` and maybe put that inside an assert.
pub struct FlatSamples<T, C: AsRef<[T]>> {
    /// Underlying linear container holding sample values.
    pub samples: C,

    /// Add this to an index to get to the next sample in x-direction.
    pub horizontal_stride: usize,

    /// Add this to an index to get to the next sample in y-direction.
    pub vertical_stride: usize,

    /// Add this to an index to get to the sample in the next channel.
    pub channel_stride: usize,

    /// The width of the represented image.
    pub width: u32,

    /// The height of the represented image.
    pub height: u32,

    /// The number of channels in the color representation of the image.
    pub channels: u8,

    /// Notes the sample type to the compiler.
    pub phantom: PhantomData<T>,
}

impl<T, C: AsRef<[T]>> FlatSamples<T, C> {
    /// Get the strides for indexing matrix-like [(h, w, c)].
    ///
    /// For a row-major layout with grouped samples, this tuple is strictly
    /// decreasing.
    pub fn strides_hwc(&self) -> (usize, usize, usize) {
        (self.horizontal_stride, self.vertical_stride, self.channel_stride)
    }

    /// Get the dimensions (height, width, channels).
    ///
    /// Warning: width and height are swapped compared to 2D size methods such
    /// as `ImageBuffer::dimensions`. The interface is optimized for use with
    /// `strides_hwc` instead.
    pub fn extents(&self) -> (usize, usize, usize) {
        (self.channels as usize, self.width as usize, self.height as usize)
    }

    /// Get a reference based version.
    pub fn as_ref(&self) -> FlatSamples<T, &[T]> {
        FlatSamples {
            samples: self.samples.as_ref(),
            horizontal_stride: self.horizontal_stride,
            vertical_stride: self.vertical_stride,
            channel_stride: self.channel_stride,
            width: self.width,
            height: self.height,
            channels: self.channels,
            phantom: PhantomData,
        }
    }

    /// View the samples as a slice.
    pub fn as_slice(&self) -> &[T] {
        self.samples.as_ref()
    }

    /// Check if every indexable pixel refers inside the samples.
    /// 
    /// This method will allow zero strides, allowing compact representations
    /// of monochrome images. To check that no aliasing occurs, try
    /// `check_alias_invariants` [WIP].
    pub fn has_valid_indexing(&self) -> bool {
        // Construct the maximum index, and test that.  Note that when one
        // dimensions is `0`, there is no maximum index so the check should
        // succeed.
        let max = self.index(
            self.width.saturating_sub(1),
            self.height.saturating_sub(1),
            self.channels.saturating_sub(1),
        );

        max < Some(self.as_slice().len())
    }

    /// If there are any samples aliasing each other.
    ///
    /// If this is not the case, it would always be safe to allow mutable access to two different
    /// samples at the same time. Otherwise, this operation would need additional checks. When one
    /// dimension overflows `usize` with its stride we also consider this aliasing.
    pub fn has_aliased_samples(&self) -> bool {
        // Order extents by strides, then check that each is less equal than the next stride.
        let strides = self.strides_hwc();
        let sizes = self.extents();
        let grouped: [(usize, usize); 3] = [
            (strides.0, sizes.0),
            (strides.1, sizes.1),
            (strides.2, sizes.2)];

        let min_dim = grouped[0].min(grouped[1]).min(grouped[2]);
        let max_dim = grouped[0].max(grouped[1]).max(grouped[2]);
        let mid_dim = (grouped[0].max(grouped[1]))
            .min(grouped[0].max(grouped[2]));

        let min_size = match min_dim.0.checked_mul(min_dim.1) {
            None => return true,
            Some(size) => size,
        };

        let mid_size = match mid_dim.0.checked_mul(mid_dim.1) {
            None => return true,
            Some(size) => size,
        };

        let _max_size = match max_dim.0.checked_mul(max_dim.1) {
            None => return true,
            Some(_) => (), // Only want to know this didn't overflow.
        };

        return min_size > mid_dim.0 || mid_size > max_dim.0;
    }

    /// Resolve the index of a particular sample.
    ///
    /// `None` if the index is outside the bounds or does not fit into a `usize`.
    pub fn index(&self, x: u32, y: u32, channel: u8) -> Option<usize> {
        let base = if x < self.width && y < self.height && channel < self.channels { 
            Some(0usize) 
        } else {
            None
        };

        let idx_x = (x as usize).checked_mul(self.vertical_stride);
        let idx_y = (y as usize).checked_mul(self.horizontal_stride);
        let idx_c = (channel as usize).checked_mul(self.channel_stride);

        base
            .and_then(|b| idx_x.and_then(|x| b.checked_add(x)))
            .and_then(|b| idx_y.and_then(|y| b.checked_add(y)))
            .and_then(|b| idx_c.and_then(|c| b.checked_add(c)))
    }
}

/// Denotes invalid flat sample buffers when trying to convert to stricter types.
///
/// The biggest use case being `ImageBuffer` which expects closely packed
/// samples in a row major matrix representation. But this error type may be
/// resused for other import functions. A more versatile user may also try to
/// correct the underlying representation depending on the error variant.
pub enum ImportError {
    /// The pixels may not alias to be used underlying the target, but they do.
    Aliasing,

    /// There may be no holes using the image strides.
    ///
    /// In other words, the target requires all strides to be strictly equal to the stride below
    /// them times the size in that dimension.
    Unpacked,

    /// The represented image was too large.
    ///
    /// The optional value denotes a possibly accepted maximal bound.
    TooLarge,

    /// The represented image can not use this representation.
    InvalidFormat {
        /// The normalized form that would be accepted.
        require: Option<NormalForm>,
    },
}

/// Different normal forms of buffers.
///
/// A normal form is an unaliased buffer with some additional constraints.  The `ÌmageBuffer` uses
/// row major form with packed samples.
pub enum NormalForm {
    /// No further constraints than no pixel aliases another.
    Unaliased,

    /// At least pixels are packed.
    ///
    /// Images of these types can wrap `[T]`-slices into the standard color types. This is a
    /// precondition for `GenericImage` which requires by-reference access to pixels.
    PixelPacked,

    /// The samples are in row-major form and all samples are packed.
    ///
    /// In addition to `PixelPacked` this also asserts that the pixel matrix is in row-major form
    /// and all rows and columns are also packed. Therefore, the number of elements in the
    /// underlying buffer is exactly `channels*width*height`.
    RowMajorPacked,
}
