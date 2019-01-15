use std::marker::PhantomData;

use num_traits::Zero;

use buffer::Pixel;
use image::GenericImageView;

/// A flat buffer over a (multi channel) image.
///
/// Note that the strides need not conform to the assumption that constructed
/// indices actually refer inside the underlying buffer but return values
/// of library functions will always guarantee this. To manually make this
/// check use `check_index_validities` and maybe put that inside an assert.
#[derive(Clone, Debug)]
pub struct FlatSamples<Buffer> {
    /// Underlying linear container holding sample values.
    pub samples: Buffer,

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
}

impl<Buffer> FlatSamples<Buffer> {
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
    pub fn as_ref<T>(&self) -> FlatSamples<&[T]> where Buffer: AsRef<[T]> {
        FlatSamples {
            samples: self.samples.as_ref(),
            horizontal_stride: self.horizontal_stride,
            vertical_stride: self.vertical_stride,
            channel_stride: self.channel_stride,
            width: self.width,
            height: self.height,
            channels: self.channels,
        }
    }

    /// View this buffer as an image over some type of samples.
    pub fn view<P>(self) -> Result<View<Buffer, P>, ViewError<Buffer>> 
        where P: Pixel, Buffer: AsRef<[P::Subpixel]>,
    {
        // The length must be smaller than the maximum index. `usize::max_value()` is a safe
        // default value in case the maximum index calculation overflowed as there is no larger
        // length that could still fulfill this condition.
        if self.samples.as_ref().len() <= self.max_index().unwrap_or(usize::max_value()) {
            return Err(ViewError::BufferTooSmall(self))
        }

        if self.channels != P::channel_count() {
            return Err(ViewError::WrongChannels(self))
        }

        Ok(View {
            inner: self,
            phantom: PhantomData,
        })
    }

    /// View the samples as a slice.
    pub fn as_slice<T>(&self) -> &[T] where Buffer: AsRef<[T]> {
        self.samples.as_ref()
    }

    /// Get the largest index of a sample in this image.
    /// 
    /// This method will allow zero strides, allowing compact representations of monochrome images.
    /// To check that no aliasing occurs, try `check_alias_invariants`.
    pub fn max_index(&self) -> Option<usize> {
        self.index(
            self.width.saturating_sub(1),
            self.height.saturating_sub(1),
            self.channels.saturating_sub(1),
        )
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

    /// Check that the pixel and the channel index are in bounds.
    pub fn in_bounds(&self, x: u32, y: u32, channel: u8) -> bool {
        return x < self.width && y < self.height && channel < self.channels
    }

    /// Resolve the index of a particular sample.
    ///
    /// `None` if the index is outside the bounds or does not fit into a `usize`.
    pub fn index(&self, x: u32, y: u32, channel: u8) -> Option<usize> {
        if !self.in_bounds(x, y, channel) {
            return None
        }

        let idx_x = (x as usize).checked_mul(self.vertical_stride);
        let idx_y = (y as usize).checked_mul(self.horizontal_stride);
        let idx_c = (channel as usize).checked_mul(self.channel_stride);

        Some(0usize)
            .and_then(|b| idx_x.and_then(|x| b.checked_add(x)))
            .and_then(|b| idx_y.and_then(|y| b.checked_add(y)))
            .and_then(|b| idx_c.and_then(|c| b.checked_add(c)))
    }
}

/// A flat buffer that can be used as an image view.
///
/// This is a nearly trivial wrapper around a buffer but at least sanitizes by checking the buffer
/// length first and constraining the pixel type.
///
/// Note that this does not eliminate panics as the `AsRef<[T]` implementation of `Buffer` may be
/// unreliable, i.e. return different buffers at different times. This of course is a non-issue for
/// all common collections where the bounds check once must be enough.
#[derive(Clone, Debug)]
pub struct View<Buffer, P: Pixel> 
where 
    Buffer: AsRef<[P::Subpixel]> 
{
    inner: FlatSamples<Buffer>,
    phantom: PhantomData<P>,
}

#[derive(Debug)]
pub enum ViewError<Buffer> {
    /// The buffer was smaller than the strides suggest.
    BufferTooSmall(FlatSamples<Buffer>),

    /// The channel count of the buffer and the pixel type differ.
    ///
    /// In some cases you might be able to fix this by lowering the reported pixel count of the
    /// buffer without touching the strides.
    ///
    /// In very special circumstances you *may* do the opposite. This is **VERY** dangerous but not
    /// directly memory unsafe although that will likely alias pixels. One scenario is when you
    /// want to construct an `Rgba` image but have only 3 bytes per pixel and for some reason don't
    /// care about the value of the alpha channel even though you need `Rgba`.
    WrongChannels(FlatSamples<Buffer>),
}

/// Denotes invalid flat sample buffers when trying to convert to stricter types.
///
/// The biggest use case being `ImageBuffer` which expects closely packed
/// samples in a row major matrix representation. But this error type may be
/// resused for other import functions. A more versatile user may also try to
/// correct the underlying representation depending on the error variant.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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

impl<Buffer, P> View<Buffer, P> 
where
    P: Pixel,
    Buffer: AsRef<[P::Subpixel]>,
{
    /// Get an index provided it is inbouds.
    ///
    /// The computation can not overflow as we could represent the maximum coordinate.
    fn in_bounds_index(&self, x: u32, y: u32, c: u8) -> usize {
        let (y_stride, x_stride, c_stride) = self.inner.strides_hwc();
        (y as usize * y_stride) + (x as usize * x_stride) + (c as usize * c_stride)
    }
}

impl<Buffer, P> GenericImageView for View<Buffer, P> 
where
    P: Pixel,
    Buffer: AsRef<[P::Subpixel]>,
{
    type Pixel = P;

    // We don't proxy an inner image.
    type InnerImageView = Self;

    fn dimensions(&self) -> (u32, u32) {
        (self.inner.width, self.inner.height)
    }

    fn bounds(&self) -> (u32, u32, u32, u32) {
        (0, self.inner.width, 0, self.inner.height)
    }

    fn in_bounds(&self, x: u32, y: u32) -> bool {
        let (w, h) = self.dimensions();
        x < w && y < h
    }

    fn get_pixel(&self, x: u32, y: u32) -> Self::Pixel {
        if !self.inner.in_bounds(x, y, 0) {
            panic!("Image index {:?} out of bounds {:?}", (x, y), (self.inner.width, self.inner.height))
        }

        let image = self.inner.samples.as_ref();
        let base_index = self.in_bounds_index(x, y, 0);
        let channels = P::channel_count() as usize;

        let mut buffer = [Zero::zero(); 256];
        buffer.iter_mut().enumerate().take(channels).for_each(|(c, to)| {
            let index = base_index + c*self.inner.channel_stride;
            *to = image[index];
        });

        P::from_slice(&buffer[..channels]).clone()
    }

    fn inner(&self) -> &Self {
        self // There is no other inner image.
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use color::Rgb;

    #[test]
    fn aliasing_view() {
       let buffer = FlatSamples {
           samples: &[42],
           horizontal_stride: 0,
           vertical_stride: 0,
           channel_stride: 0,
           width: 100,
           height: 100,
           channels: 3,
       };

       let view = buffer.view::<Rgb<usize>>()
           .expect("This is a valid view");
       let pixel_count = view.pixels()
           .inspect(|pixel| assert!(pixel.2 == Rgb([42, 42, 42])))
           .count();
       assert_eq!(pixel_count, 100*100);
    }
}
