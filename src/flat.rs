use std::cmp;
use std::marker::PhantomData;

use num_traits::Zero;

use buffer::Pixel;
use color::ColorType;
use image::{GenericImage, GenericImageView, ImageError};

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

    /// The number of channels in the color representation of the image.
    pub channels: u8,

    /// Add this to an index to get to the sample in the next channel.
    pub channel_stride: usize,

    /// The width of the represented image.
    pub width: u32,

    /// Add this to an index to get to the next sample in x-direction.
    pub width_stride: usize,

    /// The height of the represented image.
    pub height: u32,

    /// Add this to an index to get to the next sample in y-direction.
    pub height_stride: usize,
}

/// Helper struct for a (stride, length) pair.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Dim(usize, usize);

impl<Buffer> FlatSamples<Buffer> {
    /// Get the strides for indexing matrix-like [(h, w, c)].
    ///
    /// For a row-major layout with grouped samples, this tuple is strictly
    /// decreasing.
    pub fn strides_hwc(&self) -> (usize, usize, usize) {
        (self.height_stride, self.width_stride, self.channel_stride)
    }

    /// Get the dimensions (height, width, channels).
    ///
    /// Warning: width and height are swapped compared to 2D size methods such
    /// as `ImageBuffer::dimensions`. The interface is optimized for use with
    /// `strides_hwc` instead.
    pub fn extents(&self) -> (usize, usize, usize) {
        (self.height as usize, self.width as usize, self.channels as usize)
    }

    /// Get a reference based version.
    pub fn as_ref<T>(&self) -> FlatSamples<&[T]> where Buffer: AsRef<[T]> {
        // This initialization order is more beautiful <3
        FlatSamples {
            samples: self.samples.as_ref(),
            width_stride: self.width_stride,
            height_stride: self.height_stride,
            channel_stride: self.channel_stride,
            width: self.width,
            height: self.height,
            channels: self.channels,
        }
    }

    /// Get a mutable reference based version.
    pub fn as_mut<T>(&mut self) -> FlatSamples<&mut [T]> where Buffer: AsMut<[T]> {
        FlatSamples {
            samples: self.samples.as_mut(),
            width_stride: self.width_stride,
            height_stride: self.height_stride,
            channel_stride: self.channel_stride,
            width: self.width,
            height: self.height,
            channels: self.channels,
        }
    }

    /// View this buffer as an image over some type of samples.
    pub fn as_view<P>(&self) -> Result<View<&[P::Subpixel], P>, Error> 
        where P: Pixel, Buffer: AsRef<[P::Subpixel]>,
    {
        let as_ref = self.as_ref();

        // The length must be smaller than the maximum index. `usize::max_value()` is a safe
        // default value in case the maximum index calculation overflowed as there is no larger
        // length that could still fulfill this condition.
        if as_ref.samples.len() <= self.max_index().unwrap_or(usize::max_value()) {
            return Err(Error::TooLarge)
        }

        if self.channels != P::channel_count() {
            return Err(Error::WrongColor(P::color_type()))
        }

        Ok(View {
            inner: as_ref,
            phantom: PhantomData,
        })
    }

    /// Interpret this buffer as a mutable image.
    ///
    /// To succeed, the pixels in this buffer may not alias each other and the samples of each
    /// pixel must be packed (i.e. `channel_stride` is `1`).
    ///
    /// This is similar to an `ImageBuffer` except it is a temporary view that is not normalized as
    /// strongly. To get an owning version, consider copying the data into an `ImageBuffer`. This
    /// provides many more operations, is possibly faster (if not you may want to open an issue) is
    /// generally polished. You can also try to convert this buffer inline, see
    /// `ImageBuffer::from_raw`.
    pub fn as_view_mut<P>(&mut self) -> Result<ViewMut<&mut [P::Subpixel], P>, Error>
        where P: Pixel, Buffer: AsMut<[P::Subpixel]>,
    {
        if self.has_aliased_samples() {
            return Err(Error::NormalFormRequired(NormalForm::Unaliased))
        }

        if self.channel_stride != 1 {
            return Err(Error::NormalFormRequired(NormalForm::PixelPacked))
        }

        if self.channels != P::channel_count() {
            return Err(Error::WrongColor(P::color_type()))
        }

        let max_index = self.max_index().unwrap_or(usize::max_value());
        let as_mut = self.as_mut();

        if as_mut.samples.len() <= max_index {
            return Err(Error::TooLarge)
        }

        Ok(ViewMut {
            inner: as_mut,
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

    /// The extents of this array, in order of increasing strides.
    fn increasing_stride_dims(&self) -> [Dim; 3] {
        impl Dim {
            fn stride(self) -> usize {
                self.0
            }

            /// Length of this dimension in memory.
            fn checked_len(self) -> Option<usize> {
                self.0.checked_mul(self.1)
            }

            fn len(self) -> usize {
                self.0*self.1
            }
        }

        // Order extents by strides, then check that each is less equal than the next stride.
        let mut grouped: [Dim; 3] = [
            Dim(self.channel_stride, self.channels as usize),
            Dim(self.width_stride, self.width as usize),
            Dim(self.height_stride, self.height as usize)];

        grouped.sort();

        let [min_dim, mid_dim, max_dim] = grouped;
        assert!(min_dim.stride() <= mid_dim.stride() && mid_dim.stride() <= max_dim.stride());
        
        grouped
    }

    /// If there are any samples aliasing each other.
    ///
    /// If this is not the case, it would always be safe to allow mutable access to two different
    /// samples at the same time. Otherwise, this operation would need additional checks. When one
    /// dimension overflows `usize` with its stride we also consider this aliasing.
    pub fn has_aliased_samples(&self) -> bool {
        let [min_dim, mid_dim, max_dim] = self.increasing_stride_dims();

        let min_size = match min_dim.checked_len() {
            None => return true,
            Some(size) => size,
        };

        let mid_size = match mid_dim.checked_len() {
            None => return true,
            Some(size) => size,
        };

        let _max_size = match max_dim.checked_len() {
            None => return true,
            Some(_) => (), // Only want to know this didn't overflow.
        };

        // Each higher dimension must walk over all of one lower dimension.
        min_size > mid_dim.stride() || mid_size > max_dim.stride()
    }

    /// Check if a buffer fulfills the requirements of a normal form.
    ///
    /// Certain conversions have preconditions on the structure of the sample buffer that are not
    /// captured (by design) by the type system. These are then checked before the conversion. Such
    /// checks can all be done in constant time and will not inspect the buffer content. You can
    /// perform these checks yourself when the conversion is not required at this moment but maybe
    /// still performed later.
    pub fn is_normal(&self, form: NormalForm) -> bool {
        if self.has_aliased_samples() {
            return false;
        }

        if form >= NormalForm::PixelPacked && self.channel_stride != 1 {
            return false;
        }

        if form >= NormalForm::ImagePacked {
            // has aliased already checked for overflows.
            let [min_dim, mid_dim, max_dim] = self.increasing_stride_dims();

            if 1 != min_dim.stride() {
                return false;
            }

            if min_dim.len() != mid_dim.stride() {
                return false;
            }

            if  mid_dim.len() != max_dim.stride() {
                return false;
            }
        }

        if form >= NormalForm::RowMajorPacked {
            if self.width_stride != self.channels as usize {
                return false;
            }

            if self.width as usize*self.width_stride != self.height_stride {
                return false;
            }
        }

        if form >= NormalForm::ColumnMajorPacked {
            if self.height_stride != self.channels as usize {
                return false;
            }
            
            if self.height as usize*self.height_stride != self.width_stride {
                return false;
            }
        }

        return true;
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

        let idx_c = (channel as usize).checked_mul(self.channel_stride);
        let idx_x = (x as usize).checked_mul(self.width_stride);
        let idx_y = (y as usize).checked_mul(self.height_stride);

        let (idx_c, idx_x, idx_y) = match (idx_c, idx_x, idx_y) {
            (Some(idx_c), Some(idx_x), Some(idx_y)) => (idx_c, idx_x, idx_y),
            _ => return None,
        };

        Some(0usize)
            .and_then(|b| b.checked_add(idx_c))
            .and_then(|b| b.checked_add(idx_x))
            .and_then(|b| b.checked_add(idx_y))
    }

    /// Get an index provided it is inbouds.
    ///
    /// The computation can not overflow as we could represent the maximum coordinate.
    pub fn in_bounds_index(&self, x: u32, y: u32, c: u8) -> usize {
        let (y_stride, x_stride, c_stride) = self.strides_hwc();
        (y as usize * y_stride) + (x as usize * x_stride) + (c as usize * c_stride)
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

/// A mutable owning version of a flat buffer.
///
/// While this wraps a buffer similar to `ImageBuffer`, this is mostly intended as a utility. The
/// library endorsed normalized representation is still `ImageBuffer`. Also, the implementation of
/// `AsMut<[P::Subpixel]>` must always yield the same buffer. Therefore there is no public way to
/// construct this with an owning buffer.
#[derive(Clone, Debug)]
pub struct ViewMut<Buffer, P: Pixel> 
where 
    Buffer: AsMut<[P::Subpixel]> 
{
    inner: FlatSamples<Buffer>,
    phantom: PhantomData<P>,
}

/// Denotes invalid flat sample buffers when trying to convert to stricter types.
///
/// The biggest use case being `ImageBuffer` which expects closely packed
/// samples in a row major matrix representation. But this error type may be
/// resused for other import functions. A more versatile user may also try to
/// correct the underlying representation depending on the error variant.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Error {
    /// The represented image was too large.
    ///
    /// The optional value denotes a possibly accepted maximal bound.
    TooLarge,

    /// The represented image can not use this representation.
    ///
    /// The normalized form that would be accepted.
    NormalFormRequired(NormalForm),

    /// The color format did not match the channel count.
    ///
    /// In some cases you might be able to fix this by lowering the reported pixel count of the
    /// buffer without touching the strides.
    ///
    /// In very special circumstances you *may* do the opposite. This is **VERY** dangerous but not
    /// directly memory unsafe although that will likely alias pixels. One scenario is when you
    /// want to construct an `Rgba` image but have only 3 bytes per pixel and for some reason don't
    /// care about the value of the alpha channel even though you need `Rgba`.
    WrongColor(ColorType),
}

/// Different normal forms of buffers.
///
/// A normal form is an unaliased buffer with some additional constraints.  The `ÌmageBuffer` uses
/// row major form with packed samples. 
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum NormalForm {
    /// No pixel aliases another.
    ///
    /// Unaliased also guarantees that all index calculations in the image bounds using
    /// `dim_index*dim_stride` (such as `x*width_stride + y*height_stride`) do not overflow.
    Unaliased,

    /// At least pixels are packed.
    ///
    /// Images of these types can wrap `[T]`-slices into the standard color types. This is a
    /// precondition for `GenericImage` which requires by-reference access to pixels.
    PixelPacked,

    /// All samples are packed.
    ///
    /// This is orthogonal to `PixelPacked`. It requires that there are no holes in the image but
    /// it is not necessary that the pixel samples themselves are adjacent. An example of this
    /// behaviour is a planar image format.
    ImagePacked,

    /// The samples are in row-major form and all samples are packed.
    ///
    /// In addition to `PixelPacked` and `ImagePacked` this also asserts that the pixel matrix is
    /// in row-major form. 
    RowMajorPacked,

    /// The samples are in column-major form and all samples are packed.
    ///
    /// In addition to `PixelPacked` and `ImagePacked` this also asserts that the pixel matrix is
    /// in column-major form. 
    ColumnMajorPacked,
}

impl<Buffer, P: Pixel> GenericImageView for View<Buffer, P> 
    where Buffer: AsRef<[P::Subpixel]>
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
        let base_index = self.inner.in_bounds_index(x, y, 0);
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

impl<Buffer, P: Pixel> GenericImageView for ViewMut<Buffer, P> 
    where Buffer: AsMut<[P::Subpixel]> + AsRef<[P::Subpixel]>,
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
        let base_index = self.inner.in_bounds_index(x, y, 0);
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

impl<Buffer, P: Pixel> GenericImage for ViewMut<Buffer, P> 
    where Buffer: AsMut<[P::Subpixel]> + AsRef<[P::Subpixel]>,
{
    type InnerImage = Self;

    fn get_pixel_mut(&mut self, x: u32, y: u32) -> &mut Self::Pixel {
        if !self.inner.in_bounds(x, y, 0) {
            panic!("Image index {:?} out of bounds {:?}", (x, y), (self.inner.width, self.inner.height))
        }

        let base_index = self.inner.in_bounds_index(x, y, 0);
        let channel_count = <P as Pixel>::channel_count() as usize;
        let pixel_range = base_index..base_index + channel_count;
        P::from_slice_mut(&mut self.inner.samples.as_mut()[pixel_range])
    }

    fn put_pixel(&mut self, x: u32, y: u32, pixel: Self::Pixel) {
        *self.get_pixel_mut(x, y) = pixel;
    }

    fn blend_pixel(&mut self, x: u32, y: u32, pixel: Self::Pixel) {
        self.get_pixel_mut(x, y).blend(&pixel);
    }

    fn inner_mut(&mut self) -> &mut Self {
        self
    }
}

impl From<Error> for ImageError {
    fn from(error: Error) -> ImageError {
        match error {
            Error::TooLarge => ImageError::DimensionError,
            Error::WrongColor(color) => ImageError::UnsupportedColor(color),
            Error::NormalFormRequired(form) => ImageError::FormatError(
                format!("Required sample buffer in normal form {:?}", form)),
        }
    }
}

impl PartialOrd for NormalForm {
    /// Compares the logical preconditions.
    ///
    /// `a < b` if the normal form `a` has less preconditions than `b`.
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        match (*self, *other) {
            (NormalForm::Unaliased, NormalForm::Unaliased) => Some(cmp::Ordering::Equal),
            (NormalForm::PixelPacked, NormalForm::PixelPacked) => Some(cmp::Ordering::Equal),
            (NormalForm::ImagePacked, NormalForm::ImagePacked) => Some(cmp::Ordering::Equal),
            (NormalForm::RowMajorPacked, NormalForm::RowMajorPacked) => Some(cmp::Ordering::Equal),
            (NormalForm::ColumnMajorPacked, NormalForm::ColumnMajorPacked) => Some(cmp::Ordering::Equal),

            (NormalForm::Unaliased, _) => Some(cmp::Ordering::Less),
            (_, NormalForm::Unaliased) => Some(cmp::Ordering::Greater),

            (NormalForm::PixelPacked, NormalForm::ColumnMajorPacked) => Some(cmp::Ordering::Less),
            (NormalForm::PixelPacked, NormalForm::RowMajorPacked) => Some(cmp::Ordering::Less),
            (NormalForm::RowMajorPacked, NormalForm::PixelPacked) => Some(cmp::Ordering::Greater),
            (NormalForm::ColumnMajorPacked, NormalForm::PixelPacked) => Some(cmp::Ordering::Greater),

            (NormalForm::ImagePacked, NormalForm::ColumnMajorPacked) => Some(cmp::Ordering::Less),
            (NormalForm::ImagePacked, NormalForm::RowMajorPacked) => Some(cmp::Ordering::Less),
            (NormalForm::RowMajorPacked, NormalForm::ImagePacked) => Some(cmp::Ordering::Greater),
            (NormalForm::ColumnMajorPacked, NormalForm::ImagePacked) => Some(cmp::Ordering::Greater),

            (NormalForm::ImagePacked, NormalForm::PixelPacked) => None,
            (NormalForm::PixelPacked, NormalForm::ImagePacked) => None,
            (NormalForm::RowMajorPacked, NormalForm::ColumnMajorPacked) => None,
            (NormalForm::ColumnMajorPacked, NormalForm::RowMajorPacked) => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use color::{LumaA, Rgb};

    #[test]
    fn aliasing_view() {
       let buffer = FlatSamples {
           samples: &[42],
           channels: 3,
           channel_stride: 0,
           width: 100,
           width_stride: 0,
           height: 100,
           height_stride: 0,
       };

       let view = buffer.as_view::<Rgb<usize>>()
           .expect("This is a valid view");
       let pixel_count = view.pixels()
           .inspect(|pixel| assert!(pixel.2 == Rgb([42, 42, 42])))
           .count();
       assert_eq!(pixel_count, 100*100);
    }

    #[test]
    fn mutable_view() {
        let mut buffer = FlatSamples {
            samples: [0; 18],
            channels: 2,
            channel_stride: 1,
            width: 3,
            width_stride: 2,
            height: 3,
            height_stride: 6,
        };

        {
            let mut view = buffer.as_view_mut::<LumaA<usize>>()
                .expect("This should be a valid mutable buffer");
            #[allow(deprecated)]
            let pixel_count = view.pixels_mut()
                .enumerate()
                .map(|(idx, (_, _, pixel))| *pixel = LumaA([2*idx, 2*idx + 1]))
                .count();
            assert_eq!(pixel_count, 9);
        }

        buffer.samples.iter()
            .enumerate()
            .for_each(|(idx, sample)| assert_eq!(idx, *sample));
    }

    #[test]
    fn normal_forms() {
        assert!(FlatSamples {
            samples: [0u8; 0],
            channels: 2,
            channel_stride: 1,
            width: 3,
            width_stride: 9,
            height: 3,
            height_stride: 28,
        }.is_normal(NormalForm::PixelPacked));

        assert!(FlatSamples {
            samples: [0u8; 0],
            channels: 2,
            channel_stride: 8,
            width: 4,
            width_stride: 1,
            height: 2,
            height_stride: 4,
        }.is_normal(NormalForm::ImagePacked));

        assert!(FlatSamples {
            samples: [0u8; 0],
            channels: 2,
            channel_stride: 1,
            width: 4,
            width_stride: 2,
            height: 2,
            height_stride: 8,
        }.is_normal(NormalForm::RowMajorPacked));

        assert!(FlatSamples {
            samples: [0u8; 0],
            channels: 2,
            channel_stride: 1,
            width: 4,
            width_stride: 4,
            height: 2,
            height_stride: 2,
        }.is_normal(NormalForm::ColumnMajorPacked));
    }
}
