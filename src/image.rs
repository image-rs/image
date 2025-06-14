use std::io;

use crate::color::{ColorType, ExtendedColorType};
use crate::error::{
    ImageError, ImageFormatHint, ImageResult, ParameterError, ParameterErrorKind, UnsupportedError,
    UnsupportedErrorKind,
};
use crate::math::Rect;
use crate::metadata::Orientation;
use crate::traits::Pixel;
use crate::SubImage;

use crate::animation::Frames;

// This struct manages buffering associated with implementing `Read` and `Seek` on decoders that can
// must decode ranges of bytes at a time.
#[allow(dead_code)]
// When no image formats that use it are enabled
pub(crate) struct ImageReadBuffer {
    scanline_bytes: usize,
    buffer: Vec<u8>,
    consumed: usize,

    total_bytes: u64,
    offset: u64,
}

impl ImageReadBuffer {
    /// Create a new `ImageReadBuffer`.
    ///
    /// Panics if `scanline_bytes` doesn't fit into a usize, because that would mean reading anything
    /// from the image would take more RAM than the entire virtual address space. In other words,
    /// actually using this struct would instantly OOM so just get it out of the way now.
    #[allow(dead_code)]
    // When no image formats that use it are enabled
    pub(crate) fn new(scanline_bytes: u64, total_bytes: u64) -> Self {
        Self {
            scanline_bytes: usize::try_from(scanline_bytes).unwrap(),
            buffer: Vec::new(),
            consumed: 0,
            total_bytes,
            offset: 0,
        }
    }

    #[allow(dead_code)]
    // When no image formats that use it are enabled
    pub(crate) fn read<F>(&mut self, buf: &mut [u8], mut read_scanline: F) -> io::Result<usize>
    where
        F: FnMut(&mut [u8]) -> io::Result<usize>,
    {
        if self.buffer.len() == self.consumed {
            if self.offset == self.total_bytes {
                return Ok(0);
            } else if buf.len() >= self.scanline_bytes {
                // If there is nothing buffered and the user requested a full scanline worth of
                // data, skip buffering.
                let bytes_read = read_scanline(&mut buf[..self.scanline_bytes])?;
                self.offset += u64::try_from(bytes_read).unwrap();
                return Ok(bytes_read);
            } else {
                // Lazily allocate buffer the first time that read is called with a buffer smaller
                // than the scanline size.
                if self.buffer.is_empty() {
                    self.buffer.resize(self.scanline_bytes, 0);
                }

                self.consumed = 0;
                let bytes_read = read_scanline(&mut self.buffer[..])?;
                self.buffer.resize(bytes_read, 0);
                self.offset += u64::try_from(bytes_read).unwrap();

                assert!(bytes_read == self.scanline_bytes || self.offset == self.total_bytes);
            }
        }

        // Finally, copy bytes into output buffer.
        let bytes_buffered = self.buffer.len() - self.consumed;
        if bytes_buffered > buf.len() {
            buf.copy_from_slice(&self.buffer[self.consumed..][..buf.len()]);
            self.consumed += buf.len();
            Ok(buf.len())
        } else {
            buf[..bytes_buffered].copy_from_slice(&self.buffer[self.consumed..][..bytes_buffered]);
            self.consumed = self.buffer.len();
            Ok(bytes_buffered)
        }
    }
}

/// The trait that all decoders implement
pub trait ImageDecoder {
    /// Returns a tuple containing the width and height of the image
    fn dimensions(&self) -> (u32, u32);

    /// Returns the color type of the image data produced by this decoder
    fn color_type(&self) -> ColorType;

    /// Returns the color type of the image file before decoding
    fn original_color_type(&self) -> ExtendedColorType {
        self.color_type().into()
    }

    /// Returns the ICC color profile embedded in the image, or `Ok(None)` if the image does not have one.
    ///
    /// For formats that don't support embedded profiles this function should always return `Ok(None)`.
    fn icc_profile(&mut self) -> ImageResult<Option<Vec<u8>>> {
        Ok(None)
    }

    /// Returns the raw [Exif](https://en.wikipedia.org/wiki/Exif) chunk, if it is present.
    /// A third-party crate such as [`kamadak-exif`](https://docs.rs/kamadak-exif/) is required to actually parse it.
    ///
    /// For formats that don't support embedded profiles this function should always return `Ok(None)`.
    fn exif_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        Ok(None)
    }

    /// Returns the orientation of the image.
    ///
    /// This is usually obtained from the Exif metadata, if present. Formats that don't support
    /// indicating orientation in their image metadata will return `Ok(Orientation::NoTransforms)`.
    fn orientation(&mut self) -> ImageResult<Orientation> {
        Ok(self
            .exif_metadata()?
            .and_then(|chunk| Orientation::from_exif_chunk(&chunk))
            .unwrap_or(Orientation::NoTransforms))
    }

    /// Returns the total number of bytes in the decoded image.
    ///
    /// This is the size of the buffer that must be passed to `read_image` or
    /// `read_image_with_progress`. The returned value may exceed `usize::MAX`, in
    /// which case it isn't actually possible to construct a buffer to decode all the image data
    /// into. If, however, the size does not fit in a u64 then `u64::MAX` is returned.
    fn total_bytes(&self) -> u64 {
        let dimensions = self.dimensions();
        let total_pixels = u64::from(dimensions.0) * u64::from(dimensions.1);
        let bytes_per_pixel = u64::from(self.color_type().bytes_per_pixel());
        total_pixels.saturating_mul(bytes_per_pixel)
    }

    /// Returns all the bytes in the image.
    ///
    /// This function takes a slice of bytes and writes the pixel data of the image into it.
    /// `buf` does not need to be aligned to any byte boundaries. However,
    /// alignment to 2 or 4 byte boundaries may result in small performance
    /// improvements for certain decoder implementations.
    ///
    /// The returned pixel data will always be in native endian. This allows
    /// `[u16]` and `[f32]` slices to be cast to `[u8]` and used for this method.
    ///
    /// # Panics
    ///
    /// This function panics if `buf.len() != self.total_bytes()`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use image::ImageDecoder;
    /// fn read_16bit_image(decoder: impl ImageDecoder) -> Vec<u16> {
    ///     let mut buf: Vec<u16> = vec![0; (decoder.total_bytes() / 2) as usize];
    ///     decoder.read_image(bytemuck::cast_slice_mut(&mut buf));
    ///     buf
    /// }
    /// ```
    fn read_image(self, buf: &mut [u8]) -> ImageResult<()>
    where
        Self: Sized;

    /// Set the decoder to have the specified limits. See [`Limits`] for the different kinds of
    /// limits that is possible to set.
    ///
    /// Note to implementors: make sure you call [`Limits::check_support`] so that
    /// decoding fails if any unsupported strict limits are set. Also make sure
    /// you call [`Limits::check_dimensions`] to check the `max_image_width` and
    /// `max_image_height` limits.
    ///
    /// [`Limits`]: ./io/struct.Limits.html
    /// [`Limits::check_support`]: ./io/struct.Limits.html#method.check_support
    /// [`Limits::check_dimensions`]: ./io/struct.Limits.html#method.check_dimensions
    fn set_limits(&mut self, limits: crate::Limits) -> ImageResult<()> {
        limits.check_support(&crate::LimitSupport::default())?;
        let (width, height) = self.dimensions();
        limits.check_dimensions(width, height)?;
        Ok(())
    }

    /// Use `read_image` instead; this method is an implementation detail needed so the trait can
    /// be object safe.
    ///
    /// Note to implementors: This method should be implemented by calling `read_image` on
    /// the boxed decoder...
    /// ```ignore
    /// fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()> {
    ///     (*self).read_image(buf)
    /// }
    /// ```
    fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()>;
}

impl<T: ?Sized + ImageDecoder> ImageDecoder for Box<T> {
    fn dimensions(&self) -> (u32, u32) {
        (**self).dimensions()
    }
    fn color_type(&self) -> ColorType {
        (**self).color_type()
    }
    fn original_color_type(&self) -> ExtendedColorType {
        (**self).original_color_type()
    }
    fn icc_profile(&mut self) -> ImageResult<Option<Vec<u8>>> {
        (**self).icc_profile()
    }
    fn exif_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        (**self).exif_metadata()
    }
    fn total_bytes(&self) -> u64 {
        (**self).total_bytes()
    }
    fn read_image(self, buf: &mut [u8]) -> ImageResult<()>
    where
        Self: Sized,
    {
        T::read_image_boxed(self, buf)
    }
    fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()> {
        T::read_image_boxed(*self, buf)
    }
    fn set_limits(&mut self, limits: crate::Limits) -> ImageResult<()> {
        (**self).set_limits(limits)
    }
}

/// Specialized image decoding not be supported by all formats
pub trait ImageDecoderRect: ImageDecoder {
    /// Decode a rectangular section of the image.
    ///
    /// This function takes a slice of bytes and writes the pixel data of the image into it.
    /// The rectangle is specified by the x and y coordinates of the top left corner, the width
    /// and height of the rectangle, and the row pitch of the buffer. The row pitch is the number
    /// of bytes between the start of one row and the start of the next row. The row pitch must be
    /// at least as large as the width of the rectangle in bytes.
    fn read_rect(
        &mut self,
        x: u32,
        y: u32,
        width: u32,
        height: u32,
        buf: &mut [u8],
        row_pitch: usize,
    ) -> ImageResult<()>;
}

/// `AnimationDecoder` trait
pub trait AnimationDecoder<'a> {
    /// Consume the decoder producing a series of frames.
    fn into_frames(self) -> Frames<'a>;
}

/// The trait all encoders implement
pub trait ImageEncoder {
    /// Writes all the bytes in an image to the encoder.
    ///
    /// This function takes a slice of bytes of the pixel data of the image
    /// and encodes them. Just like for [`ImageDecoder::read_image`], no particular
    /// alignment is required and data is expected to be in native endian.
    /// The implementation will reorder the endianness as necessary for the target encoding format.
    ///
    /// # Panics
    ///
    /// Panics if `width * height * color_type.bytes_per_pixel() != buf.len()`.
    fn write_image(
        self,
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ExtendedColorType,
    ) -> ImageResult<()>;

    /// Set the ICC profile to use for the image.
    ///
    /// This function is a no-op for formats that don't support ICC profiles.
    /// For formats that do support ICC profiles, the profile will be embedded
    /// in the image when it is saved.
    ///
    /// # Errors
    ///
    /// This function returns an error if the format does not support ICC profiles.
    fn set_icc_profile(&mut self, icc_profile: Vec<u8>) -> Result<(), UnsupportedError> {
        let _ = icc_profile;
        Err(UnsupportedError::from_format_and_kind(
            ImageFormatHint::Unknown,
            UnsupportedErrorKind::GenericFeature(
                "ICC profiles are not supported for this format".into(),
            ),
        ))
    }
}

/// Immutable pixel iterator
#[derive(Debug)]
pub struct Pixels<'a, I: ?Sized + 'a> {
    image: &'a I,
    x: u32,
    y: u32,
    width: u32,
    height: u32,
}

impl<I: GenericImageView> Iterator for Pixels<'_, I> {
    type Item = (u32, u32, I::Pixel);

    fn next(&mut self) -> Option<(u32, u32, I::Pixel)> {
        if self.x >= self.width {
            self.x = 0;
            self.y += 1;
        }

        if self.y >= self.height {
            None
        } else {
            let pixel = self.image.get_pixel(self.x, self.y);
            let p = (self.x, self.y, pixel);

            self.x += 1;

            Some(p)
        }
    }
}

impl<I: ?Sized> Clone for Pixels<'_, I> {
    fn clone(&self) -> Self {
        Pixels { ..*self }
    }
}

/// Trait to inspect an image.
///
/// ```
/// use image::{GenericImageView, Rgb, RgbImage};
///
/// let buffer = RgbImage::new(10, 10);
/// let image: &dyn GenericImageView<Pixel = Rgb<u8>> = &buffer;
/// ```
pub trait GenericImageView {
    /// The type of pixel.
    type Pixel: Pixel;

    /// The width and height of this image.
    fn dimensions(&self) -> (u32, u32);

    /// The width of this image.
    fn width(&self) -> u32 {
        let (w, _) = self.dimensions();
        w
    }

    /// The height of this image.
    fn height(&self) -> u32 {
        let (_, h) = self.dimensions();
        h
    }

    /// Returns true if this x, y coordinate is contained inside the image.
    fn in_bounds(&self, x: u32, y: u32) -> bool {
        let (width, height) = self.dimensions();
        x < width && y < height
    }

    /// Returns the pixel located at (x, y). Indexed from top left.
    ///
    /// # Panics
    ///
    /// Panics if `(x, y)` is out of bounds.
    fn get_pixel(&self, x: u32, y: u32) -> Self::Pixel;

    /// Returns the pixel located at (x, y). Indexed from top left.
    ///
    /// This function can be implemented in a way that ignores bounds checking.
    /// # Safety
    ///
    /// The coordinates must be [`in_bounds`] of the image.
    ///
    /// [`in_bounds`]: #method.in_bounds
    unsafe fn unsafe_get_pixel(&self, x: u32, y: u32) -> Self::Pixel {
        self.get_pixel(x, y)
    }

    /// Returns an Iterator over the pixels of this image.
    /// The iterator yields the coordinates of each pixel
    /// along with their value
    fn pixels(&self) -> Pixels<'_, Self>
    where
        Self: Sized,
    {
        let (width, height) = self.dimensions();

        Pixels {
            image: self,
            x: 0,
            y: 0,
            width,
            height,
        }
    }

    /// Returns a subimage that is an immutable view into this image.
    /// You can use [`GenericImage::sub_image`] if you need a mutable view instead.
    /// The coordinates set the position of the top left corner of the view.
    fn view(&self, x: u32, y: u32, width: u32, height: u32) -> SubImage<&Self>
    where
        Self: Sized,
    {
        assert!(u64::from(x) + u64::from(width) <= u64::from(self.width()));
        assert!(u64::from(y) + u64::from(height) <= u64::from(self.height()));
        SubImage::new(self, x, y, width, height)
    }
}

/// A trait for manipulating images.
pub trait GenericImage: GenericImageView {
    /// Gets a reference to the mutable pixel at location `(x, y)`. Indexed from top left.
    ///
    /// # Panics
    ///
    /// Panics if `(x, y)` is out of bounds.
    ///
    /// Panics for dynamic images (this method is deprecated and will be removed).
    ///
    /// ## Known issues
    ///
    /// This requires the buffer to contain a unique set of continuous channels in the exact order
    /// and byte representation that the pixel type requires. This is somewhat restrictive.
    ///
    /// TODO: Maybe use some kind of entry API? this would allow pixel type conversion on the fly
    /// while still doing only one array lookup:
    ///
    /// ```ignore
    /// let px = image.pixel_entry_at(x,y);
    /// px.set_from_rgba(rgba)
    /// ```
    #[deprecated(since = "0.24.0", note = "Use `get_pixel` and `put_pixel` instead.")]
    fn get_pixel_mut(&mut self, x: u32, y: u32) -> &mut Self::Pixel;

    /// Put a pixel at location (x, y). Indexed from top left.
    ///
    /// # Panics
    ///
    /// Panics if `(x, y)` is out of bounds.
    fn put_pixel(&mut self, x: u32, y: u32, pixel: Self::Pixel);

    /// Puts a pixel at location (x, y). Indexed from top left.
    ///
    /// This function can be implemented in a way that ignores bounds checking.
    /// # Safety
    ///
    /// The coordinates must be [`in_bounds`] of the image.
    ///
    /// [`in_bounds`]: traits.GenericImageView.html#method.in_bounds
    unsafe fn unsafe_put_pixel(&mut self, x: u32, y: u32, pixel: Self::Pixel) {
        self.put_pixel(x, y, pixel);
    }

    /// Put a pixel at location (x, y), taking into account alpha channels
    #[deprecated(
        since = "0.24.0",
        note = "Use iterator `pixels_mut` to blend the pixels directly"
    )]
    fn blend_pixel(&mut self, x: u32, y: u32, pixel: Self::Pixel);

    /// Copies all of the pixels from another image into this image.
    ///
    /// The other image is copied with the top-left corner of the
    /// other image placed at (x, y).
    ///
    /// In order to copy only a piece of the other image, use [`GenericImageView::view`].
    ///
    /// You can use [`FlatSamples`] to source pixels from an arbitrary regular raster of channel
    /// values, for example from a foreign interface or a fixed image.
    ///
    /// # Returns
    /// Returns an error if the image is too large to be copied at the given position
    ///
    /// [`GenericImageView::view`]: trait.GenericImageView.html#method.view
    /// [`FlatSamples`]: flat/struct.FlatSamples.html
    fn copy_from<O>(&mut self, other: &O, x: u32, y: u32) -> ImageResult<()>
    where
        O: GenericImageView<Pixel = Self::Pixel>,
    {
        // Do bounds checking here so we can use the non-bounds-checking
        // functions to copy pixels.
        if self.width() < other.width() + x || self.height() < other.height() + y {
            return Err(ImageError::Parameter(ParameterError::from_kind(
                ParameterErrorKind::DimensionMismatch,
            )));
        }

        for k in 0..other.height() {
            for i in 0..other.width() {
                let p = other.get_pixel(i, k);
                self.put_pixel(i + x, k + y, p);
            }
        }
        Ok(())
    }

    /// Copies all of the pixels from one part of this image to another part of this image.
    ///
    /// The destination rectangle of the copy is specified with the top-left corner placed at (x, y).
    ///
    /// # Returns
    /// `true` if the copy was successful, `false` if the image could not
    /// be copied due to size constraints.
    fn copy_within(&mut self, source: Rect, x: u32, y: u32) -> bool {
        let Rect {
            x: sx,
            y: sy,
            width,
            height,
        } = source;
        let dx = x;
        let dy = y;
        assert!(sx < self.width() && dx < self.width());
        assert!(sy < self.height() && dy < self.height());
        if self.width() - dx.max(sx) < width || self.height() - dy.max(sy) < height {
            return false;
        }
        // since `.rev()` creates a new dype we would either have to go with dynamic dispatch for the ranges
        // or have quite a lot of code bloat. A macro gives us static dispatch with less visible bloat.
        macro_rules! copy_within_impl_ {
            ($xiter:expr, $yiter:expr) => {
                for y in $yiter {
                    let sy = sy + y;
                    let dy = dy + y;
                    for x in $xiter {
                        let sx = sx + x;
                        let dx = dx + x;
                        let pixel = self.get_pixel(sx, sy);
                        self.put_pixel(dx, dy, pixel);
                    }
                }
            };
        }
        // check how target and source rectangles relate to each other so we dont overwrite data before we copied it.
        match (sx < dx, sy < dy) {
            (true, true) => copy_within_impl_!((0..width).rev(), (0..height).rev()),
            (true, false) => copy_within_impl_!((0..width).rev(), 0..height),
            (false, true) => copy_within_impl_!(0..width, (0..height).rev()),
            (false, false) => copy_within_impl_!(0..width, 0..height),
        }
        true
    }

    /// Returns a mutable subimage that is a view into this image.
    /// If you want an immutable subimage instead, use [`GenericImageView::view`]
    /// The coordinates set the position of the top left corner of the `SubImage`.
    fn sub_image(&mut self, x: u32, y: u32, width: u32, height: u32) -> SubImage<&mut Self>
    where
        Self: Sized,
    {
        assert!(u64::from(x) + u64::from(width) <= u64::from(self.width()));
        assert!(u64::from(y) + u64::from(height) <= u64::from(self.height()));
        SubImage::new(self, x, y, width, height)
    }
}

#[cfg(test)]
mod tests {
    use super::{ColorType, GenericImage, GenericImageView, ImageDecoder, ImageResult};

    use crate::color::Rgba;
    use crate::math::Rect;
    use crate::{GrayImage, ImageBuffer};

    #[test]
    #[allow(deprecated)]
    /// Test that alpha blending works as expected
    fn test_image_alpha_blending() {
        let mut target = ImageBuffer::new(1, 1);
        target.put_pixel(0, 0, Rgba([255u8, 0, 0, 255]));
        assert!(*target.get_pixel(0, 0) == Rgba([255, 0, 0, 255]));
        target.blend_pixel(0, 0, Rgba([0, 255, 0, 255]));
        assert!(*target.get_pixel(0, 0) == Rgba([0, 255, 0, 255]));

        // Blending an alpha channel onto a solid background
        target.blend_pixel(0, 0, Rgba([255, 0, 0, 127]));
        assert!(*target.get_pixel(0, 0) == Rgba([127, 127, 0, 255]));

        // Blending two alpha channels
        target.put_pixel(0, 0, Rgba([0, 255, 0, 127]));
        target.blend_pixel(0, 0, Rgba([255, 0, 0, 127]));
        assert!(*target.get_pixel(0, 0) == Rgba([169, 85, 0, 190]));
    }

    #[test]
    fn test_in_bounds() {
        let mut target = ImageBuffer::new(2, 2);
        target.put_pixel(0, 0, Rgba([255u8, 0, 0, 255]));

        assert!(target.in_bounds(0, 0));
        assert!(target.in_bounds(1, 0));
        assert!(target.in_bounds(0, 1));
        assert!(target.in_bounds(1, 1));

        assert!(!target.in_bounds(2, 0));
        assert!(!target.in_bounds(0, 2));
        assert!(!target.in_bounds(2, 2));
    }

    #[test]
    fn test_can_subimage_clone_nonmut() {
        let mut source = ImageBuffer::new(3, 3);
        source.put_pixel(1, 1, Rgba([255u8, 0, 0, 255]));

        // A non-mutable copy of the source image
        let source = source.clone();

        // Clone a view into non-mutable to a separate buffer
        let cloned = source.view(1, 1, 1, 1).to_image();

        assert!(cloned.get_pixel(0, 0) == source.get_pixel(1, 1));
    }

    #[test]
    fn test_can_nest_views() {
        let mut source = ImageBuffer::from_pixel(3, 3, Rgba([255u8, 0, 0, 255]));

        {
            let mut sub1 = source.sub_image(0, 0, 2, 2);
            let mut sub2 = sub1.sub_image(1, 1, 1, 1);
            sub2.put_pixel(0, 0, Rgba([0, 0, 0, 0]));
        }

        assert_eq!(*source.get_pixel(1, 1), Rgba([0, 0, 0, 0]));

        let view1 = source.view(0, 0, 2, 2);
        assert_eq!(*source.get_pixel(1, 1), view1.get_pixel(1, 1));

        let view2 = view1.view(1, 1, 1, 1);
        assert_eq!(*source.get_pixel(1, 1), view2.get_pixel(0, 0));
    }

    #[test]
    #[should_panic]
    fn test_view_out_of_bounds() {
        let source = ImageBuffer::from_pixel(3, 3, Rgba([255u8, 0, 0, 255]));
        source.view(1, 1, 3, 3);
    }

    #[test]
    #[should_panic]
    fn test_view_coordinates_out_of_bounds() {
        let source = ImageBuffer::from_pixel(3, 3, Rgba([255u8, 0, 0, 255]));
        source.view(3, 3, 3, 3);
    }

    #[test]
    #[should_panic]
    fn test_view_width_out_of_bounds() {
        let source = ImageBuffer::from_pixel(3, 3, Rgba([255u8, 0, 0, 255]));
        source.view(1, 1, 3, 2);
    }

    #[test]
    #[should_panic]
    fn test_view_height_out_of_bounds() {
        let source = ImageBuffer::from_pixel(3, 3, Rgba([255u8, 0, 0, 255]));
        source.view(1, 1, 2, 3);
    }

    #[test]
    #[should_panic]
    fn test_view_x_out_of_bounds() {
        let source = ImageBuffer::from_pixel(3, 3, Rgba([255u8, 0, 0, 255]));
        source.view(3, 1, 3, 3);
    }

    #[test]
    #[should_panic]
    fn test_view_y_out_of_bounds() {
        let source = ImageBuffer::from_pixel(3, 3, Rgba([255u8, 0, 0, 255]));
        source.view(1, 3, 3, 3);
    }

    #[test]
    fn test_view_in_bounds() {
        let source = ImageBuffer::from_pixel(3, 3, Rgba([255u8, 0, 0, 255]));
        source.view(0, 0, 3, 3);
        source.view(1, 1, 2, 2);
        source.view(2, 2, 0, 0);
    }

    #[test]
    fn test_copy_sub_image() {
        let source = ImageBuffer::from_pixel(3, 3, Rgba([255u8, 0, 0, 255]));
        let view = source.view(0, 0, 3, 3);
        let _view2 = view;
        view.to_image();
    }

    #[test]
    fn test_generic_image_copy_within_oob() {
        let mut image: GrayImage = ImageBuffer::from_raw(4, 4, vec![0u8; 16]).unwrap();
        assert!(!image.sub_image(0, 0, 4, 4).copy_within(
            Rect {
                x: 0,
                y: 0,
                width: 5,
                height: 4
            },
            0,
            0
        ));
        assert!(!image.sub_image(0, 0, 4, 4).copy_within(
            Rect {
                x: 0,
                y: 0,
                width: 4,
                height: 5
            },
            0,
            0
        ));
        assert!(!image.sub_image(0, 0, 4, 4).copy_within(
            Rect {
                x: 1,
                y: 0,
                width: 4,
                height: 4
            },
            0,
            0
        ));
        assert!(!image.sub_image(0, 0, 4, 4).copy_within(
            Rect {
                x: 0,
                y: 0,
                width: 4,
                height: 4
            },
            1,
            0
        ));
        assert!(!image.sub_image(0, 0, 4, 4).copy_within(
            Rect {
                x: 0,
                y: 1,
                width: 4,
                height: 4
            },
            0,
            0
        ));
        assert!(!image.sub_image(0, 0, 4, 4).copy_within(
            Rect {
                x: 0,
                y: 0,
                width: 4,
                height: 4
            },
            0,
            1
        ));
        assert!(!image.sub_image(0, 0, 4, 4).copy_within(
            Rect {
                x: 1,
                y: 1,
                width: 4,
                height: 4
            },
            0,
            0
        ));
    }

    #[test]
    fn test_generic_image_copy_within_tl() {
        let data = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15];
        let expected = [0, 1, 2, 3, 4, 0, 1, 2, 8, 4, 5, 6, 12, 8, 9, 10];
        let mut image: GrayImage = ImageBuffer::from_raw(4, 4, Vec::from(&data[..])).unwrap();
        assert!(image.sub_image(0, 0, 4, 4).copy_within(
            Rect {
                x: 0,
                y: 0,
                width: 3,
                height: 3
            },
            1,
            1
        ));
        assert_eq!(&image.into_raw(), &expected);
    }

    #[test]
    fn test_generic_image_copy_within_tr() {
        let data = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15];
        let expected = [0, 1, 2, 3, 1, 2, 3, 7, 5, 6, 7, 11, 9, 10, 11, 15];
        let mut image: GrayImage = ImageBuffer::from_raw(4, 4, Vec::from(&data[..])).unwrap();
        assert!(image.sub_image(0, 0, 4, 4).copy_within(
            Rect {
                x: 1,
                y: 0,
                width: 3,
                height: 3
            },
            0,
            1
        ));
        assert_eq!(&image.into_raw(), &expected);
    }

    #[test]
    fn test_generic_image_copy_within_bl() {
        let data = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15];
        let expected = [0, 4, 5, 6, 4, 8, 9, 10, 8, 12, 13, 14, 12, 13, 14, 15];
        let mut image: GrayImage = ImageBuffer::from_raw(4, 4, Vec::from(&data[..])).unwrap();
        assert!(image.sub_image(0, 0, 4, 4).copy_within(
            Rect {
                x: 0,
                y: 1,
                width: 3,
                height: 3
            },
            1,
            0
        ));
        assert_eq!(&image.into_raw(), &expected);
    }

    #[test]
    fn test_generic_image_copy_within_br() {
        let data = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15];
        let expected = [5, 6, 7, 3, 9, 10, 11, 7, 13, 14, 15, 11, 12, 13, 14, 15];
        let mut image: GrayImage = ImageBuffer::from_raw(4, 4, Vec::from(&data[..])).unwrap();
        assert!(image.sub_image(0, 0, 4, 4).copy_within(
            Rect {
                x: 1,
                y: 1,
                width: 3,
                height: 3
            },
            0,
            0
        ));
        assert_eq!(&image.into_raw(), &expected);
    }

    #[test]
    fn total_bytes_overflow() {
        struct D;
        impl ImageDecoder for D {
            fn color_type(&self) -> ColorType {
                ColorType::Rgb8
            }
            fn dimensions(&self) -> (u32, u32) {
                (0xffff_ffff, 0xffff_ffff)
            }
            fn read_image(self, _buf: &mut [u8]) -> ImageResult<()> {
                unimplemented!()
            }
            fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()> {
                (*self).read_image(buf)
            }
        }
        assert_eq!(D.total_bytes(), u64::MAX);

        let v: ImageResult<Vec<u8>> = crate::io::free_functions::decoder_to_vec(D);
        assert!(v.is_err());
    }
}
