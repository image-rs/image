//! Contains the generic `ImageBuffer` struct.
use num_traits::Zero;
use std::fmt;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut, Index, IndexMut, Range};
use std::path::Path;
use std::slice::{ChunksExact, ChunksExactMut};

use crate::color::{FromColor, Luma, LumaA, Rgb, Rgba};
use crate::dynimage::{save_buffer, save_buffer_with_format, write_buffer_with_format};
use crate::error::ImageResult;
use crate::flat::{FlatSamples, SampleLayout};
use crate::image::{GenericImage, GenericImageView, ImageFormat, ImageOutputFormat};
use crate::math::Rect;
use crate::traits::{EncodableLayout, Pixel, PixelWithColorType};
use crate::utils::expand_packed;

/// Iterate over pixel refs.
pub struct Pixels<'a, P: Pixel + 'a>
where
    P::Subpixel: 'a,
{
    chunks: ChunksExact<'a, P::Subpixel>,
}

impl<'a, P: Pixel + 'a> Iterator for Pixels<'a, P>
where
    P::Subpixel: 'a,
{
    type Item = &'a P;

    #[inline(always)]
    fn next(&mut self) -> Option<&'a P> {
        self.chunks.next().map(|v| <P as Pixel>::from_slice(v))
    }

    #[inline(always)]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }
}

impl<'a, P: Pixel + 'a> ExactSizeIterator for Pixels<'a, P>
where
    P::Subpixel: 'a,
{
    fn len(&self) -> usize {
        self.chunks.len()
    }
}

impl<'a, P: Pixel + 'a> DoubleEndedIterator for Pixels<'a, P>
where
    P::Subpixel: 'a,
{
    #[inline(always)]
    fn next_back(&mut self) -> Option<&'a P> {
        self.chunks.next_back().map(|v| <P as Pixel>::from_slice(v))
    }
}

impl<P: Pixel> Clone for Pixels<'_, P> {
    fn clone(&self) -> Self {
        Pixels {
            chunks: self.chunks.clone(),
        }
    }
}

impl<P: Pixel> fmt::Debug for Pixels<'_, P>
where
    P::Subpixel: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Pixels")
            .field("chunks", &self.chunks)
            .finish()
    }
}

/// Iterate over mutable pixel refs.
pub struct PixelsMut<'a, P: Pixel + 'a>
where
    P::Subpixel: 'a,
{
    chunks: ChunksExactMut<'a, P::Subpixel>,
}

impl<'a, P: Pixel + 'a> Iterator for PixelsMut<'a, P>
where
    P::Subpixel: 'a,
{
    type Item = &'a mut P;

    #[inline(always)]
    fn next(&mut self) -> Option<&'a mut P> {
        self.chunks.next().map(|v| <P as Pixel>::from_slice_mut(v))
    }

    #[inline(always)]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }
}

impl<'a, P: Pixel + 'a> ExactSizeIterator for PixelsMut<'a, P>
where
    P::Subpixel: 'a,
{
    fn len(&self) -> usize {
        self.chunks.len()
    }
}

impl<'a, P: Pixel + 'a> DoubleEndedIterator for PixelsMut<'a, P>
where
    P::Subpixel: 'a,
{
    #[inline(always)]
    fn next_back(&mut self) -> Option<&'a mut P> {
        self.chunks
            .next_back()
            .map(|v| <P as Pixel>::from_slice_mut(v))
    }
}

impl<P: Pixel> fmt::Debug for PixelsMut<'_, P>
where
    P::Subpixel: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("PixelsMut")
            .field("chunks", &self.chunks)
            .finish()
    }
}

/// Iterate over rows of an image
///
/// This iterator is created with [`ImageBuffer::rows`]. See its document for details.
///
/// [`ImageBuffer::rows`]: ../struct.ImageBuffer.html#method.rows
pub struct Rows<'a, P: Pixel + 'a>
where
    <P as Pixel>::Subpixel: 'a,
{
    pixels: ChunksExact<'a, P::Subpixel>,
}

impl<'a, P: Pixel + 'a> Rows<'a, P> {
    /// Construct the iterator from image pixels. This is not public since it has a (hidden) panic
    /// condition. The `pixels` slice must be large enough so that all pixels are addressable.
    fn with_image(pixels: &'a [P::Subpixel], width: u32, height: u32) -> Self {
        let row_len = (width as usize) * usize::from(<P as Pixel>::CHANNEL_COUNT);
        if row_len == 0 {
            Rows {
                pixels: [].chunks_exact(1),
            }
        } else {
            let pixels = pixels
                .get(..row_len * height as usize)
                .expect("Pixel buffer has too few subpixels");
            // Rows are physically present. In particular, height is smaller than `usize::MAX` as
            // all subpixels can be indexed.
            Rows {
                pixels: pixels.chunks_exact(row_len),
            }
        }
    }
}

impl<'a, P: Pixel + 'a> Iterator for Rows<'a, P>
where
    P::Subpixel: 'a,
{
    type Item = Pixels<'a, P>;

    #[inline(always)]
    fn next(&mut self) -> Option<Pixels<'a, P>> {
        let row = self.pixels.next()?;
        Some(Pixels {
            // Note: this is not reached when CHANNEL_COUNT is 0.
            chunks: row.chunks_exact(<P as Pixel>::CHANNEL_COUNT as usize),
        })
    }

    #[inline(always)]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }
}

impl<'a, P: Pixel + 'a> ExactSizeIterator for Rows<'a, P>
where
    P::Subpixel: 'a,
{
    fn len(&self) -> usize {
        self.pixels.len()
    }
}

impl<'a, P: Pixel + 'a> DoubleEndedIterator for Rows<'a, P>
where
    P::Subpixel: 'a,
{
    #[inline(always)]
    fn next_back(&mut self) -> Option<Pixels<'a, P>> {
        let row = self.pixels.next_back()?;
        Some(Pixels {
            // Note: this is not reached when CHANNEL_COUNT is 0.
            chunks: row.chunks_exact(<P as Pixel>::CHANNEL_COUNT as usize),
        })
    }
}

impl<P: Pixel> Clone for Rows<'_, P> {
    fn clone(&self) -> Self {
        Rows {
            pixels: self.pixels.clone(),
        }
    }
}

impl<P: Pixel> fmt::Debug for Rows<'_, P>
where
    P::Subpixel: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Rows")
            .field("pixels", &self.pixels)
            .finish()
    }
}

/// Iterate over mutable rows of an image
///
/// This iterator is created with [`ImageBuffer::rows_mut`]. See its document for details.
///
/// [`ImageBuffer::rows_mut`]: ../struct.ImageBuffer.html#method.rows_mut
pub struct RowsMut<'a, P: Pixel + 'a>
where
    <P as Pixel>::Subpixel: 'a,
{
    pixels: ChunksExactMut<'a, P::Subpixel>,
}

impl<'a, P: Pixel + 'a> RowsMut<'a, P> {
    /// Construct the iterator from image pixels. This is not public since it has a (hidden) panic
    /// condition. The `pixels` slice must be large enough so that all pixels are addressable.
    fn with_image(pixels: &'a mut [P::Subpixel], width: u32, height: u32) -> Self {
        let row_len = (width as usize) * usize::from(<P as Pixel>::CHANNEL_COUNT);
        if row_len == 0 {
            RowsMut {
                pixels: [].chunks_exact_mut(1),
            }
        } else {
            let pixels = pixels
                .get_mut(..row_len * height as usize)
                .expect("Pixel buffer has too few subpixels");
            // Rows are physically present. In particular, height is smaller than `usize::MAX` as
            // all subpixels can be indexed.
            RowsMut {
                pixels: pixels.chunks_exact_mut(row_len),
            }
        }
    }
}

impl<'a, P: Pixel + 'a> Iterator for RowsMut<'a, P>
where
    P::Subpixel: 'a,
{
    type Item = PixelsMut<'a, P>;

    #[inline(always)]
    fn next(&mut self) -> Option<PixelsMut<'a, P>> {
        let row = self.pixels.next()?;
        Some(PixelsMut {
            // Note: this is not reached when CHANNEL_COUNT is 0.
            chunks: row.chunks_exact_mut(<P as Pixel>::CHANNEL_COUNT as usize),
        })
    }

    #[inline(always)]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }
}

impl<'a, P: Pixel + 'a> ExactSizeIterator for RowsMut<'a, P>
where
    P::Subpixel: 'a,
{
    fn len(&self) -> usize {
        self.pixels.len()
    }
}

impl<'a, P: Pixel + 'a> DoubleEndedIterator for RowsMut<'a, P>
where
    P::Subpixel: 'a,
{
    #[inline(always)]
    fn next_back(&mut self) -> Option<PixelsMut<'a, P>> {
        let row = self.pixels.next_back()?;
        Some(PixelsMut {
            // Note: this is not reached when CHANNEL_COUNT is 0.
            chunks: row.chunks_exact_mut(<P as Pixel>::CHANNEL_COUNT as usize),
        })
    }
}

impl<P: Pixel> fmt::Debug for RowsMut<'_, P>
where
    P::Subpixel: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("RowsMut")
            .field("pixels", &self.pixels)
            .finish()
    }
}

/// Enumerate the pixels of an image.
pub struct EnumeratePixels<'a, P: Pixel + 'a>
where
    <P as Pixel>::Subpixel: 'a,
{
    pixels: Pixels<'a, P>,
    x: u32,
    y: u32,
    width: u32,
}

impl<'a, P: Pixel + 'a> Iterator for EnumeratePixels<'a, P>
where
    P::Subpixel: 'a,
{
    type Item = (u32, u32, &'a P);

    #[inline(always)]
    fn next(&mut self) -> Option<(u32, u32, &'a P)> {
        if self.x >= self.width {
            self.x = 0;
            self.y += 1;
        }
        let (x, y) = (self.x, self.y);
        self.x += 1;
        self.pixels.next().map(|p| (x, y, p))
    }

    #[inline(always)]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }
}

impl<'a, P: Pixel + 'a> ExactSizeIterator for EnumeratePixels<'a, P>
where
    P::Subpixel: 'a,
{
    fn len(&self) -> usize {
        self.pixels.len()
    }
}

impl<P: Pixel> Clone for EnumeratePixels<'_, P> {
    fn clone(&self) -> Self {
        EnumeratePixels {
            pixels: self.pixels.clone(),
            ..*self
        }
    }
}

impl<P: Pixel> fmt::Debug for EnumeratePixels<'_, P>
where
    P::Subpixel: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("EnumeratePixels")
            .field("pixels", &self.pixels)
            .field("x", &self.x)
            .field("y", &self.y)
            .field("width", &self.width)
            .finish()
    }
}

/// Enumerate the rows of an image.
pub struct EnumerateRows<'a, P: Pixel + 'a>
where
    <P as Pixel>::Subpixel: 'a,
{
    rows: Rows<'a, P>,
    y: u32,
    width: u32,
}

impl<'a, P: Pixel + 'a> Iterator for EnumerateRows<'a, P>
where
    P::Subpixel: 'a,
{
    type Item = (u32, EnumeratePixels<'a, P>);

    #[inline(always)]
    fn next(&mut self) -> Option<(u32, EnumeratePixels<'a, P>)> {
        let y = self.y;
        self.y += 1;
        self.rows.next().map(|r| {
            (
                y,
                EnumeratePixels {
                    x: 0,
                    y,
                    width: self.width,
                    pixels: r,
                },
            )
        })
    }

    #[inline(always)]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }
}

impl<'a, P: Pixel + 'a> ExactSizeIterator for EnumerateRows<'a, P>
where
    P::Subpixel: 'a,
{
    fn len(&self) -> usize {
        self.rows.len()
    }
}

impl<P: Pixel> Clone for EnumerateRows<'_, P> {
    fn clone(&self) -> Self {
        EnumerateRows {
            rows: self.rows.clone(),
            ..*self
        }
    }
}

impl<P: Pixel> fmt::Debug for EnumerateRows<'_, P>
where
    P::Subpixel: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("EnumerateRows")
            .field("rows", &self.rows)
            .field("y", &self.y)
            .field("width", &self.width)
            .finish()
    }
}

/// Enumerate the pixels of an image.
pub struct EnumeratePixelsMut<'a, P: Pixel + 'a>
where
    <P as Pixel>::Subpixel: 'a,
{
    pixels: PixelsMut<'a, P>,
    x: u32,
    y: u32,
    width: u32,
}

impl<'a, P: Pixel + 'a> Iterator for EnumeratePixelsMut<'a, P>
where
    P::Subpixel: 'a,
{
    type Item = (u32, u32, &'a mut P);

    #[inline(always)]
    fn next(&mut self) -> Option<(u32, u32, &'a mut P)> {
        if self.x >= self.width {
            self.x = 0;
            self.y += 1;
        }
        let (x, y) = (self.x, self.y);
        self.x += 1;
        self.pixels.next().map(|p| (x, y, p))
    }

    #[inline(always)]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }
}

impl<'a, P: Pixel + 'a> ExactSizeIterator for EnumeratePixelsMut<'a, P>
where
    P::Subpixel: 'a,
{
    fn len(&self) -> usize {
        self.pixels.len()
    }
}

impl<P: Pixel> fmt::Debug for EnumeratePixelsMut<'_, P>
where
    P::Subpixel: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("EnumeratePixelsMut")
            .field("pixels", &self.pixels)
            .field("x", &self.x)
            .field("y", &self.y)
            .field("width", &self.width)
            .finish()
    }
}

/// Enumerate the rows of an image.
pub struct EnumerateRowsMut<'a, P: Pixel + 'a>
where
    <P as Pixel>::Subpixel: 'a,
{
    rows: RowsMut<'a, P>,
    y: u32,
    width: u32,
}

impl<'a, P: Pixel + 'a> Iterator for EnumerateRowsMut<'a, P>
where
    P::Subpixel: 'a,
{
    type Item = (u32, EnumeratePixelsMut<'a, P>);

    #[inline(always)]
    fn next(&mut self) -> Option<(u32, EnumeratePixelsMut<'a, P>)> {
        let y = self.y;
        self.y += 1;
        self.rows.next().map(|r| {
            (
                y,
                EnumeratePixelsMut {
                    x: 0,
                    y,
                    width: self.width,
                    pixels: r,
                },
            )
        })
    }

    #[inline(always)]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }
}

impl<'a, P: Pixel + 'a> ExactSizeIterator for EnumerateRowsMut<'a, P>
where
    P::Subpixel: 'a,
{
    fn len(&self) -> usize {
        self.rows.len()
    }
}

impl<P: Pixel> fmt::Debug for EnumerateRowsMut<'_, P>
where
    P::Subpixel: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("EnumerateRowsMut")
            .field("rows", &self.rows)
            .field("y", &self.y)
            .field("width", &self.width)
            .finish()
    }
}

/// Generic image buffer
///
/// This is an image parameterised by its Pixel types, represented by a width and height and a
/// container of channel data. It provides direct access to its pixels and implements the
/// [`GenericImageView`] and [`GenericImage`] traits. In many ways, this is the standard buffer
/// implementing those traits. Using this concrete type instead of a generic type parameter has
/// been shown to improve performance.
///
/// The crate defines a few type aliases with regularly used pixel types for your convenience, such
/// as [`RgbImage`], [`GrayImage`] etc.
///
/// [`GenericImage`]: trait.GenericImage.html
/// [`GenericImageView`]: trait.GenericImageView.html
/// [`RgbImage`]: type.RgbImage.html
/// [`GrayImage`]: type.GrayImage.html
///
/// To convert between images of different Pixel types use [`DynamicImage`].
///
/// You can retrieve a complete description of the buffer's layout and contents through
/// [`as_flat_samples`] and [`as_flat_samples_mut`]. This can be handy to also use the contents in
/// a foreign language, map it as a GPU host buffer or other similar tasks.
///
/// [`DynamicImage`]: enum.DynamicImage.html
/// [`as_flat_samples`]: #method.as_flat_samples
/// [`as_flat_samples_mut`]: #method.as_flat_samples_mut
///
/// ## Examples
///
/// Create a simple canvas and paint a small cross.
///
/// ```
/// use image::{RgbImage, Rgb};
///
/// let mut img = RgbImage::new(32, 32);
///
/// for x in 15..=17 {
///     for y in 8..24 {
///         img.put_pixel(x, y, Rgb([255, 0, 0]));
///         img.put_pixel(y, x, Rgb([255, 0, 0]));
///     }
/// }
/// ```
///
/// Overlays an image on top of a larger background raster.
///
/// ```no_run
/// use image::{GenericImage, GenericImageView, ImageBuffer, open};
///
/// let on_top = open("path/to/some.png").unwrap().into_rgb8();
/// let mut img = ImageBuffer::from_fn(512, 512, |x, y| {
///     if (x + y) % 2 == 0 {
///         image::Rgb([0, 0, 0])
///     } else {
///         image::Rgb([255, 255, 255])
///     }
/// });
///
/// image::imageops::overlay(&mut img, &on_top, 128, 128);
/// ```
///
/// Convert an RgbaImage to a GrayImage.
///
/// ```no_run
/// use image::{open, DynamicImage};
///
/// let rgba = open("path/to/some.png").unwrap().into_rgba8();
/// let gray = DynamicImage::ImageRgba8(rgba).into_luma8();
/// ```
#[derive(Debug, Hash, PartialEq, Eq)]
pub struct ImageBuffer<P: Pixel, Container> {
    width: u32,
    height: u32,
    _phantom: PhantomData<P>,
    data: Container,
}

// generic implementation, shared along all image buffers
impl<P, Container> ImageBuffer<P, Container>
where
    P: Pixel,
    Container: Deref<Target = [P::Subpixel]>,
{
    /// Constructs a buffer from a generic container
    /// (for example a `Vec` or a slice)
    ///
    /// Returns `None` if the container is not big enough (including when the image dimensions
    /// necessitate an allocation of more bytes than supported by the container).
    pub fn from_raw(width: u32, height: u32, buf: Container) -> Option<ImageBuffer<P, Container>> {
        if Self::check_image_fits(width, height, buf.len()) {
            Some(ImageBuffer {
                data: buf,
                width,
                height,
                _phantom: PhantomData,
            })
        } else {
            None
        }
    }

    /// Returns the underlying raw buffer
    pub fn into_raw(self) -> Container {
        self.data
    }

    /// Returns the underlying raw buffer
    pub fn as_raw(&self) -> &Container {
        &self.data
    }

    /// The width and height of this image.
    pub fn dimensions(&self) -> (u32, u32) {
        (self.width, self.height)
    }

    /// The width of this image.
    pub fn width(&self) -> u32 {
        self.width
    }

    /// The height of this image.
    pub fn height(&self) -> u32 {
        self.height
    }

    // TODO: choose name under which to expose.
    pub(crate) fn inner_pixels(&self) -> &[P::Subpixel] {
        let len = Self::image_buffer_len(self.width, self.height).unwrap();
        &self.data[..len]
    }

    /// Returns an iterator over the pixels of this image.
    /// The iteration order is x = 0 to width then y = 0 to height
    pub fn pixels(&self) -> Pixels<P> {
        Pixels {
            chunks: self
                .inner_pixels()
                .chunks_exact(<P as Pixel>::CHANNEL_COUNT as usize),
        }
    }

    /// Returns an iterator over the rows of this image.
    ///
    /// Only non-empty rows can be iterated in this manner. In particular the iterator will not
    /// yield any item when the width of the image is `0` or a pixel type without any channels is
    /// used. This ensures that its length can always be represented by `usize`.
    pub fn rows(&self) -> Rows<P> {
        Rows::with_image(&self.data, self.width, self.height)
    }

    /// Enumerates over the pixels of the image.
    /// The iterator yields the coordinates of each pixel
    /// along with a reference to them.
    /// The iteration order is x = 0 to width then y = 0 to height
    /// Starting from the top left.
    pub fn enumerate_pixels(&self) -> EnumeratePixels<P> {
        EnumeratePixels {
            pixels: self.pixels(),
            x: 0,
            y: 0,
            width: self.width,
        }
    }

    /// Enumerates over the rows of the image.
    /// The iterator yields the y-coordinate of each row
    /// along with a reference to them.
    pub fn enumerate_rows(&self) -> EnumerateRows<P> {
        EnumerateRows {
            rows: self.rows(),
            y: 0,
            width: self.width,
        }
    }

    /// Gets a reference to the pixel at location `(x, y)`
    ///
    /// # Panics
    ///
    /// Panics if `(x, y)` is out of the bounds `(width, height)`.
    #[inline]
    pub fn get_pixel(&self, x: u32, y: u32) -> &P {
        match self.pixel_indices(x, y) {
            None => panic!(
                "Image index {:?} out of bounds {:?}",
                (x, y),
                (self.width, self.height)
            ),
            Some(pixel_indices) => <P as Pixel>::from_slice(&self.data[pixel_indices]),
        }
    }

    /// Gets a reference to the pixel at location `(x, y)` or returns `None` if
    /// the index is out of the bounds `(width, height)`.
    pub fn get_pixel_checked(&self, x: u32, y: u32) -> Option<&P> {
        if x >= self.width {
            return None;
        }
        let num_channels = <P as Pixel>::CHANNEL_COUNT as usize;
        let i = (y as usize)
            .saturating_mul(self.width as usize)
            .saturating_add(x as usize)
            .saturating_mul(num_channels);

        self.data
            .get(i..i + num_channels)
            .map(|pixel_indices| <P as Pixel>::from_slice(pixel_indices))
    }

    /// Test that the image fits inside the buffer.
    ///
    /// Verifies that the maximum image of pixels inside the bounds is smaller than the provided
    /// length. Note that as a corrolary we also have that the index calculation of pixels inside
    /// the bounds will not overflow.
    fn check_image_fits(width: u32, height: u32, len: usize) -> bool {
        let checked_len = Self::image_buffer_len(width, height);
        checked_len.map(|min_len| min_len <= len).unwrap_or(false)
    }

    fn image_buffer_len(width: u32, height: u32) -> Option<usize> {
        Some(<P as Pixel>::CHANNEL_COUNT as usize)
            .and_then(|size| size.checked_mul(width as usize))
            .and_then(|size| size.checked_mul(height as usize))
    }

    #[inline(always)]
    fn pixel_indices(&self, x: u32, y: u32) -> Option<Range<usize>> {
        if x >= self.width || y >= self.height {
            return None;
        }

        Some(self.pixel_indices_unchecked(x, y))
    }

    #[inline(always)]
    fn pixel_indices_unchecked(&self, x: u32, y: u32) -> Range<usize> {
        let no_channels = <P as Pixel>::CHANNEL_COUNT as usize;
        // If in bounds, this can't overflow as we have tested that at construction!
        let min_index = (y as usize * self.width as usize + x as usize) * no_channels;
        min_index..min_index + no_channels
    }

    /// Get the format of the buffer when viewed as a matrix of samples.
    pub fn sample_layout(&self) -> SampleLayout {
        // None of these can overflow, as all our memory is addressable.
        SampleLayout::row_major_packed(<P as Pixel>::CHANNEL_COUNT, self.width, self.height)
    }

    /// Return the raw sample buffer with its stride an dimension information.
    ///
    /// The returned buffer is guaranteed to be well formed in all cases. It is laid out by
    /// colors, width then height, meaning `channel_stride <= width_stride <= height_stride`. All
    /// strides are in numbers of elements but those are mostly `u8` in which case the strides are
    /// also byte strides.
    pub fn into_flat_samples(self) -> FlatSamples<Container>
    where
        Container: AsRef<[P::Subpixel]>,
    {
        // None of these can overflow, as all our memory is addressable.
        let layout = self.sample_layout();
        FlatSamples {
            samples: self.data,
            layout,
            color_hint: None, // TODO: the pixel type might contain P::COLOR_TYPE if it satisfies PixelWithColorType
        }
    }

    /// Return a view on the raw sample buffer.
    ///
    /// See [`into_flat_samples`](#method.into_flat_samples) for more details.
    pub fn as_flat_samples(&self) -> FlatSamples<&[P::Subpixel]>
    where
        Container: AsRef<[P::Subpixel]>,
    {
        let layout = self.sample_layout();
        FlatSamples {
            samples: self.data.as_ref(),
            layout,
            color_hint: None, // TODO: the pixel type might contain P::COLOR_TYPE if it satisfies PixelWithColorType
        }
    }

    /// Return a mutable view on the raw sample buffer.
    ///
    /// See [`into_flat_samples`](#method.into_flat_samples) for more details.
    pub fn as_flat_samples_mut(&mut self) -> FlatSamples<&mut [P::Subpixel]>
    where
        Container: AsMut<[P::Subpixel]>,
    {
        let layout = self.sample_layout();
        FlatSamples {
            samples: self.data.as_mut(),
            layout,
            color_hint: None, // TODO: the pixel type might contain P::COLOR_TYPE if it satisfies PixelWithColorType
        }
    }
}

impl<P, Container> ImageBuffer<P, Container>
where
    P: Pixel,
    Container: Deref<Target = [P::Subpixel]> + DerefMut,
{
    // TODO: choose name under which to expose.
    fn inner_pixels_mut(&mut self) -> &mut [P::Subpixel] {
        let len = Self::image_buffer_len(self.width, self.height).unwrap();
        &mut self.data[..len]
    }

    /// Returns an iterator over the mutable pixels of this image.
    pub fn pixels_mut(&mut self) -> PixelsMut<P> {
        PixelsMut {
            chunks: self
                .inner_pixels_mut()
                .chunks_exact_mut(<P as Pixel>::CHANNEL_COUNT as usize),
        }
    }

    /// Returns an iterator over the mutable rows of this image.
    ///
    /// Only non-empty rows can be iterated in this manner. In particular the iterator will not
    /// yield any item when the width of the image is `0` or a pixel type without any channels is
    /// used. This ensures that its length can always be represented by `usize`.
    pub fn rows_mut(&mut self) -> RowsMut<P> {
        RowsMut::with_image(&mut self.data, self.width, self.height)
    }

    /// Enumerates over the pixels of the image.
    /// The iterator yields the coordinates of each pixel
    /// along with a mutable reference to them.
    pub fn enumerate_pixels_mut(&mut self) -> EnumeratePixelsMut<P> {
        let width = self.width;
        EnumeratePixelsMut {
            pixels: self.pixels_mut(),
            x: 0,
            y: 0,
            width,
        }
    }

    /// Enumerates over the rows of the image.
    /// The iterator yields the y-coordinate of each row
    /// along with a mutable reference to them.
    pub fn enumerate_rows_mut(&mut self) -> EnumerateRowsMut<P> {
        let width = self.width;
        EnumerateRowsMut {
            rows: self.rows_mut(),
            y: 0,
            width,
        }
    }

    /// Gets a reference to the mutable pixel at location `(x, y)`
    ///
    /// # Panics
    ///
    /// Panics if `(x, y)` is out of the bounds `(width, height)`.
    #[inline]
    pub fn get_pixel_mut(&mut self, x: u32, y: u32) -> &mut P {
        match self.pixel_indices(x, y) {
            None => panic!(
                "Image index {:?} out of bounds {:?}",
                (x, y),
                (self.width, self.height)
            ),
            Some(pixel_indices) => <P as Pixel>::from_slice_mut(&mut self.data[pixel_indices]),
        }
    }

    /// Gets a reference to the mutable pixel at location `(x, y)` or returns
    /// `None` if the index is out of the bounds `(width, height)`.
    pub fn get_pixel_mut_checked(&mut self, x: u32, y: u32) -> Option<&mut P> {
        if x >= self.width {
            return None;
        }
        let num_channels = <P as Pixel>::CHANNEL_COUNT as usize;
        let i = (y as usize)
            .saturating_mul(self.width as usize)
            .saturating_add(x as usize)
            .saturating_mul(num_channels);

        self.data
            .get_mut(i..i + num_channels)
            .map(|pixel_indices| <P as Pixel>::from_slice_mut(pixel_indices))
    }

    /// Puts a pixel at location `(x, y)`
    ///
    /// # Panics
    ///
    /// Panics if `(x, y)` is out of the bounds `(width, height)`.
    #[inline]
    pub fn put_pixel(&mut self, x: u32, y: u32, pixel: P) {
        *self.get_pixel_mut(x, y) = pixel
    }
}

impl<P, Container> ImageBuffer<P, Container>
where
    P: Pixel,
    [P::Subpixel]: EncodableLayout,
    Container: Deref<Target = [P::Subpixel]>,
{
    /// Saves the buffer to a file at the path specified.
    ///
    /// The image format is derived from the file extension.
    pub fn save<Q>(&self, path: Q) -> ImageResult<()>
    where
        Q: AsRef<Path>,
        P: PixelWithColorType,
    {
        save_buffer(
            path,
            self.inner_pixels().as_bytes(),
            self.width(),
            self.height(),
            <P as PixelWithColorType>::COLOR_TYPE,
        )
    }
}

impl<P, Container> ImageBuffer<P, Container>
where
    P: Pixel,
    [P::Subpixel]: EncodableLayout,
    Container: Deref<Target = [P::Subpixel]>,
{
    /// Saves the buffer to a file at the specified path in
    /// the specified format.
    ///
    /// See [`save_buffer_with_format`](fn.save_buffer_with_format.html) for
    /// supported types.
    pub fn save_with_format<Q>(&self, path: Q, format: ImageFormat) -> ImageResult<()>
    where
        Q: AsRef<Path>,
        P: PixelWithColorType,
    {
        // This is valid as the subpixel is u8.
        save_buffer_with_format(
            path,
            self.inner_pixels().as_bytes(),
            self.width(),
            self.height(),
            <P as PixelWithColorType>::COLOR_TYPE,
            format,
        )
    }
}

impl<P, Container> ImageBuffer<P, Container>
where
    P: Pixel,
    [P::Subpixel]: EncodableLayout,
    Container: Deref<Target = [P::Subpixel]>,
{
    /// Writes the buffer to a writer in the specified format.
    ///
    /// Assumes the writer is buffered. In most cases,
    /// you should wrap your writer in a `BufWriter` for best performance.
    ///
    /// See [`ImageOutputFormat`](enum.ImageOutputFormat.html) for
    /// supported types.
    pub fn write_to<W, F>(&self, writer: &mut W, format: F) -> ImageResult<()>
    where
        W: std::io::Write + std::io::Seek,
        F: Into<ImageOutputFormat>,
        P: PixelWithColorType,
    {
        // This is valid as the subpixel is u8.
        write_buffer_with_format(
            writer,
            self.inner_pixels().as_bytes(),
            self.width(),
            self.height(),
            <P as PixelWithColorType>::COLOR_TYPE,
            format,
        )
    }
}

impl<P, Container> Default for ImageBuffer<P, Container>
where
    P: Pixel,
    Container: Default,
{
    fn default() -> Self {
        Self {
            width: 0,
            height: 0,
            _phantom: PhantomData,
            data: Default::default(),
        }
    }
}

impl<P, Container> Deref for ImageBuffer<P, Container>
where
    P: Pixel,
    Container: Deref<Target = [P::Subpixel]>,
{
    type Target = [P::Subpixel];

    fn deref(&self) -> &<Self as Deref>::Target {
        &*self.data
    }
}

impl<P, Container> DerefMut for ImageBuffer<P, Container>
where
    P: Pixel,
    Container: Deref<Target = [P::Subpixel]> + DerefMut,
{
    fn deref_mut(&mut self) -> &mut <Self as Deref>::Target {
        &mut *self.data
    }
}

impl<P, Container> Index<(u32, u32)> for ImageBuffer<P, Container>
where
    P: Pixel,
    Container: Deref<Target = [P::Subpixel]>,
{
    type Output = P;

    fn index(&self, (x, y): (u32, u32)) -> &P {
        self.get_pixel(x, y)
    }
}

impl<P, Container> IndexMut<(u32, u32)> for ImageBuffer<P, Container>
where
    P: Pixel,
    Container: Deref<Target = [P::Subpixel]> + DerefMut,
{
    fn index_mut(&mut self, (x, y): (u32, u32)) -> &mut P {
        self.get_pixel_mut(x, y)
    }
}

impl<P, Container> Clone for ImageBuffer<P, Container>
where
    P: Pixel,
    Container: Deref<Target = [P::Subpixel]> + Clone,
{
    fn clone(&self) -> ImageBuffer<P, Container> {
        ImageBuffer {
            data: self.data.clone(),
            width: self.width,
            height: self.height,
            _phantom: PhantomData,
        }
    }
}

impl<P, Container> GenericImageView for ImageBuffer<P, Container>
where
    P: Pixel,
    Container: Deref<Target = [P::Subpixel]> + Deref,
{
    type Pixel = P;

    fn dimensions(&self) -> (u32, u32) {
        self.dimensions()
    }

    fn bounds(&self) -> (u32, u32, u32, u32) {
        (0, 0, self.width, self.height)
    }

    fn get_pixel(&self, x: u32, y: u32) -> P {
        *self.get_pixel(x, y)
    }

    /// Returns the pixel located at (x, y), ignoring bounds checking.
    #[inline(always)]
    unsafe fn unsafe_get_pixel(&self, x: u32, y: u32) -> P {
        let indices = self.pixel_indices_unchecked(x, y);
        *<P as Pixel>::from_slice(self.data.get_unchecked(indices))
    }
}

impl<P, Container> GenericImage for ImageBuffer<P, Container>
where
    P: Pixel,
    Container: Deref<Target = [P::Subpixel]> + DerefMut,
{
    fn get_pixel_mut(&mut self, x: u32, y: u32) -> &mut P {
        self.get_pixel_mut(x, y)
    }

    fn put_pixel(&mut self, x: u32, y: u32, pixel: P) {
        *self.get_pixel_mut(x, y) = pixel
    }

    /// Puts a pixel at location (x, y), ignoring bounds checking.
    #[inline(always)]
    unsafe fn unsafe_put_pixel(&mut self, x: u32, y: u32, pixel: P) {
        let indices = self.pixel_indices_unchecked(x, y);
        let p = <P as Pixel>::from_slice_mut(self.data.get_unchecked_mut(indices));
        *p = pixel
    }

    /// Put a pixel at location (x, y), taking into account alpha channels
    ///
    /// DEPRECATED: This method will be removed. Blend the pixel directly instead.
    fn blend_pixel(&mut self, x: u32, y: u32, p: P) {
        self.get_pixel_mut(x, y).blend(&p)
    }

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

        if sy < dy {
            for y in (0..height).rev() {
                let sy = sy + y;
                let dy = dy + y;
                let Range { start, .. } = self.pixel_indices_unchecked(sx, sy);
                let Range { end, .. } = self.pixel_indices_unchecked(sx + width - 1, sy);
                let dst = self.pixel_indices_unchecked(dx, dy).start;
                self.data.copy_within(start..end, dst);
            }
        } else {
            for y in 0..height {
                let sy = sy + y;
                let dy = dy + y;
                let Range { start, .. } = self.pixel_indices_unchecked(sx, sy);
                let Range { end, .. } = self.pixel_indices_unchecked(sx + width - 1, sy);
                let dst = self.pixel_indices_unchecked(dx, dy).start;
                self.data.copy_within(start..end, dst);
            }
        }
        true
    }
}

// concrete implementation for `Vec`-backed buffers
// TODO: I think that rustc does not "see" this impl any more: the impl with
// Container meets the same requirements. At least, I got compile errors that
// there is no such function as `into_vec`, whereas `into_raw` did work, and
// `into_vec` is redundant anyway, because `into_raw` will give you the vector,
// and it is more generic.
impl<P: Pixel> ImageBuffer<P, Vec<P::Subpixel>> {
    /// Creates a new image buffer based on a `Vec<P::Subpixel>`.
    ///
    /// # Panics
    ///
    /// Panics when the resulting image is larger the the maximum size of a vector.
    pub fn new(width: u32, height: u32) -> ImageBuffer<P, Vec<P::Subpixel>> {
        let size = Self::image_buffer_len(width, height)
            .expect("Buffer length in `ImageBuffer::new` overflows usize");
        ImageBuffer {
            data: vec![Zero::zero(); size],
            width,
            height,
            _phantom: PhantomData,
        }
    }

    /// Constructs a new ImageBuffer by copying a pixel
    ///
    /// # Panics
    ///
    /// Panics when the resulting image is larger the the maximum size of a vector.
    pub fn from_pixel(width: u32, height: u32, pixel: P) -> ImageBuffer<P, Vec<P::Subpixel>> {
        let mut buf = ImageBuffer::new(width, height);
        for p in buf.pixels_mut() {
            *p = pixel
        }
        buf
    }

    /// Constructs a new ImageBuffer by repeated application of the supplied function.
    ///
    /// The arguments to the function are the pixel's x and y coordinates.
    ///
    /// # Panics
    ///
    /// Panics when the resulting image is larger the the maximum size of a vector.
    pub fn from_fn<F>(width: u32, height: u32, mut f: F) -> ImageBuffer<P, Vec<P::Subpixel>>
    where
        F: FnMut(u32, u32) -> P,
    {
        let mut buf = ImageBuffer::new(width, height);
        for (x, y, p) in buf.enumerate_pixels_mut() {
            *p = f(x, y)
        }
        buf
    }

    /// Creates an image buffer out of an existing buffer.
    /// Returns None if the buffer is not big enough.
    pub fn from_vec(
        width: u32,
        height: u32,
        buf: Vec<P::Subpixel>,
    ) -> Option<ImageBuffer<P, Vec<P::Subpixel>>> {
        ImageBuffer::from_raw(width, height, buf)
    }

    /// Consumes the image buffer and returns the underlying data
    /// as an owned buffer
    pub fn into_vec(self) -> Vec<P::Subpixel> {
        self.into_raw()
    }
}

/// Provides color conversions for whole image buffers.
pub trait ConvertBuffer<T> {
    /// Converts `self` to a buffer of type T
    ///
    /// A generic implementation is provided to convert any image buffer to a image buffer
    /// based on a `Vec<T>`.
    fn convert(&self) -> T;
}

// concrete implementation Luma -> Rgba
impl GrayImage {
    /// Expands a color palette by re-using the existing buffer.
    /// Assumes 8 bit per pixel. Uses an optionally transparent index to
    /// adjust it's alpha value accordingly.
    pub fn expand_palette(
        self,
        palette: &[(u8, u8, u8)],
        transparent_idx: Option<u8>,
    ) -> RgbaImage {
        let (width, height) = self.dimensions();
        let mut data = self.into_raw();
        let entries = data.len();
        data.resize(entries.checked_mul(4).unwrap(), 0);
        let mut buffer = ImageBuffer::from_vec(width, height, data).unwrap();
        expand_packed(&mut buffer, 4, 8, |idx, pixel| {
            let (r, g, b) = palette[idx as usize];
            let a = if let Some(t_idx) = transparent_idx {
                if t_idx == idx {
                    0
                } else {
                    255
                }
            } else {
                255
            };
            pixel[0] = r;
            pixel[1] = g;
            pixel[2] = b;
            pixel[3] = a;
        });
        buffer
    }
}

// TODO: Equality constraints are not yet supported in where clauses, when they
// are, the T parameter should be removed in favor of ToType::Subpixel, which
// will then be FromType::Subpixel.
impl<'a, 'b, Container, FromType: Pixel, ToType: Pixel>
    ConvertBuffer<ImageBuffer<ToType, Vec<ToType::Subpixel>>> for ImageBuffer<FromType, Container>
where
    Container: Deref<Target = [FromType::Subpixel]>,
    ToType: FromColor<FromType>,
{
    /// # Examples
    /// Convert RGB image to gray image.
    /// ```no_run
    /// use image::buffer::ConvertBuffer;
    /// use image::GrayImage;
    ///
    /// let image_path = "examples/fractal.png";
    /// let image = image::open(&image_path)
    ///     .expect("Open file failed")
    ///     .to_rgba8();
    ///
    /// let gray_image: GrayImage = image.convert();
    /// ```
    fn convert(&self) -> ImageBuffer<ToType, Vec<ToType::Subpixel>> {
        let mut buffer: ImageBuffer<ToType, Vec<ToType::Subpixel>> =
            ImageBuffer::new(self.width, self.height);
        for (to, from) in buffer.pixels_mut().zip(self.pixels()) {
            to.from_color(from)
        }
        buffer
    }
}

/// Sendable Rgb image buffer
pub type RgbImage = ImageBuffer<Rgb<u8>, Vec<u8>>;
/// Sendable Rgb + alpha channel image buffer
pub type RgbaImage = ImageBuffer<Rgba<u8>, Vec<u8>>;
/// Sendable grayscale image buffer
pub type GrayImage = ImageBuffer<Luma<u8>, Vec<u8>>;
/// Sendable grayscale + alpha channel image buffer
pub type GrayAlphaImage = ImageBuffer<LumaA<u8>, Vec<u8>>;
/// Sendable 16-bit Rgb image buffer
pub(crate) type Rgb16Image = ImageBuffer<Rgb<u16>, Vec<u16>>;
/// Sendable 16-bit Rgb + alpha channel image buffer
pub(crate) type Rgba16Image = ImageBuffer<Rgba<u16>, Vec<u16>>;
/// Sendable 16-bit grayscale image buffer
pub(crate) type Gray16Image = ImageBuffer<Luma<u16>, Vec<u16>>;
/// Sendable 16-bit grayscale + alpha channel image buffer
pub(crate) type GrayAlpha16Image = ImageBuffer<LumaA<u16>, Vec<u16>>;

/// An image buffer for 32-bit float RGB pixels,
/// where the backing container is a flattened vector of floats.
pub type Rgb32FImage = ImageBuffer<Rgb<f32>, Vec<f32>>;

/// An image buffer for 32-bit float RGBA pixels,
/// where the backing container is a flattened vector of floats.
pub type Rgba32FImage = ImageBuffer<Rgba<f32>, Vec<f32>>;

#[cfg(test)]
mod test {
    use super::{GrayImage, ImageBuffer, ImageOutputFormat, RgbImage};
    use crate::math::Rect;
    use crate::GenericImage as _;
    use crate::{color, Rgb};

    #[test]
    /// Tests if image buffers from slices work
    fn slice_buffer() {
        let data = [0; 9];
        let buf: ImageBuffer<color::Luma<u8>, _> = ImageBuffer::from_raw(3, 3, &data[..]).unwrap();
        assert_eq!(&*buf, &data[..])
    }

    #[test]
    fn get_pixel() {
        let mut a: RgbImage = ImageBuffer::new(10, 10);
        {
            let b = a.get_mut(3 * 10).unwrap();
            *b = 255;
        }
        assert_eq!(a.get_pixel(0, 1)[0], 255)
    }

    #[test]
    fn get_pixel_checked() {
        let mut a: RgbImage = ImageBuffer::new(10, 10);
        a.get_pixel_mut_checked(0, 1).map(|b| b[0] = 255);

        assert_eq!(a.get_pixel_checked(0, 1), Some(&Rgb([255, 0, 0])));
        assert_eq!(a.get_pixel_checked(0, 1).unwrap(), a.get_pixel(0, 1));
        assert_eq!(a.get_pixel_checked(10, 0), None);
        assert_eq!(a.get_pixel_checked(0, 10), None);
        assert_eq!(a.get_pixel_mut_checked(10, 0), None);
        assert_eq!(a.get_pixel_mut_checked(0, 10), None);

        // From image/issues/1672
        const WHITE: Rgb<u8> = Rgb([255_u8, 255, 255]);
        let mut a = RgbImage::new(2, 1);
        a.put_pixel(1, 0, WHITE);

        assert_eq!(a.get_pixel_checked(1, 0), Some(&WHITE));
        assert_eq!(a.get_pixel_checked(1, 0).unwrap(), a.get_pixel(1, 0));
    }

    #[test]
    fn mut_iter() {
        let mut a: RgbImage = ImageBuffer::new(10, 10);
        {
            let val = a.pixels_mut().next().unwrap();
            *val = Rgb([42, 0, 0]);
        }
        assert_eq!(a.data[0], 42)
    }

    #[test]
    fn zero_width_zero_height() {
        let mut image = RgbImage::new(0, 0);

        assert_eq!(image.rows_mut().count(), 0);
        assert_eq!(image.pixels_mut().count(), 0);
        assert_eq!(image.rows().count(), 0);
        assert_eq!(image.pixels().count(), 0);
    }

    #[test]
    fn zero_width_nonzero_height() {
        let mut image = RgbImage::new(0, 2);

        assert_eq!(image.rows_mut().count(), 0);
        assert_eq!(image.pixels_mut().count(), 0);
        assert_eq!(image.rows().count(), 0);
        assert_eq!(image.pixels().count(), 0);
    }

    #[test]
    fn nonzero_width_zero_height() {
        let mut image = RgbImage::new(2, 0);

        assert_eq!(image.rows_mut().count(), 0);
        assert_eq!(image.pixels_mut().count(), 0);
        assert_eq!(image.rows().count(), 0);
        assert_eq!(image.pixels().count(), 0);
    }

    #[test]
    fn pixels_on_large_buffer() {
        let mut image = RgbImage::from_raw(1, 1, vec![0; 6]).unwrap();

        assert_eq!(image.pixels().count(), 1);
        assert_eq!(image.enumerate_pixels().count(), 1);
        assert_eq!(image.pixels_mut().count(), 1);
        assert_eq!(image.enumerate_pixels_mut().count(), 1);

        assert_eq!(image.rows().count(), 1);
        assert_eq!(image.rows_mut().count(), 1);
    }

    #[test]
    fn default() {
        let image = ImageBuffer::<Rgb<u8>, Vec<u8>>::default();
        assert_eq!(image.dimensions(), (0, 0));
    }

    #[test]
    #[rustfmt::skip]
    fn test_image_buffer_copy_within_oob() {
        let mut image: GrayImage = ImageBuffer::from_raw(4, 4, vec![0u8; 16]).unwrap();
        assert!(!image.copy_within(Rect { x: 0, y: 0, width: 5, height: 4 }, 0, 0));
        assert!(!image.copy_within(Rect { x: 0, y: 0, width: 4, height: 5 }, 0, 0));
        assert!(!image.copy_within(Rect { x: 1, y: 0, width: 4, height: 4 }, 0, 0));
        assert!(!image.copy_within(Rect { x: 0, y: 0, width: 4, height: 4 }, 1, 0));
        assert!(!image.copy_within(Rect { x: 0, y: 1, width: 4, height: 4 }, 0, 0));
        assert!(!image.copy_within(Rect { x: 0, y: 0, width: 4, height: 4 }, 0, 1));
        assert!(!image.copy_within(Rect { x: 1, y: 1, width: 4, height: 4 }, 0, 0));
    }

    #[test]
    fn test_image_buffer_copy_within_tl() {
        let data = &[
            00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12, 13, 14, 15,
        ];
        let expected = [
            00, 01, 02, 03, 04, 00, 01, 02, 08, 04, 05, 06, 12, 08, 09, 10,
        ];
        let mut image: GrayImage = ImageBuffer::from_raw(4, 4, Vec::from(&data[..])).unwrap();
        assert!(image.copy_within(
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
    fn test_image_buffer_copy_within_tr() {
        let data = &[
            00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12, 13, 14, 15,
        ];
        let expected = [
            00, 01, 02, 03, 01, 02, 03, 07, 05, 06, 07, 11, 09, 10, 11, 15,
        ];
        let mut image: GrayImage = ImageBuffer::from_raw(4, 4, Vec::from(&data[..])).unwrap();
        assert!(image.copy_within(
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
    fn test_image_buffer_copy_within_bl() {
        let data = &[
            00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12, 13, 14, 15,
        ];
        let expected = [
            00, 04, 05, 06, 04, 08, 09, 10, 08, 12, 13, 14, 12, 13, 14, 15,
        ];
        let mut image: GrayImage = ImageBuffer::from_raw(4, 4, Vec::from(&data[..])).unwrap();
        assert!(image.copy_within(
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
    fn test_image_buffer_copy_within_br() {
        let data = &[
            00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12, 13, 14, 15,
        ];
        let expected = [
            05, 06, 07, 03, 09, 10, 11, 07, 13, 14, 15, 11, 12, 13, 14, 15,
        ];
        let mut image: GrayImage = ImageBuffer::from_raw(4, 4, Vec::from(&data[..])).unwrap();
        assert!(image.copy_within(
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
    #[cfg(feature = "png")]
    fn write_to_with_large_buffer() {
        // A buffer of 1 pixel, padded to 4 bytes as would be common in, e.g. BMP.
        let img: GrayImage = ImageBuffer::from_raw(1, 1, vec![0u8; 4]).unwrap();
        let mut buffer = std::io::Cursor::new(vec![]);
        assert!(img.write_to(&mut buffer, ImageOutputFormat::Png).is_ok());
    }

    #[test]
    fn exact_size_iter_size_hint() {
        // The docs for `std::iter::ExactSizeIterator` requires that the implementation of
        // `size_hint` on the iterator returns the same value as the `len` implementation.

        // This test should work for any size image.
        const N: u32 = 10;

        let mut image = RgbImage::from_raw(N, N, vec![0; (N * N * 3) as usize]).unwrap();

        let iter = image.pixels();
        let exact_len = ExactSizeIterator::len(&iter);
        assert_eq!(iter.size_hint(), (exact_len, Some(exact_len)));

        let iter = image.pixels_mut();
        let exact_len = ExactSizeIterator::len(&iter);
        assert_eq!(iter.size_hint(), (exact_len, Some(exact_len)));

        let iter = image.rows();
        let exact_len = ExactSizeIterator::len(&iter);
        assert_eq!(iter.size_hint(), (exact_len, Some(exact_len)));

        let iter = image.rows_mut();
        let exact_len = ExactSizeIterator::len(&iter);
        assert_eq!(iter.size_hint(), (exact_len, Some(exact_len)));

        let iter = image.enumerate_pixels();
        let exact_len = ExactSizeIterator::len(&iter);
        assert_eq!(iter.size_hint(), (exact_len, Some(exact_len)));

        let iter = image.enumerate_rows();
        let exact_len = ExactSizeIterator::len(&iter);
        assert_eq!(iter.size_hint(), (exact_len, Some(exact_len)));

        let iter = image.enumerate_pixels_mut();
        let exact_len = ExactSizeIterator::len(&iter);
        assert_eq!(iter.size_hint(), (exact_len, Some(exact_len)));

        let iter = image.enumerate_rows_mut();
        let exact_len = ExactSizeIterator::len(&iter);
        assert_eq!(iter.size_hint(), (exact_len, Some(exact_len)));
    }
}

#[cfg(test)]
#[cfg(feature = "benchmarks")]
mod benchmarks {
    use super::{ConvertBuffer, GrayImage, ImageBuffer, Pixel, RgbImage};
    use crate::GenericImage;
    use test;

    #[bench]
    fn conversion(b: &mut test::Bencher) {
        let mut a: RgbImage = ImageBuffer::new(1000, 1000);
        for p in a.pixels_mut() {
            let rgb = p.channels_mut();
            rgb[0] = 255;
            rgb[1] = 23;
            rgb[2] = 42;
        }
        assert!(a.data[0] != 0);
        b.iter(|| {
            let b: GrayImage = a.convert();
            assert!(0 != b.data[0]);
            assert!(a.data[0] != b.data[0]);
            test::black_box(b);
        });
        b.bytes = 1000 * 1000 * 3
    }

    #[bench]
    fn image_access_row_by_row(b: &mut test::Bencher) {
        let mut a: RgbImage = ImageBuffer::new(1000, 1000);
        for p in a.pixels_mut() {
            let rgb = p.channels_mut();
            rgb[0] = 255;
            rgb[1] = 23;
            rgb[2] = 42;
        }

        b.iter(move || {
            let image: &RgbImage = test::black_box(&a);
            let mut sum: usize = 0;
            for y in 0..1000 {
                for x in 0..1000 {
                    let pixel = image.get_pixel(x, y);
                    sum = sum.wrapping_add(pixel[0] as usize);
                    sum = sum.wrapping_add(pixel[1] as usize);
                    sum = sum.wrapping_add(pixel[2] as usize);
                }
            }
            test::black_box(sum)
        });

        b.bytes = 1000 * 1000 * 3;
    }

    #[bench]
    fn image_access_col_by_col(b: &mut test::Bencher) {
        let mut a: RgbImage = ImageBuffer::new(1000, 1000);
        for p in a.pixels_mut() {
            let rgb = p.channels_mut();
            rgb[0] = 255;
            rgb[1] = 23;
            rgb[2] = 42;
        }

        b.iter(move || {
            let image: &RgbImage = test::black_box(&a);
            let mut sum: usize = 0;
            for x in 0..1000 {
                for y in 0..1000 {
                    let pixel = image.get_pixel(x, y);
                    sum = sum.wrapping_add(pixel[0] as usize);
                    sum = sum.wrapping_add(pixel[1] as usize);
                    sum = sum.wrapping_add(pixel[2] as usize);
                }
            }
            test::black_box(sum)
        });

        b.bytes = 1000 * 1000 * 3;
    }
}
