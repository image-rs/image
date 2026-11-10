//! Input and output of images.

use std::io;
use std::io::Read as _;

/// The decoder traits.
pub(crate) mod decoder;
/// The encoder traits.
pub(crate) mod encoder;

pub(crate) mod format;
pub(crate) mod free_functions;
pub(crate) mod image_reader_type;
pub(crate) mod limits;

pub use decoder::{
    DecodedAnimationAttributes, DecodedImageAttributes, DecodedMetadataHint, FormatAttributes,
    SequenceControl,
};

pub use image_reader_type::DecodedImageMetadata;

/// Adds `read_exact_vec`
pub(crate) trait ReadExt {
    fn read_exact_vec(&mut self, vec: &mut Vec<u8>, len: usize) -> io::Result<()>;
}

impl<R: io::Read> ReadExt for R {
    fn read_exact_vec(&mut self, vec: &mut Vec<u8>, len: usize) -> io::Result<()> {
        let initial_len = vec.len();
        vec.try_reserve(len)?;
        match self.take(len as u64).read_to_end(vec) {
            Ok(read) if read == len => Ok(()),
            fail => {
                vec.truncate(initial_len);
                Err(fail.err().unwrap_or(io::ErrorKind::UnexpectedEof.into()))
            }
        }
    }
}

/// Communicate the layout of an image.
///
/// Describes a packed rectangular layout with given bit-depth in
/// [`ImageDecoder::prepare_image`](crate::ImageDecoder::prepare_image). Standard layouts from `image`
/// are row-major with no padding between rows and pixels packed by consecutive channels.
#[non_exhaustive]
pub struct ImageLayout {
    /// The color model of each pixel.
    pub color: crate::ColorType,
    /// The number of pixels in the horizontal direction.
    pub width: u32,
    /// The number of pixels in the vertical direction.
    pub height: u32,
}

impl ImageLayout {
    /// A layout matching a [`DynamicImage`][`crate::DynamicImage`] of the given dimensions and
    /// color type.
    pub fn new(w: u32, h: u32, color: crate::ColorType) -> ImageLayout {
        ImageLayout {
            color,
            width: w,
            height: h,
        }
    }

    /// A layout with no pixels, of the given [`ColorType`][`crate::ColorType`].
    ///
    /// This is equivalent to `ImageLayout::new(0, 0, color)`.
    pub fn empty(color: crate::ColorType) -> Self {
        ImageLayout {
            color,
            width: 0,
            height: 0,
        }
    }

    /// Return width and height as a tuple, consistent with
    /// [`GenericImageView::dimensions`][`crate::GenericImageView::dimensions`].
    ///
    /// Note that this refers to underlying pixel matrix, not the orientation of the image as
    /// indicated to be viewed in user facing applications by metadata.
    pub fn dimensions(&self) -> (u32, u32) {
        (self.width, self.height)
    }

    /// The total number of bytes in the described image.
    ///
    /// See also [`DecoderPreparedImage::total_bytes`].
    pub fn total_bytes(&self) -> u64 {
        let ImageLayout { width, height, .. } = *self;
        let total_pixels = u64::from(width) * u64::from(height);
        let bytes_per_pixel = u64::from(self.color.bytes_per_pixel());
        total_pixels.saturating_mul(bytes_per_pixel)
    }

    /// Checks if the provided dimensions would overflow a `u64`.
    ///
    /// FIXME: instead have `try_total_bytes() -> Result<u64, _>`? But should it return an
    /// unsupported error (as formats that use this do) or should it instead return another error
    /// since the method is then unrelated to a format.
    pub(crate) fn total_bytes_overflows_u64(&self) -> bool {
        let &ImageLayout { width, height, .. } = self;
        let bytes_per_pixel: u8 = self.color.bytes_per_pixel();
        u64::from(width) * u64::from(height) > u64::MAX / u64::from(bytes_per_pixel)
    }
}

/// Describes the next image for
/// [`ImageDecoder::prepare_image`](`crate::ImageDecoder::prepare_image`).
///
/// For external crates constructing an instance, use [`Self::new`] with the intended color type
/// and then fill in all applicable fields. This initializes the layout, a minimal descriptor of an
/// expected [`DynamicImage`][`crate::DynamicImage`] (or equivalently sized other buffer).
#[non_exhaustive]
pub struct DecoderPreparedImage {
    /// The layout of the primary image data.
    pub layout: ImageLayout,
}

/// Defaults all fields except the layout.
impl From<ImageLayout> for DecoderPreparedImage {
    fn from(layout: ImageLayout) -> Self {
        DecoderPreparedImage { layout }
    }
}

impl DecoderPreparedImage {
    /// A layout matching a [`DynamicImage`][`crate::DynamicImage`] of the given dimensions and
    /// color type.
    pub fn new(w: u32, h: u32, color: crate::ColorType) -> DecoderPreparedImage {
        DecoderPreparedImage {
            layout: ImageLayout::new(w, h, color),
        }
    }

    /// The total number of bytes in the decoded image.
    ///
    /// This is the size of the buffer that must be passed to
    /// [`ImageDecoder::read_image`](`crate::ImageDecoder::read_image`) or
    /// `read_image_with_progress`. The returned value may exceed `usize::MAX`, in which case it
    /// isn't actually possible to construct a buffer to decode all the image data into. If the
    /// size does not fit in a u64 then `u64::MAX` is returned. For all practical purposes all
    /// platforms will fail to allocate that much memory.
    pub fn total_bytes(&self) -> u64 {
        self.layout.total_bytes()
    }
}
