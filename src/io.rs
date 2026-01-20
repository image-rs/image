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

#[deprecated(note = "this type has been moved to image::Limits")]
/// Deprecated re-export of `Limits`
pub type Limits = limits::Limits;
#[deprecated(note = "this type has been moved to image::LimitSupport")]
/// Deprecated re-export of `LimitSupport`
pub type LimitSupport = limits::LimitSupport;

pub use decoder::{
    DecodedAnimationAttributes, DecodedImageAttributes, DecodedMetadataHint, DecoderAttributes,
    SequenceControl,
};

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
/// Describes a packed rectangular layout with given bit-depth in [`ImageDecoder::init`]. Layouts
/// from `image` are row-major with no padding between rows and pixels packed by consecutive
/// channels.
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
    /// Return width and height as a tuple, consistent with
    /// [`GenericImageView::dimensions`][`crate::GenericImageView::dimensions`].
    ///
    /// Note that this refers to underlying pixel matrix, not the orientation of the image as
    /// indicated to be viewed in user facing applications by metadata.
    pub fn dimensions(&self) -> (u32, u32) {
        (self.width, self.height)
    }

    /// A layout with no pixels, of the given [`ColorType`][`crate::ColorType`].
    pub fn empty(color: crate::ColorType) -> Self {
        ImageLayout {
            color,
            width: 0,
            height: 0,
        }
    }

    /// Returns the total number of bytes in the decoded image.
    ///
    /// This is the size of the buffer that must be passed to `read_image` or
    /// `read_image_with_progress`. The returned value may exceed `usize::MAX`, in
    /// which case it isn't actually possible to construct a buffer to decode all the image data
    /// into. If, however, the size does not fit in a u64 then `u64::MAX` is returned.
    pub fn total_bytes(&self) -> u64 {
        let ImageLayout { width, height, .. } = *self;
        let total_pixels = u64::from(width) * u64::from(height);
        let bytes_per_pixel = u64::from(self.color.bytes_per_pixel());
        total_pixels.saturating_mul(bytes_per_pixel)
    }
}
