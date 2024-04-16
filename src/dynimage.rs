use std::io::{self, Seek, Write};
use std::path::Path;

#[cfg(feature = "fitsio")]
use fitsio::errors::Error as FitsError;
#[cfg(feature = "fitsio")]
use std::path::PathBuf;
#[cfg(feature = "fitsio")]
use crate::FitsCompression;

use serde::{Deserialize, Serialize};

#[cfg(feature = "gif")]
use crate::codecs::gif;
#[cfg(feature = "png")]
use crate::codecs::png;

use crate::buffer_::{
    ConvertBuffer, SerialGray16Image, SerialGrayAlpha16Image, SerialGrayAlphaImage,
    SerialGrayImage, SerialImageBuffer, SerialRgb16Image, SerialRgbImage, SerialRgba16Image,
    SerialRgbaImage,
};
use crate::color::{self, IntoColor};
use crate::error::{ImageError, ImageResult, ParameterError, ParameterErrorKind};
use crate::flat::FlatSamples;
use crate::image::{GenericImageView, ImageDecoder, ImageEncoder, ImageFormat, SerialGenericImage};
use crate::io::free_functions;
use crate::math::resize_dimensions;
use crate::traits::Pixel;
use crate::{image, Luma, LumaA};
use crate::{imageops, ExtendedColorType};
use crate::{SerialRgb32FImage, SerialRgba32FImage};

/// A Dynamic Image
///
/// This represents a _matrix_ of _pixels_ which are _convertible_ from and to an _RGBA_
/// representation. More variants that adhere to these principles may get added in the future, in
/// particular to cover other combinations typically used.
///
/// # Usage
///
/// This type can act as a converter between specific `SerialImageBuffer` instances.
///
/// ```
/// use image::{DynamicSerialImage, SerialGrayImage, SerialRgbImage};
///
/// let rgb: SerialRgbImage = SerialRgbImage::new(10, 10);
/// let luma: SerialGrayImage = DynamicSerialImage::ImageRgb8(rgb).into_luma8();
/// ```
///
/// # Design
///
/// There is no goal to provide an all-encompassing type with all possible memory layouts. This
/// would hardly be feasible as a simple enum, due to the sheer number of combinations of channel
/// kinds, channel order, and bit depth. Rather, this type provides an opinionated selection with
/// normalized channel order which can store common pixel values without loss.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[non_exhaustive]
pub enum DynamicSerialImage {
    /// Each pixel in this image is 8-bit Luma
    ImageLuma8(SerialGrayImage),

    /// Each pixel in this image is 8-bit Luma with alpha
    ImageLumaA8(SerialGrayAlphaImage),

    /// Each pixel in this image is 8-bit Rgb
    ImageRgb8(SerialRgbImage),

    /// Each pixel in this image is 8-bit Rgb with alpha
    ImageRgba8(SerialRgbaImage),

    /// Each pixel in this image is 16-bit Luma
    ImageLuma16(SerialGray16Image),

    /// Each pixel in this image is 16-bit Luma with alpha
    ImageLumaA16(SerialGrayAlpha16Image),

    /// Each pixel in this image is 16-bit Rgb
    ImageRgb16(SerialRgb16Image),

    /// Each pixel in this image is 16-bit Rgb with alpha
    ImageRgba16(SerialRgba16Image),

    /// Each pixel in this image is 32-bit float Rgb
    ImageRgb32F(SerialRgb32FImage),

    /// Each pixel in this image is 32-bit float Rgb with alpha
    ImageRgba32F(SerialRgba32FImage),
}

macro_rules! dynamic_map(
        ($dynimage: expr, $image: pat => $action: expr) => ({
            use DynamicSerialImage::*;
            match $dynimage {
                ImageLuma8($image) => ImageLuma8($action),
                ImageLumaA8($image) => ImageLumaA8($action),
                ImageRgb8($image) => ImageRgb8($action),
                ImageRgba8($image) => ImageRgba8($action),
                ImageLuma16($image) => ImageLuma16($action),
                ImageLumaA16($image) => ImageLumaA16($action),
                ImageRgb16($image) => ImageRgb16($action),
                ImageRgba16($image) => ImageRgba16($action),
                ImageRgb32F($image) => ImageRgb32F($action),
                ImageRgba32F($image) => ImageRgba32F($action),
            }
        });

        ($dynimage: expr, $image:pat_param, $action: expr) => (
            match $dynimage {
                DynamicSerialImage::ImageLuma8($image) => $action,
                DynamicSerialImage::ImageLumaA8($image) => $action,
                DynamicSerialImage::ImageRgb8($image) => $action,
                DynamicSerialImage::ImageRgba8($image) => $action,
                DynamicSerialImage::ImageLuma16($image) => $action,
                DynamicSerialImage::ImageLumaA16($image) => $action,
                DynamicSerialImage::ImageRgb16($image) => $action,
                DynamicSerialImage::ImageRgba16($image) => $action,
                DynamicSerialImage::ImageRgb32F($image) => $action,
                DynamicSerialImage::ImageRgba32F($image) => $action,
            }
        );
);

impl DynamicSerialImage {
    /// Creates a dynamic image backed by a buffer depending on
    /// the color type given.
    pub fn new(w: u32, h: u32, color: color::ColorType) -> DynamicSerialImage {
        use color::ColorType::*;
        match color {
            L8 => Self::new_luma8(w, h),
            La8 => Self::new_luma_a8(w, h),
            Rgb8 => Self::new_rgb8(w, h),
            Rgba8 => Self::new_rgba8(w, h),
            L16 => Self::new_luma16(w, h),
            La16 => Self::new_luma_a16(w, h),
            Rgb16 => Self::new_rgb16(w, h),
            Rgba16 => Self::new_rgba16(w, h),
            Rgb32F => Self::new_rgb32f(w, h),
            Rgba32F => Self::new_rgba32f(w, h),
        }
    }

    /// Creates a dynamic image backed by a buffer of gray pixels.
    pub fn new_luma8(w: u32, h: u32) -> DynamicSerialImage {
        DynamicSerialImage::ImageLuma8(SerialImageBuffer::new(w, h))
    }

    /// Creates a dynamic image backed by a buffer of gray
    /// pixels with transparency.
    pub fn new_luma_a8(w: u32, h: u32) -> DynamicSerialImage {
        DynamicSerialImage::ImageLumaA8(SerialImageBuffer::new(w, h))
    }

    /// Creates a dynamic image backed by a buffer of RGB pixels.
    pub fn new_rgb8(w: u32, h: u32) -> DynamicSerialImage {
        DynamicSerialImage::ImageRgb8(SerialImageBuffer::new(w, h))
    }

    /// Creates a dynamic image backed by a buffer of RGBA pixels.
    pub fn new_rgba8(w: u32, h: u32) -> DynamicSerialImage {
        DynamicSerialImage::ImageRgba8(SerialImageBuffer::new(w, h))
    }

    /// Creates a dynamic image backed by a buffer of gray pixels.
    pub fn new_luma16(w: u32, h: u32) -> DynamicSerialImage {
        DynamicSerialImage::ImageLuma16(SerialImageBuffer::new(w, h))
    }

    /// Creates a dynamic image backed by a buffer of gray
    /// pixels with transparency.
    pub fn new_luma_a16(w: u32, h: u32) -> DynamicSerialImage {
        DynamicSerialImage::ImageLumaA16(SerialImageBuffer::new(w, h))
    }

    /// Creates a dynamic image backed by a buffer of RGB pixels.
    pub fn new_rgb16(w: u32, h: u32) -> DynamicSerialImage {
        DynamicSerialImage::ImageRgb16(SerialImageBuffer::new(w, h))
    }

    /// Creates a dynamic image backed by a buffer of RGBA pixels.
    pub fn new_rgba16(w: u32, h: u32) -> DynamicSerialImage {
        DynamicSerialImage::ImageRgba16(SerialImageBuffer::new(w, h))
    }

    /// Creates a dynamic image backed by a buffer of RGB pixels.
    pub fn new_rgb32f(w: u32, h: u32) -> DynamicSerialImage {
        DynamicSerialImage::ImageRgb32F(SerialImageBuffer::new(w, h))
    }

    /// Creates a dynamic image backed by a buffer of RGBA pixels.
    pub fn new_rgba32f(w: u32, h: u32) -> DynamicSerialImage {
        DynamicSerialImage::ImageRgba32F(SerialImageBuffer::new(w, h))
    }

    /// Decodes an encoded image into a dynamic image.
    pub fn from_decoder(decoder: impl ImageDecoder) -> ImageResult<Self> {
        decoder_to_image(decoder)
    }

    /// Returns a copy of this image as an RGB image.
    pub fn to_rgb8(&self) -> SerialRgbImage {
        dynamic_map!(*self, ref p, p.convert())
    }

    /// Returns a copy of this image as an RGB image.
    pub fn to_rgb16(&self) -> SerialRgb16Image {
        dynamic_map!(*self, ref p, p.convert())
    }

    /// Returns a copy of this image as an RGB image.
    pub fn to_rgb32f(&self) -> SerialRgb32FImage {
        dynamic_map!(*self, ref p, p.convert())
    }

    /// Returns a copy of this image as an RGBA image.
    pub fn to_rgba8(&self) -> SerialRgbaImage {
        dynamic_map!(*self, ref p, p.convert())
    }

    /// Returns a copy of this image as an RGBA image.
    pub fn to_rgba16(&self) -> SerialRgba16Image {
        dynamic_map!(*self, ref p, p.convert())
    }

    /// Returns a copy of this image as an RGBA image.
    pub fn to_rgba32f(&self) -> SerialRgba32FImage {
        dynamic_map!(*self, ref p, p.convert())
    }

    /// Returns a copy of this image as a Luma image.
    pub fn to_luma8(&self) -> SerialGrayImage {
        dynamic_map!(*self, ref p, p.convert())
    }

    /// Returns a copy of this image as a Luma image.
    pub fn to_luma16(&self) -> SerialGray16Image {
        dynamic_map!(*self, ref p, p.convert())
    }

    /// Returns a copy of this image as a Luma image.
    pub fn to_luma32f(&self) -> SerialImageBuffer<Luma<f32>, Vec<f32>> {
        dynamic_map!(*self, ref p, p.convert())
    }

    /// Returns a copy of this image as a LumaA image.
    pub fn to_luma_alpha8(&self) -> SerialGrayAlphaImage {
        dynamic_map!(*self, ref p, p.convert())
    }

    /// Returns a copy of this image as a LumaA image.
    pub fn to_luma_alpha16(&self) -> SerialGrayAlpha16Image {
        dynamic_map!(*self, ref p, p.convert())
    }

    /// Returns a copy of this image as a LumaA image.
    pub fn to_luma_alpha32f(&self) -> SerialImageBuffer<LumaA<f32>, Vec<f32>> {
        dynamic_map!(*self, ref p, p.convert())
    }

    /// Consume the image and returns a RGB image.
    ///
    /// If the image was already the correct format, it is returned as is.
    /// Otherwise, a copy is created.
    pub fn into_rgb8(self) -> SerialRgbImage {
        match self {
            DynamicSerialImage::ImageRgb8(x) => x,
            x => x.to_rgb8(),
        }
    }

    /// Consume the image and returns a RGB image.
    ///
    /// If the image was already the correct format, it is returned as is.
    /// Otherwise, a copy is created.
    pub fn into_rgb16(self) -> SerialRgb16Image {
        match self {
            DynamicSerialImage::ImageRgb16(x) => x,
            x => x.to_rgb16(),
        }
    }

    /// Consume the image and returns a RGB image.
    ///
    /// If the image was already the correct format, it is returned as is.
    /// Otherwise, a copy is created.
    pub fn into_rgb32f(self) -> SerialRgb32FImage {
        match self {
            DynamicSerialImage::ImageRgb32F(x) => x,
            x => x.to_rgb32f(),
        }
    }

    /// Consume the image and returns a RGBA image.
    ///
    /// If the image was already the correct format, it is returned as is.
    /// Otherwise, a copy is created.
    pub fn into_rgba8(self) -> SerialRgbaImage {
        match self {
            DynamicSerialImage::ImageRgba8(x) => x,
            x => x.to_rgba8(),
        }
    }

    /// Consume the image and returns a RGBA image.
    ///
    /// If the image was already the correct format, it is returned as is.
    /// Otherwise, a copy is created.
    pub fn into_rgba16(self) -> SerialRgba16Image {
        match self {
            DynamicSerialImage::ImageRgba16(x) => x,
            x => x.to_rgba16(),
        }
    }

    /// Consume the image and returns a RGBA image.
    ///
    /// If the image was already the correct format, it is returned as is.
    /// Otherwise, a copy is created.
    pub fn into_rgba32f(self) -> SerialRgba32FImage {
        match self {
            DynamicSerialImage::ImageRgba32F(x) => x,
            x => x.to_rgba32f(),
        }
    }

    /// Consume the image and returns a Luma image.
    ///
    /// If the image was already the correct format, it is returned as is.
    /// Otherwise, a copy is created.
    pub fn into_luma8(self) -> SerialGrayImage {
        match self {
            DynamicSerialImage::ImageLuma8(x) => x,
            x => x.to_luma8(),
        }
    }

    /// Consume the image and returns a Luma image.
    ///
    /// If the image was already the correct format, it is returned as is.
    /// Otherwise, a copy is created.
    pub fn into_luma16(self) -> SerialGray16Image {
        match self {
            DynamicSerialImage::ImageLuma16(x) => x,
            x => x.to_luma16(),
        }
    }

    /// Consume the image and returns a LumaA image.
    ///
    /// If the image was already the correct format, it is returned as is.
    /// Otherwise, a copy is created.
    pub fn into_luma_alpha8(self) -> SerialGrayAlphaImage {
        match self {
            DynamicSerialImage::ImageLumaA8(x) => x,
            x => x.to_luma_alpha8(),
        }
    }

    /// Consume the image and returns a LumaA image.
    ///
    /// If the image was already the correct format, it is returned as is.
    /// Otherwise, a copy is created.
    pub fn into_luma_alpha16(self) -> SerialGrayAlpha16Image {
        match self {
            DynamicSerialImage::ImageLumaA16(x) => x,
            x => x.to_luma_alpha16(),
        }
    }

    /// Return a cut-out of this image delimited by the bounding rectangle.
    ///
    /// Note: this method does *not* modify the object,
    /// and its signature will be replaced with `crop_imm()`'s in the 0.24 release
    pub fn crop(&mut self, x: u32, y: u32, width: u32, height: u32) -> DynamicSerialImage {
        dynamic_map!(*self, ref mut p => imageops::crop(p, x, y, width, height).to_image())
    }

    /// Return a cut-out of this image delimited by the bounding rectangle.
    pub fn crop_imm(&self, x: u32, y: u32, width: u32, height: u32) -> DynamicSerialImage {
        dynamic_map!(*self, ref p => imageops::crop_imm(p, x, y, width, height).to_image())
    }

    /// Return a reference to an 8bit RGB image
    pub fn as_rgb8(&self) -> Option<&SerialRgbImage> {
        match *self {
            DynamicSerialImage::ImageRgb8(ref p) => Some(p),
            _ => None,
        }
    }

    /// Return a mutable reference to an 8bit RGB image
    pub fn as_mut_rgb8(&mut self) -> Option<&mut SerialRgbImage> {
        match *self {
            DynamicSerialImage::ImageRgb8(ref mut p) => Some(p),
            _ => None,
        }
    }

    /// Return a reference to an 8bit RGBA image
    pub fn as_rgba8(&self) -> Option<&SerialRgbaImage> {
        match *self {
            DynamicSerialImage::ImageRgba8(ref p) => Some(p),
            _ => None,
        }
    }

    /// Return a mutable reference to an 8bit RGBA image
    pub fn as_mut_rgba8(&mut self) -> Option<&mut SerialRgbaImage> {
        match *self {
            DynamicSerialImage::ImageRgba8(ref mut p) => Some(p),
            _ => None,
        }
    }

    /// Return a reference to an 8bit Grayscale image
    pub fn as_luma8(&self) -> Option<&SerialGrayImage> {
        match *self {
            DynamicSerialImage::ImageLuma8(ref p) => Some(p),
            _ => None,
        }
    }

    /// Return a mutable reference to an 8bit Grayscale image
    pub fn as_mut_luma8(&mut self) -> Option<&mut SerialGrayImage> {
        match *self {
            DynamicSerialImage::ImageLuma8(ref mut p) => Some(p),
            _ => None,
        }
    }

    /// Return a reference to an 8bit Grayscale image with an alpha channel
    pub fn as_luma_alpha8(&self) -> Option<&SerialGrayAlphaImage> {
        match *self {
            DynamicSerialImage::ImageLumaA8(ref p) => Some(p),
            _ => None,
        }
    }

    /// Return a mutable reference to an 8bit Grayscale image with an alpha channel
    pub fn as_mut_luma_alpha8(&mut self) -> Option<&mut SerialGrayAlphaImage> {
        match *self {
            DynamicSerialImage::ImageLumaA8(ref mut p) => Some(p),
            _ => None,
        }
    }

    /// Return a reference to an 16bit RGB image
    pub fn as_rgb16(&self) -> Option<&SerialRgb16Image> {
        match *self {
            DynamicSerialImage::ImageRgb16(ref p) => Some(p),
            _ => None,
        }
    }

    /// Return a mutable reference to an 16bit RGB image
    pub fn as_mut_rgb16(&mut self) -> Option<&mut SerialRgb16Image> {
        match *self {
            DynamicSerialImage::ImageRgb16(ref mut p) => Some(p),
            _ => None,
        }
    }

    /// Return a reference to an 16bit RGBA image
    pub fn as_rgba16(&self) -> Option<&SerialRgba16Image> {
        match *self {
            DynamicSerialImage::ImageRgba16(ref p) => Some(p),
            _ => None,
        }
    }

    /// Return a mutable reference to an 16bit RGBA image
    pub fn as_mut_rgba16(&mut self) -> Option<&mut SerialRgba16Image> {
        match *self {
            DynamicSerialImage::ImageRgba16(ref mut p) => Some(p),
            _ => None,
        }
    }

    /// Return a reference to an 32bit RGB image
    pub fn as_rgb32f(&self) -> Option<&SerialRgb32FImage> {
        match *self {
            DynamicSerialImage::ImageRgb32F(ref p) => Some(p),
            _ => None,
        }
    }

    /// Return a mutable reference to an 32bit RGB image
    pub fn as_mut_rgb32f(&mut self) -> Option<&mut SerialRgb32FImage> {
        match *self {
            DynamicSerialImage::ImageRgb32F(ref mut p) => Some(p),
            _ => None,
        }
    }

    /// Return a reference to an 32bit RGBA image
    pub fn as_rgba32f(&self) -> Option<&SerialRgba32FImage> {
        match *self {
            DynamicSerialImage::ImageRgba32F(ref p) => Some(p),
            _ => None,
        }
    }

    /// Return a mutable reference to an 16bit RGBA image
    pub fn as_mut_rgba32f(&mut self) -> Option<&mut SerialRgba32FImage> {
        match *self {
            DynamicSerialImage::ImageRgba32F(ref mut p) => Some(p),
            _ => None,
        }
    }

    /// Return a reference to an 16bit Grayscale image
    pub fn as_luma16(&self) -> Option<&SerialGray16Image> {
        match *self {
            DynamicSerialImage::ImageLuma16(ref p) => Some(p),
            _ => None,
        }
    }

    /// Return a mutable reference to an 16bit Grayscale image
    pub fn as_mut_luma16(&mut self) -> Option<&mut SerialGray16Image> {
        match *self {
            DynamicSerialImage::ImageLuma16(ref mut p) => Some(p),
            _ => None,
        }
    }

    /// Return a reference to an 16bit Grayscale image with an alpha channel
    pub fn as_luma_alpha16(&self) -> Option<&SerialGrayAlpha16Image> {
        match *self {
            DynamicSerialImage::ImageLumaA16(ref p) => Some(p),
            _ => None,
        }
    }

    /// Return a mutable reference to an 16bit Grayscale image with an alpha channel
    pub fn as_mut_luma_alpha16(&mut self) -> Option<&mut SerialGrayAlpha16Image> {
        match *self {
            DynamicSerialImage::ImageLumaA16(ref mut p) => Some(p),
            _ => None,
        }
    }

    /// Return a view on the raw sample buffer for 8 bit per channel images.
    pub fn as_flat_samples_u8(&self) -> Option<FlatSamples<&[u8]>> {
        match *self {
            DynamicSerialImage::ImageLuma8(ref p) => Some(p.as_flat_samples()),
            DynamicSerialImage::ImageLumaA8(ref p) => Some(p.as_flat_samples()),
            DynamicSerialImage::ImageRgb8(ref p) => Some(p.as_flat_samples()),
            DynamicSerialImage::ImageRgba8(ref p) => Some(p.as_flat_samples()),
            _ => None,
        }
    }

    /// Return a view on the raw sample buffer for 16 bit per channel images.
    pub fn as_flat_samples_u16(&self) -> Option<FlatSamples<&[u16]>> {
        match *self {
            DynamicSerialImage::ImageLuma16(ref p) => Some(p.as_flat_samples()),
            DynamicSerialImage::ImageLumaA16(ref p) => Some(p.as_flat_samples()),
            DynamicSerialImage::ImageRgb16(ref p) => Some(p.as_flat_samples()),
            DynamicSerialImage::ImageRgba16(ref p) => Some(p.as_flat_samples()),
            _ => None,
        }
    }

    /// Return a view on the raw sample buffer for 32bit per channel images.
    pub fn as_flat_samples_f32(&self) -> Option<FlatSamples<&[f32]>> {
        match *self {
            DynamicSerialImage::ImageRgb32F(ref p) => Some(p.as_flat_samples()),
            DynamicSerialImage::ImageRgba32F(ref p) => Some(p.as_flat_samples()),
            _ => None,
        }
    }

    /// Return this image's pixels as a native endian byte slice.
    pub fn as_bytes(&self) -> &[u8] {
        // we can do this because every variant contains an `SerialImageBuffer<_, Vec<_>>`
        dynamic_map!(
            *self,
            ref image_buffer,
            bytemuck::cast_slice(image_buffer.as_raw().as_ref())
        )
    }

    // TODO: choose a name under which to expose?
    fn inner_bytes(&self) -> &[u8] {
        // we can do this because every variant contains an `SerialImageBuffer<_, Vec<_>>`
        dynamic_map!(
            *self,
            ref image_buffer,
            bytemuck::cast_slice(image_buffer.inner_pixels())
        )
    }

    #[cfg(feature = "fitsio")]
    /// Save the image data to a FITS file. The file name
    /// will be of the form `{file_prefix}_{yyyymmdd}_{hhmmss}.fits`.
    ///
    /// ### Note
    /// If compression is enabled, the compressed image data is stored
    /// in HDU 1 (IMAGE), while the uncompressed data is stored in the
    /// primary HDU. HDU 1 is created only if compression is enabled.
    /// The HDU containing the image also contains all the necessary
    /// metadata. In case compression is enabled, the primary HDU contains
    /// a key `COMPRESSED_IMAGE` with value `T` to indicate that the compressed
    /// image data is present in HDU 1.
    ///
    /// # Arguments
    ///  * `dir_prefix` - The directory where the file will be saved.
    ///  * `file_prefix` - The prefix of the file name. The file name will be of the form `{file_prefix}_{yyyymmdd}_{hhmmss}.fits`.
    ///  * `progname` - The name of the program that generated the image.
    ///  * `compress` - Whether to compress the FITS file. Compression uses the GZIP algorithm.
    ///  * `overwrite` - Whether to overwrite the file if it already exists.
    ///
    /// # Errors
    ///  * [`fitsio::errors::Error`] with the error description.
    pub fn savefits(
        &self,
        dir_prefix: &Path,
        file_prefix: &str,
        progname: Option<&str>,
        compress: FitsCompression,
        overwrite: bool,
    ) -> Result<PathBuf, FitsError> {
        match self {
            DynamicSerialImage::ImageLuma8(p) => p.savefits(
                dir_prefix,
                file_prefix,
                progname,
                compress,
                overwrite,
            ),
            DynamicSerialImage::ImageLumaA8(p) => p.savefits(
                dir_prefix,
                file_prefix,
                progname,
                compress,
                overwrite,
            ),
            DynamicSerialImage::ImageRgb8(p) => p.savefits(
                dir_prefix,
                file_prefix,
                progname,
                compress,
                overwrite,
            ),
            DynamicSerialImage::ImageRgba8(p) => p.savefits(
                dir_prefix,
                file_prefix,
                progname,
                compress,
                overwrite,
            ),
            DynamicSerialImage::ImageLuma16(p) => p.savefits(
                dir_prefix,
                file_prefix,
                progname,
                compress,
                overwrite,
            ),
            DynamicSerialImage::ImageLumaA16(p) => p.savefits(
                dir_prefix,
                file_prefix,
                progname,
                compress,
                overwrite,
            ),
            DynamicSerialImage::ImageRgb16(p) => p.savefits(
                dir_prefix,
                file_prefix,
                progname,
                compress,
                overwrite,
            ),
            DynamicSerialImage::ImageRgba16(p) => p.savefits(
                dir_prefix,
                file_prefix,
                progname,
                compress,
                overwrite,
            ),
            DynamicSerialImage::ImageRgb32F(p) => p.savefits(
                dir_prefix,
                file_prefix,
                progname,
                compress,
                overwrite,
            ),
            DynamicSerialImage::ImageRgba32F(p) => p.savefits(
                dir_prefix,
                file_prefix,
                progname,
                compress,
                overwrite,
            ),

        }
    }

    /// Return this image's pixels as a byte vector. If the `SerialImageBuffer`
    /// container is `Vec<u8>`, this operation is free. Otherwise, a copy
    /// is returned.
    pub fn into_bytes(self) -> Vec<u8> {
        // we can do this because every variant contains an `SerialImageBuffer<_, Vec<_>>`
        dynamic_map!(self, image_buffer, {
            match bytemuck::allocation::try_cast_vec(image_buffer.into_raw()) {
                Ok(vec) => vec,
                Err((_, vec)) => {
                    // Fallback: vector requires an exact alignment and size match
                    // Reuse of the allocation as done in the Ok branch only works if the
                    // underlying container is exactly Vec<u8> (or compatible but that's the only
                    // alternative at the time of writing).
                    // In all other cases we must allocate a new vector with the 'same' contents.
                    bytemuck::cast_slice(&vec).to_owned()
                }
            }
        })
    }

    /// Return this image's color type.
    pub fn color(&self) -> color::ColorType {
        match *self {
            DynamicSerialImage::ImageLuma8(_) => color::ColorType::L8,
            DynamicSerialImage::ImageLumaA8(_) => color::ColorType::La8,
            DynamicSerialImage::ImageRgb8(_) => color::ColorType::Rgb8,
            DynamicSerialImage::ImageRgba8(_) => color::ColorType::Rgba8,
            DynamicSerialImage::ImageLuma16(_) => color::ColorType::L16,
            DynamicSerialImage::ImageLumaA16(_) => color::ColorType::La16,
            DynamicSerialImage::ImageRgb16(_) => color::ColorType::Rgb16,
            DynamicSerialImage::ImageRgba16(_) => color::ColorType::Rgba16,
            DynamicSerialImage::ImageRgb32F(_) => color::ColorType::Rgb32F,
            DynamicSerialImage::ImageRgba32F(_) => color::ColorType::Rgba32F,
        }
    }

    /// Returns the width of the underlying image
    pub fn width(&self) -> u32 {
        dynamic_map!(*self, ref p, { p.width() })
    }

    /// Returns the height of the underlying image
    pub fn height(&self) -> u32 {
        dynamic_map!(*self, ref p, { p.height() })
    }

    /// Return a grayscale version of this image.
    /// Returns `Luma` images in most cases. However, for `f32` images,
    /// this will return a grayscale `Rgb/Rgba` image instead.
    pub fn grayscale(&self) -> DynamicSerialImage {
        match *self {
            DynamicSerialImage::ImageLuma8(ref p) => DynamicSerialImage::ImageLuma8(p.clone()),
            DynamicSerialImage::ImageLumaA8(ref p) => {
                DynamicSerialImage::ImageLumaA8(imageops::grayscale_alpha(p))
            }
            DynamicSerialImage::ImageRgb8(ref p) => {
                DynamicSerialImage::ImageLuma8(imageops::grayscale(p))
            }
            DynamicSerialImage::ImageRgba8(ref p) => {
                DynamicSerialImage::ImageLumaA8(imageops::grayscale_alpha(p))
            }
            DynamicSerialImage::ImageLuma16(ref p) => DynamicSerialImage::ImageLuma16(p.clone()),
            DynamicSerialImage::ImageLumaA16(ref p) => {
                DynamicSerialImage::ImageLumaA16(imageops::grayscale_alpha(p))
            }
            DynamicSerialImage::ImageRgb16(ref p) => {
                DynamicSerialImage::ImageLuma16(imageops::grayscale(p))
            }
            DynamicSerialImage::ImageRgba16(ref p) => {
                DynamicSerialImage::ImageLumaA16(imageops::grayscale_alpha(p))
            }
            DynamicSerialImage::ImageRgb32F(ref p) => {
                DynamicSerialImage::ImageRgb32F(imageops::grayscale_with_type(p))
            }
            DynamicSerialImage::ImageRgba32F(ref p) => {
                DynamicSerialImage::ImageRgba32F(imageops::grayscale_with_type_alpha(p))
            }
        }
    }

    /// Invert the colors of this image.
    /// This method operates inplace.
    pub fn invert(&mut self) {
        dynamic_map!(*self, ref mut p, imageops::invert(p))
    }

    /// Resize this image using the specified filter algorithm.
    /// Returns a new image. The image's aspect ratio is preserved.
    /// The image is scaled to the maximum possible size that fits
    /// within the bounds specified by `nwidth` and `nheight`.
    pub fn resize(
        &self,
        nwidth: u32,
        nheight: u32,
        filter: imageops::FilterType,
    ) -> DynamicSerialImage {
        if (nwidth, nheight) == self.dimensions() {
            return self.clone();
        }
        let (width2, height2) =
            resize_dimensions(self.width(), self.height(), nwidth, nheight, false);

        self.resize_exact(width2, height2, filter)
    }

    /// Resize this image using the specified filter algorithm.
    /// Returns a new image. Does not preserve aspect ratio.
    /// `nwidth` and `nheight` are the new image's dimensions
    pub fn resize_exact(
        &self,
        nwidth: u32,
        nheight: u32,
        filter: imageops::FilterType,
    ) -> DynamicSerialImage {
        dynamic_map!(*self, ref p => imageops::resize(p, nwidth, nheight, filter))
    }

    /// Scale this image down to fit within a specific size.
    /// Returns a new image. The image's aspect ratio is preserved.
    /// The image is scaled to the maximum possible size that fits
    /// within the bounds specified by `nwidth` and `nheight`.
    ///
    /// This method uses a fast integer algorithm where each source
    /// pixel contributes to exactly one target pixel.
    /// May give aliasing artifacts if new size is close to old size.
    pub fn thumbnail(&self, nwidth: u32, nheight: u32) -> DynamicSerialImage {
        let (width2, height2) =
            resize_dimensions(self.width(), self.height(), nwidth, nheight, false);
        self.thumbnail_exact(width2, height2)
    }

    /// Scale this image down to a specific size.
    /// Returns a new image. Does not preserve aspect ratio.
    /// `nwidth` and `nheight` are the new image's dimensions.
    /// This method uses a fast integer algorithm where each source
    /// pixel contributes to exactly one target pixel.
    /// May give aliasing artifacts if new size is close to old size.
    pub fn thumbnail_exact(&self, nwidth: u32, nheight: u32) -> DynamicSerialImage {
        dynamic_map!(*self, ref p => imageops::thumbnail(p, nwidth, nheight))
    }

    /// Resize this image using the specified filter algorithm.
    /// Returns a new image. The image's aspect ratio is preserved.
    /// The image is scaled to the maximum possible size that fits
    /// within the larger (relative to aspect ratio) of the bounds
    /// specified by `nwidth` and `nheight`, then cropped to
    /// fit within the other bound.
    pub fn resize_to_fill(
        &self,
        nwidth: u32,
        nheight: u32,
        filter: imageops::FilterType,
    ) -> DynamicSerialImage {
        let (width2, height2) =
            resize_dimensions(self.width(), self.height(), nwidth, nheight, true);

        let mut intermediate = self.resize_exact(width2, height2, filter);
        let (iwidth, iheight) = intermediate.dimensions();
        let ratio = u64::from(iwidth) * u64::from(nheight);
        let nratio = u64::from(nwidth) * u64::from(iheight);

        if nratio > ratio {
            intermediate.crop(0, (iheight - nheight) / 2, nwidth, nheight)
        } else {
            intermediate.crop((iwidth - nwidth) / 2, 0, nwidth, nheight)
        }
    }

    /// Performs a Gaussian blur on this image.
    /// `sigma` is a measure of how much to blur by.
    pub fn blur(&self, sigma: f32) -> DynamicSerialImage {
        dynamic_map!(*self, ref p => imageops::blur(p, sigma))
    }

    /// Performs an unsharpen mask on this image.
    /// `sigma` is the amount to blur the image by.
    /// `threshold` is a control of how much to sharpen.
    ///
    /// See <https://en.wikipedia.org/wiki/Unsharp_masking#Digital_unsharp_masking>
    pub fn unsharpen(&self, sigma: f32, threshold: i32) -> DynamicSerialImage {
        dynamic_map!(*self, ref p => imageops::unsharpen(p, sigma, threshold))
    }

    /// Filters this image with the specified 3x3 kernel.
    pub fn filter3x3(&self, kernel: &[f32]) -> DynamicSerialImage {
        if kernel.len() != 9 {
            panic!("filter must be 3 x 3")
        }

        dynamic_map!(*self, ref p => imageops::filter3x3(p, kernel))
    }

    /// Adjust the contrast of this image.
    /// `contrast` is the amount to adjust the contrast by.
    /// Negative values decrease the contrast and positive values increase the contrast.
    pub fn adjust_contrast(&self, c: f32) -> DynamicSerialImage {
        dynamic_map!(*self, ref p => imageops::contrast(p, c))
    }

    /// Brighten the pixels of this image.
    /// `value` is the amount to brighten each pixel by.
    /// Negative values decrease the brightness and positive values increase it.
    pub fn brighten(&self, value: i32) -> DynamicSerialImage {
        dynamic_map!(*self, ref p => imageops::brighten(p, value))
    }

    /// Hue rotate the supplied image.
    /// `value` is the degrees to rotate each pixel by.
    /// 0 and 360 do nothing, the rest rotates by the given degree value.
    /// just like the css webkit filter hue-rotate(180)
    pub fn huerotate(&self, value: i32) -> DynamicSerialImage {
        dynamic_map!(*self, ref p => imageops::huerotate(p, value))
    }

    /// Flip this image vertically
    pub fn flipv(&self) -> DynamicSerialImage {
        dynamic_map!(*self, ref p => imageops::flip_vertical(p))
    }

    /// Flip this image horizontally
    pub fn fliph(&self) -> DynamicSerialImage {
        dynamic_map!(*self, ref p => imageops::flip_horizontal(p))
    }

    /// Rotate this image 90 degrees clockwise.
    pub fn rotate90(&self) -> DynamicSerialImage {
        dynamic_map!(*self, ref p => imageops::rotate90(p))
    }

    /// Rotate this image 180 degrees clockwise.
    pub fn rotate180(&self) -> DynamicSerialImage {
        dynamic_map!(*self, ref p => imageops::rotate180(p))
    }

    /// Rotate this image 270 degrees clockwise.
    pub fn rotate270(&self) -> DynamicSerialImage {
        dynamic_map!(*self, ref p => imageops::rotate270(p))
    }

    /// Encode this image and write it to ```w```.
    ///
    /// Assumes the writer is buffered. In most cases,
    /// you should wrap your writer in a `BufWriter` for best performance.
    pub fn write_to<W: Write + Seek>(&self, w: &mut W, format: ImageFormat) -> ImageResult<()> {
        let bytes = self.inner_bytes();
        let (width, height) = self.dimensions();
        let color: ExtendedColorType = self.color().into();

        // TODO do not repeat this match statement across the crate

        #[allow(deprecated)]
        match format {
            #[cfg(feature = "png")]
            ImageFormat::Png => {
                let p = png::PngEncoder::new(w);
                p.write_image(bytes, width, height, color)?;
                Ok(())
            }

            #[cfg(feature = "gif")]
            ImageFormat::Gif => {
                let mut g = gif::GifEncoder::new(w);
                g.encode_frame(crate::animation::Frame::new(self.to_rgba8()))?;
                Ok(())
            }

            format => write_buffer_with_format(w, bytes, width, height, color, format),
        }
    }

    /// Encode this image with the provided encoder.
    pub fn write_with_encoder(&self, encoder: impl ImageEncoder) -> ImageResult<()> {
        dynamic_map!(self, ref p, p.write_with_encoder(encoder))
    }

    /// Saves the buffer to a file at the path specified.
    ///
    /// The image format is derived from the file extension.
    pub fn save<Q>(&self, path: Q) -> ImageResult<()>
    where
        Q: AsRef<Path>,
    {
        dynamic_map!(*self, ref p, p.save(path))
    }

    /// Saves the buffer to a file at the specified path in
    /// the specified format.
    ///
    /// See [`save_buffer_with_format`](fn.save_buffer_with_format.html) for
    /// supported types.
    pub fn save_with_format<Q>(&self, path: Q, format: ImageFormat) -> ImageResult<()>
    where
        Q: AsRef<Path>,
    {
        dynamic_map!(*self, ref p, p.save_with_format(path, format))
    }
}

impl From<SerialGrayImage> for DynamicSerialImage {
    fn from(image: SerialGrayImage) -> Self {
        DynamicSerialImage::ImageLuma8(image)
    }
}

impl From<SerialGrayAlphaImage> for DynamicSerialImage {
    fn from(image: SerialGrayAlphaImage) -> Self {
        DynamicSerialImage::ImageLumaA8(image)
    }
}

impl From<SerialRgbImage> for DynamicSerialImage {
    fn from(image: SerialRgbImage) -> Self {
        DynamicSerialImage::ImageRgb8(image)
    }
}

impl From<SerialRgbaImage> for DynamicSerialImage {
    fn from(image: SerialRgbaImage) -> Self {
        DynamicSerialImage::ImageRgba8(image)
    }
}

impl From<SerialGray16Image> for DynamicSerialImage {
    fn from(image: SerialGray16Image) -> Self {
        DynamicSerialImage::ImageLuma16(image)
    }
}

impl From<SerialGrayAlpha16Image> for DynamicSerialImage {
    fn from(image: SerialGrayAlpha16Image) -> Self {
        DynamicSerialImage::ImageLumaA16(image)
    }
}

impl From<SerialRgb16Image> for DynamicSerialImage {
    fn from(image: SerialRgb16Image) -> Self {
        DynamicSerialImage::ImageRgb16(image)
    }
}

impl From<SerialRgba16Image> for DynamicSerialImage {
    fn from(image: SerialRgba16Image) -> Self {
        DynamicSerialImage::ImageRgba16(image)
    }
}

impl From<SerialRgb32FImage> for DynamicSerialImage {
    fn from(image: SerialRgb32FImage) -> Self {
        DynamicSerialImage::ImageRgb32F(image)
    }
}

impl From<SerialRgba32FImage> for DynamicSerialImage {
    fn from(image: SerialRgba32FImage) -> Self {
        DynamicSerialImage::ImageRgba32F(image)
    }
}

impl From<SerialImageBuffer<Luma<f32>, Vec<f32>>> for DynamicSerialImage {
    fn from(image: SerialImageBuffer<Luma<f32>, Vec<f32>>) -> Self {
        DynamicSerialImage::ImageRgb32F(image.convert())
    }
}

impl From<SerialImageBuffer<LumaA<f32>, Vec<f32>>> for DynamicSerialImage {
    fn from(image: SerialImageBuffer<LumaA<f32>, Vec<f32>>) -> Self {
        DynamicSerialImage::ImageRgba32F(image.convert())
    }
}

#[allow(deprecated)]
impl GenericImageView for DynamicSerialImage {
    type Pixel = color::Rgba<u8>; // TODO use f32 as default for best precision and unbounded color?

    fn dimensions(&self) -> (u32, u32) {
        dynamic_map!(*self, ref p, p.dimensions())
    }

    fn get_pixel(&self, x: u32, y: u32) -> color::Rgba<u8> {
        dynamic_map!(*self, ref p, p.get_pixel(x, y).to_rgba().into_color())
    }
}

#[allow(deprecated)]
impl SerialGenericImage for DynamicSerialImage {
    fn put_pixel(&mut self, x: u32, y: u32, pixel: color::Rgba<u8>) {
        match *self {
            DynamicSerialImage::ImageLuma8(ref mut p) => p.put_pixel(x, y, pixel.to_luma()),
            DynamicSerialImage::ImageLumaA8(ref mut p) => p.put_pixel(x, y, pixel.to_luma_alpha()),
            DynamicSerialImage::ImageRgb8(ref mut p) => p.put_pixel(x, y, pixel.to_rgb()),
            DynamicSerialImage::ImageRgba8(ref mut p) => p.put_pixel(x, y, pixel),
            DynamicSerialImage::ImageLuma16(ref mut p) => {
                p.put_pixel(x, y, pixel.to_luma().into_color())
            }
            DynamicSerialImage::ImageLumaA16(ref mut p) => {
                p.put_pixel(x, y, pixel.to_luma_alpha().into_color())
            }
            DynamicSerialImage::ImageRgb16(ref mut p) => {
                p.put_pixel(x, y, pixel.to_rgb().into_color())
            }
            DynamicSerialImage::ImageRgba16(ref mut p) => p.put_pixel(x, y, pixel.into_color()),
            DynamicSerialImage::ImageRgb32F(ref mut p) => {
                p.put_pixel(x, y, pixel.to_rgb().into_color())
            }
            DynamicSerialImage::ImageRgba32F(ref mut p) => p.put_pixel(x, y, pixel.into_color()),
        }
    }

    fn blend_pixel(&mut self, x: u32, y: u32, pixel: color::Rgba<u8>) {
        match *self {
            DynamicSerialImage::ImageLuma8(ref mut p) => p.blend_pixel(x, y, pixel.to_luma()),
            DynamicSerialImage::ImageLumaA8(ref mut p) => {
                p.blend_pixel(x, y, pixel.to_luma_alpha())
            }
            DynamicSerialImage::ImageRgb8(ref mut p) => p.blend_pixel(x, y, pixel.to_rgb()),
            DynamicSerialImage::ImageRgba8(ref mut p) => p.blend_pixel(x, y, pixel),
            DynamicSerialImage::ImageLuma16(ref mut p) => {
                p.blend_pixel(x, y, pixel.to_luma().into_color())
            }
            DynamicSerialImage::ImageLumaA16(ref mut p) => {
                p.blend_pixel(x, y, pixel.to_luma_alpha().into_color())
            }
            DynamicSerialImage::ImageRgb16(ref mut p) => {
                p.blend_pixel(x, y, pixel.to_rgb().into_color())
            }
            DynamicSerialImage::ImageRgba16(ref mut p) => p.blend_pixel(x, y, pixel.into_color()),
            DynamicSerialImage::ImageRgb32F(ref mut p) => {
                p.blend_pixel(x, y, pixel.to_rgb().into_color())
            }
            DynamicSerialImage::ImageRgba32F(ref mut p) => p.blend_pixel(x, y, pixel.into_color()),
        }
    }

    /// Do not use is function: It is unimplemented!
    fn get_pixel_mut(&mut self, _: u32, _: u32) -> &mut color::Rgba<u8> {
        unimplemented!()
    }
}

impl Default for DynamicSerialImage {
    fn default() -> Self {
        Self::ImageRgba8(Default::default())
    }
}

/// Decodes an image and stores it into a dynamic image
fn decoder_to_image<I: ImageDecoder>(decoder: I) -> ImageResult<DynamicSerialImage> {
    let (w, h) = decoder.dimensions();
    let color_type = decoder.color_type();

    let image = match color_type {
        color::ColorType::Rgb8 => {
            let buf = image::decoder_to_vec(decoder)?;
            SerialImageBuffer::from_raw(w, h, buf).map(DynamicSerialImage::ImageRgb8)
        }

        color::ColorType::Rgba8 => {
            let buf = image::decoder_to_vec(decoder)?;
            SerialImageBuffer::from_raw(w, h, buf).map(DynamicSerialImage::ImageRgba8)
        }

        color::ColorType::L8 => {
            let buf = image::decoder_to_vec(decoder)?;
            SerialImageBuffer::from_raw(w, h, buf).map(DynamicSerialImage::ImageLuma8)
        }

        color::ColorType::La8 => {
            let buf = image::decoder_to_vec(decoder)?;
            SerialImageBuffer::from_raw(w, h, buf).map(DynamicSerialImage::ImageLumaA8)
        }

        color::ColorType::Rgb16 => {
            let buf = image::decoder_to_vec(decoder)?;
            SerialImageBuffer::from_raw(w, h, buf).map(DynamicSerialImage::ImageRgb16)
        }

        color::ColorType::Rgba16 => {
            let buf = image::decoder_to_vec(decoder)?;
            SerialImageBuffer::from_raw(w, h, buf).map(DynamicSerialImage::ImageRgba16)
        }

        color::ColorType::Rgb32F => {
            let buf = image::decoder_to_vec(decoder)?;
            SerialImageBuffer::from_raw(w, h, buf).map(DynamicSerialImage::ImageRgb32F)
        }

        color::ColorType::Rgba32F => {
            let buf = image::decoder_to_vec(decoder)?;
            SerialImageBuffer::from_raw(w, h, buf).map(DynamicSerialImage::ImageRgba32F)
        }

        color::ColorType::L16 => {
            let buf = image::decoder_to_vec(decoder)?;
            SerialImageBuffer::from_raw(w, h, buf).map(DynamicSerialImage::ImageLuma16)
        }

        color::ColorType::La16 => {
            let buf = image::decoder_to_vec(decoder)?;
            SerialImageBuffer::from_raw(w, h, buf).map(DynamicSerialImage::ImageLumaA16)
        }
    };

    match image {
        Some(image) => Ok(image),
        None => Err(ImageError::Parameter(ParameterError::from_kind(
            ParameterErrorKind::DimensionMismatch,
        ))),
    }
}

/// Open the image located at the path specified.
/// The image's format is determined from the path's file extension.
///
/// Try [`io::Reader`] for more advanced uses, including guessing the format based on the file's
/// content before its path.
///
/// [`io::Reader`]: io/struct.Reader.html
pub fn open<P>(path: P) -> ImageResult<DynamicSerialImage>
where
    P: AsRef<Path>,
{
    crate::io::Reader::open(path)?.decode()
}

/// Read a tuple containing the (width, height) of the image located at the specified path.
/// This is faster than fully loading the image and then getting its dimensions.
///
/// Try [`io::Reader`] for more advanced uses, including guessing the format based on the file's
/// content before its path or manually supplying the format.
///
/// [`io::Reader`]: io/struct.Reader.html
pub fn image_dimensions<P>(path: P) -> ImageResult<(u32, u32)>
where
    P: AsRef<Path>,
{
    crate::io::Reader::open(path)?.into_dimensions()
}

/// Saves the supplied buffer to a file at the path specified.
///
/// The image format is derived from the file extension. The buffer is assumed to have
/// the correct format according to the specified color type.
///
/// This will lead to corrupted files if the buffer contains malformed data. Currently only
/// jpeg, png, ico, pnm, bmp, exr and tiff files are supported.
pub fn save_buffer(
    path: impl AsRef<Path>,
    buf: &[u8],
    width: u32,
    height: u32,
    color: impl Into<ExtendedColorType>,
) -> ImageResult<()> {
    // thin wrapper function to strip generics before calling save_buffer_impl
    free_functions::save_buffer_impl(path.as_ref(), buf, width, height, color.into())
}

/// Saves the supplied buffer to a file at the path specified
/// in the specified format.
///
/// The buffer is assumed to have the correct format according
/// to the specified color type.
/// This will lead to corrupted files if the buffer contains
/// malformed data. Currently only jpeg, png, ico, bmp, exr and
/// tiff files are supported.
pub fn save_buffer_with_format(
    path: impl AsRef<Path>,
    buf: &[u8],
    width: u32,
    height: u32,
    color: impl Into<ExtendedColorType>,
    format: ImageFormat,
) -> ImageResult<()> {
    // thin wrapper function to strip generics
    free_functions::save_buffer_with_format_impl(
        path.as_ref(),
        buf,
        width,
        height,
        color.into(),
        format,
    )
}

/// Writes the supplied buffer to a writer in the specified format.
///
/// The buffer is assumed to have the correct format according to the specified color type. This
/// will lead to corrupted writers if the buffer contains malformed data.
///
/// Assumes the writer is buffered. In most cases, you should wrap your writer in a `BufWriter` for
/// best performance.
pub fn write_buffer_with_format<W: Write + Seek>(
    buffered_writer: &mut W,
    buf: &[u8],
    width: u32,
    height: u32,
    color: impl Into<ExtendedColorType>,
    format: ImageFormat,
) -> ImageResult<()> {
    // thin wrapper function to strip generics
    free_functions::write_buffer_impl(buffered_writer, buf, width, height, color.into(), format)
}

/// Create a new image from a byte slice
///
/// Makes an educated guess about the image format.
/// TGA is not supported by this function.
///
/// Try [`io::Reader`] for more advanced uses.
///
/// [`io::Reader`]: io/struct.Reader.html
pub fn load_from_memory(buffer: &[u8]) -> ImageResult<DynamicSerialImage> {
    let format = free_functions::guess_format(buffer)?;
    load_from_memory_with_format(buffer, format)
}

/// Create a new image from a byte slice
///
/// This is just a simple wrapper that constructs an `std::io::Cursor` around the buffer and then
/// calls `load` with that reader.
///
/// Try [`io::Reader`] for more advanced uses.
///
/// [`load`]: fn.load.html
/// [`io::Reader`]: io/struct.Reader.html
#[inline(always)]
pub fn load_from_memory_with_format(
    buf: &[u8],
    format: ImageFormat,
) -> ImageResult<DynamicSerialImage> {
    let b = io::Cursor::new(buf);
    free_functions::load(b, format)
}

#[cfg(test)]
mod bench {
    #[bench]
    #[cfg(feature = "benchmarks")]
    fn bench_conversion(b: &mut test::Bencher) {
        let a = super::DynamicSerialImage::ImageRgb8(crate::SerialImageBuffer::new(1000, 1000));
        b.iter(|| a.to_luma8());
        b.bytes = 1000 * 1000 * 3
    }
}

#[cfg(test)]
mod test {
    use crate::color::ColorType;

    #[test]
    fn test_empty_file() {
        assert!(super::load_from_memory(b"").is_err());
    }

    #[cfg(feature = "jpeg")]
    #[test]
    fn image_dimensions() {
        let im_path = "./tests/images/jpg/progressive/cat.jpg";
        let dims = super::image_dimensions(im_path).unwrap();
        assert_eq!(dims, (320, 240));
    }

    #[cfg(feature = "png")]
    #[test]
    fn open_16bpc_png() {
        let im_path = "./tests/images/png/16bpc/basn6a16.png";
        let image = super::open(im_path).unwrap();
        assert_eq!(image.color(), ColorType::Rgba16);
    }

    fn test_grayscale(mut img: super::DynamicSerialImage, alpha_discarded: bool) {
        use crate::image::{GenericImageView, SerialGenericImage};
        img.put_pixel(0, 0, crate::color::Rgba([255, 0, 0, 100]));
        let expected_alpha = if alpha_discarded { 255 } else { 100 };
        assert_eq!(
            img.grayscale().get_pixel(0, 0),
            crate::color::Rgba([54, 54, 54, expected_alpha])
        );
    }

    fn test_grayscale_alpha_discarded(img: super::DynamicSerialImage) {
        test_grayscale(img, true);
    }

    fn test_grayscale_alpha_preserved(img: super::DynamicSerialImage) {
        test_grayscale(img, false);
    }

    #[test]
    fn test_grayscale_luma8() {
        test_grayscale_alpha_discarded(super::DynamicSerialImage::new_luma8(1, 1));
        test_grayscale_alpha_discarded(super::DynamicSerialImage::new(1, 1, ColorType::L8));
    }

    #[test]
    fn test_grayscale_luma_a8() {
        test_grayscale_alpha_preserved(super::DynamicSerialImage::new_luma_a8(1, 1));
        test_grayscale_alpha_preserved(super::DynamicSerialImage::new(1, 1, ColorType::La8));
    }

    #[test]
    fn test_grayscale_rgb8() {
        test_grayscale_alpha_discarded(super::DynamicSerialImage::new_rgb8(1, 1));
        test_grayscale_alpha_discarded(super::DynamicSerialImage::new(1, 1, ColorType::Rgb8));
    }

    #[test]
    fn test_grayscale_rgba8() {
        test_grayscale_alpha_preserved(super::DynamicSerialImage::new_rgba8(1, 1));
        test_grayscale_alpha_preserved(super::DynamicSerialImage::new(1, 1, ColorType::Rgba8));
    }

    #[test]
    fn test_grayscale_luma16() {
        test_grayscale_alpha_discarded(super::DynamicSerialImage::new_luma16(1, 1));
        test_grayscale_alpha_discarded(super::DynamicSerialImage::new(1, 1, ColorType::L16));
    }

    #[test]
    fn test_grayscale_luma_a16() {
        test_grayscale_alpha_preserved(super::DynamicSerialImage::new_luma_a16(1, 1));
        test_grayscale_alpha_preserved(super::DynamicSerialImage::new(1, 1, ColorType::La16));
    }

    #[test]
    fn test_grayscale_rgb16() {
        test_grayscale_alpha_discarded(super::DynamicSerialImage::new_rgb16(1, 1));
        test_grayscale_alpha_discarded(super::DynamicSerialImage::new(1, 1, ColorType::Rgb16));
    }

    #[test]
    fn test_grayscale_rgba16() {
        test_grayscale_alpha_preserved(super::DynamicSerialImage::new_rgba16(1, 1));
        test_grayscale_alpha_preserved(super::DynamicSerialImage::new(1, 1, ColorType::Rgba16));
    }

    #[test]
    fn test_grayscale_rgb32f() {
        test_grayscale_alpha_discarded(super::DynamicSerialImage::new_rgb32f(1, 1));
        test_grayscale_alpha_discarded(super::DynamicSerialImage::new(1, 1, ColorType::Rgb32F));
    }

    #[test]
    fn test_grayscale_rgba32f() {
        test_grayscale_alpha_preserved(super::DynamicSerialImage::new_rgba32f(1, 1));
        test_grayscale_alpha_preserved(super::DynamicSerialImage::new(1, 1, ColorType::Rgba32F));
    }

    #[test]
    fn test_dynamic_image_default_implementation() {
        // Test that structs wrapping a DynamicSerialImage are able to auto-derive the Default trait
        // ensures that DynamicSerialImage implements Default (if it didn't, this would cause a compile error).
        #[derive(Default)]
        struct Foo {
            _image: super::DynamicSerialImage,
        }
    }

    #[test]
    fn test_to_vecu8() {
        let _ = super::DynamicSerialImage::new_luma8(1, 1).into_bytes();
        let _ = super::DynamicSerialImage::new_luma16(1, 1).into_bytes();
    }

    #[test]
    fn issue_1705_can_turn_16bit_image_into_bytes() {
        let pixels = vec![65535u16; 64 * 64];
        let img = super::SerialImageBuffer::from_vec(64, 64, pixels).unwrap();

        let img = super::DynamicSerialImage::ImageLuma16(img);
        assert!(img.as_luma16().is_some());

        let bytes: Vec<u8> = img.into_bytes();
        assert_eq!(bytes, vec![0xFF; 64 * 64 * 2]);
    }
}
