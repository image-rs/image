#![allow(clippy::too_many_arguments)]
use std::convert::TryFrom;
use std::ffi::OsStr;
use std::io;
use std::io::Read;
use std::ops::{Deref, DerefMut};
use std::path::Path;
use std::usize;

use crate::ImageBuffer;
use crate::color::{ColorType, ExtendedColorType};
use crate::error::{ImageError, ImageFormatHint, ImageResult, LimitError, LimitErrorKind, ParameterError, ParameterErrorKind};
use crate::math::Rect;
use crate::traits::Pixel;

use crate::animation::Frames;

#[cfg(feature = "pnm")]
use crate::codecs::pnm::PnmSubtype;

/// An enumeration of supported image formats.
/// Not all formats support both encoding and decoding.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum ImageFormat {
    /// An Image in PNG Format
    Png,

    /// An Image in JPEG Format
    Jpeg,

    /// An Image in GIF Format
    Gif,

    /// An Image in WEBP Format
    WebP,

    /// An Image in general PNM Format
    Pnm,

    /// An Image in TIFF Format
    Tiff,

    /// An Image in TGA Format
    Tga,

    /// An Image in DDS Format
    Dds,

    /// An Image in BMP Format
    Bmp,

    /// An Image in ICO Format
    Ico,

    /// An Image in Radiance HDR Format
    Hdr,

    /// An Image in OpenEXR Format
    OpenExr,

    /// An Image in farbfeld Format
    Farbfeld,

    /// An Image in AVIF format.
    Avif,

    #[doc(hidden)]
    __NonExhaustive(crate::utils::NonExhaustiveMarker),
}

impl ImageFormat {
    /// Return the image format specified by a path's file extension.
    ///
    /// # Example
    ///
    /// ```
    /// use image::ImageFormat;
    ///
    /// let format = ImageFormat::from_extension("jpg");
    /// assert_eq!(format, Some(ImageFormat::Jpeg));
    /// ```
    #[inline]
    pub fn from_extension<S>(ext: S) -> Option<Self> where S: AsRef<OsStr> {
        // thin wrapper function to strip generics
        fn inner(ext: &OsStr) -> Option<ImageFormat> {
            let ext = ext.to_str()?.to_ascii_lowercase();

            Some(match ext.as_str() {
                "avif" => ImageFormat::Avif,
                "jpg" | "jpeg" => ImageFormat::Jpeg,
                "png" => ImageFormat::Png,
                "gif" => ImageFormat::Gif,
                "webp" => ImageFormat::WebP,
                "tif" | "tiff" => ImageFormat::Tiff,
                "tga" => ImageFormat::Tga,
                "dds" => ImageFormat::Dds,
                "bmp" => ImageFormat::Bmp,
                "ico" => ImageFormat::Ico,
                "hdr" => ImageFormat::Hdr,
                "exr" => ImageFormat::OpenExr,
                "pbm" | "pam" | "ppm" | "pgm" => ImageFormat::Pnm,
                "ff" | "farbfeld" => ImageFormat::Farbfeld,
                _ => return None,
            })
        }

        inner(ext.as_ref())
    }

    /// Return the image format specified by the path's file extension.
    ///
    /// # Example
    ///
    /// ```
    /// use image::ImageFormat;
    ///
    /// let format = ImageFormat::from_path("images/ferris.png")?;
    /// assert_eq!(format, ImageFormat::Png);
    ///
    /// # Ok::<(), image::error::ImageError>(())
    /// ```
    #[inline]
    pub fn from_path<P>(path: P) -> ImageResult<Self> where P : AsRef<Path> {
        // thin wrapper function to strip generics
        fn inner(path: &Path) -> ImageResult<ImageFormat> {
            let exact_ext = path.extension();
            exact_ext
                .and_then(ImageFormat::from_extension)
                .ok_or_else(|| {
                    let format_hint = match exact_ext {
                        None => ImageFormatHint::Unknown,
                        Some(os) => ImageFormatHint::PathExtension(os.into()),
                    };
                    ImageError::Unsupported(format_hint.into())
                })
        }

        inner(path.as_ref())
    }

    /// Return if the ImageFormat can be decoded by the lib.
    #[inline]
    pub fn can_read(&self) -> bool {
        // Needs to be updated once a new variant's decoder is added to free_functions.rs::load
        match self {
            ImageFormat::Png => true,
            ImageFormat::Gif => true,
            ImageFormat::Jpeg => true,
            ImageFormat::WebP => true,
            ImageFormat::Tiff => true,
            ImageFormat::Tga => true,
            ImageFormat::Dds => false,
            ImageFormat::Bmp => true,
            ImageFormat::Ico => true,
            ImageFormat::Hdr => true,
            ImageFormat::OpenExr => true,
            ImageFormat::Pnm => true,
            ImageFormat::Farbfeld => true,
            ImageFormat::Avif => true,
            ImageFormat::__NonExhaustive(marker) => match marker._private {},
        }
    }

    /// Return if the ImageFormat can be encoded by the lib.
    #[inline]
    pub fn can_write(&self) -> bool {
        // Needs to be updated once a new variant's encoder is added to free_functions.rs::save_buffer_with_format_impl
        match self {
            ImageFormat::Gif => true,
            ImageFormat::Ico => true,
            ImageFormat::Jpeg => true,
            ImageFormat::Png => true,
            ImageFormat::Bmp => true,
            ImageFormat::Tiff => true,
            ImageFormat::Tga => true,
            ImageFormat::Pnm => true,
            ImageFormat::Farbfeld => true,
            ImageFormat::Avif => true,
            ImageFormat::WebP => false,
            ImageFormat::Hdr => false,
            ImageFormat::OpenExr => true,
            ImageFormat::Dds => false,
            ImageFormat::__NonExhaustive(marker) => match marker._private {},
        }
    }

    /// Return a list of applicable extensions for this format.
    ///
    /// All currently recognized image formats specify at least on extension but for future
    /// compatibility you should not rely on this fact. The list may be empty if the format has no
    /// recognized file representation, for example in case it is used as a purely transient memory
    /// format.
    ///
    /// The method name `extensions` remains reserved for introducing another method in the future
    /// that yields a slice of `OsStr` which is blocked by several features of const evaluation.
    pub fn extensions_str(self) -> &'static [&'static str] {
        match self {
            ImageFormat::Png => &["png"],
            ImageFormat::Jpeg => &["jpg", "jpeg"],
            ImageFormat::Gif => &["gif"],
            ImageFormat::WebP => &["webp"],
            ImageFormat::Pnm => &["pbm", "pam", "ppm", "pgm"],
            ImageFormat::Tiff => &["tiff", "tif"],
            ImageFormat::Tga => &["tga"],
            ImageFormat::Dds => &["dds"],
            ImageFormat::Bmp => &["bmp"],
            ImageFormat::Ico => &["ico"],
            ImageFormat::Hdr => &["hdr"],
            ImageFormat::OpenExr => &["exr"],
            ImageFormat::Farbfeld => &["ff"],
            // According to: https://aomediacodec.github.io/av1-avif/#mime-registration
            ImageFormat::Avif => &["avif"],
            ImageFormat::__NonExhaustive(marker) => match marker._private {},
        }
    }
}

/// An enumeration of supported image formats for encoding.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ImageOutputFormat {
    #[cfg(feature = "png")]
    /// An Image in PNG Format
    Png,

    #[cfg(feature = "jpeg")]
    /// An Image in JPEG Format with specified quality
    Jpeg(u8),

    #[cfg(feature = "pnm")]
    /// An Image in one of the PNM Formats
    Pnm(PnmSubtype),

    #[cfg(feature = "gif")]
    /// An Image in GIF Format
    Gif,

    #[cfg(feature = "ico")]
    /// An Image in ICO Format
    Ico,

    #[cfg(feature = "bmp")]
    /// An Image in BMP Format
    Bmp,

    #[cfg(feature = "farbfeld")]
    /// An Image in farbfeld Format
    Farbfeld,

    #[cfg(feature = "tga")]
    /// An Image in TGA Format
    Tga,

    #[cfg(feature = "openexr")]
    /// An Image in OpenEXR Format
    OpenExr,

    #[cfg(feature = "tiff")]
    /// An Image in TIFF Format
    Tiff,

    #[cfg(feature = "avif-encoder")]
    /// An image in AVIF Format
    Avif,

    /// A value for signalling an error: An unsupported format was requested
    // Note: When TryFrom is stabilized, this value should not be needed, and
    // a TryInto<ImageOutputFormat> should be used instead of an Into<ImageOutputFormat>.
    Unsupported(String),

    #[doc(hidden)]
    __NonExhaustive(crate::utils::NonExhaustiveMarker),
}

impl From<ImageFormat> for ImageOutputFormat {
    fn from(fmt: ImageFormat) -> Self {
        match fmt {
            #[cfg(feature = "png")]
            ImageFormat::Png => ImageOutputFormat::Png,
            #[cfg(feature = "jpeg")]
            ImageFormat::Jpeg => ImageOutputFormat::Jpeg(75),
            #[cfg(feature = "pnm")]
            ImageFormat::Pnm => ImageOutputFormat::Pnm(PnmSubtype::ArbitraryMap),
            #[cfg(feature = "gif")]
            ImageFormat::Gif => ImageOutputFormat::Gif,
            #[cfg(feature = "ico")]
            ImageFormat::Ico => ImageOutputFormat::Ico,
            #[cfg(feature = "bmp")]
            ImageFormat::Bmp => ImageOutputFormat::Bmp,
            #[cfg(feature = "farbfeld")]
            ImageFormat::Farbfeld => ImageOutputFormat::Farbfeld,
            #[cfg(feature = "tga")]
            ImageFormat::Tga => ImageOutputFormat::Tga,
            #[cfg(feature = "openexr")]
            ImageFormat::OpenExr => ImageOutputFormat::OpenExr,
            #[cfg(feature = "tiff")]
            ImageFormat::Tiff => ImageOutputFormat::Tiff,

            #[cfg(feature = "avif-encoder")]
            ImageFormat::Avif => ImageOutputFormat::Avif,

            f => ImageOutputFormat::Unsupported(format!("{:?}", f)),
        }
    }
}

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
    /// Create a new ImageReadBuffer.
    ///
    /// Panics if scanline_bytes doesn't fit into a usize, because that would mean reading anything
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
            crate::copy_memory(&self.buffer[self.consumed..][..buf.len()], &mut buf[..]);
            self.consumed += buf.len();
            Ok(buf.len())
        } else {
            crate::copy_memory(&self.buffer[self.consumed..], &mut buf[..bytes_buffered]);
            self.consumed = self.buffer.len();
            Ok(bytes_buffered)
        }
    }
}

/// Decodes a specific region of the image, represented by the rectangle
/// starting from ```x``` and ```y``` and having ```length``` and ```width```
#[allow(dead_code)]
// When no image formats that use it are enabled
pub(crate) fn load_rect<'a, D, F, F1, F2, E>(x: u32, y: u32, width: u32, height: u32, buf: &mut [u8],
                                          progress_callback: F,
                                          decoder: &mut D,
                                          mut seek_scanline: F1,
                                          mut read_scanline: F2) -> ImageResult<()>
    where D: ImageDecoder<'a>,
          F: Fn(Progress),
          F1: FnMut(&mut D, u64) -> io::Result<()>,
          F2: FnMut(&mut D, &mut [u8]) -> Result<(), E>,
          ImageError: From<E>,
{
    let (x, y, width, height) = (u64::from(x), u64::from(y), u64::from(width), u64::from(height));
    let dimensions = decoder.dimensions();
    let bytes_per_pixel = u64::from(decoder.color_type().bytes_per_pixel());
    let row_bytes = bytes_per_pixel * u64::from(dimensions.0);
    let scanline_bytes = decoder.scanline_bytes();
    let total_bytes = width * height * bytes_per_pixel;

    if buf.len() < usize::try_from(total_bytes).unwrap_or(usize::max_value()) {
        panic!("output buffer too short\n expected `{}`, provided `{}`", total_bytes, buf.len());
    }

    let mut bytes_read = 0u64;
    let mut current_scanline = 0;
    let mut tmp = Vec::new();
    let mut tmp_scanline = None;

    {
        // Read a range of the image starting from byte number `start` and continuing until byte
        // number `end`. Updates `current_scanline` and `bytes_read` appropiately.
        let mut read_image_range = |mut start: u64, end: u64| -> ImageResult<()> {
            // If the first scanline we need is already stored in the temporary buffer, then handle
            // it first.
            let target_scanline = start / scanline_bytes;
            if tmp_scanline == Some(target_scanline) {
                let position = target_scanline * scanline_bytes;
                let offset = start.saturating_sub(position);
                let len = (end - start)
                    .min(scanline_bytes - offset)
                    .min(end - position);

                buf[(bytes_read as usize)..][..len as usize]
                    .copy_from_slice(&tmp[offset as usize..][..len as usize]);
                bytes_read += len;
                start += len;

                progress_callback(Progress {current: bytes_read, total: total_bytes});

                if start == end {
                    return Ok(());
                }
            }

            let target_scanline = start / scanline_bytes;
            if target_scanline != current_scanline {
                seek_scanline(decoder, target_scanline)?;
                current_scanline = target_scanline;
            }

            let mut position = current_scanline * scanline_bytes;
            while position < end {
                if position >= start && end - position >= scanline_bytes {
                    read_scanline(decoder, &mut buf[(bytes_read as usize)..]
                                                   [..(scanline_bytes as usize)])?;
                    bytes_read += scanline_bytes;
                } else {
                    tmp.resize(scanline_bytes as usize, 0u8);
                    read_scanline(decoder, &mut tmp)?;
                    tmp_scanline = Some(current_scanline);

                    let offset = start.saturating_sub(position);
                    let len = (end - start)
                        .min(scanline_bytes - offset)
                        .min(end - position);

                    buf[(bytes_read as usize)..][..len as usize]
                        .copy_from_slice(&tmp[offset as usize..][..len as usize]);
                    bytes_read += len;
                }

                current_scanline += 1;
                position += scanline_bytes;
                progress_callback(Progress {current: bytes_read, total: total_bytes});
            }
            Ok(())
        };

        if x + width > u64::from(dimensions.0) || y + height > u64::from(dimensions.1)
            || width == 0 || height == 0 {
                return Err(ImageError::Parameter(ParameterError::from_kind(
                    ParameterErrorKind::DimensionMismatch,
                )));
            }
        if scanline_bytes > usize::max_value() as u64 {
            return Err(ImageError::Limits(LimitError::from_kind(
                LimitErrorKind::InsufficientMemory,
            )));
        }

        progress_callback(Progress {current: 0, total: total_bytes});
        if x == 0 && width == u64::from(dimensions.0) {
            let start = x * bytes_per_pixel + y * row_bytes;
            let end = (x + width) * bytes_per_pixel + (y + height - 1) * row_bytes;
            read_image_range(start, end)?;
        } else {
            for row in y..(y+height) {
                let start = x * bytes_per_pixel + row * row_bytes;
                let end = (x + width) * bytes_per_pixel + row * row_bytes;
                read_image_range(start, end)?;
            }
        }
    }

    // Seek back to the start
    Ok(seek_scanline(decoder, 0)?)
}

/// Reads all of the bytes of a decoder into a Vec<T>. No particular alignment
/// of the output buffer is guaranteed.
///
/// Panics if there isn't enough memory to decode the image.
pub(crate) fn decoder_to_vec<'a, T>(decoder: impl ImageDecoder<'a>) -> ImageResult<Vec<T>>
where
    T: crate::traits::Primitive + bytemuck::Pod,
{
    let total_bytes = usize::try_from(decoder.total_bytes());
    if total_bytes.is_err() || total_bytes.unwrap() > isize::max_value() as usize {
        return Err(ImageError::Limits(LimitError::from_kind(
            LimitErrorKind::InsufficientMemory,
        )));
    }

    let mut buf = vec![num_traits::Zero::zero(); total_bytes.unwrap() / std::mem::size_of::<T>()];
    decoder.read_image(bytemuck::cast_slice_mut(buf.as_mut_slice()))?;
    Ok(buf)
}

/// Represents the progress of an image operation.
///
/// Note that this is not necessarily accurate and no change to the values passed to the progress
/// function during decoding will be considered breaking. A decoder could in theory report the
/// progress `(0, 0)` if progress is unknown, without violating the interface contract of the type.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Progress {
    current: u64,
    total: u64,
}

impl Progress {
    /// Create Progress. Result in invalid progress if you provide a greater `current` than `total`.
    pub(crate) fn new(current: u64, total: u64) -> Self {
        Self { current, total }
    }

    /// A measure of completed decoding.
    pub fn current(self) -> u64 {
        self.current
    }

    /// A measure of all necessary decoding work.
    ///
    /// This is in general greater or equal than `current`.
    pub fn total(self) -> u64 {
        self.total
    }

    /// Calculate a measure for remaining decoding work.
    pub fn remaining(self) -> u64 {
        self.total.max(self.current) - self.current
    }
}

/// The trait that all decoders implement
pub trait ImageDecoder<'a>: Sized {
    /// The type of reader produced by `into_reader`.
    type Reader: Read + 'a;

    /// Returns a tuple containing the width and height of the image
    fn dimensions(&self) -> (u32, u32);

    /// Returns the color type of the image data produced by this decoder
    fn color_type(&self) -> ColorType;

    /// Retuns the color type of the image file before decoding
    fn original_color_type(&self) -> ExtendedColorType {
        self.color_type().into()
    }

    /// Returns a reader that can be used to obtain the bytes of the image. For the best
    /// performance, always try to read at least `scanline_bytes` from the reader at a time. Reading
    /// fewer bytes will cause the reader to perform internal buffering.
    fn into_reader(self) -> ImageResult<Self::Reader>;

    /// Returns the total number of bytes in the decoded image.
    ///
    /// This is the size of the buffer that must be passed to `read_image` or
    /// `read_image_with_progress`. The returned value may exceed usize::MAX, in
    /// which case it isn't actually possible to construct a buffer to decode all the image data
    /// into. If, however, the size does not fit in a u64 then u64::MAX is returned.
    fn total_bytes(&self) -> u64 {
        let dimensions = self.dimensions();
        let total_pixels = u64::from(dimensions.0) * u64::from(dimensions.1);
        let bytes_per_pixel = u64::from(self.color_type().bytes_per_pixel());
        total_pixels.saturating_mul(bytes_per_pixel)
    }

    /// Returns the minimum number of bytes that can be efficiently read from this decoder. This may
    /// be as few as 1 or as many as `total_bytes()`.
    fn scanline_bytes(&self) -> u64 {
        self.total_bytes()
    }

    /// Returns all the bytes in the image.
    ///
    /// This function takes a slice of bytes and writes the pixel data of the image into it.
    /// Although not required, for certain color types callers may want to pass buffers which are
    /// aligned to 2 or 4 byte boundaries to the slice can be cast to a [u16] or [u32]. To accommodate
    /// such casts, the returned contents will always be in native endian.
    ///
    /// # Panics
    ///
    /// This function panics if buf.len() != self.total_bytes().
    ///
    /// # Examples
    ///
    /// ```no_build
    /// use zerocopy::{AsBytes, FromBytes};
    /// fn read_16bit_image(decoder: impl ImageDecoder) -> Vec<16> {
    ///     let mut buf: Vec<u16> = vec![0; decoder.total_bytes()/2];
    ///     decoder.read_image(buf.as_bytes());
    ///     buf
    /// }
    fn read_image(self, buf: &mut [u8]) -> ImageResult<()> {
        self.read_image_with_progress(buf, |_| {})
    }

    /// Same as `read_image` but periodically calls the provided callback to give updates on loading
    /// progress.
    fn read_image_with_progress<F: Fn(Progress)>(
        self,
        buf: &mut [u8],
        progress_callback: F,
    ) -> ImageResult<()> {
        assert_eq!(u64::try_from(buf.len()), Ok(self.total_bytes()));

        let total_bytes = self.total_bytes() as usize;
        let scanline_bytes = self.scanline_bytes() as usize;
        let target_read_size = if scanline_bytes < 4096 {
            (4096 / scanline_bytes) * scanline_bytes
        } else {
            scanline_bytes
        };

        let mut reader = self.into_reader()?;

        let mut bytes_read = 0;
        while bytes_read < total_bytes {
            let read_size = target_read_size.min(total_bytes - bytes_read);
            reader.read_exact(&mut buf[bytes_read..][..read_size])?;
            bytes_read += read_size;

            progress_callback(Progress {
                current: bytes_read as u64,
                total: total_bytes as u64,
            });
        }

        Ok(())
    }
}

/// Specialized image decoding not be supported by all formats
pub trait ImageDecoderExt<'a>: ImageDecoder<'a> + Sized {
    /// Decode a rectangular section of the image; see [`read_rect_with_progress()`](#fn.read_rect_with_progress).
    fn read_rect(
        &mut self,
        x: u32,
        y: u32,
        width: u32,
        height: u32,
        buf: &mut [u8],
    ) -> ImageResult<()> {
        self.read_rect_with_progress(x, y, width, height, buf, |_|{})
    }

    /// Decode a rectangular section of the image, periodically reporting progress.
    ///
    /// The output buffer will be filled with fields specified by
    /// [`ImageDecoder::color_type()`](trait.ImageDecoder.html#fn.color_type),
    /// in that order, each field represented in native-endian.
    ///
    /// The progress callback will be called at least once at the start and the end of decoding,
    /// implementations are encouraged to call this more often,
    /// with a frequency meaningful for display to the end-user.
    ///
    /// This function will panic if the output buffer isn't at least
    /// `color_type().bytes_per_pixel() * color_type().channel_count() * width * height` bytes long.
    fn read_rect_with_progress<F: Fn(Progress)>(
        &mut self,
        x: u32,
        y: u32,
        width: u32,
        height: u32,
        buf: &mut [u8],
        progress_callback: F,
    ) -> ImageResult<()>;
}

/// AnimationDecoder trait
pub trait AnimationDecoder<'a> {
    /// Consume the decoder producing a series of frames.
    fn into_frames(self) -> Frames<'a>;
}

/// The trait all encoders implement
pub trait ImageEncoder {
    /// Writes all the bytes in an image to the encoder.
    ///
    /// This function takes a slice of bytes of the pixel data of the image
    /// and encodes them. Unlike particular format encoders inherent impl encode
    /// methods where endianness is not specified, here image data bytes should
    /// always be in native endian. The implementor will reorder the endianess
    /// as necessary for the target encoding format.
    ///
    /// See also `ImageDecoder::read_image` which reads byte buffers into
    /// native endian.
    fn write_image(
        self,
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ColorType,
    ) -> ImageResult<()>;
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

impl<'a, I: GenericImageView> Iterator for Pixels<'a, I> {
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
pub trait GenericImageView {
    /// The type of pixel.
    type Pixel: Pixel;

    /// Underlying image type. This is mainly used by SubImages in order to
    /// always have a reference to the original image. This allows for less
    /// indirections and it eases the use of nested SubImages.
    type InnerImageView: GenericImageView<Pixel = Self::Pixel>;

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

    /// The bounding rectangle of this image.
    fn bounds(&self) -> (u32, u32, u32, u32);

    /// Returns true if this x, y coordinate is contained inside the image.
    fn in_bounds(&self, x: u32, y: u32) -> bool {
        let (ix, iy, iw, ih) = self.bounds();
        x >= ix && x < ix + iw && y >= iy && y < iy + ih
    }

    /// Returns the pixel located at (x, y). Indexed from top left.
    ///
    /// # Panics
    ///
    /// Panics if `(x, y)` is out of bounds.
    ///
    /// TODO: change this signature to &P
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
    fn pixels(&self) -> Pixels<Self> {
        let (width, height) = self.dimensions();

        Pixels {
            image: self,
            x: 0,
            y: 0,
            width,
            height,
        }
    }

    /// Returns a reference to the underlying image.
    fn inner(&self) -> &Self::InnerImageView;

    /// Returns an subimage that is an immutable view into this image.
    /// You can use [`GenericImage::sub_image`] if you need a mutable view instead.
    /// The coordinates set the position of the top left corner of the view.
    fn view(&self, x: u32, y: u32, width: u32, height: u32) -> SubImage<&Self::InnerImageView> {
        SubImage::new(self.inner(), x, y, width, height)
    }
}

/// A trait for manipulating images.
pub trait GenericImage: GenericImageView {
    /// Underlying image type. This is mainly used by SubImages in order to
    /// always have a reference to the original image. This allows for less
    /// indirections and it eases the use of nested SubImages.
    type InnerImage: GenericImage<Pixel = Self::Pixel>;

    /// Gets a reference to the mutable pixel at location `(x, y)`. Indexed from top left.
    ///
    /// # Panics
    ///
    /// Panics if `(x, y)` is out of bounds.
    ///
    /// Panics for dynamic images (this method is deprecated and will be removed).
    #[deprecated(note="Use `get_pixel` and `put_pixel` instead.")]
    // TODO: Maybe use some kind of entry API? this would allow pixel type conversion on the fly while still doing only one array lookup
    //  `let px = image.pixel_entry_at(x,y); px.set_from_rgba(rgba)`
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
    #[deprecated(note="Use iterator `pixels_mut` to blend the pixels directly")]
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
        let Rect { x: sx, y: sy, width, height } = source;
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

    /// Returns a mutable reference to the underlying image.
    fn inner_mut(&mut self) -> &mut Self::InnerImage;

    /// Returns a mutable subimage that is a view into this image.
    /// If you want an immutable subimage instead, use [`GenericImageView::view`]
    /// The coordinates set the position of the top left corner of the SubImage.
    fn sub_image(
        &mut self,
        x: u32,
        y: u32,
        width: u32,
        height: u32,
    ) -> SubImage<&mut Self::InnerImage> {
        SubImage::new(self.inner_mut(), x, y, width, height)
    }
}

/// A View into another image
///
/// Instances of this struct can be created using:
///   - [`GenericImage::sub_image`] to create a mutable view,
///   - [`GenericImageView::view`] to create an immutable view,
///   - [`SubImage::new`] to instantiate the struct directly.
pub struct SubImage<I> {
    image: I,
    xoffset: u32,
    yoffset: u32,
    xstride: u32,
    ystride: u32,
}

/// Alias to access Pixel behind a reference
type DerefPixel<I> = <<I as Deref>::Target as GenericImageView>::Pixel;

/// Alias to access Subpixel behind a reference
type DerefSubpixel<I> = <DerefPixel<I> as Pixel>::Subpixel;

impl<I> SubImage<I> {
    /// Construct a new subimage
    /// The coordinates set the position of the top left corner of the SubImage.
    pub fn new(image: I, x: u32, y: u32, width: u32, height: u32) -> SubImage<I> {
        SubImage {
            image,
            xoffset: x,
            yoffset: y,
            xstride: width,
            ystride: height,
        }
    }

    /// Change the coordinates of this subimage.
    pub fn change_bounds(&mut self, x: u32, y: u32, width: u32, height: u32) {
        self.xoffset = x;
        self.yoffset = y;
        self.xstride = width;
        self.ystride = height;
    }

    /// Convert this subimage to an ImageBuffer
    pub fn to_image(&self) -> ImageBuffer<DerefPixel<I>, Vec<DerefSubpixel<I>>>
    where
        I: Deref,
        I::Target: GenericImageView + 'static,
    {
        let mut out = ImageBuffer::new(self.xstride, self.ystride);
        let borrowed = self.image.deref();

        for y in 0..self.ystride {
            for x in 0..self.xstride {
                let p = borrowed.get_pixel(x + self.xoffset, y + self.yoffset);
                out.put_pixel(x, y, p);
            }
        }

        out
    }
}

#[allow(deprecated)]
impl<I> GenericImageView for SubImage<I>
where
    I: Deref,
    I::Target: GenericImageView + Sized,
{
    type Pixel = DerefPixel<I>;
    type InnerImageView = I::Target;

    fn dimensions(&self) -> (u32, u32) {
        (self.xstride, self.ystride)
    }

    fn bounds(&self) -> (u32, u32, u32, u32) {
        (self.xoffset, self.yoffset, self.xstride, self.ystride)
    }

    fn get_pixel(&self, x: u32, y: u32) -> Self::Pixel {
        self.image.get_pixel(x + self.xoffset, y + self.yoffset)
    }

    fn view(&self, x: u32, y: u32, width: u32, height: u32) -> SubImage<&Self::InnerImageView> {
        let x = self.xoffset + x;
        let y = self.yoffset + y;
        SubImage::new(self.inner(), x, y, width, height)
    }

    fn inner(&self) -> &Self::InnerImageView {
        &self.image
    }
}

#[allow(deprecated)]
impl<I> GenericImage for SubImage<I>
where
    I: DerefMut,
    I::Target: GenericImage + Sized,
{
    type InnerImage = I::Target;

    fn get_pixel_mut(&mut self, x: u32, y: u32) -> &mut Self::Pixel {
        self.image.get_pixel_mut(x + self.xoffset, y + self.yoffset)
    }

    fn put_pixel(&mut self, x: u32, y: u32, pixel: Self::Pixel) {
        self.image
            .put_pixel(x + self.xoffset, y + self.yoffset, pixel)
    }

    /// DEPRECATED: This method will be removed. Blend the pixel directly instead.
    fn blend_pixel(&mut self, x: u32, y: u32, pixel: Self::Pixel) {
        self.image
            .blend_pixel(x + self.xoffset, y + self.yoffset, pixel)
    }

    fn sub_image(
        &mut self,
        x: u32,
        y: u32,
        width: u32,
        height: u32,
    ) -> SubImage<&mut Self::InnerImage> {
        let x = self.xoffset + x;
        let y = self.yoffset + y;
        SubImage::new(self.inner_mut(), x, y, width, height)
    }

    fn inner_mut(&mut self) -> &mut Self::InnerImage {
        &mut self.image
    }
}

#[cfg(test)]
mod tests {
    use std::io;
    use std::path::Path;

    use super::{ColorType, ImageDecoder, ImageResult, GenericImage, GenericImageView, load_rect, ImageFormat};
    use crate::{GrayImage, ImageBuffer};
    use crate::color::Rgba;
    use crate::math::Rect;

    #[test]
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
    fn test_load_rect() {
        struct MockDecoder {scanline_number: u64, scanline_bytes: u64}
        impl<'a> ImageDecoder<'a> for MockDecoder {
            type Reader = Box<dyn io::Read>;
            fn dimensions(&self) -> (u32, u32) {(5, 5)}
            fn color_type(&self) -> ColorType {  ColorType::L8 }
            fn into_reader(self) -> ImageResult<Self::Reader> {unimplemented!()}
            fn scanline_bytes(&self) -> u64 { self.scanline_bytes }
        }

        const DATA: [u8; 25] = [0,  1,  2,  3,  4,
                                5,  6,  7,  8,  9,
                                10, 11, 12, 13, 14,
                                15, 16, 17, 18, 19,
                                20, 21, 22, 23, 24];

        fn seek_scanline(m: &mut MockDecoder, n: u64) -> io::Result<()> {
            m.scanline_number = n;
            Ok(())
        }
        fn read_scanline(m: &mut MockDecoder, buf: &mut [u8]) -> io::Result<()> {
            let bytes_read = m.scanline_number * m.scanline_bytes;
            if bytes_read >= 25 {
                return Ok(());
            }

            let len = m.scanline_bytes.min(25 - bytes_read);
            buf[..(len as usize)].copy_from_slice(&DATA[(bytes_read as usize)..][..(len as usize)]);
            m.scanline_number += 1;
            Ok(())
        }

        for scanline_bytes in 1..30 {
            let mut output = [0u8; 26];

            load_rect(0, 0, 5, 5, &mut output, |_|{},
                      &mut MockDecoder{scanline_number:0, scanline_bytes},
                      seek_scanline, read_scanline).unwrap();
            assert_eq!(output[0..25], DATA);
            assert_eq!(output[25], 0);

            output = [0u8; 26];
            load_rect(3, 2, 1, 1, &mut output, |_|{},
                      &mut MockDecoder{scanline_number:0, scanline_bytes},
                      seek_scanline, read_scanline).unwrap();
            assert_eq!(output[0..2], [13, 0]);

            output = [0u8; 26];
            load_rect(3, 2, 2, 2, &mut output, |_|{},
                      &mut MockDecoder{scanline_number:0, scanline_bytes},
                      seek_scanline, read_scanline).unwrap();
            assert_eq!(output[0..5], [13, 14, 18, 19, 0]);


            output = [0u8; 26];
            load_rect(1, 1, 2, 4, &mut output, |_|{},
                      &mut MockDecoder{scanline_number:0, scanline_bytes},
                      seek_scanline, read_scanline).unwrap();
            assert_eq!(output[0..9], [6, 7, 11, 12, 16, 17, 21, 22, 0]);

        }
    }

    #[test]
    fn test_load_rect_single_scanline() {
        const DATA: [u8; 25] = [0,  1,  2,  3,  4,
                                5,  6,  7,  8,  9,
                                10, 11, 12, 13, 14,
                                15, 16, 17, 18, 19,
                                20, 21, 22, 23, 24];

        struct MockDecoder;
        impl<'a> ImageDecoder<'a> for MockDecoder {
            type Reader = Box<dyn io::Read>;
            fn dimensions(&self) -> (u32, u32) {(5, 5)}
            fn color_type(&self) -> ColorType {  ColorType::L8 }
            fn into_reader(self) -> ImageResult<Self::Reader> {unimplemented!()}
            fn scanline_bytes(&self) -> u64 { 25 }
        }

        // Ensure that seek scanline is called only once.
        let mut seeks = 0;
        let seek_scanline = |_d: &mut MockDecoder, n: u64| -> io::Result<()> {
            seeks += 1;
            assert_eq!(n, 0);
            assert_eq!(seeks, 1);
            Ok(())
        };

        fn read_scanline(_m: &mut MockDecoder, buf: &mut [u8]) -> io::Result<()> {
            buf.copy_from_slice(&DATA);
            Ok(())
        }

        let mut output = [0; 26];
        load_rect(1, 1, 2, 4, &mut output, |_|{},
                    &mut MockDecoder,
                    seek_scanline, read_scanline).unwrap();
        assert_eq!(output[0..9], [6, 7, 11, 12, 16, 17, 21, 22, 0]);
    }


    #[test]
    fn test_image_format_from_path() {
        fn from_path(s: &str) -> ImageResult<ImageFormat> {
            ImageFormat::from_path(Path::new(s))
        }
        assert_eq!(from_path("./a.jpg").unwrap(), ImageFormat::Jpeg);
        assert_eq!(from_path("./a.jpeg").unwrap(), ImageFormat::Jpeg);
        assert_eq!(from_path("./a.JPEG").unwrap(), ImageFormat::Jpeg);
        assert_eq!(from_path("./a.pNg").unwrap(), ImageFormat::Png);
        assert_eq!(from_path("./a.gif").unwrap(), ImageFormat::Gif);
        assert_eq!(from_path("./a.webp").unwrap(), ImageFormat::WebP);
        assert_eq!(from_path("./a.tiFF").unwrap(), ImageFormat::Tiff);
        assert_eq!(from_path("./a.tif").unwrap(), ImageFormat::Tiff);
        assert_eq!(from_path("./a.tga").unwrap(), ImageFormat::Tga);
        assert_eq!(from_path("./a.dds").unwrap(), ImageFormat::Dds);
        assert_eq!(from_path("./a.bmp").unwrap(), ImageFormat::Bmp);
        assert_eq!(from_path("./a.Ico").unwrap(), ImageFormat::Ico);
        assert_eq!(from_path("./a.hdr").unwrap(), ImageFormat::Hdr);
        assert_eq!(from_path("./a.exr").unwrap(), ImageFormat::OpenExr);
        assert_eq!(from_path("./a.pbm").unwrap(), ImageFormat::Pnm);
        assert_eq!(from_path("./a.pAM").unwrap(), ImageFormat::Pnm);
        assert_eq!(from_path("./a.Ppm").unwrap(), ImageFormat::Pnm);
        assert_eq!(from_path("./a.pgm").unwrap(), ImageFormat::Pnm);
        assert_eq!(from_path("./a.AViF").unwrap(), ImageFormat::Avif);
        assert!(from_path("./a.txt").is_err());
        assert!(from_path("./a").is_err());
    }

    #[test]
    fn test_generic_image_copy_within_oob() {
        let mut image: GrayImage = ImageBuffer::from_raw(4, 4, vec![0u8; 16]).unwrap();
        assert!(!image.sub_image(0, 0, 4, 4).copy_within(Rect { x: 0, y: 0, width: 5, height: 4 }, 0, 0));
        assert!(!image.sub_image(0, 0, 4, 4).copy_within(Rect { x: 0, y: 0, width: 4, height: 5 }, 0, 0));
        assert!(!image.sub_image(0, 0, 4, 4).copy_within(Rect { x: 1, y: 0, width: 4, height: 4 }, 0, 0));
        assert!(!image.sub_image(0, 0, 4, 4).copy_within(Rect { x: 0, y: 0, width: 4, height: 4 }, 1, 0));
        assert!(!image.sub_image(0, 0, 4, 4).copy_within(Rect { x: 0, y: 1, width: 4, height: 4 }, 0, 0));
        assert!(!image.sub_image(0, 0, 4, 4).copy_within(Rect { x: 0, y: 0, width: 4, height: 4 }, 0, 1));
        assert!(!image.sub_image(0, 0, 4, 4).copy_within(Rect { x: 1, y: 1, width: 4, height: 4 }, 0, 0));
    }

    #[test]
    fn test_generic_image_copy_within_tl() {
        let data = &[
            00, 01, 02, 03,
            04, 05, 06, 07,
            08, 09, 10, 11,
            12, 13, 14, 15
        ];
        let expected = [
            00, 01, 02, 03,
            04, 00, 01, 02,
            08, 04, 05, 06,
            12, 08, 09, 10,
        ];
        let mut image: GrayImage = ImageBuffer::from_raw(4, 4, Vec::from(&data[..])).unwrap();
        assert!(image.sub_image(0, 0, 4, 4).copy_within(Rect { x: 0, y: 0, width: 3, height: 3 }, 1, 1));
        assert_eq!(&image.into_raw(), &expected);
    }

    #[test]
    fn test_generic_image_copy_within_tr() {
        let data = &[
            00, 01, 02, 03,
            04, 05, 06, 07,
            08, 09, 10, 11,
            12, 13, 14, 15
        ];
        let expected = [
            00, 01, 02, 03,
            01, 02, 03, 07,
            05, 06, 07, 11,
            09, 10, 11, 15
        ];
        let mut image: GrayImage = ImageBuffer::from_raw(4, 4, Vec::from(&data[..])).unwrap();
        assert!(image.sub_image(0, 0, 4, 4).copy_within(Rect { x: 1, y: 0, width: 3, height: 3 }, 0, 1));
        assert_eq!(&image.into_raw(), &expected);
    }

    #[test]
    fn test_generic_image_copy_within_bl() {
        let data = &[
            00, 01, 02, 03,
            04, 05, 06, 07,
            08, 09, 10, 11,
            12, 13, 14, 15
        ];
        let expected = [
            00, 04, 05, 06,
            04, 08, 09, 10,
            08, 12, 13, 14,
            12, 13, 14, 15
        ];
        let mut image: GrayImage = ImageBuffer::from_raw(4, 4, Vec::from(&data[..])).unwrap();
        assert!(image.sub_image(0, 0, 4, 4).copy_within(Rect { x: 0, y: 1, width: 3, height: 3 }, 1, 0));
        assert_eq!(&image.into_raw(), &expected);
    }

    #[test]
    fn test_generic_image_copy_within_br() {
        let data = &[
            00, 01, 02, 03,
            04, 05, 06, 07,
            08, 09, 10, 11,
            12, 13, 14, 15
        ];
        let expected = [
            05, 06, 07, 03,
            09, 10, 11, 07,
            13, 14, 15, 11,
            12, 13, 14, 15
        ];
        let mut image: GrayImage = ImageBuffer::from_raw(4, 4, Vec::from(&data[..])).unwrap();
        assert!(image.sub_image(0, 0, 4, 4).copy_within(Rect { x: 1, y: 1, width: 3, height: 3 }, 0, 0));
        assert_eq!(&image.into_raw(), &expected);
    }

    #[test]
    fn image_formats_are_recognized() {
        use ImageFormat::*;
        const ALL_FORMATS: &'static [ImageFormat] = &[Avif, Png, Jpeg, Gif, WebP, Pnm, Tiff, Tga, Dds, Bmp, Ico, Hdr, Farbfeld, OpenExr];
        for &format in ALL_FORMATS {
            let mut file = Path::new("file.nothing").to_owned();
            for ext in format.extensions_str() {
                assert!(file.set_extension(ext));
                match ImageFormat::from_path(&file) {
                    Err(_) => panic!("Path {} not recognized as {:?}", file.display(), format),
                    Ok(result) => assert_eq!(format, result),
                }
            }
        }
    }

    #[test]
    fn total_bytes_overflow() {
        struct D;
        impl<'a> ImageDecoder<'a> for D {
            type Reader = std::io::Cursor<Vec<u8>>;
            fn color_type(&self) -> ColorType { ColorType::Rgb8 }
            fn dimensions(&self) -> (u32, u32) { (0xffffffff, 0xffffffff) }
            fn into_reader(self) -> ImageResult<Self::Reader> { unreachable!() }
        }
        assert_eq!(D.total_bytes(), u64::max_value());

        let v: ImageResult<Vec<u8>> = super::decoder_to_vec(D);
        assert!(v.is_err());
    }
}
