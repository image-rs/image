use std::ffi::OsString;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Cursor, Read, Seek, SeekFrom};
use std::path::Path;

use crate::error::{
    ImageFormatHint, ImageResult, ParameterErrorKind, UnsupportedError, UnsupportedErrorKind,
};
use crate::io::limits::Limits;
use crate::io::DecodedImageAttributes;
use crate::io::SequenceControl;
use crate::{hooks, Delay, Frame, Frames};
use crate::{DynamicImage, ImageDecoder, ImageError, ImageFormat};

use super::free_functions;

#[derive(Clone)]
enum Format {
    BuiltIn(ImageFormat),
    Extension(OsString),
}

/// Determine the format for an image reader.
///
/// Wraps an input stream to facilitate automatic detection of an image's format, appropriate
/// decoding method, and turn it into an [`ImageReader`] or a boxed [`ImageDecoder`]
/// implementation. For convenience, it also allows directly decoding into a [`DynamicImage`].
///
/// ## Usage
///
/// Opening a file, deducing the format based on the file path automatically, and trying to decode
/// the image contained can be performed by constructing the reader and immediately consuming it.
///
/// ```no_run
/// # use image::ImageError;
/// # use image::ImageReaderOptions;
/// # fn main() -> Result<(), ImageError> {
/// let image = ImageReaderOptions::open("path/to/image.png")?
///     .decode()?;
/// # Ok(()) }
/// ```
///
/// It is also possible to make a guess based on the content. This is especially handy if the
/// source is some blob in memory and you have constructed the reader in another way. Here is an
/// example with a `pnm` black-and-white subformat that encodes its pixel matrix with ascii values.
///
/// ```
/// # use image::ImageError;
/// # use image::ImageReaderOptions;
/// # fn main() -> Result<(), ImageError> {
/// use std::io::Cursor;
/// use image::ImageFormat;
///
/// let raw_data = b"P1 2 2\n\
///     0 1\n\
///     1 0\n";
///
/// let mut reader = ImageReaderOptions::new(Cursor::new(raw_data))
///     .with_guessed_format()
///     .expect("Cursor io never fails");
/// assert_eq!(reader.format(), Some(ImageFormat::Pnm));
///
/// # #[cfg(feature = "pnm")]
/// let image = reader.decode()?;
/// # Ok(()) }
/// ```
///
/// As a final fallback or if only a specific format must be used, the reader always allows manual
/// specification of the supposed image format with [`set_format`].
///
/// [`set_format`]: #method.set_format
pub struct ImageReaderOptions<R: Read + Seek> {
    /// The reader. Should be buffered.
    inner: R,
    /// The format, if one has been set or deduced.
    format: Option<Format>,
    /// Decoding limits
    limits: Limits,
}

impl<'a, R: 'a + BufRead + Seek> ImageReaderOptions<R> {
    /// Create a new image reader without a preset format.
    ///
    /// Assumes the reader is already buffered. For optimal performance,
    /// consider wrapping the reader with a `BufReader::new()`.
    ///
    /// It is possible to guess the format based on the content of the read object with
    /// [`with_guessed_format`], or to set the format directly with [`set_format`].
    ///
    /// [`with_guessed_format`]: #method.with_guessed_format
    /// [`set_format`]: method.set_format
    pub fn new(buffered_reader: R) -> Self {
        ImageReaderOptions {
            inner: buffered_reader,
            format: None,
            limits: Limits::default(),
        }
    }

    /// Construct a reader with specified format.
    ///
    /// Assumes the reader is already buffered. For optimal performance,
    /// consider wrapping the reader with a `BufReader::new()`.
    pub fn with_format(buffered_reader: R, format: ImageFormat) -> Self {
        ImageReaderOptions {
            inner: buffered_reader,
            format: Some(Format::BuiltIn(format)),
            limits: Limits::default(),
        }
    }

    /// Get the currently determined format.
    pub fn format(&self) -> Option<ImageFormat> {
        match self.format {
            Some(Format::BuiltIn(ref format)) => Some(*format),
            Some(Format::Extension(ref ext)) => ImageFormat::from_extension(ext),
            None => None,
        }
    }

    /// Supply the format as which to interpret the read image.
    pub fn set_format(&mut self, format: ImageFormat) {
        self.format = Some(Format::BuiltIn(format));
    }

    /// Remove the current information on the image format.
    ///
    /// Note that many operations require format information to be present and will return e.g. an
    /// `ImageError::Unsupported` when the image format has not been set.
    pub fn clear_format(&mut self) {
        self.format = None;
    }

    /// Disable all decoding limits.
    pub fn no_limits(&mut self) {
        self.limits = Limits::no_limits();
    }

    /// Set a custom set of decoding limits.
    pub fn limits(&mut self, limits: Limits) {
        self.limits = limits;
    }

    /// Unwrap the reader.
    pub fn into_inner(self) -> R {
        self.inner
    }

    /// Take the readable IO stream and construct a decoder around it.
    fn make_decoder(format: Format, reader: R) -> ImageResult<Box<dyn ImageDecoder + 'a>> {
        #[allow(unused)]
        use crate::codecs::*;

        let format = match format {
            Format::BuiltIn(format) => format,
            Format::Extension(ext) => {
                if let Some(hook) = hooks::get_decoding_hook(&ext) {
                    return hook(hooks::GenericReader::new(reader));
                }

                ImageFormat::from_extension(&ext).ok_or(ImageError::Unsupported(
                    ImageFormatHint::PathExtension(ext.into()).into(),
                ))?
            }
        };

        #[allow(unreachable_patterns)]
        // Default is unreachable if all features are supported.
        Ok(match format {
            #[cfg(feature = "avif-native")]
            ImageFormat::Avif => Box::new(avif::AvifDecoder::new(reader)?),
            #[cfg(feature = "png")]
            ImageFormat::Png => Box::new(png::PngDecoder::new(reader)),
            #[cfg(feature = "gif")]
            ImageFormat::Gif => Box::new(gif::GifDecoder::new(reader)?),
            #[cfg(feature = "jpeg")]
            ImageFormat::Jpeg => Box::new(jpeg::JpegDecoder::new(reader)?),
            #[cfg(feature = "webp")]
            ImageFormat::WebP => Box::new(webp::WebPDecoder::new(reader)?),
            #[cfg(feature = "tiff")]
            ImageFormat::Tiff => Box::new(tiff::TiffDecoder::new(reader)?),
            #[cfg(feature = "tga")]
            ImageFormat::Tga => Box::new(tga::TgaDecoder::new(reader)?),
            #[cfg(feature = "dds")]
            ImageFormat::Dds => Box::new(dds::DdsDecoder::new(reader)?),
            #[cfg(feature = "bmp")]
            ImageFormat::Bmp => Box::new(bmp::BmpDecoder::new(reader)?),
            #[cfg(feature = "ico")]
            ImageFormat::Ico => Box::new(ico::IcoDecoder::new(reader)?),
            #[cfg(feature = "hdr")]
            ImageFormat::Hdr => Box::new(hdr::HdrDecoder::new(reader)?),
            #[cfg(feature = "exr")]
            ImageFormat::OpenExr => Box::new(openexr::OpenExrDecoder::new(reader)?),
            #[cfg(feature = "pnm")]
            ImageFormat::Pnm => Box::new(pnm::PnmDecoder::new(reader)?),
            #[cfg(feature = "ff")]
            ImageFormat::Farbfeld => Box::new(farbfeld::FarbfeldDecoder::new(reader)?),
            #[cfg(feature = "qoi")]
            ImageFormat::Qoi => Box::new(qoi::QoiDecoder::new(reader)?),
            format => {
                return Err(ImageError::Unsupported(
                    ImageFormatHint::Exact(format).into(),
                ));
            }
        })
    }

    /// Convert the file into its raw decoder ready to read an image.
    pub fn into_decoder(mut self) -> ImageResult<impl ImageDecoder + 'a> {
        let mut decoder = Self::make_decoder(self.require_format()?, self.inner)?;
        decoder.set_limits(self.limits)?;
        Ok(decoder)
    }

    /// Convert the file into a reader object.
    pub fn into_reader(mut self) -> ImageResult<ImageReader<'a>> {
        let format = self.require_format()?;
        let mut decoder = Self::make_decoder(format, self.inner)?;

        if let Some(max_alloc) = &mut self.limits.max_alloc {
            // We'll take half for ourselves, half to the decoder.
            *max_alloc /= 2;
        }

        decoder.set_limits(self.limits.clone())?;

        Ok(ImageReader {
            inner: decoder,
            limits: self.limits,
            last_attributes: Default::default(),
        })
    }

    /// Make a format guess based on the content, replacing it on success.
    ///
    /// Returns `Ok` with the guess if no io error occurs. Additionally, replaces the current
    /// format if the guess was successful. If the guess was unable to determine a format then
    /// the current format of the reader is unchanged.
    ///
    /// Returns an error if the underlying reader fails. The format is unchanged. The error is a
    /// `std::io::Error` and not `ImageError` since the only error case is an error when the
    /// underlying reader seeks.
    ///
    /// When an error occurs, the reader may not have been properly reset and it is potentially
    /// hazardous to continue with more io.
    ///
    /// ## Usage
    ///
    /// This supplements the path based type deduction from [`Self::open`] with content based
    /// deduction. This is more common in Linux and UNIX operating systems and also helpful if the
    /// path can not be directly controlled.
    ///
    /// ```no_run
    /// # use image::ImageError;
    /// # use image::ImageReaderOptions;
    /// # fn main() -> Result<(), ImageError> {
    /// let image = ImageReaderOptions::open("image.unknown")?
    ///     .with_guessed_format()?
    ///     .decode()?;
    /// # Ok(()) }
    /// ```
    pub fn with_guessed_format(mut self) -> io::Result<Self> {
        let format = self.guess_format()?;
        // Replace format if found, keep current state if not.
        self.format = format.or(self.format);
        Ok(self)
    }

    fn guess_format(&mut self) -> io::Result<Option<Format>> {
        let mut start = [0; 16];

        // Save current offset, read start, restore offset.
        let cur = self.inner.stream_position()?;
        let len = io::copy(
            // Accept shorter files but read at most 16 bytes.
            &mut self.inner.by_ref().take(16),
            &mut Cursor::new(&mut start[..]),
        )?;
        self.inner.seek(SeekFrom::Start(cur))?;
        let start = &start[..len as usize];

        if let Some(extension) = hooks::guess_format_extension(start) {
            return Ok(Some(Format::Extension(extension)));
        }

        if let Some(format) = free_functions::guess_format_impl(start) {
            return Ok(Some(Format::BuiltIn(format)));
        }

        Ok(None)
    }

    /// Read the image dimensions.
    ///
    /// Uses the current format to construct the correct reader for the format.
    ///
    /// If no format was determined, returns an `ImageError::Unsupported`.
    pub fn into_dimensions(self) -> ImageResult<(u32, u32)> {
        let mut decoder = self.into_decoder()?;
        let layout = decoder.peek_layout()?;
        Ok((layout.width, layout.height))
    }

    /// Read the image (replaces `load`).
    ///
    /// Uses the current format to construct the correct reader for the format.
    ///
    /// If no format was determined, returns an `ImageError::Unsupported`.
    pub fn decode(self) -> ImageResult<DynamicImage> {
        self.into_reader()?.decode()
    }

    fn require_format(&mut self) -> ImageResult<Format> {
        self.format.clone().ok_or_else(|| {
            ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                ImageFormatHint::Unknown,
                UnsupportedErrorKind::Format(ImageFormatHint::Unknown),
            ))
        })
    }
}

/// An abstracted image reader.
///
/// Wraps an image decoder, which operates on a stream after its format was determined.
/// [`ImageReaderOptions`] dispatches into the set of supported [`ImageDecoder`] implementations
/// and can wrap them up as an [`ImageReader`]. For decoder interface that are provided for
/// efficiency it negotiates support with the underlying decoder and then emulates them if
/// necessary.
pub struct ImageReader<'lt> {
    /// The reader. Should be buffered.
    inner: Box<dyn ImageDecoder + 'lt>,
    /// Remaining limits for allocations by the reader.
    limits: Limits,
    /// A buffered cache of the last image attributes.
    last_attributes: DecodedImageAttributes,
}

impl ImageReaderOptions<BufReader<File>> {
    /// Open a file to read, format will be guessed from path.
    ///
    /// This will not attempt any io operation on the opened file.
    ///
    /// If you want to inspect the content for a better guess on the format, which does not depend
    /// on file extensions, follow this call with a call to [`with_guessed_format`].
    ///
    /// [`with_guessed_format`]: #method.with_guessed_format
    pub fn open<P>(path: P) -> io::Result<Self>
    where
        P: AsRef<Path>,
    {
        Self::open_impl(path.as_ref())
    }

    fn open_impl(path: &Path) -> io::Result<Self> {
        let format = path
            .extension()
            .filter(|ext| !ext.is_empty())
            .map(|ext| Format::Extension(ext.to_owned()));

        Ok(ImageReaderOptions {
            inner: BufReader::new(File::open(path)?),
            format,
            limits: Limits::default(),
        })
    }
}

impl ImageReader<'_> {
    /// Decode the next image into a `DynamicImage`.
    pub fn decode(&mut self) -> ImageResult<DynamicImage> {
        let layout = self.inner.peek_layout()?;
        // This is technically redundant but it's also cheap.
        self.limits.check_dimensions(layout.width, layout.height)?;
        // Check that we do not allocate a bigger buffer than we are allowed to
        // FIXME: should this rather go in `DynamicImage::from_decoder` somehow?
        self.limits.reserve(layout.total_bytes())?;

        let mut image = DynamicImage::decoder_to_image(self.inner.as_mut(), layout)?;

        // Apply the profile. If the profile itself is not valid or not present you get the default
        // presumption: `sRGB`. Otherwise we will try to make sense of the profile and if it is not
        // RGB we'll treat it as unspecified so that downstream will know that our handling of this
        // _existing_ profile was not / could not be done with full fidelity.
        if let Some(icc) = self.inner.icc_profile()? {
            if let Some(cicp) = crate::metadata::cms_provider().parse_icc(&icc) {
                // We largely ignore the error itself here, you just get the image with no color
                // space attached to it.
                if let Ok(rgb) = cicp.try_into_rgb() {
                    image.set_rgb_primaries(rgb.primaries);
                    image.set_transfer_function(rgb.transfer);
                } else {
                    image.set_rgb_primaries(crate::metadata::CicpColorPrimaries::Unspecified);
                    image.set_transfer_function(
                        crate::metadata::CicpTransferCharacteristics::Unspecified,
                    );
                }
            }
        }

        Ok(image)
    }

    /// Skip the next image, discard its image data.
    ///
    /// This will attempt to read the image data with as little allocation as possible while still
    /// running the usual verification routines. It will inform the underlying decoder that it is
    /// uninterested in all of the image data, then run its decoding routine.
    pub fn skip(&mut self) -> ImageResult<()> {
        // TODO: with `viewbox` (temporarily removed) we can inform the decoder that no data is
        // required which may be quite efficient. Other variants of achieving the same may also be
        // possible. We can just try out until one works.

        // Some decoders may still want a buffer, so we can't fully ignore it.
        let layout = self.inner.peek_layout()?;
        // This is technically redundant but it's also cheap.
        self.limits.check_dimensions(layout.width, layout.height)?;
        let bytes = layout.total_bytes();

        if bytes < 512 {
            let mut stack = [0u8; 512];
            self.inner.read_image(&mut stack[..bytes as usize])?;
        } else {
            // Check that we do not allocate a bigger buffer than we are allowed to
            // FIXME: should this rather go in `DynamicImage::from_decoder` somehow?
            self.limits.reserve(bytes)?;
            DynamicImage::decoder_to_image(self.inner.as_mut(), layout)?;
            self.limits.free(bytes);
        }

        Ok(())
    }

    /// Query the layout that the image will have.
    pub fn layout(&mut self) -> ImageResult<crate::ImageLayout> {
        self.inner.peek_layout()
    }

    /// Get the previously decoded EXIF metadata if any.
    pub fn exif_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        self.inner.exif_metadata()
    }

    /// Get the previously decoded ICC profile if any.
    pub fn icc_profile(&mut self) -> ImageResult<Option<Vec<u8>>> {
        self.inner.icc_profile()
    }

    /// Get the previously decoded XMP metadata if any.
    pub fn xmp_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        self.inner.xmp_metadata()
    }

    /// Get the previously decoded IPTC metadata if any.
    pub fn iptc_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        self.inner.iptc_metadata()
    }
}

impl<'stream> ImageReader<'stream> {
    /// Open the image in a readable stream.
    ///
    /// The format is guessed from a fixed array of bytes at stream's start. Hooks can be
    /// configured to customize this behavior, see [`hooks`](crate::hooks) for details.
    ///
    /// The reader will use default limits. Use [`ImageReaderOptions`] to configure the reader
    /// before use.
    pub fn new<R: 'stream + BufRead + Seek>(reader: R) -> ImageResult<Self> {
        ImageReaderOptions::new(reader)
            .with_guessed_format()?
            .into_reader()
    }

    /// Open the image located at the path specified.
    ///
    /// The image's format is determined from the path's file extension. Hooks can be configured to
    /// customize this behavior, see [`hooks`](crate::hooks) for details.
    ///
    /// The reader will use default limits. Use [`ImageReaderOptions`] to configure the reader
    /// before use.
    pub fn open<P: AsRef<Path>>(path: P) -> ImageResult<Self> {
        ImageReaderOptions::open(path)?.into_reader()
    }

    /// Read images from a boxed decoder.
    ///
    /// This can be used to interact with decoder instances that have not been created by `image`
    /// or registered hooks.
    ///
    /// The [`ImageReader`] abstracts interaction with the decoder as user facing methods to decode
    /// any further images that the decoder can provide. The decoder is assumed to be already
    /// configured with limits but the reader will make some additional allocations for which it
    /// has its own set of default limits.
    pub fn from_decoder(boxed: Box<dyn ImageDecoder + 'stream>) -> Self {
        ImageReader {
            inner: boxed,
            limits: Limits::default(),
            last_attributes: Default::default(),
        }
    }

    /// Reconfigure the limits for decoding.
    pub fn set_limits(&mut self, mut limits: Limits) -> ImageResult<()> {
        if let Some(max_alloc) = &mut limits.max_alloc {
            // We'll take half for ourselves, half to the decoder.
            *max_alloc /= 2;
        }

        self.inner.set_limits(limits.clone())?;
        self.limits = limits;
        Ok(())
    }

    /// Consume the reader as a series of frames.
    ///
    /// The iterator will end (start returning `None`) when the decoder indicates that no more
    /// images are present in the stream by setting [`ImageDecoder::more_images`] to
    /// [`SequenceControl::None`]. Decoding can return [`ParameterError`] in
    /// [`ImageDecoder::peek_layout`] or [`ImageDecoder::read_image`] with kind set to
    /// [`None`](crate::io::SequenceControl::None), which is also treated as end of stream. This
    /// may be used by decoders which can not determine the number of images in advance.
    pub fn into_frames(mut self) -> Frames<'stream> {
        fn is_end_reached(err: &ImageError) -> bool {
            if let ImageError::Parameter(ref param_err) = err {
                matches!(param_err.kind(), ParameterErrorKind::NoMoreData)
            } else {
                false
            }
        }

        let iter = core::iter::from_fn(move || {
            match self.inner.more_images() {
                SequenceControl::MaybeMore => {}
                SequenceControl::None => return None,
            }

            let no_delay = Delay::from_saturating_duration(Default::default());

            let frame = match self.decode() {
                Ok(frame) => frame,
                Err(ref err) if is_end_reached(err) => return None,
                Err(err) => return Some(Err(err)),
            };

            let x = self.last_attributes.x;
            let y = self.last_attributes.y;
            let delay = self.last_attributes.delay.unwrap_or(no_delay);
            let frame = frame.into_rgba8();

            let frame = Frame::from_parts(frame, x, y, delay);
            Some(Ok(frame))
        });

        Frames::new(Box::new(iter))
    }
}
