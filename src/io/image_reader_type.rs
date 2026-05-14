use std::ffi::OsString;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Cursor, Read, Seek};
use std::path::Path;

use crate::error::{
    ImageFormatHint, ImageResult, ParameterError, ParameterErrorKind, UnsupportedError,
    UnsupportedErrorKind,
};
use crate::io::limits::Limits;
use crate::io::{DecodedAnimationAttributes, DecodedImageAttributes, DecoderPreparedImage};
use crate::io::{DecodedMetadataHint, SequenceControl};
use crate::metadata::Orientation;
use crate::{hooks, Delay, Frame, Frames};
use crate::{DynamicImage, ImageDecoder, ImageError, ImageFormat};

use super::free_functions;

/// Controls how strictly an image decoder adheres to the format specification.
/// The default is [`SpecCompliance::Lenient`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SpecCompliance {
    /// Strictly follow the format specification.
    Strict,
    /// Allow non-conformant files that can still be decoded at best effort.
    #[default]
    Lenient,
}

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
/// example with a `pbm` black-and-white subformat that encodes its pixel matrix with ascii values.
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
/// assert_eq!(reader.format(), Some(ImageFormat::Pbm));
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
    /// Spec compliance mode
    spec_compliance: SpecCompliance,
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
            spec_compliance: SpecCompliance::default(),
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
            spec_compliance: SpecCompliance::default(),
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

    /// Set the spec compliance mode for decoding.
    pub fn set_spec_compliance(&mut self, spec: SpecCompliance) {
        self.spec_compliance = spec;
    }

    /// Unwrap the reader.
    pub fn into_inner(self) -> R {
        self.inner
    }

    /// Makes a decoder.
    ///
    /// For all formats except PNG, the limits are ignored and can be set with
    /// `ImageDecoder::set_limits` after calling this function. PNG is handled specially because that
    /// decoder has a different API which does not allow setting limits after construction.
    fn make_decoder(
        format: Format,
        reader: R,
        spec_compliance: SpecCompliance,
    ) -> ImageResult<Box<dyn ImageDecoder + 'a>> {
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
            ImageFormat::Jpeg => Box::new(jpeg::JpegDecoder::new_with_spec_compliance(
                reader,
                spec_compliance,
            )?),
            #[cfg(feature = "webp")]
            ImageFormat::WebP => Box::new(webp::WebPDecoder::new(reader)?),
            #[cfg(feature = "tiff")]
            ImageFormat::Tiff => Box::new(tiff::TiffDecoder::new(reader)?),
            #[cfg(feature = "tga")]
            ImageFormat::Tga => Box::new(tga::TgaDecoder::new(reader)?),
            #[cfg(feature = "bmp")]
            ImageFormat::Bmp => Box::new(bmp::BmpDecoder::new_with_spec_compliance(
                reader,
                spec_compliance,
            )?),
            #[cfg(feature = "ico")]
            ImageFormat::Ico => Box::new(ico::IcoDecoder::new(reader)?),
            #[cfg(feature = "hdr")]
            ImageFormat::Hdr => Box::new(hdr::HdrDecoder::new_with_spec_compliance(
                reader,
                spec_compliance,
            )?),
            #[cfg(feature = "exr")]
            ImageFormat::OpenExr => Box::new(openexr::OpenExrDecoder::new(reader)?),
            #[cfg(feature = "pnm")]
            ImageFormat::Pbm
            | ImageFormat::Pgm
            | ImageFormat::Ppm
            | ImageFormat::Pnm
            | ImageFormat::Pam => Box::new(pnm::PnmDecoder::new(reader)?),
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
        let mut decoder =
            Self::make_decoder(self.require_format()?, self.inner, self.spec_compliance)?;
        decoder.set_limits(self.limits)?;
        Ok(decoder)
    }

    /// Convert the file into a reader object.
    pub fn into_reader(mut self) -> ImageResult<ImageReader<'a>> {
        let format = self.require_format()?;
        let decoder = Self::make_decoder(format, self.inner, self.spec_compliance)?;
        let mut reader = ImageReader::from_decoder(decoder);
        reader.set_limits(self.limits)?;
        Ok(reader)
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
        let len = io::copy(
            // Accept shorter files but read at most 16 bytes.
            &mut self.inner.by_ref().take(16),
            &mut Cursor::new(&mut start[..]),
        )?;
        self.inner.seek_relative(-(len as i64))?;
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
        let layout = decoder.prepare_image()?;
        Ok(layout.layout.dimensions())
    }

    /// Read the image (replaces `load`).
    ///
    /// Uses the current format to construct the correct reader for the format.
    ///
    /// If no format was determined, returns an `ImageError::Unsupported`.
    pub fn decode(self) -> ImageResult<DynamicImage> {
        let (image, _meta) = self.into_reader()?.decode()?;
        Ok(image)
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
    /// Settings of the reader, not the underlying decoder.
    ///
    /// Those apply to each individual `read_image` call, i.e. can be modified during reading.
    settings: ImageReaderSettings,
    /// Remaining limits for allocations by the reader.
    limits: Limits,
    /// A buffered cache of the last image attributes.
    last_attributes: DecodedImageAttributes,
    /// The metadata of formats is stored in varying places.
    metadata_buffers: MetadataBuffers,
}

#[derive(Default)]
struct MetadataBuffers {
    exif: MetadataBlock,
    icc: MetadataBlock,
    xmp: MetadataBlock,
    iptc: MetadataBlock,
    first_meta_retrieved: bool,
}

/// Buffer state for one item of metadata, to surface the error at the right time.
#[derive(Default)]
enum MetadataBlock {
    /// No buffered metadata.
    #[default]
    None,
    Ok(Vec<u8>),
    /// There was an error acquiring the metadata, this is the original error.
    Err(ImageError),
    /// The error was already polled. We continue to error but now with a replacement.
    ErrorTaken,
    /// The error was an `Unsupported`, similar to `ErrorTaken` but also return as
    /// `ImageError::Unsupported` with the original format hint.
    Unsupported(ImageFormatHint),
}

impl MetadataBlock {
    fn is_not_none(&self) -> bool {
        !matches!(self, MetadataBlock::None)
    }

    fn get(&mut self) -> ImageResult<Option<Vec<u8>>> {
        match self {
            MetadataBlock::None => Ok(None),
            MetadataBlock::Ok(data) => Ok(Some(data.clone())),
            MetadataBlock::Err(err) => {
                // Doing a little dance to change the variant to ErrorTaken in-place.
                let replacement_err = ImageError::Parameter(ParameterError::from_kind(
                    ParameterErrorKind::FailedAlready,
                ));

                let err = core::mem::replace(err, replacement_err);

                *self = if let ImageError::Unsupported(e) = &err {
                    MetadataBlock::Unsupported(e.format_hint())
                } else {
                    MetadataBlock::ErrorTaken
                };

                Err(err)
            }
            MetadataBlock::ErrorTaken => Err(ImageError::Parameter(ParameterError::from_kind(
                ParameterErrorKind::FailedAlready,
            ))),
            MetadataBlock::Unsupported(hint) => Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    hint.clone(),
                    UnsupportedErrorKind::GenericFeature("metadata".to_string()),
                ),
            )),
        }
    }
}

#[derive(Clone, Copy)]
struct ImageReaderSettings {
    apply_orientation: bool,
}

impl Default for ImageReaderSettings {
    fn default() -> Self {
        ImageReaderSettings {
            apply_orientation: true,
        }
    }
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
            spec_compliance: SpecCompliance::default(),
        })
    }
}

impl ImageReader<'_> {
    /// Query the layout that the image will have.
    pub fn peek_layout(&mut self) -> ImageResult<DecoderPreparedImage> {
        self.inner.prepare_image()
    }

    /// Decode the next image into a `DynamicImage`.
    ///
    /// # Examples
    ///
    #[cfg_attr(feature = "png", doc = "```")]
    #[cfg_attr(not(feature = "png"), doc = "```no_run")]
    /// use image::ImageReader;
    ///
    /// let mut reader = ImageReader::open("tests/images/png/iptc.png")?;
    /// let (data, mut meta) = reader.decode()?;
    ///
    /// // This image has IPTC metadata attached to it.
    /// let iptc = meta.iptc_metadata()?;
    /// assert!(iptc.is_some());
    ///
    /// # Ok::<_, image::error::ImageError>(())
    /// ```
    ///
    /// # Related
    ///
    /// If you want to enable buffer reuse, consider using [`Self::decode_into`] will can use an
    /// existing buffer in some instances.
    pub fn decode(&mut self) -> ImageResult<(DynamicImage, DecodedImageMetadata<'_>)> {
        let mut empty = DynamicImage::default();
        let meta = self.decode_to_dynimage(&mut empty)?;
        Ok((empty, meta))
    }

    /// Decode an image into a provided buffer and retrieve metadata.
    ///
    /// # Examples
    ///
    /// ```ignore
    /// // Enable if exposed.
    /// use image::{DynamicImage, ImageReader};
    /// use glob::glob;
    ///
    /// let mut buffer = DynamicImage::default();
    ///
    /// for entry in glob("tests/images/**/*.png").unwrap() {
    ///     let Ok(path) = entry else {
    ///         continue;
    ///     };
    ///
    ///     let mut reader = ImageReader::open(path)?;
    ///     let mut meta = reader.decode_to_dynimage(&mut buffer)?;
    ///     // …
    /// # break; // Avoid actually looping in the test here.
    /// }
    ///
    /// # Ok::<_, image::error::ImageError>(())
    /// ```
    pub(crate) fn decode_to_dynimage(
        &mut self,
        image: &mut DynamicImage,
    ) -> ImageResult<DecodedImageMetadata<'_>> {
        let layout = self.inner.prepare_image()?;
        self.fill_header_metadata_if_any();

        // This is technically redundant but it's also cheap.
        self.limits.check_layout_dimensions(&layout)?;
        // Check that we do not allocate a bigger buffer than we are allowed to
        // FIXME: should this rather go in `DynamicImage::from_decoder` somehow?
        self.limits.reserve(layout.total_bytes())?;

        // Retrieve the raw image data as indicated by the layout.
        self.last_attributes = image.decode_raw(self.inner.as_mut(), layout)?;

        let mut meta = DecodedImageMetadata {
            inner: self.inner.as_mut(),
            attributes: &mut self.last_attributes,
            metadata_buffers: &mut self.metadata_buffers,
        };

        meta.apply_metdata(&self.settings, image)?;

        Ok(meta)
    }

    /// Decode the next image into a pre-allocated buffer.
    ///
    /// Note that this will produce raw image data. You'll be on your own to ensure that metadata
    /// such as the orientation of the image or color space transformations is accurately
    /// represented.
    ///
    /// # Examples
    ///
    #[cfg_attr(feature = "png", doc = "```")]
    #[cfg_attr(not(feature = "png"), doc = "```no_run")]
    /// use image::ImageReader;
    ///
    /// let mut reader = ImageReader::open("tests/images/png/iptc.png")?;
    /// let buf_size = reader.peek_layout()?.total_bytes();
    /// let mut buffer = vec![0; buf_size as usize];
    ///
    /// let mut meta = reader.decode_into(&mut buffer)?;
    /// // This image also has IPTC metadata attached to it.
    /// let iptc = meta.iptc_metadata()?;
    /// assert!(iptc.is_some());
    ///
    /// # Ok::<_, image::error::ImageError>(())
    /// ```
    pub fn decode_into(&mut self, buffer: &mut [u8]) -> ImageResult<DecodedImageMetadata<'_>> {
        let layout = self.inner.prepare_image()?;
        self.fill_header_metadata_if_any();

        let actual = buffer.len();

        if usize::try_from(layout.total_bytes()).ok() != Some(actual) {
            return Err(ImageError::Parameter(ParameterError::from_kind(
                ParameterErrorKind::BufferSizeMismatch,
            )));
        }

        self.limits.check_layout_dimensions(&layout)?;

        self.last_attributes = self.inner.read_image(buffer)?;

        Ok(DecodedImageMetadata {
            inner: self.inner.as_mut(),
            attributes: &mut self.last_attributes,
            metadata_buffers: &mut self.metadata_buffers,
        })
    }

    /// Skip the next image, discard its image data.
    ///
    /// This will attempt to read the image data with as little allocation as possible while still
    /// running the usual verification routines. It will inform the underlying decoder that it is
    /// uninterested in all of the image data, then run its decoding routine.
    pub fn skip_image(&mut self) -> ImageResult<()> {
        // TODO: with `viewbox` (temporarily removed) we can inform the decoder that no data is
        // required which may be quite efficient. Other variants of achieving the same may also be
        // possible. We can just try out until one works.

        // Some decoders may still want a buffer, so we can't fully ignore it.
        let layout = self.inner.prepare_image()?;
        self.fill_header_metadata_if_any();

        // This is technically redundant but it's also cheap.
        self.limits.check_layout_dimensions(&layout)?;
        let bytes = layout.total_bytes();

        if bytes < 512 {
            let mut stack = [0u8; 512];
            self.inner.read_image(&mut stack[..bytes as usize])?;
        } else {
            // Check that we do not allocate a bigger buffer than we are allowed to
            // FIXME: should this rather go in `DynamicImage::from_decoder` somehow?
            // Or should we make an extension method on `ImageDecoder`?
            let mut placeholder = DynamicImage::default();
            self.limits.reserve(bytes)?;
            placeholder.decode_raw(self.inner.as_mut(), layout)?;
            self.limits.free(bytes);
        }

        Ok(())
    }

    /// Get the animation attributes of the file if any.
    pub fn animation_attributes(&mut self) -> Option<DecodedAnimationAttributes> {
        let _ = self.inner.prepare_image();
        self.inner.animation_attributes()
    }

    /// Must be called after `peek_layout`.
    ///
    /// Polls the underlying decoder for any `InHeader` metadata that is constant across a file,
    /// applicable to all images, and appears early.
    fn fill_header_metadata_if_any(&mut self) {
        type MetadataFn<'a> =
            fn(&mut (dyn ImageDecoder + 'a)) -> Result<Option<Vec<u8>>, ImageError>;

        // We retrieve `InHeader` metadata only once, before reading any our images.
        let first_meta_retrieved = self.metadata_buffers.first_meta_retrieved;
        let format_attrs = self.inner.format_attributes();

        let attributes = [
            (
                format_attrs.exif,
                &mut self.metadata_buffers.exif,
                <dyn ImageDecoder + '_>::exif_metadata as MetadataFn,
            ),
            (
                format_attrs.icc,
                &mut self.metadata_buffers.icc,
                <dyn ImageDecoder + '_>::icc_profile as MetadataFn,
            ),
            (
                format_attrs.xmp,
                &mut self.metadata_buffers.xmp,
                <dyn ImageDecoder + '_>::xmp_metadata as MetadataFn,
            ),
            (
                format_attrs.iptc,
                &mut self.metadata_buffers.iptc,
                <dyn ImageDecoder + '_>::iptc_metadata as MetadataFn,
            ),
        ];

        for (hint, buffer, getter) in attributes {
            let should_buffer_now = match hint {
                DecodedMetadataHint::InHeader => !first_meta_retrieved,
                DecodedMetadataHint::PerImage => true,
                DecodedMetadataHint::Unsupported | DecodedMetadataHint::None => false,
            };

            if !should_buffer_now {
                continue;
            }

            // We might have already tried this and succeeded. Expect the same result as last time
            // but avoids the allocation associated with that. A repeated `None` should be cheap,
            // most probably. This holds for variants that retrieve it once.
            if matches!(hint, DecodedMetadataHint::InHeader) && buffer.is_not_none() {
                continue;
            }

            match getter(self.inner.as_mut()) {
                Ok(None) => *buffer = MetadataBlock::None,
                Ok(Some(metadata)) => *buffer = MetadataBlock::Ok(metadata),
                Err(err) => {
                    *buffer = MetadataBlock::Err(err);
                }
            }
        }

        // Note: on error we do not set this flag. You can try again.
        self.metadata_buffers.first_meta_retrieved = true;
    }
}

impl<'stream> ImageReader<'stream> {
    /// Open image data as a readable stream of image(s).
    ///
    /// The format is guessed from a fixed array of bytes at stream's start. Hooks can be
    /// configured to customize this behavior, see [`hooks`](crate::hooks) for details.
    ///
    /// The reader will use default limits.
    ///
    /// # Examples
    ///
    #[cfg_attr(feature = "png", doc = "```")]
    #[cfg_attr(not(feature = "png"), doc = "```no_run")]
    /// use std::io::{BufReader, Cursor};
    /// use image::ImageReader;
    ///
    /// let binary_data: Vec<u8> = /* */
    ///     std::fs::read("tests/images/png/transparency/acid2.png")?;
    /// let stream = BufReader::new(Cursor::new(binary_data));
    ///
    /// let mut reader = ImageReader::new(stream)?;
    /// let (data, meta) = reader.decode()?;
    ///
    /// # Ok::<_, image::error::ImageError>(())
    /// ```
    ///
    /// # Related
    ///
    /// Use [`ImageReaderOptions`] to configure the reader in detail before use. In the simple case
    /// where you only access a single image without looking at its metadata you may call
    /// [`ImageReaderOptions::decode`] directly without creating an [`ImageReader`].
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
    /// The reader will use default limits.
    ///
    #[cfg_attr(feature = "png", doc = "```")]
    #[cfg_attr(not(feature = "png"), doc = "```no_run")]
    /// use image::ImageReader;
    ///
    /// let mut reader = ImageReader::open("tests/images/png/transparency/acid2.png")?;
    /// let (data, meta) = reader.decode()?;
    ///
    /// # Ok::<_, image::error::ImageError>(())
    /// ```
    ///
    /// # Related
    ///
    /// Use [`ImageReaderOptions`] to configure the reader in detail before use. In the simple case
    /// where you only access a single image without looking at its metadata you may call
    /// [`ImageReaderOptions::decode`] directly without creating an [`ImageReader`].
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
            settings: ImageReaderSettings::default(),
            limits: Limits::default(),
            last_attributes: Default::default(),
            metadata_buffers: MetadataBuffers::default(),
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
    /// [`SequenceControl::None`]. Decoding can return
    /// [`ParameterError`](`crate::error::ParameterError`) in [`ImageDecoder::peek_layout`] or
    /// [`ImageDecoder::read_image`] with kind set to [`None`](crate::io::SequenceControl::None),
    /// which is also treated as end of stream. This may be used by decoders which can not
    /// determine the number of images in advance.
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

            let mut frame = DynamicImage::default();
            let frame_decoded = match self.decode_to_dynimage(&mut frame) {
                Ok(frame) => frame,
                Err(ref err) if is_end_reached(err) => return None,
                Err(err) => return Some(Err(err)),
            };

            let x = frame_decoded.attributes().x;
            let y = frame_decoded.attributes().y;
            let delay = frame_decoded.attributes().delay.unwrap_or(no_delay);
            let frame = frame.into_rgba8();

            let frame = Frame::from_parts(frame, x, y, delay);
            Some(Ok(frame))
        });

        Frames::new(Box::new(iter))
    }
}

/// Result of [`ImageReader::decode_into`] that provides access to metadata.
pub struct DecodedImageMetadata<'reader> {
    inner: &'reader mut (dyn ImageDecoder + 'reader),
    attributes: &'reader mut DecodedImageAttributes,
    metadata_buffers: &'reader mut MetadataBuffers,
}

impl<'lt> DecodedImageMetadata<'lt> {
    /// Get the EXIF metadata of the previous image if any.
    pub fn exif_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        Self::access_block_with(
            &mut self.metadata_buffers.exif,
            self.inner.format_attributes().exif,
            self.inner,
            <dyn ImageDecoder + '_>::exif_metadata,
        )
    }

    /// Get the ICC profile of the previous image if any.
    pub fn icc_profile(&mut self) -> ImageResult<Option<Vec<u8>>> {
        Self::access_block_with(
            &mut self.metadata_buffers.icc,
            self.inner.format_attributes().icc,
            self.inner,
            <dyn ImageDecoder + '_>::icc_profile,
        )
    }

    /// Get the XMP metadata of the previous image if any.
    pub fn xmp_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        Self::access_block_with(
            &mut self.metadata_buffers.xmp,
            self.inner.format_attributes().xmp,
            self.inner,
            <dyn ImageDecoder + '_>::xmp_metadata,
        )
    }

    /// Get the IPTC metadata of the previous image if any.
    pub fn iptc_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        Self::access_block_with(
            &mut self.metadata_buffers.iptc,
            self.inner.format_attributes().iptc,
            self.inner,
            <dyn ImageDecoder + '_>::iptc_metadata,
        )
    }

    fn apply_metdata(
        &mut self,
        settings: &ImageReaderSettings,
        image: &mut DynamicImage,
    ) -> Result<(), ImageError> {
        // Run all the metadata extraction which we may need.
        let icc = self.icc_profile()?;
        let exif = self.exif_metadata()?;

        // Apply the profile. If the profile itself is not valid or not present you get the default
        // presumption: `sRGB`. Otherwise we will try to make sense of the profile and if it is not
        // RGB we'll treat it as unspecified so that downstream will know that our handling of this
        // _existing_ profile was not / could not be done with full fidelity.
        if let Some(icc) = icc {
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

        let mut orientation = self.attributes.orientation;

        // Determine which orientation to use.
        if orientation.is_none() {
            orientation = exif.and_then(|chunk| Orientation::from_exif_chunk(&chunk));
        }

        self.attributes.orientation = orientation;

        if settings.apply_orientation {
            if let Some(orient) = orientation {
                image.apply_orientation(orient);
            }
        }

        Ok(())
    }

    fn access_block_with<'l>(
        block: &mut MetadataBlock,
        meta: DecodedMetadataHint,
        decoder: &mut (dyn ImageDecoder + 'l),
        access: fn(&'_ mut (dyn ImageDecoder + 'l)) -> ImageResult<Option<Vec<u8>>>,
    ) -> ImageResult<Option<Vec<u8>>> {
        match meta {
            DecodedMetadataHint::InHeader => {
                if matches!(block, MetadataBlock::ErrorTaken) {
                    // We can retry this, prefer rechecking from the source.
                    access(decoder)
                } else {
                    block.get()
                }
            }
            DecodedMetadataHint::PerImage => {
                // This error is sensitive to the timing (after read_image it may or may not refer
                // to the next image until we access the decoder with `peek_layout` again. We don't
                // want to guess, any call after the first may substitute the error.
                block.get()
            }
            DecodedMetadataHint::Unsupported | DecodedMetadataHint::None => Ok(None),
        }
    }

    /// Get auxiliary attributes of the previous image.
    pub fn attributes(&self) -> &DecodedImageAttributes {
        self.attributes
    }
}

#[cfg(test)]
mod tests {
    use crate::{error::DecodingError, io::FormatAttributes};

    use super::*;

    struct InjectedReader {
        attr: FormatAttributes,
        xmp_metadata: InjectedXmp,
        xmp_per_image: Vec<InjectedXmp>,
        image: usize,
    }

    #[derive(Clone, Debug, Default)]
    enum InjectedXmp {
        #[default]
        None,
        Data(Vec<u8>),
        Unsupported,
        DecodeErr,
    }

    impl ImageDecoder for InjectedReader {
        fn prepare_image(&mut self) -> ImageResult<DecoderPreparedImage> {
            self.xmp_metadata = self
                .xmp_per_image
                .get(self.image)
                .cloned()
                .unwrap_or_default();
            Ok(DecoderPreparedImage::new(0, 0, crate::ColorType::Rgba8))
        }

        fn format_attributes(&self) -> FormatAttributes {
            self.attr.clone()
        }

        fn xmp_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
            match self.xmp_metadata {
                InjectedXmp::None => Ok(None),
                InjectedXmp::Data(ref data) => Ok(Some(data.clone())),
                InjectedXmp::DecodeErr => Err(ImageError::Decoding(DecodingError::new(
                    ImageFormatHint::Unknown,
                    "simulating that XMP metadata could not be decoded".to_string(),
                ))),
                InjectedXmp::Unsupported => Err(ImageError::Unsupported(
                    UnsupportedError::from_format_and_kind(
                        ImageFormatHint::Unknown,
                        UnsupportedErrorKind::GenericFeature("".into()),
                    ),
                )),
            }
        }

        fn read_image(&mut self, _: &mut [u8]) -> ImageResult<DecodedImageAttributes> {
            if matches!(self.attr.xmp, DecodedMetadataHint::PerImage) {
                self.xmp_metadata = InjectedXmp::None;
            }

            self.image += 1;
            Ok(Default::default())
        }

        fn more_images(&self) -> SequenceControl {
            if Some(self.image) < self.xmp_per_image.len().checked_sub(1) {
                SequenceControl::MaybeMore
            } else {
                SequenceControl::None
            }
        }
    }

    #[test]
    fn in_header_data_applies() -> Result<(), ImageError> {
        const DATA: &[u8] = b"<xmp>FAKE</xmp>";

        let mut reader = ImageReader::from_decoder(Box::new(InjectedReader {
            attr: FormatAttributes {
                xmp: DecodedMetadataHint::InHeader,
                ..FormatAttributes::default()
            },
            xmp_metadata: InjectedXmp::default(),
            xmp_per_image: vec![InjectedXmp::Data(DATA.to_vec()), InjectedXmp::None],
            image: 0,
        }));

        let (_, mut meta) = reader.decode()?;
        assert_eq!(meta.xmp_metadata()?, Some(DATA.to_vec()));

        // In-Header applies to all images.
        let (_, mut meta) = reader.decode()?;
        assert_eq!(meta.xmp_metadata()?, Some(DATA.to_vec()));

        Ok(())
    }

    #[test]
    fn error_stays_error() -> Result<(), ImageError> {
        let mut reader = ImageReader::from_decoder(Box::new(InjectedReader {
            attr: FormatAttributes {
                xmp: DecodedMetadataHint::InHeader,
                ..FormatAttributes::default()
            },
            xmp_metadata: InjectedXmp::default(),
            xmp_per_image: vec![InjectedXmp::DecodeErr],
            image: 0,
        }));

        let (_, mut meta) = reader.decode()?;
        assert!(matches!(meta.xmp_metadata(), Err(ImageError::Decoding(_))));

        // In-header should retry the meta data acquisition.
        assert!(matches!(meta.xmp_metadata(), Err(ImageError::Decoding(_))));

        Ok(())
    }

    #[test]
    fn unsupported_error_repeats() -> Result<(), ImageError> {
        let mut reader = ImageReader::from_decoder(Box::new(InjectedReader {
            attr: FormatAttributes {
                xmp: DecodedMetadataHint::PerImage,
                ..FormatAttributes::default()
            },
            xmp_metadata: InjectedXmp::default(),
            xmp_per_image: vec![InjectedXmp::Unsupported],
            image: 0,
        }));

        let (_, mut meta) = reader.decode()?;
        assert!(matches!(
            meta.xmp_metadata(),
            Err(ImageError::Unsupported(_))
        ));

        assert!(matches!(
            meta.xmp_metadata(),
            Err(ImageError::Unsupported(_))
        ));

        Ok(())
    }
}
