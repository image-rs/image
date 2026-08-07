use core::num::NonZeroU32;
use std::io::{BufRead, Seek};

use png::{BlendOp, DisposeOp};

use crate::animation::Delay;
use crate::color::{ColorType, ExtendedColorType};
use crate::error::{
    DecodingError, ImageError, ImageResult, LimitError, LimitErrorKind, ParameterError,
    ParameterErrorKind, UnsupportedError, UnsupportedErrorKind,
};
use crate::io::decoder::DecodedMetadataHint;
use crate::io::{
    DecodedAnimationAttributes, DecodedImageAttributes, DecoderPreparedImage, FormatAttributes,
    SequenceControl,
};
use crate::math::Rect;
use crate::metadata::LoopCount;
use crate::{
    DynamicImage, GenericImage, GenericImageView, ImageDecoder, ImageFormat, ImageLayout, Limits,
    Luma, LumaA, Rgb, Rgba,
};

const IPTC_KEYS: &[&str] = &["Raw profile type iptc", "Raw profile type 8bim"];

/// Represents the inner state of a typical decoder:
/// 1. `Init` - the decoder has been created but not yet started decoding.
/// 2. `Decoding` - the decoder has read its reader and/or started being used for decoding.
/// 3. `Failed` - the decoder has encountered an error and cannot continue.
enum InnerState<I, D> {
    Init(I),
    Decoding(D),
    Failed,
}
impl<I, D> InnerState<I, D> {
    fn decode(&mut self, start_decoding: impl FnOnce(I) -> ImageResult<D>) -> ImageResult<&mut D> {
        if matches!(self, InnerState::Init(_)) {
            let decoder = match std::mem::replace(self, InnerState::Failed) {
                InnerState::Init(decoder) => start_decoding(decoder)?,
                _ => unreachable!(),
            };
            *self = InnerState::Decoding(decoder);
        }

        match self {
            InnerState::Init(_) => unreachable!(),
            InnerState::Decoding(decoder) => Ok(decoder),
            InnerState::Failed => Err(failed_already()),
        }
    }
}

/// PNG decoder that can decode both animated and non-animated PNGs.
///
/// ## Decoding animated PNGs
///
/// Reading the complete animation requires more memory than reading the data from the IDAT
/// frame. Multiple frame buffers need to be reserved at the same time.
///
/// If something is not supported or a limit is violated then the decoding step that requires
/// them will fail and an error will be returned instead of the frame. No further frames will
/// be returned.
pub struct PngDecoder<R: BufRead + Seek> {
    inner: InnerState<BasePngDecoder<R>, Decoder<R>>,
}
enum Decoder<R: BufRead + Seek> {
    Image(BasePngDecoder<R>),
    Animation(ApngDecoder<R>),
}

impl<R: BufRead + Seek> PngDecoder<R> {
    /// Creates a new decoder that decodes from the stream ```r```
    pub fn new(r: R) -> Self {
        Self::with_limits(r, Limits::no_limits())
    }

    /// Creates a new decoder that decodes from the stream ```r``` with the given limits.
    pub fn with_limits(r: R, limits: Limits) -> Self {
        Self {
            inner: InnerState::Init(BasePngDecoder::with_limits(r, limits)),
        }
    }

    /// Returns the gamma value of the image or None if no gamma value is indicated.
    ///
    /// If an sRGB chunk is present this method returns a gamma value of 0.45455 and ignores the
    /// value in the gAMA chunk. This is the recommended behavior according to the PNG standard:
    ///
    /// > When the sRGB chunk is present, [...] decoders that recognize the sRGB chunk but are not
    /// > capable of colour management are recommended to ignore the gAMA and cHRM chunks, and use
    /// > the values given above as if they had appeared in gAMA and cHRM chunks.
    pub fn gamma_value(&mut self) -> ImageResult<Option<f64>> {
        self.base_decoder()?.gamma_value()
    }

    /// Returns if the image contains an animation.
    ///
    /// This returns `true` if and only if [`ImageDecoder::animation_attributes`] returns `Some`.
    ///
    /// Note that the file itself decides if the default image is considered to be part of the
    /// animation. When it is not the common interpretation is to use it as a thumbnail.
    pub fn is_apng(&mut self) -> ImageResult<bool> {
        Ok(matches!(self.decoder()?, Decoder::Animation(_)))
    }

    fn decoder(&mut self) -> ImageResult<&mut Decoder<R>> {
        self.inner.decode(|mut decoder| {
            let animation_control = decoder.ensure_reader_and_header()?.info().animation_control;
            if let Some(animation_control) = animation_control {
                Ok(Decoder::Animation(ApngDecoder::new(
                    decoder,
                    animation_control,
                )?))
            } else {
                Ok(Decoder::Image(decoder))
            }
        })
    }
    fn base_decoder(&mut self) -> ImageResult<&mut BasePngDecoder<R>> {
        match &mut self.inner {
            InnerState::Init(decoder) => Ok(decoder),
            InnerState::Decoding(Decoder::Image(decoder)) => Ok(decoder),
            InnerState::Decoding(Decoder::Animation(decoder)) => Ok(&mut decoder.inner),
            InnerState::Failed => Err(failed_already()),
        }
    }

    fn has_more(&self) -> bool {
        match &self.inner {
            InnerState::Init(_) => true,
            InnerState::Decoding(Decoder::Image(_)) => {
                // TODO: Keep track of decoded images to return false after the first and only image has been decoded.
                true
            }
            InnerState::Decoding(Decoder::Animation(decoder)) => decoder.remaining > 0,
            InnerState::Failed => false,
        }
    }
}
impl<R: BufRead + Seek> ImageDecoder for PngDecoder<R> {
    fn format_attributes(&self) -> FormatAttributes {
        FormatAttributes {
            supports_animation: true,
            ..base_format_attributes()
        }
    }

    fn animation_attributes(&mut self) -> Option<DecodedAnimationAttributes> {
        // TODO: This silently swallows errors. This is a problem with the API design as errors cannot be returned.
        let Ok(Decoder::Animation(decoder)) = self.decoder() else {
            // not an animation
            return None;
        };

        let loop_count = NonZeroU32::new(decoder.num_plays)
            .map(LoopCount::Finite)
            .unwrap_or(LoopCount::Infinite);

        Some(DecodedAnimationAttributes { loop_count })
    }

    fn prepare_image(&mut self) -> ImageResult<DecoderPreparedImage> {
        if !self.has_more() {
            return Err(no_more_frames());
        }
        self.base_decoder()?.prepare_image()
    }

    fn set_limits(&mut self, limits: Limits) -> ImageResult<()> {
        self.base_decoder()?.set_limits(limits)
    }

    fn icc_profile(&mut self) -> ImageResult<Option<Vec<u8>>> {
        self.base_decoder()?.icc_profile()
    }

    fn exif_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        self.base_decoder()?.exif_metadata()
    }

    fn xmp_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        self.base_decoder()?.xmp_metadata()
    }

    fn iptc_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        self.base_decoder()?.iptc_metadata()
    }

    fn read_image(&mut self, buf: &mut [u8]) -> ImageResult<DecodedImageAttributes> {
        match self.decoder()? {
            Decoder::Image(decoder) => decoder.read_image(buf),
            Decoder::Animation(decoder) => decoder.mix_next_frame(buf),
        }
    }

    fn more_images(&self) -> SequenceControl {
        if self.has_more() {
            SequenceControl::MaybeMore
        } else {
            SequenceControl::None
        }
    }
}

/// The base PNG decoder for decoding metadata and the first frame (of thumbnail) of a PNG file.
struct BasePngDecoder<R: BufRead + Seek> {
    inner: InnerState<png::Decoder<R>, png::Reader<R>>,
    color_type: ColorType,
    limits: Limits,
}

impl<R: BufRead + Seek> BasePngDecoder<R> {
    /// Creates a new decoder that decodes from the stream ```r``` with the given limits.
    fn with_limits(r: R, limits: Limits) -> Self {
        let max_bytes = usize::try_from(limits.max_alloc.unwrap_or(u64::MAX)).unwrap_or(usize::MAX);
        let mut decoder = png::Decoder::new_with_limits(r, png::Limits { bytes: max_bytes });
        decoder.set_ignore_text_chunk(false);

        BasePngDecoder {
            inner: InnerState::Init(decoder),
            // We'll replace this once we have a reader.
            color_type: ColorType::L8,
            limits,
        }
    }

    fn ensure_reader_and_header(&mut self) -> ImageResult<&mut png::Reader<R>> {
        self.inner.decode(|mut decoder| {
            self.limits.check_support(&crate::LimitSupport::default())?;

            let info = decoder.read_header_info().map_err(ImageError::from_png)?;
            self.limits.check_dimensions(info.width, info.height)?;

            // By default the PNG decoder will scale 16 bpc to 8 bpc, so custom
            // transformations must be set. EXPAND preserves the default behavior
            // expanding bpc < 8 to 8 bpc.
            decoder.set_transformations(png::Transformations::EXPAND);
            let reader = decoder.read_info().map_err(ImageError::from_png)?;
            let (color_type, bits) = reader.output_color_type();

            self.color_type = to_supported_color_type(color_type, bits)?;

            Ok(reader)
        })
    }

    fn gamma_value(&mut self) -> ImageResult<Option<f64>> {
        let reader = self.ensure_reader_and_header()?;
        Ok(reader
            .info()
            .source_gamma
            .map(|x| f64::from(x.into_scaled()) / 100_000.0))
    }

    /// The maximum number of bytes iTXt and zTXt are allowed to decompress to.
    /// This guards against decompression bombs.
    fn text_decompress_limit(&mut self) -> usize {
        let max = png::text_metadata::DECOMPRESSION_LIMIT as u64;
        self.limits.max_alloc.unwrap_or(max).min(max) as usize
    }
}

fn to_supported_color_type(
    color_type: png::ColorType,
    bit_depth: png::BitDepth,
) -> ImageResult<ColorType> {
    match (color_type, bit_depth) {
        (png::ColorType::Grayscale, png::BitDepth::Eight) => Ok(ColorType::L8),
        (png::ColorType::Grayscale, png::BitDepth::Sixteen) => Ok(ColorType::L16),
        (png::ColorType::GrayscaleAlpha, png::BitDepth::Eight) => Ok(ColorType::La8),
        (png::ColorType::GrayscaleAlpha, png::BitDepth::Sixteen) => Ok(ColorType::La16),
        (png::ColorType::Rgb, png::BitDepth::Eight) => Ok(ColorType::Rgb8),
        (png::ColorType::Rgb, png::BitDepth::Sixteen) => Ok(ColorType::Rgb16),
        (png::ColorType::Rgba, png::BitDepth::Eight) => Ok(ColorType::Rgba8),
        (png::ColorType::Rgba, png::BitDepth::Sixteen) => Ok(ColorType::Rgba16),

        _ => Err(unsupported_color(to_extended_color_type(
            color_type, bit_depth,
        ))),
    }
}
fn to_extended_color_type(
    color_type: png::ColorType,
    bit_depth: png::BitDepth,
) -> ExtendedColorType {
    match (color_type, bit_depth) {
        (png::ColorType::Grayscale, png::BitDepth::One) => ExtendedColorType::L1,
        (png::ColorType::Grayscale, png::BitDepth::Two) => ExtendedColorType::L2,
        (png::ColorType::Grayscale, png::BitDepth::Four) => ExtendedColorType::L4,
        (png::ColorType::Grayscale, png::BitDepth::Eight) => ExtendedColorType::L8,
        (png::ColorType::Grayscale, png::BitDepth::Sixteen) => ExtendedColorType::L16,
        (png::ColorType::GrayscaleAlpha, png::BitDepth::One) => ExtendedColorType::La1,
        (png::ColorType::GrayscaleAlpha, png::BitDepth::Two) => ExtendedColorType::La2,
        (png::ColorType::GrayscaleAlpha, png::BitDepth::Four) => ExtendedColorType::La4,
        (png::ColorType::GrayscaleAlpha, png::BitDepth::Eight) => ExtendedColorType::La8,
        (png::ColorType::GrayscaleAlpha, png::BitDepth::Sixteen) => ExtendedColorType::La16,
        (png::ColorType::Rgb, png::BitDepth::One) => ExtendedColorType::Rgb1,
        (png::ColorType::Rgb, png::BitDepth::Two) => ExtendedColorType::Rgb2,
        (png::ColorType::Rgb, png::BitDepth::Four) => ExtendedColorType::Rgb4,
        (png::ColorType::Rgb, png::BitDepth::Eight) => ExtendedColorType::Rgb8,
        (png::ColorType::Rgb, png::BitDepth::Sixteen) => ExtendedColorType::Rgb16,
        (png::ColorType::Rgba, png::BitDepth::One) => ExtendedColorType::Rgba1,
        (png::ColorType::Rgba, png::BitDepth::Two) => ExtendedColorType::Rgba2,
        (png::ColorType::Rgba, png::BitDepth::Four) => ExtendedColorType::Rgba4,
        (png::ColorType::Rgba, png::BitDepth::Eight) => ExtendedColorType::Rgba8,
        (png::ColorType::Rgba, png::BitDepth::Sixteen) => ExtendedColorType::Rgba16,
        (png::ColorType::Indexed, bit_depth) => ExtendedColorType::Unknown(bit_depth as u8),
    }
}

fn attributes_from_info(info: &png::Info<'_>) -> DecodedImageAttributes {
    let delay = info.frame_control().map(|fc| {
        // PNG delays are rations in seconds.
        let num = u32::from(fc.delay_num) * 1_000u32;
        let denom = match fc.delay_den {
            // The standard dictates to replace by 100 when the denominator is 0.
            0 => 100,
            d => u32::from(d),
        };

        Delay::from_numer_denom_ms(num, denom)
    });

    DecodedImageAttributes {
        // We do not set x_offset and y_offset since the decoder performs composition according
        // to Dispose and blend. For reading raw frames we'd pass the `fc.x_offset` here.
        delay,
        ..DecodedImageAttributes::default()
    }
}

fn unsupported_color(color: ExtendedColorType) -> ImageError {
    ImageError::Unsupported(UnsupportedError::from_format_and_kind(
        ImageFormat::Png.into(),
        UnsupportedErrorKind::Color(color),
    ))
}
fn no_more_frames() -> ImageError {
    ImageError::Parameter(ParameterError::from_kind(ParameterErrorKind::NoMoreData))
}
fn failed_already() -> ImageError {
    ImageError::Parameter(ParameterError::from_kind(ParameterErrorKind::FailedAlready))
}
fn decoding_started_already() -> ImageError {
    // TODO: This parameter error makes no sense for how this is used
    ImageError::Parameter(ParameterError::from_kind(ParameterErrorKind::NoMoreData))
}

/// PNG images are big endian. For 16 bit per channel and larger types, the buffer may need
/// to be reordered to native endianness per the contract of `read_image`. Assumes equal
/// depth which is the only supported output from `png` with our options.
fn big_endian_to_native_endian(buf: &mut [u8], color: ColorType) {
    let bytes_per_channel = color.bytes_per_pixel() / color.channel_count();

    match bytes_per_channel {
        1 => (), // No reodering necessary for u8
        2 => buf.as_chunks_mut::<2>().0.iter_mut().for_each(|c| {
            *c = u16::from_be_bytes(*c).to_ne_bytes();
        }),
        // not emitted by png crate
        _ => unreachable!(),
    }
}

fn base_format_attributes() -> FormatAttributes {
    FormatAttributes {
        // is any sort of iTXT chunk.
        // FIXME: we do not collect these in advance.
        xmp: DecodedMetadataHint::InHeader,
        // is any sort of iTXT chunk.
        // FIXME: we do not collect these in advance.
        iptc: DecodedMetadataHint::InHeader,
        // see iCCP chunk order.
        icc: DecodedMetadataHint::InHeader,
        // see eXIf chunk order.
        exif: DecodedMetadataHint::InHeader,
        ..FormatAttributes::default()
    }
}

impl<R: BufRead + Seek> ImageDecoder for BasePngDecoder<R> {
    fn prepare_image(&mut self) -> ImageResult<DecoderPreparedImage> {
        let reader = self.ensure_reader_and_header()?;
        let (width, height) = reader.info().size();
        Ok(DecoderPreparedImage::new(width, height, self.color_type))
    }

    fn format_attributes(&self) -> FormatAttributes {
        base_format_attributes()
    }

    fn icc_profile(&mut self) -> ImageResult<Option<Vec<u8>>> {
        let reader = self.ensure_reader_and_header()?;
        Ok(reader.info().icc_profile.as_ref().map(|x| x.to_vec()))
    }

    fn exif_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        let reader = self.ensure_reader_and_header()?;
        Ok(reader.info().exif_metadata.as_ref().map(|x| x.to_vec()))
    }

    fn xmp_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        let decompression_limit = self.text_decompress_limit();
        let reader = self.ensure_reader_and_header()?;

        if let Some(mut itx_chunk) = reader
            .info()
            .utf8_text
            .iter()
            .find(|chunk| chunk.keyword == super::XMP_KEY)
            .cloned()
        {
            itx_chunk
                .decompress_text_with_limit(decompression_limit)
                .map_err(ImageError::from_png)?;
            return itx_chunk
                .get_text()
                .map(|text| Some(text.into_bytes()))
                .map_err(ImageError::from_png);
        }

        Ok(None)
    }

    fn iptc_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        let decompression_limit = self.text_decompress_limit();
        let reader = self.ensure_reader_and_header()?;

        if let Some(mut text_chunk) = reader
            .info()
            .compressed_latin1_text
            .iter()
            .find(|chunk| IPTC_KEYS.iter().any(|key| chunk.keyword.contains(key)))
            .cloned()
        {
            text_chunk
                .decompress_text_with_limit(decompression_limit)
                .map_err(ImageError::from_png)?;
            return text_chunk
                .get_text()
                .map(|text| Some(text.into_bytes()))
                .map_err(ImageError::from_png);
        }

        if let Some(text_chunk) = reader
            .info()
            .uncompressed_latin1_text
            .iter()
            .find(|chunk| IPTC_KEYS.iter().any(|key| chunk.keyword.contains(key)))
            .cloned()
        {
            return Ok(Some(text_chunk.text.into_bytes()));
        }
        Ok(None)
    }

    fn read_image(&mut self, buf: &mut [u8]) -> ImageResult<DecodedImageAttributes> {
        let layout = self.prepare_image()?;
        assert_eq!(u64::try_from(buf.len()), Ok(layout.total_bytes()));

        let reader = self.ensure_reader_and_header()?;
        let info = reader.info();
        let original_color_type = to_extended_color_type(info.color_type, info.bit_depth);
        reader.next_frame(buf).map_err(ImageError::from_png)?;

        big_endian_to_native_endian(buf, layout.layout.color);

        Ok(DecodedImageAttributes {
            original_color_type: Some(original_color_type),
            ..DecodedImageAttributes::default()
        })
    }

    fn set_limits(&mut self, limits: Limits) -> ImageResult<()> {
        limits.check_support(&crate::LimitSupport::default())?;

        if let InnerState::Init(decoder) = &mut self.inner {
            decoder.set_limits(png::Limits {
                bytes: match limits.max_alloc {
                    None => usize::MAX,
                    Some(limit) => limit.try_into().unwrap_or(usize::MAX),
                },
            });

            self.limits = limits;
            Ok(())
        } else {
            Err(decoding_started_already())
        }
    }
}

/// An animated adapter of [`BasePngDecoder`].
struct ApngDecoder<R: BufRead + Seek> {
    inner: BasePngDecoder<R>,
    /// The current output buffer.
    current: Option<DynamicImage>,
    /// The previous output buffer, used for dispose op previous.
    previous: Option<DynamicImage>,
    /// The dispose op of the current frame.
    dispose: DisposeOp,
    /// Buffer to put the frame data which is to be composed onto the current frame.
    raw_frame_buffer: Vec<u8>,

    /// The region to dispose of the previous frame.
    dispose_region: Option<Rect>,
    /// The number of image still expected to be able to load.
    remaining: u32,
    /// The number of times the animation should be played.
    num_plays: u32,
    /// The next (first) image is the thumbnail.
    has_thumbnail: bool,
}

impl<R: BufRead + Seek> ApngDecoder<R> {
    fn new(
        mut inner: BasePngDecoder<R>,
        animation_control: png::AnimationControl,
    ) -> ImageResult<Self> {
        let reader = inner.ensure_reader_and_header()?;

        // If the IDAT has no fcTL then it is not part of the animation counted by
        // num_frames. All following fdAT chunks must be preceded by an fcTL
        let has_thumbnail = reader.info().frame_control.is_none();

        Ok(ApngDecoder {
            inner,
            current: None,
            previous: None,
            raw_frame_buffer: vec![],
            dispose: DisposeOp::Background,
            dispose_region: None,
            remaining: animation_control.num_frames,
            num_plays: animation_control.num_plays,
            has_thumbnail,
        })
    }

    /// Decode one subframe and overlay it on the canvas.
    fn mix_next_frame(&mut self, buf: &mut [u8]) -> Result<DecodedImageAttributes, ImageError> {
        // Remove this image from remaining.
        self.remaining = match self.remaining.checked_sub(1) {
            None => return Err(no_more_frames()),
            Some(next) => next,
        };

        // Allocate the buffers, honoring the memory limits
        let layout = self.inner.prepare_image()?;
        let ImageLayout {
            width,
            height,
            color,
        } = layout.layout;

        assert_eq!(u64::try_from(buf.len()), Ok(layout.total_bytes()));

        // Shorten ourselves to 0 in case of error.
        let remaining = self.remaining;
        self.remaining = 0;

        // Skip the thumbnail that is not part of the animation.
        if self.has_thumbnail {
            let reader = self.inner.ensure_reader_and_header()?;
            reader.next_frame(buf).map_err(ImageError::from_png)?;
            self.has_thumbnail = false;
        }

        {
            let limits = &mut self.inner.limits;

            if self.previous.is_none() {
                limits.reserve_buffer(width, height, color)?;
                self.previous = Some(DynamicImage::new(width, height, color));
            }

            if self.current.is_none() {
                limits.reserve_buffer(width, height, color)?;
                self.current = Some(DynamicImage::new(width, height, color));
            }
        }

        // We've initialized them earlier in this function
        let previous = self.previous.as_mut().unwrap();
        let current = self.current.as_mut().unwrap();

        // Dispose of the previous frame.
        match self.dispose {
            DisposeOp::None => {
                previous.clone_from(current);
            }
            DisposeOp::Background => {
                previous.clone_from(current);
                if let Some(rect) = self.dispose_region {
                    let mut region_current = current.sub_image(rect);

                    // FIXME: This is a workaround for the fact that `pixels_mut` is not implemented
                    let pixels: Vec<_> = region_current.pixels().collect();

                    for (x, y, _) in &pixels {
                        region_current.put_pixel(*x, *y, Rgba::from([0, 0, 0, 0]));
                    }
                } else {
                    // The first frame is always a background frame.
                    current.as_mut_bytes().fill(0);
                }
            }
            DisposeOp::Previous => {
                let rect = self
                    .dispose_region
                    .expect("The first frame must not set dispose=Previous");
                let region_previous = previous.sub_image(rect);
                current
                    .copy_from(&region_previous.to_image(), rect.x, rect.y)
                    .unwrap();
            }
        }

        // The allocations from now on are not going to persist,
        // and will be destroyed at the end of the scope.
        // Clone the limits so that any changes to them die with the allocations.
        let mut limits = self.inner.limits.clone();
        let reader = self.inner.ensure_reader_and_header()?;

        // Read next frame data.
        let raw_frame_size = reader.output_buffer_size().ok_or_else(|| {
            ImageError::Limits(LimitError::from_kind(LimitErrorKind::InsufficientMemory))
        })?;

        // The frame size depends on frame control. If possible, we want to read it into the
        // (temporary) output buffer that's been allocated for us anyways.
        let buffer = if raw_frame_size <= buf.len() {
            &mut buf[..raw_frame_size]
        } else if raw_frame_size <= self.raw_frame_buffer.len() {
            &mut self.raw_frame_buffer[..raw_frame_size]
        } else {
            limits.free_usize(self.raw_frame_buffer.len());
            limits.reserve_usize(raw_frame_size)?;
            self.raw_frame_buffer.resize(raw_frame_size, 0);
            &mut self.raw_frame_buffer[..]
        };

        // TODO: add `png::Reader::change_limits()` and call it here
        // to also constrain the internal buffer allocations in the PNG crate
        reader.next_frame(buffer).map_err(ImageError::from_png)?;

        big_endian_to_native_endian(buffer, color);

        // Find out how to interpret the decoded frame.
        let info = reader.info();
        let attributes = attributes_from_info(info);

        let (dispose_region, blend);
        match info.frame_control() {
            None => {
                dispose_region = Rect {
                    width: info.width,
                    height: info.height,
                    x: 0,
                    y: 0,
                };

                blend = BlendOp::Source;
            }
            Some(fc) => {
                dispose_region = Rect {
                    width: fc.width,
                    height: fc.height,
                    x: fc.x_offset,
                    y: fc.y_offset,
                };

                blend = fc.blend_op;
                self.dispose = fc.dispose_op;
            }
        }

        self.dispose_region = Some(dispose_region);

        match blend {
            BlendOp::Source => {
                copy_pixel_bytes(
                    current.as_mut_bytes(),
                    &layout.layout,
                    &buffer[..],
                    &dispose_region,
                );
            }
            BlendOp::Over => {
                // TODO: investigate speed, speed-ups, and bounds-checks.
                blend_pixel_bytes(
                    current.as_mut_bytes(),
                    &layout.layout,
                    &buffer[..],
                    &dispose_region,
                )
            }
        }

        // Ok, we can proceed with actually remaining images.
        self.remaining = remaining;

        // Return composited output buffer.
        buf.copy_from_slice(current.as_bytes());

        Ok(attributes)
    }
}

impl ImageError {
    fn from_png(err: png::DecodingError) -> ImageError {
        use png::DecodingError::*;
        match err {
            IoError(err) => ImageError::IoError(err),
            // The input image was not a valid PNG.
            err @ Format(_) => {
                ImageError::Decoding(DecodingError::new(ImageFormat::Png.into(), err))
            }
            // Other is used when:
            // - The decoder is polled for more animation frames despite being done (or not being animated
            //   in the first place).
            // - The output buffer does not have the required size.
            err @ Parameter(_) => ImageError::Parameter(ParameterError::from_kind(
                ParameterErrorKind::Generic(err.to_string()),
            )),
            LimitsExceeded => {
                ImageError::Limits(LimitError::from_kind(LimitErrorKind::InsufficientMemory))
            }
        }
    }
}

fn copy_pixel_bytes(bytes: &mut [u8], layout: &ImageLayout, from: &[u8], region: &Rect) {
    let bpp = usize::from(layout.color.bytes_per_pixel());

    let bytes_per_row = layout.width as usize * bpp;
    let bytes_per_copy = region.width as usize * bpp;

    let start = region.x as usize * bpp + region.y as usize * bytes_per_row;
    let from = &from[..region.height as usize * bytes_per_copy];

    for (target, src) in bytes[start..]
        .chunks_exact_mut(bytes_per_row)
        .zip(from.chunks_exact(bytes_per_copy))
    {
        target[..bytes_per_copy].copy_from_slice(src);
    }
}

fn blend_pixel_bytes(bytes: &mut [u8], layout: &ImageLayout, from: &[u8], region: &Rect) {
    fn inner<P: crate::Pixel>(bytes: &mut [u8], region: &[u8])
    where
        P::Subpixel: bytemuck::Pod,
    {
        let target = bytemuck::cast_slice_mut::<_, P::Subpixel>(bytes);
        let source = bytemuck::cast_slice::<_, P::Subpixel>(region);

        for (target, source) in target
            .chunks_exact_mut(usize::from(P::CHANNEL_COUNT))
            .zip(source.chunks_exact(usize::from(P::CHANNEL_COUNT)))
        {
            P::from_slice_mut(target).blend(P::from_slice(source));
        }
    }

    let row_transformer = match layout.color {
        ColorType::L8 => inner::<Luma<u8>>,
        ColorType::La8 => inner::<LumaA<u8>>,
        ColorType::Rgb8 => inner::<Rgb<u8>>,
        ColorType::Rgba8 => inner::<Rgba<u8>>,
        ColorType::L16 => inner::<Luma<u16>>,
        ColorType::La16 => inner::<LumaA<u16>>,
        ColorType::Rgb16 => inner::<Rgb<u16>>,
        ColorType::Rgba16 => inner::<Rgba<u16>>,
        ColorType::L32F | ColorType::La32F | ColorType::Rgb32F | ColorType::Rgba32F => {
            unreachable!("No floating point formats in PNG")
        }
    };

    let bpp = usize::from(layout.color.bytes_per_pixel());

    let bytes_per_row = layout.width as usize * bpp;
    let bytes_per_copy = region.width as usize * bpp;

    let start = region.x as usize * bpp + region.y as usize * bytes_per_row;
    let from = &from[..region.height as usize * bytes_per_copy];

    for (target, src) in bytes[start..]
        .chunks_exact_mut(bytes_per_row)
        .zip(from.chunks_exact(bytes_per_copy))
    {
        row_transformer(&mut target[..bytes_per_copy], src);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{io::free_functions::decoder_to_vec, ImageEncoder};
    use std::io::{BufReader, Cursor, Read};

    #[test]
    fn ensure_no_decoder_off_by_one() {
        let mut dec = PngDecoder::new(BufReader::new(
            std::fs::File::open("tests/images/png/bugfixes/debug_triangle_corners_widescreen.png")
                .unwrap(),
        ));

        let layout = dec
            .prepare_image()
            .expect("Unable to read PNG file (does it exist?)");

        assert_eq![(2000, 1000), layout.layout.dimensions()];

        assert_eq![
            ColorType::Rgb8,
            layout.layout.color,
            "Image MUST have the Rgb8 format"
        ];

        let (data, _) = decoder_to_vec(&mut dec).expect("Unable to read file");

        let correct_bytes = data
            .bytes()
            .map(|x| x.expect("Unable to read byte"))
            .collect::<Vec<u8>>();

        assert_eq![6_000_000, correct_bytes.len()];
    }

    #[test]
    fn underlying_error() {
        use std::error::Error;

        let mut not_png =
            std::fs::read("tests/images/png/bugfixes/debug_triangle_corners_widescreen.png")
                .unwrap();
        not_png[0] = 0;

        let mut decoder = PngDecoder::new(Cursor::new(&not_png));
        let error = decoder.prepare_image().err().unwrap();

        let _ = error
            .source()
            .unwrap()
            .downcast_ref::<png::DecodingError>()
            .expect("Caused by a png error");
    }

    #[test]
    fn encode_bad_color_type() {
        // regression test for issues #1663 and #2787
        let image = DynamicImage::new_rgb32f(1, 1);
        let mut target = Cursor::new(vec![]);
        assert!(image.write_to(&mut target, ImageFormat::Png).is_ok());
    }

    #[test]
    fn roundtrip_xmp() {
        let img = [255u8, 0, 0, 0, 255, 0, 0, 0, 255];
        let xmp = b"<x:xmpmeta xmlns:x=\"adobe:ns:meta/\"><rdf:RDF></rdf:RDF></x:xmpmeta>".to_vec();

        let mut encoded = Vec::new();
        {
            let mut encoder = super::super::PngEncoder::new(&mut encoded);
            encoder.set_xmp_metadata(xmp.clone()).unwrap();
            encoder
                .write_image(&img, 3, 1, ExtendedColorType::Rgb8)
                .expect("Could not encode image");
        }

        let mut decoder = PngDecoder::new(Cursor::new(&encoded));
        let _ = decoder.prepare_image().unwrap();
        let decoded_xmp = decoder
            .xmp_metadata()
            .expect("Error decoding XMP")
            .expect("XMP is empty");
        assert_eq!(xmp, decoded_xmp);
    }

    #[test]
    fn is_apng() {
        let file = BufReader::new(std::fs::File::open("tests/images/png/apng/ball.png").unwrap());
        let is_apng = PngDecoder::new(file).is_apng().unwrap();
        assert!(is_apng);
    }

    #[test]
    fn gamma() {
        let file = BufReader::new(std::fs::File::open("tests/images/png/apng/ball.png").unwrap());
        let gamma = PngDecoder::new(file).gamma_value().unwrap();
        assert_eq!(gamma, None);
    }

    #[test]
    fn apng_16_bits_per_channel() {
        let file = BufReader::new(std::fs::File::open("tests/images/png/apng/rgba16.png").unwrap());
        let decoder = PngDecoder::new(file);
        let reader = crate::ImageReader::from_decoder(Box::new(decoder));
        let frames = reader.into_frames().collect_frames().unwrap();
        assert_eq!(frames.len(), 3);

        let colors = [
            *frames[0].buffer().get_pixel(0, 0),
            *frames[1].buffer().get_pixel(0, 0),
            *frames[2].buffer().get_pixel(0, 0),
        ];
        assert_eq!(
            colors,
            [
                Rgba([255, 0, 0, 255]),
                Rgba([0, 255, 0, 255]),
                Rgba([0, 0, 255, 255]),
            ]
        );
    }
}
