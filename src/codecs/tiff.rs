//! Decoding and Encoding of TIFF Images
//!
//! TIFF (Tagged Image File Format) is a versatile image format that supports
//! lossless and lossy compression.
//!
//! # Related Links
//! * <http://partners.adobe.com/public/developer/tiff/index.html> - The TIFF specification
use std::io::{BufRead, Seek, Write};

use tiff::decoder::ifd::Value;
use tiff::decoder::{Decoder, DecodingResult};
use tiff::tags::Tag;

use crate::color::{ColorType, ExtendedColorType};
use crate::error::{
    DecodingError, EncodingError, ImageError, ImageResult, LimitError, LimitErrorKind,
    ParameterError, ParameterErrorKind, UnsupportedError, UnsupportedErrorKind,
};
use crate::metadata::Orientation;
use crate::{utils, ImageDecoder, ImageEncoder, ImageFormat};

const TAG_XML_PACKET: Tag = Tag::Unknown(700);
const TAG_YCBCR_COEFFICIENTS: Tag = Tag::Unknown(529);
const TAG_YCBCR_SUBSAMPLING: Tag = Tag::Unknown(530);
const TAG_RICHTIFFIPTC: Tag = Tag::Unknown(33723);
const TAG_PHOTOSHOP: Tag = Tag::Unknown(34377);

/// Decoder for TIFF images.
pub struct TiffDecoder<R>
where
    R: BufRead + Seek,
{
    dimensions: (u32, u32),
    color_type: ColorType,
    original_color_type: ExtendedColorType,
    ycbcr_coefficients: [f32; 3],

    // We only use an Option here so we can call with_limits on the decoder without moving.
    inner: Option<Decoder<R>>,
    buffer: DecodingResult,
}

impl<R> TiffDecoder<R>
where
    R: BufRead + Seek,
{
    /// Create a new `TiffDecoder`.
    pub fn new(r: R) -> Result<TiffDecoder<R>, ImageError> {
        let mut inner = Decoder::new(r).map_err(ImageError::from_tiff_decode)?;

        let dimensions = inner.dimensions().map_err(ImageError::from_tiff_decode)?;
        let tiff_color_type = inner.colortype().map_err(ImageError::from_tiff_decode)?;

        match inner.find_tag_unsigned_vec::<u16>(Tag::SampleFormat) {
            Ok(Some(sample_formats)) => {
                for format in sample_formats {
                    check_sample_format(format, tiff_color_type)?;
                }
            }
            Ok(None) => { /* assume UInt format */ }
            Err(other) => return Err(ImageError::from_tiff_decode(other)),
        }

        let color_type = match tiff_color_type {
            tiff::ColorType::Gray(1) => ColorType::L8,
            tiff::ColorType::Gray(8) => ColorType::L8,
            tiff::ColorType::Gray(16) => ColorType::L16,
            tiff::ColorType::GrayA(8) => ColorType::La8,
            tiff::ColorType::GrayA(16) => ColorType::La16,
            tiff::ColorType::RGB(8) => ColorType::Rgb8,
            tiff::ColorType::RGB(16) => ColorType::Rgb16,
            tiff::ColorType::RGBA(8) => ColorType::Rgba8,
            tiff::ColorType::RGBA(16) => ColorType::Rgba16,
            tiff::ColorType::CMYK(8) => ColorType::Rgb8,
            tiff::ColorType::CMYK(16) => ColorType::Rgb16,
            tiff::ColorType::RGB(32) => ColorType::Rgb32F,
            tiff::ColorType::RGBA(32) => ColorType::Rgba32F,
            tiff::ColorType::YCbCr(8) => ColorType::Rgb8,

            tiff::ColorType::Palette(n) | tiff::ColorType::Gray(n) => {
                return Err(err_unknown_color_type(n))
            }
            tiff::ColorType::GrayA(n) => return Err(err_unknown_color_type(n.saturating_mul(2))),
            tiff::ColorType::RGB(n) => return Err(err_unknown_color_type(n.saturating_mul(3))),
            tiff::ColorType::YCbCr(n) => return Err(err_unknown_color_type(n.saturating_mul(3))),
            tiff::ColorType::RGBA(n) | tiff::ColorType::CMYK(n) => {
                return Err(err_unknown_color_type(n.saturating_mul(4)))
            }
            tiff::ColorType::Multiband {
                bit_depth,
                num_samples,
            } => {
                return Err(err_unknown_color_type(
                    bit_depth.saturating_mul(num_samples.min(255) as u8),
                ))
            }
            _ => return Err(err_unknown_color_type(0)),
        };

        let original_color_type = match tiff_color_type {
            tiff::ColorType::Gray(1) => ExtendedColorType::L1,
            tiff::ColorType::CMYK(8) => ExtendedColorType::Cmyk8,
            tiff::ColorType::CMYK(16) => ExtendedColorType::Cmyk16,
            tiff::ColorType::YCbCr(8) => ExtendedColorType::YCbCr8,
            _ => color_type.into(),
        };

        let mut ycbcr_coefficients = [0.0; 3];
        if matches!(tiff_color_type, tiff::ColorType::YCbCr(8)) {
            check_ycbcr_subsampling(&mut inner)?;
            ycbcr_coefficients = read_ycbcr_coefficients(&mut inner)?;
        }

        Ok(TiffDecoder {
            dimensions,
            color_type,
            original_color_type,
            ycbcr_coefficients,
            inner: Some(inner),
            buffer: DecodingResult::U8(vec![]),
        })
    }

    // The buffer can be larger for CMYK than the RGB output
    fn total_bytes_buffer(&self) -> u64 {
        let dimensions = self.dimensions();
        let total_pixels = u64::from(dimensions.0) * u64::from(dimensions.1);

        let bytes_per_pixel = match self.original_color_type {
            ExtendedColorType::Cmyk8 => 4,
            ExtendedColorType::Cmyk16 => 8,
            _ => u64::from(self.color_type().bytes_per_pixel()),
        };
        total_pixels.saturating_mul(bytes_per_pixel)
    }

    /// Interleave planes in our `buffer` into `output`.
    fn interleave_planes(
        &mut self,
        layout: tiff::decoder::BufferLayoutPreference,
        output: &mut [u8],
    ) -> ImageResult<()> {
        if self.original_color_type != self.color_type.into() {
            return Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Tiff.into(),
                    UnsupportedErrorKind::GenericFeature(
                        "Planar TIFF with CMYK color type is not supported".to_string(),
                    ),
                ),
            ));
        }

        // This only works if we and `tiff` agree on the layout, including the color type, of
        // the sample matrix.
        //
        // TODO: triple buffer in the other case and fixup the planar layout independent of
        // sample type. Problem description follows:
        //
        // That will suck since we can't call `interleave_planes` with a `ColorType` argument,
        // Changing that parameter to `ExtendedColorType` is a can of worms, and exposing the
        // underlying generic function is an optimization killer (we may want to help LLVM
        // optimize this interleaving by SIMD). For LumaAlpha(1) colors we should do the bit
        // expansion at the same time as interleaving to avoid wasting the memory traversal but
        // expand-then-interleave is at least clear, albeit an extra buffer required. Meanwhile
        // for `Cmyk8`/`Cmyk16` our output is smaller than the tiff buffer (4 samples to 3, or
        // 5 to 4 if we had alpha) and not wanting multiple conversion function implementations
        // we should interleave-then-expand?
        //
        // The hard part of the solution will be managing complexity.
        let plane_stride = layout.plane_stride.map_or(0, |n| n.get());
        let bytes = self.buffer.as_buffer(0);

        let planes = bytes
            .as_bytes()
            .chunks_exact(plane_stride)
            .collect::<Vec<_>>();

        // Gracefully handle a mismatch of expectations. This should not occur in practice as we
        // check that all planes have been read (see note on `read_image_to_buffer` usage below).
        if planes.len() < usize::from(self.color_type.channel_count()) {
            return Err(ImageError::Decoding(DecodingError::new(
                ImageFormat::Tiff.into(),
                "Not enough planes read from TIFF image".to_string(),
            )));
        }

        utils::interleave_planes(
            output,
            self.color_type,
            &planes[..usize::from(self.color_type.channel_count())],
        );

        Ok(())
    }
}

fn check_ycbcr_subsampling<R: BufRead + Seek>(decoder: &mut Decoder<R>) -> ImageResult<()> {
    let compression = decoder
        .find_tag(Tag::Compression)
        .map_err(ImageError::from_tiff_decode)?
        .and_then(|v| v.into_u16().ok());

    const COMPRESSION_MODERN_JPEG: u16 = 7;
    if compression == Some(COMPRESSION_MODERN_JPEG) {
        return Ok(());
    }

    let subsampling = decoder
        .find_tag(TAG_YCBCR_SUBSAMPLING)
        .map_err(ImageError::from_tiff_decode)?
        .map(|value| value.into_u16_vec())
        .transpose()
        .map_err(ImageError::from_tiff_decode)?;

    let subsampling = subsampling.as_deref().unwrap_or(&[2, 2]);

    if subsampling != [1, 1] {
        return Err(ImageError::Unsupported(
            UnsupportedError::from_format_and_kind(
                ImageFormat::Tiff.into(),
                UnsupportedErrorKind::GenericFeature(format!(
                "Subsampling {:?} is not supported. Only (1,1) is supported for non-JPEG YCbCr.",
                subsampling
            )),
            ),
        ));
    }

    Ok(())
}

fn read_ycbcr_coefficients<R: BufRead + Seek>(decoder: &mut Decoder<R>) -> ImageResult<[f32; 3]> {
    let value = decoder
        .find_tag(TAG_YCBCR_COEFFICIENTS)
        .map_err(ImageError::from_tiff_decode)?;

    const DEFAULT_YCBCR_COEFFICIENTS: [f32; 3] = [0.299, 0.587, 0.114];
    let Some(value) = value else {
        return Ok(DEFAULT_YCBCR_COEFFICIENTS);
    };

    let list = match value {
        Value::List(list) if list.len() == 3 => list,
        _ => {
            return Err(ImageError::Decoding(DecodingError::new(
                ImageFormat::Tiff.into(),
                "YCbCrCoefficients tag (529) must contain exactly 3 rational values".to_string(),
            )));
        }
    };

    let mut coefficients = [0.0f32; 3];
    for (i, value) in list.iter().enumerate() {
        match value {
            Value::Rational(num, denom) if *denom != 0 => {
                coefficients[i] = *num as f32 / *denom as f32
            }
            _ => {
                return Err(ImageError::Decoding(DecodingError::new(
                    ImageFormat::Tiff.into(),
                    "YCbCrCoefficients tag (529) contains an invalid rational value".to_string(),
                )));
            }
        }
    }

    Ok(coefficients)
}

fn check_sample_format(sample_format: u16, color_type: tiff::ColorType) -> Result<(), ImageError> {
    use tiff::{tags::SampleFormat, ColorType};
    let num_bits = match color_type {
        ColorType::CMYK(k) => k,
        ColorType::Gray(k) => k,
        ColorType::RGB(k) => k,
        ColorType::RGBA(k) => k,
        ColorType::GrayA(k) => k,
        ColorType::YCbCr(8) => 8,
        ColorType::Palette(k) | ColorType::YCbCr(k) => {
            return Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Tiff.into(),
                    UnsupportedErrorKind::GenericFeature(format!(
                        "Unhandled TIFF color type {color_type:?} for {k} bits",
                    )),
                ),
            ))
        }
        _ => {
            return Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Tiff.into(),
                    UnsupportedErrorKind::GenericFeature(format!(
                        "Unhandled TIFF color type {color_type:?}",
                    )),
                ),
            ))
        }
    };

    match SampleFormat::from_u16(sample_format) {
        Some(SampleFormat::Uint) if num_bits <= 16 => Ok(()),
        Some(SampleFormat::IEEEFP) if num_bits == 32 => Ok(()),
        _ => Err(ImageError::Unsupported(
            UnsupportedError::from_format_and_kind(
                ImageFormat::Tiff.into(),
                UnsupportedErrorKind::GenericFeature(format!(
                    "Unhandled TIFF sample format {sample_format:?} for {num_bits} bits",
                )),
            ),
        )),
    }
}

fn err_unknown_color_type(value: u8) -> ImageError {
    ImageError::Unsupported(UnsupportedError::from_format_and_kind(
        ImageFormat::Tiff.into(),
        UnsupportedErrorKind::Color(ExtendedColorType::Unknown(value)),
    ))
}

impl ImageError {
    fn from_tiff_decode(err: tiff::TiffError) -> ImageError {
        match err {
            tiff::TiffError::IoError(err) => ImageError::IoError(err),
            err @ (tiff::TiffError::FormatError(_)
            | tiff::TiffError::IntSizeError
            | tiff::TiffError::UsageError(_)) => {
                ImageError::Decoding(DecodingError::new(ImageFormat::Tiff.into(), err))
            }
            tiff::TiffError::UnsupportedError(desc) => {
                ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                    ImageFormat::Tiff.into(),
                    UnsupportedErrorKind::GenericFeature(desc.to_string()),
                ))
            }
            tiff::TiffError::LimitsExceeded => {
                ImageError::Limits(LimitError::from_kind(LimitErrorKind::InsufficientMemory))
            }
        }
    }

    fn from_tiff_encode(err: tiff::TiffError) -> ImageError {
        match err {
            tiff::TiffError::IoError(err) => ImageError::IoError(err),
            err @ (tiff::TiffError::FormatError(_)
            | tiff::TiffError::IntSizeError
            | tiff::TiffError::UsageError(_)) => {
                ImageError::Encoding(EncodingError::new(ImageFormat::Tiff.into(), err))
            }
            tiff::TiffError::UnsupportedError(desc) => {
                ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                    ImageFormat::Tiff.into(),
                    UnsupportedErrorKind::GenericFeature(desc.to_string()),
                ))
            }
            tiff::TiffError::LimitsExceeded => {
                ImageError::Limits(LimitError::from_kind(LimitErrorKind::InsufficientMemory))
            }
        }
    }
}

impl<R: BufRead + Seek> ImageDecoder for TiffDecoder<R> {
    fn dimensions(&self) -> (u32, u32) {
        self.dimensions
    }

    fn color_type(&self) -> ColorType {
        self.color_type
    }

    fn original_color_type(&self) -> ExtendedColorType {
        self.original_color_type
    }

    fn icc_profile(&mut self) -> ImageResult<Option<Vec<u8>>> {
        if let Some(decoder) = &mut self.inner {
            Ok(decoder.get_tag_u8_vec(Tag::IccProfile).ok())
        } else {
            Ok(None)
        }
    }

    fn xmp_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        let Some(decoder) = &mut self.inner else {
            return Ok(None);
        };

        let value = match decoder.get_tag(TAG_XML_PACKET) {
            Ok(value) => value,
            Err(tiff::TiffError::FormatError(tiff::TiffFormatError::RequiredTagNotFound(_))) => {
                return Ok(None);
            }
            Err(err) => return Err(ImageError::from_tiff_decode(err)),
        };
        value
            .into_u8_vec()
            .map(Some)
            .map_err(ImageError::from_tiff_decode)
    }

    fn orientation(&mut self) -> ImageResult<Orientation> {
        if let Some(decoder) = &mut self.inner {
            Ok(decoder
                .find_tag(Tag::Orientation)
                .map_err(ImageError::from_tiff_decode)?
                .and_then(|v| Orientation::from_exif(v.into_u16().ok()?.min(255) as u8))
                .unwrap_or(Orientation::NoTransforms))
        } else {
            Ok(Orientation::NoTransforms)
        }
    }

    fn set_limits(&mut self, limits: crate::Limits) -> ImageResult<()> {
        limits.check_support(&crate::LimitSupport::default())?;

        let (width, height) = self.dimensions();
        limits.check_dimensions(width, height)?;

        let max_alloc = limits.max_alloc.unwrap_or(u64::MAX);
        let max_intermediate_alloc = max_alloc.saturating_sub(self.total_bytes_buffer());

        let mut tiff_limits: tiff::decoder::Limits = Default::default();
        tiff_limits.decoding_buffer_size =
            usize::try_from(max_alloc - max_intermediate_alloc).unwrap_or(usize::MAX);
        tiff_limits.intermediate_buffer_size =
            usize::try_from(max_intermediate_alloc).unwrap_or(usize::MAX);
        tiff_limits.ifd_value_size = tiff_limits.intermediate_buffer_size;
        self.inner = Some(self.inner.take().unwrap().with_limits(tiff_limits));

        Ok(())
    }

    fn iptc_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        let Some(decoder) = &mut self.inner else {
            return Ok(None);
        };

        // Try Photoshop tag
        if let Ok(data) = decoder.get_tag_u8_vec(TAG_PHOTOSHOP) {
            if extract_iptc_from_photoshop_irb(&data).is_some() {
                return Ok(Some(data));
            }
        }

        // Try RichTIFFIPTC tag
        if let Ok(value) = decoder.get_tag(TAG_RICHTIFFIPTC) {
            // Standard representation: defined as UNDEFINED or BYTE.
            if let Some(vec) = value.clone().into_u8_vec().ok().filter(|v| !v.is_empty()) {
                return Ok(Some(vec));
            }
            // Fallback: Adobe software sometimes incorrectly writes this as LONG (u32).
            // We convert the u32 integers back to raw little-endian bytes to recover the payload.
            if let Some(vec) = value
                .into_u32_vec()
                .ok()
                .map(|vec| {
                    vec.into_iter()
                        .flat_map(|v| v.to_le_bytes())
                        .collect::<Vec<u8>>()
                })
                .filter(|v| !v.is_empty())
            {
                return Ok(Some(vec));
            }
        }

        Ok(None)
    }

    fn read_image(mut self, buf: &mut [u8]) -> ImageResult<()> {
        assert_eq!(u64::try_from(buf.len()), Ok(self.total_bytes()));

        let layout = self
            .inner
            .as_mut()
            .unwrap()
            .read_image_to_buffer(&mut self.buffer)
            .map_err(ImageError::from_tiff_decode)?;

        // Check if we have all of the planes. Otherwise we ran into the allocation limit.
        if self.buffer.as_buffer(0).as_bytes().len() < layout.complete_len {
            return Err(ImageError::Limits(LimitError::from_kind(
                LimitErrorKind::InsufficientMemory,
            )));
        }

        if layout.planes > 1 {
            // Note that we do not support planar layouts if we have to do conversion. Yet. See a
            // more detailed comment in the implementation.
            return self.interleave_planes(layout, buf);
        }

        match self.buffer {
            DecodingResult::U8(v) if self.original_color_type == ExtendedColorType::Cmyk8 => {
                let buf = buf.as_chunks_mut::<3>().0;
                for (cmyk, rgb) in v.as_chunks::<4>().0.iter().zip(buf) {
                    *rgb = cmyk_to_rgb(cmyk);
                }
            }
            DecodingResult::U16(v) if self.original_color_type == ExtendedColorType::Cmyk16 => {
                let buf = buf.as_chunks_mut::<6>().0;
                for (cmyk, rgb) in v.as_chunks::<4>().0.iter().zip(buf) {
                    *rgb = bytemuck::cast(cmyk_to_rgb16(cmyk));
                }
            }
            DecodingResult::U8(v) if self.original_color_type == ExtendedColorType::L1 => {
                let width = self.dimensions.0;
                let row_bytes = width.div_ceil(8);

                for (in_row, out_row) in v
                    .chunks_exact(row_bytes as usize)
                    .zip(buf.chunks_exact_mut(width as usize))
                {
                    out_row.copy_from_slice(&utils::expand_bits(1, width, in_row));
                }
            }
            DecodingResult::U8(v) if self.original_color_type == ExtendedColorType::YCbCr8 => {
                let [lr, lg, lb] = self.ycbcr_coefficients;
                let ycbcr = v.as_chunks::<3>().0;
                let out = buf.as_chunks_mut::<3>().0;

                ycbcr_to_rgb8(ycbcr, lr, lg, lb, out);
            }
            DecodingResult::U8(v) => {
                buf.copy_from_slice(&v);
            }
            DecodingResult::U16(v) => {
                buf.copy_from_slice(bytemuck::cast_slice(&v));
            }
            DecodingResult::U32(v) => {
                buf.copy_from_slice(bytemuck::cast_slice(&v));
            }
            DecodingResult::U64(v) => {
                buf.copy_from_slice(bytemuck::cast_slice(&v));
            }
            DecodingResult::I8(v) => {
                buf.copy_from_slice(bytemuck::cast_slice(&v));
            }
            DecodingResult::I16(v) => {
                buf.copy_from_slice(bytemuck::cast_slice(&v));
            }
            DecodingResult::I32(v) => {
                buf.copy_from_slice(bytemuck::cast_slice(&v));
            }
            DecodingResult::I64(v) => {
                buf.copy_from_slice(bytemuck::cast_slice(&v));
            }
            DecodingResult::F32(v) => {
                buf.copy_from_slice(bytemuck::cast_slice(&v));
            }
            DecodingResult::F64(v) => {
                buf.copy_from_slice(bytemuck::cast_slice(&v));
            }
            DecodingResult::F16(_) => unreachable!(),
        }

        Ok(())
    }

    fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()> {
        (*self).read_image(buf)
    }
}

/// Encoder for tiff images
pub struct TiffEncoder<W> {
    w: W,
    icc: Option<Vec<u8>>,
}

fn ycbcr_to_rgb8(ycbcr: &[[u8; 3]], lr: f32, lg: f32, lb: f32, out: &mut [[u8; 3]]) {
    let coeff_r = 2.0 * (1.0 - lr);
    let coeff_b = 2.0 * (1.0 - lb);
    let inv_lg = 1.0 / lg;

    for (src, dst) in ycbcr.iter().zip(out.iter_mut()) {
        let y = f32::from(src[0]);
        let cb = f32::from(src[1]) - 128.0;
        let cr = f32::from(src[2]) - 128.0;

        let r = y + cr * coeff_r;
        let b = y + cb * coeff_b;
        let g = (y - lr * r - lb * b) * inv_lg;

        dst[0] = (r + 0.5) as u8;
        dst[1] = (g + 0.5) as u8;
        dst[2] = (b + 0.5) as u8;
    }
}

fn cmyk_to_rgb(cmyk: &[u8; 4]) -> [u8; 3] {
    let c = f32::from(cmyk[0]);
    let m = f32::from(cmyk[1]);
    let y = f32::from(cmyk[2]);
    let kf = 1. - f32::from(cmyk[3]) / 255.;
    [
        ((255. - c) * kf) as u8,
        ((255. - m) * kf) as u8,
        ((255. - y) * kf) as u8,
    ]
}

fn cmyk_to_rgb16(cmyk: &[u16; 4]) -> [u16; 3] {
    let c = f32::from(cmyk[0]);
    let m = f32::from(cmyk[1]);
    let y = f32::from(cmyk[2]);
    let kf = 1. - f32::from(cmyk[3]) / 65535.;
    [
        ((65535. - c) * kf) as u16,
        ((65535. - m) * kf) as u16,
        ((65535. - y) * kf) as u16,
    ]
}

/// Convert a slice of sample bytes to its semantic type, being a `Pod`.
fn u8_slice_as_pod<P: bytemuck::Pod>(buf: &[u8]) -> ImageResult<std::borrow::Cow<'_, [P]>> {
    bytemuck::try_cast_slice(buf)
        .map(std::borrow::Cow::Borrowed)
        .or_else(|err| {
            match err {
                bytemuck::PodCastError::TargetAlignmentGreaterAndInputNotAligned => {
                    // If the buffer is not aligned for a native slice, copy the buffer into a Vec,
                    // aligning it in the process. This is only done if the element count can be
                    // represented exactly.
                    let vec = bytemuck::allocation::pod_collect_to_vec(buf);
                    Ok(std::borrow::Cow::Owned(vec))
                }
                /* only expecting: bytemuck::PodCastError::OutputSliceWouldHaveSlop */
                _ => {
                    // `bytemuck::PodCastError` of bytemuck-1.2.0 does not implement `Error` and
                    // `Display` trait.
                    // See <https://github.com/Lokathor/bytemuck/issues/22>.
                    Err(ImageError::Parameter(ParameterError::from_kind(
                        ParameterErrorKind::Generic(format!(
                            "Casting samples to their representation failed: {err:?}",
                        )),
                    )))
                }
            }
        })
}

impl<W: Write + Seek> TiffEncoder<W> {
    /// Create a new encoder that writes its output to `w`
    pub fn new(w: W) -> TiffEncoder<W> {
        TiffEncoder { w, icc: None }
    }

    /// Private wrapper function to encode the image with a generic color type. This is used to reduce code duplication in the public `write_image` function.
    fn write_tiff<C: tiff::encoder::colortype::ColorType<Inner: bytemuck::Pod>>(
        self,
        width: u32,
        height: u32,
        data: &[u8],
    ) -> ImageResult<()>
    where
        [C::Inner]: tiff::encoder::TiffValue,
    {
        let mut encoder =
            tiff::encoder::TiffEncoder::new(self.w).map_err(ImageError::from_tiff_encode)?;
        let data = u8_slice_as_pod::<C::Inner>(data)?;
        let mut img_encoder = encoder
            .new_image::<C>(width, height)
            .map_err(ImageError::from_tiff_encode)?;
        if let Some(icc_profile) = self.icc {
            // An ICC device profile is embedded, in its entirety, as a single TIFF field or Image File Directory (IFD) entry in
            // the IFD containing the corresponding image data. An IFD should contain no more than one embedded profile.
            // A TIFF file may contain more than one image, and so, more than one IFD. Each IFD may have its own
            // embedded profile.
            // -- Specification ICC.1:2004-10 (Profile version 4.2.0.0), https://www.color.org/icc1V42.pdf
            let ifd_encoder = img_encoder.encoder(); // low-level TIFF directory encoder
            ifd_encoder
                .write_tag(Tag::IccProfile, icc_profile.as_slice())
                .map_err(ImageError::from_tiff_encode)?;
        }
        img_encoder
            .write_data(&data)
            .map_err(ImageError::from_tiff_encode)
    }
}

impl<W: Write + Seek> ImageEncoder for TiffEncoder<W> {
    /// Encodes the image `image` that has dimensions `width` and `height` and `ColorType` `c`.
    ///
    /// 16-bit types assume the buffer is native endian.
    ///
    /// # Panics
    ///
    /// Panics if `width * height * color_type.bytes_per_pixel() != data.len()`.
    #[track_caller]
    fn write_image(
        self,
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ExtendedColorType,
    ) -> ImageResult<()> {
        use tiff::encoder::colortype::{
            Gray16, Gray8, RGB32Float, RGBA32Float, RGB16, RGB8, RGBA16, RGBA8,
        };
        let expected_buffer_len = color_type.buffer_size(width, height);
        assert_eq!(
            expected_buffer_len,
            buf.len() as u64,
            "Invalid buffer length: expected {expected_buffer_len} got {} for {width}x{height} image",
            buf.len(),
        );
        match color_type {
            ExtendedColorType::L8 => self.write_tiff::<Gray8>(width, height, buf),
            ExtendedColorType::Rgb8 => self.write_tiff::<RGB8>(width, height, buf),
            ExtendedColorType::Rgba8 => self.write_tiff::<RGBA8>(width, height, buf),
            ExtendedColorType::L16 => self.write_tiff::<Gray16>(width, height, buf),
            ExtendedColorType::Rgb16 => self.write_tiff::<RGB16>(width, height, buf),
            ExtendedColorType::Rgba16 => self.write_tiff::<RGBA16>(width, height, buf),
            ExtendedColorType::Rgb32F => self.write_tiff::<RGB32Float>(width, height, buf),
            ExtendedColorType::Rgba32F => self.write_tiff::<RGBA32Float>(width, height, buf),
            _ => Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Tiff.into(),
                    UnsupportedErrorKind::Color(color_type),
                ),
            )),
        }
    }

    fn set_icc_profile(&mut self, icc_profile: Vec<u8>) -> Result<(), UnsupportedError> {
        self.icc = Some(icc_profile);
        Ok(())
    }
}

struct IrbReader<'a> {
    data: &'a [u8],
}

impl<'a> IrbReader<'a> {
    fn new(data: &'a [u8]) -> Self {
        Self { data }
    }

    fn read_slice(&mut self, len: usize) -> Option<&'a [u8]> {
        if self.data.len() < len {
            return None;
        }
        let (head, tail) = self.data.split_at(len);
        self.data = tail;
        Some(head)
    }

    fn read_u16(&mut self) -> Option<u16> {
        let bytes = self.read_slice(2)?;
        Some(u16::from_be_bytes([bytes[0], bytes[1]]))
    }

    fn read_u32(&mut self) -> Option<u32> {
        let bytes = self.read_slice(4)?;
        Some(u32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
    }

    fn skip_padding(&mut self, size: usize) {
        if !size.is_multiple_of(2) && !self.data.is_empty() {
            self.data = &self.data[1..];
        }
    }
}

fn extract_iptc_from_photoshop_irb(data: &[u8]) -> Option<&[u8]> {
    const SIGNATURE: &[u8] = b"8BIM";
    const IPTC_ID: u16 = 0x0404;
    const MIN_IRB_BLOCK_SIZE: usize = 12;

    let mut reader = IrbReader::new(data);

    while reader.data.len() >= MIN_IRB_BLOCK_SIZE {
        let sig = reader.read_slice(SIGNATURE.len())?;
        if sig != SIGNATURE {
            break;
        }

        let id = reader.read_u16()?;

        let name_len = reader.read_slice(1)?[0] as usize;
        reader.read_slice(name_len)?;
        reader.skip_padding(1 + name_len);

        let size = reader.read_u32()? as usize;
        let block_data = reader.read_slice(size)?;

        if id == IPTC_ID {
            return Some(block_data);
        }

        reader.skip_padding(size);
    }
    None
}
