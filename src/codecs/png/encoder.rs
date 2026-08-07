use std::{borrow::Cow, io::Write};

use crate::{
    error::{EncodingError, LimitError, LimitErrorKind, UnsupportedError, UnsupportedErrorKind},
    utils::vec_try_with_capacity,
    ColorType, DynamicImage, ExtendedColorType, ImageEncoder, ImageError, ImageFormat, ImageResult,
};

/// PNG encoder
pub struct PngEncoder<W: Write> {
    w: W,
    compression: CompressionType,
    filter: FilterType,
    icc_profile: Vec<u8>,
    exif_metadata: Vec<u8>,
    xmp_metadata: Option<String>,
}

/// DEFLATE compression level of a PNG encoder. The default setting is `Fast`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
#[derive(Default)]
pub enum CompressionType {
    /// No compression whatsoever
    Uncompressed,
    /// Fast, minimal compression
    #[default]
    Fast,
    /// Balance between speed and compression level
    Balanced,
    /// High compression level
    Best,
    /// Detailed compression level between 1 and 9
    Level(u8),
}

/// Filter algorithms used to process image data to improve compression.
///
/// The default filter is `Adaptive`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
#[derive(Default)]
pub enum FilterType {
    /// No processing done, best used for low bit depth grayscale or data with a
    /// low color count
    NoFilter,
    /// Filters based on previous pixel in the same scanline
    Sub,
    /// Filters based on the scanline above
    Up,
    /// Filters based on the average of left and right neighbor pixels
    Avg,
    /// Algorithm that takes into account the left, upper left, and above pixels
    Paeth,
    /// Uses a heuristic to select one of the preceding filters for each
    /// scanline rather than one filter for the entire image
    #[default]
    Adaptive,
}

impl<W: Write> PngEncoder<W> {
    /// Create a new encoder that writes its output to ```w```
    pub fn new(w: W) -> PngEncoder<W> {
        PngEncoder {
            w,
            compression: CompressionType::default(),
            filter: FilterType::default(),
            icc_profile: Vec::new(),
            exif_metadata: Vec::new(),
            xmp_metadata: None,
        }
    }

    /// Create a new encoder that writes its output to `w` with `CompressionType` `compression` and
    /// `FilterType` `filter`.
    ///
    /// It is best to view the options as a _hint_ to the implementation on the smallest or fastest
    /// option for encoding a particular image. That is, using options that map directly to a PNG
    /// image parameter will use this parameter where possible. But variants that have no direct
    /// mapping may be interpreted differently in minor versions. The exact output is expressly
    /// __not__ part of the SemVer stability guarantee.
    ///
    /// Note that it is not optimal to use a single filter type, so an adaptive
    /// filter type is selected as the default. The filter which best minimizes
    /// file size may change with the type of compression used.
    pub fn new_with_quality(
        w: W,
        compression: CompressionType,
        filter: FilterType,
    ) -> PngEncoder<W> {
        PngEncoder {
            w,
            compression,
            filter,
            icc_profile: Vec::new(),
            exif_metadata: Vec::new(),
            xmp_metadata: None,
        }
    }

    fn encode_inner(
        self,
        data: &[u8],
        width: u32,
        height: u32,
        color: ExtendedColorType,
    ) -> ImageResult<()> {
        let (ct, bits) = match color {
            ExtendedColorType::L8 => (png::ColorType::Grayscale, png::BitDepth::Eight),
            ExtendedColorType::L16 => (png::ColorType::Grayscale, png::BitDepth::Sixteen),
            ExtendedColorType::La8 => (png::ColorType::GrayscaleAlpha, png::BitDepth::Eight),
            ExtendedColorType::La16 => (png::ColorType::GrayscaleAlpha, png::BitDepth::Sixteen),
            ExtendedColorType::Rgb8 => (png::ColorType::Rgb, png::BitDepth::Eight),
            ExtendedColorType::Rgb16 => (png::ColorType::Rgb, png::BitDepth::Sixteen),
            ExtendedColorType::Rgba8 => (png::ColorType::Rgba, png::BitDepth::Eight),
            ExtendedColorType::Rgba16 => (png::ColorType::Rgba, png::BitDepth::Sixteen),
            _ => {
                return Err(ImageError::Unsupported(
                    UnsupportedError::from_format_and_kind(
                        ImageFormat::Png.into(),
                        UnsupportedErrorKind::Color(color),
                    ),
                ))
            }
        };

        let comp = match self.compression {
            CompressionType::Balanced => png::Compression::Balanced,
            CompressionType::Best => png::Compression::High,
            CompressionType::Fast => png::Compression::Fast,
            CompressionType::Uncompressed => png::Compression::NoCompression,
            CompressionType::Level(0) => png::Compression::NoCompression,
            CompressionType::Level(_) => png::Compression::Fast, // whatever, will be overridden
        };

        let advanced_comp = match self.compression {
            // Do not set level 0 as a Zlib level to avoid Zlib backend variance.
            // For example, in miniz_oxide level 0 is very slow.
            CompressionType::Level(n @ 1..) => Some(png::DeflateCompression::Level(n)),
            _ => None,
        };

        let filter = match self.filter {
            FilterType::NoFilter => png::Filter::NoFilter,
            FilterType::Sub => png::Filter::Sub,
            FilterType::Up => png::Filter::Up,
            FilterType::Avg => png::Filter::Avg,
            FilterType::Paeth => png::Filter::Paeth,
            FilterType::Adaptive => png::Filter::Adaptive,
        };

        let mut info = png::Info::with_size(width, height);

        if !self.icc_profile.is_empty() {
            info.icc_profile = Some(Cow::Borrowed(&self.icc_profile));
        }
        if !self.exif_metadata.is_empty() {
            info.exif_metadata = Some(Cow::Borrowed(&self.exif_metadata));
        }

        let mut encoder = png::Encoder::with_info(self.w, info).map_err(from_png)?;

        if let Some(xmp_text) = self.xmp_metadata {
            encoder
                .add_itxt_chunk(super::XMP_KEY.to_string(), xmp_text)
                .map_err(from_png)?;
        }

        encoder.set_color(ct);
        encoder.set_depth(bits);
        encoder.set_compression(comp);
        if let Some(compression) = advanced_comp {
            encoder.set_deflate_compression(compression);
        }
        encoder.set_filter(filter);
        let mut writer = encoder.write_header().map_err(from_png)?;
        writer.write_image_data(data).map_err(from_png)?;
        writer.finish().map_err(from_png)
    }
}

impl<W: Write> ImageEncoder for PngEncoder<W> {
    /// Write a PNG image with the specified width, height, and color type.
    ///
    /// For color types with 16-bit per channel or larger, the contents of `buf` should be in
    /// native endian. `PngEncoder` will automatically convert to big endian as required by the
    /// underlying PNG format.
    #[track_caller]
    fn write_image(
        self,
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ExtendedColorType,
    ) -> ImageResult<()> {
        use ExtendedColorType::*;

        let expected_buffer_len = color_type.buffer_size(width, height);
        assert_eq!(
            expected_buffer_len,
            buf.len() as u64,
            "Invalid buffer length: expected {expected_buffer_len} got {} for {width}x{height} image",
            buf.len(),
        );

        // PNG images are big endian. For 16 bit per channel and larger types,
        // the buffer may need to be reordered to big endian per the
        // contract of `write_image`.
        // TODO: assumes equal channel bit depth.
        match color_type {
            L8 | La8 | Rgb8 | Rgba8 => {
                // No reodering necessary for u8
                self.encode_inner(buf, width, height, color_type)
            }
            L16 | La16 | Rgb16 | Rgba16 => {
                // Because the buffer is immutable and the PNG encoder does not
                // yet take Write/Read traits, create a temporary buffer for
                // big endian reordering.
                let mut reordered;
                let buf = if cfg!(target_endian = "little") {
                    reordered = vec_try_with_capacity(buf.len())?;
                    reordered.extend(buf.as_chunks::<2>().0.iter().flat_map(|le| [le[1], le[0]]));
                    &reordered
                } else {
                    buf
                };
                self.encode_inner(buf, width, height, color_type)
            }
            _ => Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Png.into(),
                    UnsupportedErrorKind::Color(color_type),
                ),
            )),
        }
    }

    fn set_icc_profile(&mut self, icc_profile: Vec<u8>) -> Result<(), UnsupportedError> {
        self.icc_profile = icc_profile;
        Ok(())
    }

    fn set_exif_metadata(&mut self, exif: Vec<u8>) -> Result<(), UnsupportedError> {
        self.exif_metadata = exif;
        Ok(())
    }

    fn set_xmp_metadata(&mut self, xmp: Vec<u8>) -> Result<(), UnsupportedError> {
        self.xmp_metadata = Some(String::from_utf8(xmp).map_err(|_| {
            UnsupportedError::from_format_and_kind(
                ImageFormat::Png.into(),
                UnsupportedErrorKind::GenericFeature("XMP metadata is not valid UTF-8".to_string()),
            )
        })?);
        Ok(())
    }

    fn make_compatible_img(
        &self,
        _: crate::io::encoder::MethodSealedToImage,
        img: &DynamicImage,
    ) -> Option<DynamicImage> {
        use ColorType::*;
        match img.color() {
            L32F => Some(img.to_luma16().into()),
            La32F => Some(img.to_luma_alpha16().into()),
            Rgb32F => Some(img.to_rgb16().into()),
            Rgba32F => Some(img.to_rgba16().into()),
            L8 | La8 | Rgb8 | Rgba8 | L16 | La16 | Rgb16 | Rgba16 => None,
        }
    }
}

fn from_png(err: png::EncodingError) -> ImageError {
    use png::EncodingError::*;
    match err {
        IoError(error) => ImageError::IoError(error),
        LimitsExceeded => {
            ImageError::Limits(LimitError::from_kind(LimitErrorKind::InsufficientMemory))
        }
        _ => ImageError::Encoding(EncodingError::new(ImageFormat::Png.into(), Box::new(err))),
    }
}
