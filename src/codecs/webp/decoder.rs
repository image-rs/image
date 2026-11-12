use std::io::{BufRead, Seek};

use image_webp::LoopCount;

use crate::error::{
    DecodingError, ImageError, ImageResult, LimitError, LimitErrorKind, ParameterError,
    ParameterErrorKind,
};
use crate::io::{
    DecodedAnimationAttributes, DecodedImageAttributes, DecodedMetadataHint, DecoderPreparedImage,
    FormatAttributes, SequenceControl,
};
use crate::{ColorType, Delay, ImageDecoder, ImageFormat, Limits, Rgba};

/// WebP Image format decoder.
///
/// Supports both lossless and lossy WebP images.
pub struct WebPDecoder<R> {
    inner: image_webp::WebPDecoder<R>,
    current: u32,
}

impl<R: BufRead + Seek> WebPDecoder<R> {
    /// Create a new `WebPDecoder` from the Reader `r`.
    pub fn new(r: R) -> ImageResult<Self> {
        Ok(Self {
            inner: image_webp::WebPDecoder::new(r).map_err(ImageError::from_webp_decode)?,
            current: 0,
        })
    }

    /// Returns true if the image as described by the bitstream is animated.
    pub fn has_animation(&self) -> bool {
        self.inner.is_animated()
    }

    /// Sets the background color if the image is an extended and animated webp.
    pub fn set_background_color(&mut self, color: Rgba<u8>) -> ImageResult<()> {
        self.inner
            .set_background_color(color.0)
            .map_err(ImageError::from_webp_decode)
    }
}

impl<R: BufRead + Seek> ImageDecoder for WebPDecoder<R> {
    fn format_attributes(&self) -> FormatAttributes {
        FormatAttributes {
            // As per extended file format description:
            // <https://developers.google.com/speed/webp/docs/riff_container#extended_file_format>
            icc: DecodedMetadataHint::InHeader,
            exif: DecodedMetadataHint::InHeader,
            xmp: DecodedMetadataHint::InHeader,
            ..FormatAttributes::default()
        }
    }

    fn animation_attributes(&mut self) -> Option<DecodedAnimationAttributes> {
        let loop_count = match self.inner.loop_count() {
            LoopCount::Forever => crate::metadata::LoopCount::Infinite,
            LoopCount::Times(n) => crate::metadata::LoopCount::Finite(n.into()),
        };

        Some(DecodedAnimationAttributes { loop_count })
    }

    fn prepare_image(&mut self) -> ImageResult<DecoderPreparedImage> {
        let (width, height) = self.inner.dimensions();
        let color = if self.inner.has_alpha() {
            ColorType::Rgba8
        } else {
            ColorType::Rgb8
        };

        Ok(DecoderPreparedImage::new(width, height, color))
    }

    fn read_image(&mut self, buf: &mut [u8]) -> ImageResult<DecodedImageAttributes> {
        let is_animated = self.inner.is_animated();

        if is_animated && self.current == self.inner.num_frames() {
            return Err(ImageError::Parameter(ParameterError::from_kind(
                ParameterErrorKind::NoMoreData,
            )));
        }

        let layout = self.prepare_image()?;
        assert_eq!(u64::try_from(buf.len()), Ok(layout.total_bytes()));

        // `read_frame` panics if the image is not animated.
        let delay = if is_animated {
            let delay = self
                .inner
                .read_frame(buf)
                .map_err(ImageError::from_webp_decode)?;
            Some(Delay::from_millis(delay))
        } else {
            self.inner
                .read_image(buf)
                .map_err(ImageError::from_webp_decode)?;
            None
        };

        self.current += 1;

        Ok(DecodedImageAttributes {
            delay,
            ..DecodedImageAttributes::default()
        })
    }

    fn icc_profile(&mut self) -> ImageResult<Option<Vec<u8>>> {
        self.inner
            .icc_profile()
            .map_err(ImageError::from_webp_decode)
    }

    fn exif_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        let exif = self
            .inner
            .exif_metadata()
            .map_err(ImageError::from_webp_decode)?;

        Ok(exif)
    }

    fn xmp_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        self.inner
            .xmp_metadata()
            .map_err(ImageError::from_webp_decode)
    }

    fn set_limits(&mut self, limits: Limits) -> ImageResult<()> {
        limits.check_support(&crate::LimitSupport::default())?;
        let layout = self.prepare_image()?;
        limits.check_layout_dimensions(&layout)?;

        let max_alloc = limits.max_alloc.unwrap_or(u64::MAX);
        self.inner
            .set_memory_limit(usize::try_from(max_alloc).unwrap_or(usize::MAX));
        Ok(())
    }

    fn more_images(&self) -> SequenceControl {
        if self.current == self.inner.num_frames() {
            SequenceControl::None
        } else {
            SequenceControl::MaybeMore
        }
    }
}

impl ImageError {
    fn from_webp_decode(e: image_webp::DecodingError) -> Self {
        match e {
            image_webp::DecodingError::IoError(e) => ImageError::IoError(e),
            image_webp::DecodingError::NoMoreFrames => {
                ImageError::Parameter(ParameterError::from_kind(ParameterErrorKind::NoMoreData))
            }
            image_webp::DecodingError::MemoryLimitExceeded => {
                ImageError::Limits(LimitError::from_kind(LimitErrorKind::InsufficientMemory))
            }
            _ => ImageError::Decoding(DecodingError::new(ImageFormat::WebP.into(), e)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add_with_overflow_size() {
        let bytes = vec![
            0x52, 0x49, 0x46, 0x46, 0xaf, 0x37, 0x80, 0x47, 0x57, 0x45, 0x42, 0x50, 0x6c, 0x64,
            0x00, 0x00, 0xff, 0xff, 0xff, 0xff, 0xfb, 0x7e, 0x73, 0x00, 0x06, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x00, 0x65, 0x65, 0x65, 0x65, 0x65, 0x65,
            0x40, 0xfb, 0xff, 0xff, 0x65, 0x65, 0x65, 0x65, 0x65, 0x65, 0x65, 0x65, 0x65, 0x65,
            0x00, 0x00, 0x00, 0x00, 0x62, 0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x49,
            0x49, 0x54, 0x55, 0x50, 0x4c, 0x54, 0x59, 0x50, 0x45, 0x33, 0x37, 0x44, 0x4d, 0x46,
        ];

        let data = std::io::Cursor::new(bytes);

        let _ = WebPDecoder::new(data);
    }

    #[test]
    fn metadata_chunk_respects_max_alloc() {
        // Extended 1x1 lossless image followed by an EXIF chunk that declares ~4 GiB
        // but carries only a few bytes.
        let bytes = vec![
            0x52, 0x49, 0x46, 0x46, 0x38, 0x00, 0x00, 0x00, 0x57, 0x45, 0x42, 0x50, 0x56, 0x50,
            0x38, 0x58, 0x0a, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x56, 0x50, 0x38, 0x4c, 0x0d, 0x00, 0x00, 0x00, 0x2f, 0x00, 0x00, 0x00,
            0x00, 0xcd, 0x55, 0x20, 0x22, 0x02, 0x11, 0x11, 0x09, 0x00, 0x45, 0x58, 0x49, 0x46,
            0xf0, 0xff, 0xff, 0xff, 0x49, 0x49, 0x2a, 0x00,
        ];

        let mut decoder = WebPDecoder::new(std::io::Cursor::new(bytes)).unwrap();
        let mut limits = Limits::no_limits();
        limits.max_alloc = Some(1 << 20);
        decoder.set_limits(limits).unwrap();

        assert!(matches!(
            decoder.exif_metadata(),
            Err(ImageError::Limits(_))
        ));
    }
}
