//! Encoding of WebP images.

use std::io::Write;

use crate::{
    error::{EncodingError, UnsupportedError, UnsupportedErrorKind},
    ExtendedColorType, ImageEncoder, ImageError, ImageFormat, ImageResult,
};

/// WebP Encoder.
pub struct WebPEncoder<W> {
    inner: image_webp::WebPEncoder<W>,
}

impl<W: Write> WebPEncoder<W> {
    /// Create a new encoder that writes its output to `w`.
    ///
    /// Uses "VP8L" lossless encoding.
    pub fn new_lossless(w: W) -> Self {
        Self {
            inner: image_webp::WebPEncoder::new(w),
        }
    }

    /// Encode image data with the indicated color type.
    ///
    /// The encoder requires image data be Rgb8 or Rgba8.
    ///
    /// # Panics
    ///
    /// Panics if `width * height * color.bytes_per_pixel() != data.len()`.
    #[track_caller]
    pub fn encode(
        self,
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ExtendedColorType,
    ) -> ImageResult<()> {
        let expected_buffer_len = color_type.buffer_size(width, height);
        assert_eq!(
            expected_buffer_len,
            buf.len() as u64,
            "Invalid buffer length: expected {expected_buffer_len} got {} for {width}x{height} image",
            buf.len(),
        );

        let color_type = match color_type {
            ExtendedColorType::L8 => image_webp::ColorType::L8,
            ExtendedColorType::La8 => image_webp::ColorType::La8,
            ExtendedColorType::Rgb8 => image_webp::ColorType::Rgb8,
            ExtendedColorType::Rgba8 => image_webp::ColorType::Rgba8,
            _ => {
                return Err(ImageError::Unsupported(
                    UnsupportedError::from_format_and_kind(
                        ImageFormat::WebP.into(),
                        UnsupportedErrorKind::Color(color_type),
                    ),
                ))
            }
        };

        self.inner
            .encode(buf, width, height, color_type)
            .map_err(ImageError::from_webp_encode)
    }
}

impl<W: Write> ImageEncoder for WebPEncoder<W> {
    #[track_caller]
    fn write_image(
        self,
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ExtendedColorType,
    ) -> ImageResult<()> {
        self.encode(buf, width, height, color_type)
    }
}

impl ImageError {
    fn from_webp_encode(e: image_webp::EncodingError) -> Self {
        match e {
            image_webp::EncodingError::IoError(e) => ImageError::IoError(e),
            _ => ImageError::Encoding(EncodingError::new(ImageFormat::WebP.into(), e)),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{ImageEncoder, RgbaImage};

    #[test]
    fn write_webp() {
        let img = RgbaImage::from_raw(10, 6, (0..240).collect()).unwrap();

        let mut output = Vec::new();
        super::WebPEncoder::new_lossless(&mut output)
            .write_image(
                img.inner_pixels(),
                img.width(),
                img.height(),
                crate::ExtendedColorType::Rgba8,
            )
            .unwrap();

        let img2 = crate::load_from_memory_with_format(&output, crate::ImageFormat::WebP)
            .unwrap()
            .to_rgba8();

        assert_eq!(img, img2);
    }
}
