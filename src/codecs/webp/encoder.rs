//! Encoding of WebP images.

use std::io::Write;

use crate::{
    error::{EncodingError, UnsupportedError, UnsupportedErrorKind}, ColorType, ImageEncoder, ImageError, ImageFormat, ImageResult,
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
    pub fn encode(self, data: &[u8], width: u32, height: u32, color: ColorType) -> ImageResult<()> {
        let expected_buffer_len =
            (width as u64 * height as u64).saturating_mul(color.bytes_per_pixel() as u64);
        assert_eq!(
            expected_buffer_len,
            data.len() as u64,
            "Invalid buffer length: expected {expected_buffer_len} got {} for {width}x{height} image",
            data.len(),
        );

        let color = match color {
            ColorType::L8 => image_webp::ColorType::L8,
            ColorType::La8 => image_webp::ColorType::La8,
            ColorType::Rgb8 => image_webp::ColorType::Rgb8,
            ColorType::Rgba8 => image_webp::ColorType::Rgba8,
            _ => {
                return Err(ImageError::Unsupported(
                    UnsupportedError::from_format_and_kind(
                        ImageFormat::WebP.into(),
                        UnsupportedErrorKind::Color(color.into()),
                    ),
                ))
            }
        };

        Ok(self.inner.encode(data, width, height, color)?)
    }
}

impl<W: Write> ImageEncoder for WebPEncoder<W> {
    #[track_caller]
    fn write_image(
        self,
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ColorType,
    ) -> ImageResult<()> {
        self.encode(buf, width, height, color_type)
    }
}

impl From<image_webp::EncodingError> for ImageError {
    fn from(e: image_webp::EncodingError) -> ImageError {
        ImageError::Encoding(EncodingError::new(ImageFormat::WebP.into(), e))
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
                crate::ColorType::Rgba8,
            )
            .unwrap();

        let img2 = crate::load_from_memory_with_format(&output, crate::ImageFormat::WebP)
            .unwrap()
            .to_rgba8();

        assert_eq!(img, img2);
    }
}
