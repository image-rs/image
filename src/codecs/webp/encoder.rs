//! Encoding of WebP images.
///
/// Uses the simple encoding API from the [libwebp] library.
///
/// [libwebp]: https://developers.google.com/speed/webp/docs/api#simple_encoding_api
use std::io::Write;

use libwebp::{Encoder, PixelLayout, WebPMemory};

use crate::error::{
    EncodingError, ParameterError, ParameterErrorKind, UnsupportedError, UnsupportedErrorKind,
};
use crate::flat::SampleLayout;
use crate::{ColorType, ImageEncoder, ImageError, ImageFormat, ImageResult};

/// WebP Encoder.
pub struct WebPEncoder<W> {
    inner: W,
    quality: WebPQuality,
}

/// WebP encoder quality.
#[derive(Debug, Copy, Clone)]
pub struct WebPQuality(Quality);

#[derive(Debug, Copy, Clone)]
enum Quality {
    Lossless,
    Lossy(u8),
}

impl WebPQuality {
    /// Minimum lossy quality value (0).
    pub const MIN: u8 = 0;
    /// Maximum lossy quality value (100).
    pub const MAX: u8 = 100;
    /// Default lossy quality (80), providing a balance of quality and file size.
    pub const DEFAULT: u8 = 80;

    /// Lossless encoding.
    pub fn lossless() -> Self {
        Self(Quality::Lossless)
    }

    /// Lossy encoding. 0 = low quality, small size; 100 = high quality, large size.
    ///
    /// Values are clamped from 0 to 100.
    pub fn lossy(quality: u8) -> Self {
        Self(Quality::Lossy(quality.clamp(Self::MIN, Self::MAX)))
    }
}

impl Default for WebPQuality {
    fn default() -> Self {
        Self::lossy(WebPQuality::DEFAULT)
    }
}

impl<W: Write> WebPEncoder<W> {
    /// Create a new encoder that writes its output to `w`.
    ///
    /// Defaults to lossy encoding, see [`WebPQuality::DEFAULT`].
    pub fn new(w: W) -> Self {
        WebPEncoder::new_with_quality(w, WebPQuality::default())
    }

    /// Create a new encoder with the specified quality, that writes its output to `w`.
    pub fn new_with_quality(w: W, quality: WebPQuality) -> Self {
        Self { inner: w, quality }
    }

    /// Encode image data with the indicated color type.
    ///
    /// The encoder requires image data be Rgb8 or Rgba8.
    pub fn encode(
        mut self,
        data: &[u8],
        width: u32,
        height: u32,
        color: ColorType,
    ) -> ImageResult<()> {
        // TODO: convert color types internally?
        let layout = match color {
            ColorType::Rgb8 => PixelLayout::Rgb,
            ColorType::Rgba8 => PixelLayout::Rgba,
            _ => {
                return Err(ImageError::Unsupported(
                    UnsupportedError::from_format_and_kind(
                        ImageFormat::WebP.into(),
                        UnsupportedErrorKind::Color(color.into()),
                    ),
                ))
            }
        };

        // Validate dimensions upfront to avoid panics.
        if width == 0
            || height == 0
            || !SampleLayout::row_major_packed(color.channel_count(), width, height)
                .fits(data.len())
        {
            return Err(ImageError::Parameter(ParameterError::from_kind(
                ParameterErrorKind::DimensionMismatch,
            )));
        }

        // Call the native libwebp library to encode the image.
        let encoder = Encoder::new(data, layout, width, height);
        let encoded: WebPMemory = match self.quality.0 {
            Quality::Lossless => encoder.encode_lossless(),
            Quality::Lossy(quality) => encoder.encode(quality as f32),
        };

        // The simple encoding API in libwebp does not return errors.
        if encoded.is_empty() {
            return Err(ImageError::Encoding(EncodingError::new(
                ImageFormat::WebP.into(),
                "encoding failed, output empty",
            )));
        }

        self.inner.write_all(&encoded)?;
        Ok(())
    }
}

impl<W: Write> ImageEncoder for WebPEncoder<W> {
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

#[cfg(test)]
mod tests {
    use crate::codecs::webp::{WebPEncoder, WebPQuality};
    use crate::{ColorType, ImageEncoder};

    #[test]
    fn webp_lossless_deterministic() {
        // 1x1 8-bit image buffer containing a single red pixel.
        let rgb: &[u8] = &[255, 0, 0];
        let rgba: &[u8] = &[255, 0, 0, 128];
        for (color, img, expected) in [
            (
                ColorType::Rgb8,
                rgb,
                [
                    82, 73, 70, 70, 28, 0, 0, 0, 87, 69, 66, 80, 86, 80, 56, 76, 15, 0, 0, 0, 47,
                    0, 0, 0, 0, 7, 16, 253, 143, 254, 7, 34, 162, 255, 1, 0,
                ],
            ),
            (
                ColorType::Rgba8,
                rgba,
                [
                    82, 73, 70, 70, 28, 0, 0, 0, 87, 69, 66, 80, 86, 80, 56, 76, 15, 0, 0, 0, 47,
                    0, 0, 0, 16, 7, 16, 253, 143, 2, 6, 34, 162, 255, 1, 0,
                ],
            ),
        ] {
            // Encode it into a memory buffer.
            let mut encoded_img = Vec::new();
            {
                let encoder =
                    WebPEncoder::new_with_quality(&mut encoded_img, WebPQuality::lossless());
                encoder
                    .write_image(&img, 1, 1, color)
                    .expect("image encoding failed");
            }

            // WebP encoding should be deterministic.
            assert_eq!(encoded_img, expected);
        }
    }

    #[derive(Debug, Clone)]
    struct MockImage {
        width: u32,
        height: u32,
        color: ColorType,
        data: Vec<u8>,
    }

    impl quickcheck::Arbitrary for MockImage {
        fn arbitrary(g: &mut quickcheck::Gen) -> Self {
            // Limit to small, non-empty images <= 512x512.
            let width = u32::arbitrary(g) % 512 + 1;
            let height = u32::arbitrary(g) % 512 + 1;
            let (color, stride) = if bool::arbitrary(g) {
                (ColorType::Rgb8, 3)
            } else {
                (ColorType::Rgba8, 4)
            };
            let size = width * height * stride;
            let data: Vec<u8> = (0..size).map(|_| u8::arbitrary(g)).collect();
            MockImage {
                width,
                height,
                color,
                data,
            }
        }
    }

    quickcheck! {
        fn fuzz_webp_valid_image(image: MockImage, quality: u8) -> bool {
            // Check valid images do not panic.
            let mut buffer = Vec::<u8>::new();
            for webp_quality in [WebPQuality::lossless(), WebPQuality::lossy(quality)] {
                buffer.clear();
                let encoder = WebPEncoder::new_with_quality(&mut buffer, webp_quality);
                if !encoder
                    .write_image(&image.data, image.width, image.height, image.color)
                    .is_ok() {
                    return false;
                }
            }
            true
        }

        fn fuzz_webp_no_panic(data: Vec<u8>, width: u8, height: u8, quality: u8) -> bool {
            // Check random (usually invalid) parameters do not panic.
            let mut buffer = Vec::<u8>::new();
            for color in [ColorType::Rgb8, ColorType::Rgba8] {
                for webp_quality in [WebPQuality::lossless(), WebPQuality::lossy(quality)] {
                    buffer.clear();
                    let encoder = WebPEncoder::new_with_quality(&mut buffer, webp_quality);
                    // Ignore errors.
                    let _ = encoder
                        .write_image(&data, width as u32, height as u32, color);
                }
            }
            true
        }
    }
}
