#![allow(clippy::too_many_arguments)]
use std::io::Write;
use std::{error, fmt};

use crate::error::{
    EncodingError, ImageError, ImageFormatHint, ImageResult, ParameterError, ParameterErrorKind,
    UnsupportedError, UnsupportedErrorKind,
};
use crate::{ColorType, DynamicImage, ExtendedColorType, ImageEncoder, ImageFormat};

use jpeg_encoder::Encoder;

/// Represents a unit in which the density of an image is measured
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PixelDensityUnit {
    /// Represents the absence of a unit, the values indicate only a
    /// [pixel aspect ratio](https://en.wikipedia.org/wiki/Pixel_aspect_ratio)
    PixelAspectRatio,

    /// Pixels per inch (2.54 cm)
    Inches,

    /// Pixels per centimeter
    Centimeters,
}

/// Represents the pixel density of an image
///
/// For example, a 300 DPI image is represented by:
///
/// ```rust
/// use image::codecs::jpeg::*;
/// let hdpi = PixelDensity::dpi(300);
/// assert_eq!(hdpi, PixelDensity {density: (300,300), unit: PixelDensityUnit::Inches})
/// ```
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct PixelDensity {
    /// A couple of values for (Xdensity, Ydensity)
    pub density: (u16, u16),
    /// The unit in which the density is measured
    pub unit: PixelDensityUnit,
}

impl PixelDensity {
    /// Creates the most common pixel density type:
    /// the horizontal and the vertical density are equal,
    /// and measured in pixels per inch.
    #[must_use]
    pub fn dpi(density: u16) -> Self {
        PixelDensity {
            density: (density, density),
            unit: PixelDensityUnit::Inches,
        }
    }

    /// Converts pixel density to the representation used by jpeg-encoder crate
    fn to_encoder_repr(&self) -> jpeg_encoder::Density {
        match self.unit {
            PixelDensityUnit::PixelAspectRatio => jpeg_encoder::Density::None, // TODO: https://github.com/vstroebel/jpeg-encoder/issues/21
            PixelDensityUnit::Inches => jpeg_encoder::Density::Inch {
                x: self.density.0,
                y: self.density.1,
            },
            PixelDensityUnit::Centimeters => jpeg_encoder::Density::Centimeter {
                x: self.density.0,
                y: self.density.1,
            },
        }
    }
}

impl Default for PixelDensity {
    /// Returns a pixel density with a pixel aspect ratio of 1
    fn default() -> Self {
        PixelDensity {
            density: (1, 1),
            unit: PixelDensityUnit::PixelAspectRatio,
        }
    }
}

/// Errors that can occur when encoding a JPEG image
#[derive(Debug, Copy, Clone)]
enum EncoderError {
    /// JPEG does not support this size
    InvalidSize(u32, u32),
}

impl fmt::Display for EncoderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EncoderError::InvalidSize(w, h) => f.write_fmt(format_args!(
                "Invalid image size ({w} x {h}) to encode as JPEG: \
                 width and height must be >= 1 and <= 65535"
            )),
        }
    }
}

impl From<EncoderError> for ImageError {
    fn from(e: EncoderError) -> ImageError {
        ImageError::Encoding(EncodingError::new(ImageFormat::Jpeg.into(), e))
    }
}

impl error::Error for EncoderError {}

/// The representation of a JPEG encoder
pub struct JpegEncoder<W: Write> {
    encoder: Encoder<W>,
    exif: Vec<u8>,
}

impl<W: Write> JpegEncoder<W> {
    /// Create a new encoder that writes its output to ```w```
    pub fn new(w: W) -> JpegEncoder<W> {
        JpegEncoder::new_with_quality(w, 75)
    }

    /// Create a new encoder that writes its output to ```w```, and has
    /// the quality parameter ```quality``` with a value in the range 1-100
    /// where 1 is the worst and 100 is the best.
    pub fn new_with_quality(w: W, quality: u8) -> JpegEncoder<W> {
        JpegEncoder {
            encoder: Encoder::new(w, quality),
            exif: Vec::new(),
        }
    }

    /// Set the pixel density of the images the encoder will encode.
    /// If this method is not called, then a default pixel aspect ratio of 1x1 will be applied,
    /// and no DPI information will be stored in the image.
    pub fn set_pixel_density(&mut self, pixel_density: PixelDensity) {
        self.encoder.set_density(pixel_density.to_encoder_repr());
    }

    /// Encodes the image stored in the raw byte buffer ```image```
    /// that has dimensions ```width``` and ```height```
    /// and ```ColorType``` ```c```
    ///
    /// # Panics
    ///
    /// Panics if `width * height * color_type.bytes_per_pixel() != image.len()`.
    #[track_caller]
    fn encode(
        mut self,
        image: &[u8],
        width: u32,
        height: u32,
        color_type: ExtendedColorType,
    ) -> ImageResult<()> {
        let expected_buffer_len = color_type.buffer_size(width, height);
        assert_eq!(
            expected_buffer_len,
            image.len() as u64,
            "Invalid buffer length: expected {expected_buffer_len} got {} for {width}x{height} image",
            image.len(),
        );

        let (width, height) = match (u16::try_from(width), u16::try_from(height)) {
            (Ok(w @ 1..), Ok(h @ 1..)) => (w, h),
            _ => return Err(EncoderError::InvalidSize(width, height).into()),
        };

        self.write_exif()?;

        let encode_jpeg = |color: jpeg_encoder::ColorType| {
            self.encoder
                .encode(image, width, height, color)
                .map_err(|err| {
                    ImageError::Encoding(EncodingError::new(
                        ImageFormatHint::Exact(ImageFormat::Jpeg),
                        err,
                    ))
                })
        };

        match color_type {
            ExtendedColorType::L8 => {
                let color = jpeg_encoder::ColorType::Luma;
                encode_jpeg(color)
            }
            ExtendedColorType::Rgb8 => {
                let color = jpeg_encoder::ColorType::Rgb;
                encode_jpeg(color)
            }
            _ => Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Jpeg.into(),
                    UnsupportedErrorKind::Color(color_type),
                ),
            )),
        }
    }

    fn write_exif(&mut self) -> ImageResult<()> {
        if !self.exif.is_empty() {
            let mut formatted = EXIF_HEADER.to_vec();
            formatted.extend_from_slice(&self.exif);
            self.encoder
                .add_app_segment(APP1, &formatted)
                .map_err(|_| {
                    ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                        ImageFormat::Jpeg.into(),
                        UnsupportedErrorKind::GenericFeature("Exif chunk too large".to_string()),
                    ))
                })?;
        }

        Ok(())
    }
}

// E x i f \0 \0
/// The header for an EXIF APP1 segment
const EXIF_HEADER: [u8; 6] = [0x45, 0x78, 0x69, 0x66, 0x00, 0x00];
const APP1: u8 = 1;

impl<W: Write> ImageEncoder for JpegEncoder<W> {
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

    fn set_icc_profile(&mut self, icc_profile: Vec<u8>) -> Result<(), UnsupportedError> {
        self.encoder.add_icc_profile(&icc_profile).map_err(|_| {
            UnsupportedError::from_format_and_kind(
                ImageFormat::Jpeg.into(),
                UnsupportedErrorKind::GenericFeature("ICC chunk too large".to_string()),
            )
        })
    }

    fn set_exif_metadata(&mut self, exif: Vec<u8>) -> Result<(), UnsupportedError> {
        self.exif = exif;
        Ok(())
    }

    fn make_compatible_img(
        &self,
        _: crate::io::encoder::MethodSealedToImage,
        img: &DynamicImage,
    ) -> Option<DynamicImage> {
        use ColorType::*;
        match img.color() {
            L8 | Rgb8 => None,
            La8 | L16 | La16 => Some(img.to_luma8().into()),
            Rgba8 | Rgb16 | Rgb32F | Rgba16 | Rgba32F => Some(img.to_rgb8().into()),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    #[cfg(feature = "benchmarks")]
    extern crate test;
    #[cfg(feature = "benchmarks")]
    use test::Bencher;

    use crate::{ColorType, DynamicImage, ExtendedColorType, ImageEncoder, ImageError};
    use crate::{ImageDecoder as _, ImageFormat};

    use super::super::{JpegDecoder, JpegEncoder};

    fn decode(encoded: &[u8]) -> Vec<u8> {
        let decoder = JpegDecoder::new(Cursor::new(encoded)).expect("Could not decode image");

        let mut decoded = vec![0; decoder.total_bytes() as usize];
        decoder
            .read_image(&mut decoded)
            .expect("Could not decode image");
        decoded
    }

    #[test]
    fn roundtrip_sanity_check() {
        // create a 1x1 8-bit image buffer containing a single red pixel
        let img = [255u8, 0, 0];

        // encode it into a memory buffer
        let mut encoded_img = Vec::new();
        {
            let encoder = JpegEncoder::new_with_quality(&mut encoded_img, 100);
            encoder
                .write_image(&img, 1, 1, ExtendedColorType::Rgb8)
                .expect("Could not encode image");
        }

        // decode it from the memory buffer
        {
            let decoded = decode(&encoded_img);
            // note that, even with the encode quality set to 100, we do not get the same image
            // back. Therefore, we're going to assert that it's at least red-ish:
            assert_eq!(3, decoded.len());
            assert!(decoded[0] > 0x80);
            assert!(decoded[1] < 0x80);
            assert!(decoded[2] < 0x80);
        }
    }

    #[test]
    fn grayscale_roundtrip_sanity_check() {
        // create a 2x2 8-bit image buffer containing a white diagonal
        let img = [255u8, 0, 0, 255];

        // encode it into a memory buffer
        let mut encoded_img = Vec::new();
        {
            let encoder = JpegEncoder::new_with_quality(&mut encoded_img, 100);
            encoder
                .write_image(&img[..], 2, 2, ExtendedColorType::L8)
                .expect("Could not encode image");
        }

        // decode it from the memory buffer
        {
            let decoded = decode(&encoded_img);
            // note that, even with the encode quality set to 100, we do not get the same image
            // back. Therefore, we're going to assert that the diagonal is at least white-ish:
            assert_eq!(4, decoded.len());
            assert!(decoded[0] > 0x80);
            assert!(decoded[1] < 0x80);
            assert!(decoded[2] < 0x80);
            assert!(decoded[3] > 0x80);
        }
    }

    #[test]
    fn roundtrip_exif_icc() {
        // create a 2x2 8-bit image buffer containing a white diagonal
        let img = [255u8, 0, 0, 255];

        let exif = vec![1, 2, 3];
        let icc = vec![4, 5, 6];

        // encode it into a memory buffer
        let mut encoded_img = Vec::new();
        {
            let mut encoder = JpegEncoder::new_with_quality(&mut encoded_img, 100);

            encoder.set_exif_metadata(exif.clone()).unwrap();
            encoder.set_icc_profile(icc.clone()).unwrap();

            encoder
                .write_image(&img[..], 2, 2, ExtendedColorType::L8)
                .expect("Could not encode image");
        }

        let mut decoder =
            JpegDecoder::new(Cursor::new(encoded_img)).expect("Could not decode image");
        let decoded_exif = decoder
            .exif_metadata()
            .expect("Error decoding Exif")
            .expect("Exif is empty");
        assert_eq!(exif, decoded_exif);
        let decoded_icc = decoder
            .icc_profile()
            .expect("Error decoding ICC")
            .expect("ICC is empty");
        assert_eq!(icc, decoded_icc);
    }

    #[test]
    fn test_image_too_large() {
        // JPEG cannot encode images larger than 65,535×65,535
        // create a 65,536×1 8-bit black image buffer
        let img = [0; 65_536];
        // Try to encode an image that is too large
        let mut encoded = Vec::new();
        let encoder = JpegEncoder::new_with_quality(&mut encoded, 100);
        let result = encoder.write_image(&img, 65_536, 1, ExtendedColorType::L8);
        match result {
            Err(ImageError::Encoding(_)) => (),
            other => {
                panic!(
                    "Encoding an image that is too large should return an EncodingError \
                                it returned {other:?} instead"
                )
            }
        }
    }

    #[test]
    fn check_color_types() {
        const ALL: &[ColorType] = &[
            ColorType::L8,
            ColorType::L16,
            ColorType::La8,
            ColorType::Rgb8,
            ColorType::Rgba8,
            ColorType::La16,
            ColorType::Rgb16,
            ColorType::Rgba16,
            ColorType::Rgb32F,
            ColorType::Rgba32F,
        ];

        for color in ALL {
            let image = DynamicImage::new(1, 1, *color);

            image
                .write_to(&mut Cursor::new(vec![]), ImageFormat::Jpeg)
                .expect("supported or converted");
        }
    }

    #[cfg(feature = "benchmarks")]
    #[bench]
    fn bench_jpeg_encoder_new(b: &mut Bencher) {
        b.iter(|| {
            let mut y = vec![];
            let _x = JpegEncoder::new(&mut y);
        });
    }
}
