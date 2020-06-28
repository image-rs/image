use byteorder::{LittleEndian, WriteBytesExt};
use std::{convert::TryFrom, io::Write};

use super::image_type::{ImageType, ALPHA_BIT_MASK, SCREEN_ORIGIN_BIT_MASK};
use crate::color::ColorType;
use crate::error::{
    ImageResult, ParameterError, ParameterErrorKind, UnsupportedError, UnsupportedErrorKind,
};
use crate::{image::ImageEncoder, ImageError, ImageFormat};

/// TGA encoder.
pub struct TgaEncoder<W: Write> {
    writer: W,
}

impl<W: Write> TgaEncoder<W> {
    /// Create a new encoder that writes its output to ```w```.
    pub fn new(w: W) -> TgaEncoder<W> {
        TgaEncoder { writer: w }
    }

    /// Encodes the image ```buf``` that has dimensions ```width```
    /// and ```height``` and ```ColorType``` ```color_type```.
    ///
    /// The dimensions of the image must be between 1 and 65535 (inclusive) or
    /// an error will be returned.
    pub fn encode(
        mut self,
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ColorType,
    ) -> ImageResult<()> {
        let (num_alpha_bits, other_channel_bits, image_type) = get_pixel_info(color_type)?;
        let (width, height) = tga_dimensions(width, height)?;

        // Write TGA header.
        self.writer.write_u8(0)?; // No ID string.
        self.writer.write_u8(0)?; // No color map.
        self.writer.write_u8(image_type as u8)?; // Image type.
        self.writer.write_u16::<LittleEndian>(0)?; // No color map.
        self.writer.write_u16::<LittleEndian>(0)?; // No color map.
        self.writer.write_u8(0)?; // No color map.
        self.writer.write_u16::<LittleEndian>(0)?; // X-origin.
        self.writer.write_u16::<LittleEndian>(0)?; // Y-origin.
        self.writer.write_u16::<LittleEndian>(width)?; // Width.
        self.writer.write_u16::<LittleEndian>(height)?; // Height.
        self.writer.write_u8(num_alpha_bits + other_channel_bits)?; // Bits per pixel.
        self.writer
            .write_u8((num_alpha_bits & ALPHA_BIT_MASK) | SCREEN_ORIGIN_BIT_MASK)?; // Upper left origin.

        // Write out Bgr(a)8 or L(a)8 image data.
        let mut image = Vec::from(buf);

        match color_type {
            ColorType::Rgb8 | ColorType::Rgba8 => {
                for chunk in image.chunks_mut(usize::from(color_type.bytes_per_pixel())) {
                    chunk.swap(0, 2);
                }
            }
            _ => {}
        }

        self.writer.write_all(&image)?;
        Ok(())
    }
}

impl<W: Write> ImageEncoder for TgaEncoder<W> {
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

fn get_pixel_info(color_type: ColorType) -> ImageResult<(u8, u8, ImageType)> {
    let sizes = match color_type {
        ColorType::Rgba8 | ColorType::Bgra8 => (8, 24, ImageType::RawTrueColor),
        ColorType::Rgb8 | ColorType::Bgr8 => (0, 24, ImageType::RawTrueColor),
        ColorType::La8 => (8, 8, ImageType::RawGrayScale),
        ColorType::L8 => (0, 8, ImageType::RawGrayScale),
        _ => {
            return Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Tga.into(),
                    UnsupportedErrorKind::Color(color_type.into()),
                ),
            ))
        }
    };

    Ok(sizes)
}

fn tga_dimensions(width: u32, height: u32) -> ImageResult<(u16, u16)> {
    fn inner_dimensions(width: u32, height: u32) -> Option<(u16, u16)> {
        let width = u16::try_from(width).ok()?;
        let height = u16::try_from(height).ok()?;
        Some((width, height))
    }

    inner_dimensions(width, height).ok_or(ImageError::Parameter(ParameterError::from_kind(
        ParameterErrorKind::DimensionMismatch,
    )))
}

#[cfg(test)]
mod tests {
    use super::super::TgaDecoder;
    use super::TgaEncoder;
    use crate::color::ColorType;
    use crate::{error::ParameterErrorKind, image::ImageDecoder, ImageError};
    use std::io::Cursor;

    fn round_trip_image(image: &[u8], width: u32, height: u32, c: ColorType) -> Vec<u8> {
        let mut encoded_data = Vec::new();
        {
            let encoder = TgaEncoder::new(&mut encoded_data);
            encoder
                .encode(&image, width, height, c)
                .expect("could not encode image");
        }

        let decoder = TgaDecoder::new(Cursor::new(&encoded_data)).expect("failed to decode");

        let mut buf = vec![0; decoder.total_bytes() as usize];
        decoder.read_image(&mut buf).expect("failed to decode");
        buf
    }

    #[test]
    fn test_image_too_large() {
        // TGA cannot encode images larger than 65,535×65,535
        // create a 65,536×1 8-bit black image buffer
        let size = usize::from(u16::MAX) + 1;
        let img = vec![0u8; size];
        // Try to encode an image that is too large
        let mut encoded = Vec::new();
        let encoder = TgaEncoder::new(&mut encoded);
        let result = encoder.encode(&img, size as u32, 1, ColorType::L8);
        match result {
            Err(ImageError::Parameter(err)) => {
                assert_eq!(err.kind(), ParameterErrorKind::DimensionMismatch)
            }
            other => assert!(
                false,
                "Encoding an image that is too large should return a DimensionError \
                                it returned {:?} instead",
                other
            ),
        }
    }

    #[test]
    fn round_trip_single_pixel_rgb() {
        let image = [0, 1, 2];
        let decoded = round_trip_image(&image, 1, 1, ColorType::Rgb8);
        assert_eq!(decoded.len(), image.len());
        assert_eq!(decoded.as_slice(), image);
    }

    #[test]
    fn round_trip_single_pixel_rgba() {
        let image = [0, 1, 2, 3];
        let decoded = round_trip_image(&image, 1, 1, ColorType::Rgba8);
        assert_eq!(decoded.len(), image.len());
        assert_eq!(decoded.as_slice(), image);
    }

    #[test]
    fn round_trip_single_pixel_bgr() {
        let image = [0, 1, 2];
        let decoded = round_trip_image(&image, 1, 1, ColorType::Bgr8);
        assert_eq!(decoded.len(), image.len());
        assert_eq!(decoded.as_slice(), [2, 1, 0]);
    }

    #[test]
    fn round_trip_single_pixel_bgra() {
        let image = [0, 1, 2, 3];
        let decoded = round_trip_image(&image, 1, 1, ColorType::Bgra8);
        assert_eq!(decoded.len(), image.len());
        assert_eq!(decoded.as_slice(), [2, 1, 0, 3]);
    }

    #[test]
    fn round_trip_gray() {
        let image = [0, 1, 2];
        let decoded = round_trip_image(&image, 3, 1, ColorType::L8);
        assert_eq!(decoded.len(), image.len());
        assert_eq!(decoded.as_slice(), image);
    }

    #[test]
    fn round_trip_graya() {
        let image = [0, 1, 2, 3, 4, 5];
        let decoded = round_trip_image(&image, 1, 3, ColorType::La8);
        assert_eq!(decoded.len(), image.len());
        assert_eq!(decoded.as_slice(), image);
    }

    #[test]
    fn round_trip_3px_rgb() {
        let image = [0; 3 * 3 * 3]; // 3x3 pixels, 3 bytes per pixel
        let _decoded = round_trip_image(&image, 3, 3, ColorType::Rgb8);
    }
}
