use byteorder::{LittleEndian, WriteBytesExt};
use std::io::{self, Write};

use super::image_type::{ImageType, ALPHA_BIT_MASK, SCREEN_ORIGIN_BIT_MASK};
use crate::color::ColorType;
use crate::error::{EncodingError, ImageResult, UnsupportedError, UnsupportedErrorKind};
use crate::{image::ImageEncoder, ImageError, ImageFormat};

const MAX_IMAGE_DIMENSION: u32 = std::u16::MAX as u32;

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
        self,
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ColorType,
    ) -> ImageResult<()> {
        match color_type {
            ColorType::Rgba8
            | ColorType::Bgra8
            | ColorType::Rgb8
            | ColorType::Bgr8
            | ColorType::La8
            | ColorType::L8 => self
                .encode_impl(buf, width, height, color_type)
                .map_err(|err| {
                    ImageError::Encoding(EncodingError::new(ImageFormat::Tga.into(), err))
                }),
            _ => Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Tga.into(),
                    UnsupportedErrorKind::Color(color_type.into()),
                ),
            )),
        }
    }

    fn encode_impl(
        mut self,
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ColorType,
    ) -> io::Result<()> {
        // Get pixel encoding data.
        let (num_alpha_bits, other_channel_bits, image_type) = match color_type {
            ColorType::Rgba8 | ColorType::Bgra8 => (8, 24, ImageType::RawTrueColor),
            ColorType::Rgb8 | ColorType::Bgr8 => (0, 24, ImageType::RawTrueColor),
            ColorType::La8 => (8, 8, ImageType::RawGrayScale),
            ColorType::L8 => (0, 8, ImageType::RawGrayScale),
            _ => unreachable!(),
        };

        // Write TGA header.
        self.writer.write_u8(0)?; // No ID string.
        self.writer.write_u8(0)?; // No color map.
        self.writer.write_u8(image_type as u8)?; // Image type.
        self.writer.write_u16::<LittleEndian>(0)?; // No color map.
        self.writer.write_u16::<LittleEndian>(0)?; // No color map.
        self.writer.write_u8(0)?; // No color map.
        self.writer.write_u16::<LittleEndian>(0)?; // X-origin.
        self.writer.write_u16::<LittleEndian>(0)?; // Y-origin.
        self.write_dimension(width)?; // Width.
        self.write_dimension(height)?; // Height.
        self.writer.write_u8(num_alpha_bits + other_channel_bits)?; // Bits per pixel.
        self.writer
            .write_u8((num_alpha_bits & ALPHA_BIT_MASK) | SCREEN_ORIGIN_BIT_MASK)?; // Upper left origin.

        // Write out Bgr(a)8 or L(a)8 image data.
        let mut image = Vec::from(buf);

        match color_type {
            ColorType::Rgb8 | ColorType::Rgba8 => {
                for chunk in image.chunks_mut(color_type.bytes_per_pixel() as usize) {
                    chunk.swap(0, 2);
                }
            }
            _ => {}
        }

        self.writer.write_all(&image)?;
        Ok(())
    }

    fn write_dimension(&mut self, value: u32) -> io::Result<()> {
        if value < 1 || value > MAX_IMAGE_DIMENSION {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "Invalid TGA dimensions (width and height must be between 1 and {})",
                    MAX_IMAGE_DIMENSION
                ),
            ));
        }
        self.writer.write_u16::<LittleEndian>(value as u16)
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
