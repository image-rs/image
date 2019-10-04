//! Decoding and Encoding of PNG Images
//!
//! PNG (Portable Network Graphics) is an image format that supports lossless compression.
//!
//! # Related Links
//! * <http://www.w3.org/TR/PNG/> - The PNG Specification
//!

extern crate png;

use std::io::{self, Read, Write};

use color::{ColorType, ExtendedColorType};
use image::{ImageDecoder, ImageError, ImageResult};

/// PNG Reader
///
/// This reader will try to read the png one row at a time,
/// however for interlaced png files this is not posible and
/// these are therefore readed at once.
pub struct PNGReader<R: Read> {
    reader: png::Reader<R>,
    buffer: Vec<u8>,
    index: usize,
}

impl<R: Read> PNGReader<R> {
    fn new(mut reader: png::Reader<R>) -> ImageResult<PNGReader<R>> {
        let len = reader.output_buffer_size();
        // Since interlaced images do not come in 
        // scanline order it is almost impossible to
        // read them in a streaming fashion, however
        // this shouldn't be a too big of a problem
        // as most interlaced images should fit in memory.
        let buffer = if reader.info().interlaced {
            let mut buffer = vec![0; len];
            reader.next_frame(&mut buffer)?;
            buffer
        } else {
            Vec::new()
        };

        Ok(PNGReader {
            reader,
            buffer,
            index: 0,
        })
    }
}

impl<R: Read> Read for PNGReader<R> {
    fn read(&mut self, mut buf: &mut [u8]) -> io::Result<usize> {
        // io::Write::write for slice cannot fail
        let readed = buf.write(&self.buffer[self.index..]).unwrap();

        let mut bytes = readed;
        self.index += readed;
    
        while self.index >= self.buffer.len() {
            match self.reader.next_row()? {
                Some(row) => {
                    // Faster to copy directly to external buffer
                    let readed  = buf.write(row).unwrap();
                    bytes += readed;

                    self.buffer = (&row[readed..]).to_owned();
                    self.index = 0;
                }
                None => return Ok(bytes)
            }
        }

        Ok(bytes)
    }

    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
        let mut bytes = self.buffer.len();
        buf.extend_from_slice(&self.buffer);
        self.buffer = Vec::new();
        self.index = 0;
    
        while let Some(row) = self.reader.next_row()? {
            buf.extend_from_slice(row);
            bytes += row.len();
        }

        Ok(bytes)
    }
}

/// PNG decoder
pub struct PngDecoder<R: Read> {
    color_type: ColorType,
    reader: png::Reader<R>,
}

impl<R: Read> PngDecoder<R> {
    /// Creates a new decoder that decodes from the stream ```r```
    pub fn new(r: R) -> ImageResult<PngDecoder<R>> {
        let limits = png::Limits {
            bytes: usize::max_value(),
        };
        let decoder = png::Decoder::new_with_limits(r, limits);
        let (_, mut reader) = decoder.read_info()?;
        let (color_type, bits) = reader.output_color_type();
        let color_type = match (color_type, bits) {
            (png::ColorType::Grayscale, png::BitDepth::Eight) => ColorType::L8,
            (png::ColorType::Grayscale, png::BitDepth::Sixteen) => ColorType::L16,
            (png::ColorType::GrayscaleAlpha, png::BitDepth::Eight) => ColorType::La8,
            (png::ColorType::GrayscaleAlpha, png::BitDepth::Sixteen) => ColorType::La16,
            (png::ColorType::RGB, png::BitDepth::Eight) => ColorType::Rgb8,
            (png::ColorType::RGB, png::BitDepth::Sixteen) => ColorType::Rgb16,
            (png::ColorType::RGBA, png::BitDepth::Eight) => ColorType::Rgba8,
            (png::ColorType::RGBA, png::BitDepth::Sixteen) => ColorType::Rgba16,

            (png::ColorType::Grayscale, png::BitDepth::One) =>
                return Err(ImageError::UnsupportedColor(ExtendedColorType::L1)),
            (png::ColorType::GrayscaleAlpha, png::BitDepth::One) =>
                return Err(ImageError::UnsupportedColor(ExtendedColorType::La1)),
            (png::ColorType::RGB, png::BitDepth::One) =>
                return Err(ImageError::UnsupportedColor(ExtendedColorType::Rgb1)),
            (png::ColorType::RGBA, png::BitDepth::One) =>
                return Err(ImageError::UnsupportedColor(ExtendedColorType::Rgba1)),

            (png::ColorType::Grayscale, png::BitDepth::Two) =>
                return Err(ImageError::UnsupportedColor(ExtendedColorType::L2)),
            (png::ColorType::GrayscaleAlpha, png::BitDepth::Two) =>
                return Err(ImageError::UnsupportedColor(ExtendedColorType::La2)),
            (png::ColorType::RGB, png::BitDepth::Two) =>
                return Err(ImageError::UnsupportedColor(ExtendedColorType::Rgb2)),
            (png::ColorType::RGBA, png::BitDepth::Two) =>
                return Err(ImageError::UnsupportedColor(ExtendedColorType::Rgba2)),

            (png::ColorType::Grayscale, png::BitDepth::Four) =>
                return Err(ImageError::UnsupportedColor(ExtendedColorType::L4)),
            (png::ColorType::GrayscaleAlpha, png::BitDepth::Four) =>
                return Err(ImageError::UnsupportedColor(ExtendedColorType::La4)),
            (png::ColorType::RGB, png::BitDepth::Four) =>
                return Err(ImageError::UnsupportedColor(ExtendedColorType::Rgb4)),
            (png::ColorType::RGBA, png::BitDepth::Four) =>
                return Err(ImageError::UnsupportedColor(ExtendedColorType::Rgba4)),

            (png::ColorType::Indexed, bits) =>
                return Err(ImageError::UnsupportedColor(ExtendedColorType::Unknown(bits as u8))),
        };

        Ok(PngDecoder { color_type, reader })
    }
}

impl<'a, R: 'a + Read> ImageDecoder<'a> for PngDecoder<R> {
    type Reader = PNGReader<R>;

    fn dimensions(&self) -> (u32, u32) {
        self.reader.info().size()
    }

    fn color_type(&self) -> ColorType {
        self.color_type
    }

    fn into_reader(self) -> ImageResult<Self::Reader> {
        PNGReader::new(self.reader)
    }

    fn read_image(mut self, buf: &mut [u8]) -> ImageResult<()> {
        assert!(buf.len() as u64 == self.total_bytes());
        self.reader.next_frame(buf)?;
        Ok(())
    }

    fn scanline_bytes(&self) -> u64 {
        let width = self.reader.info().width;
        self.reader.output_line_size(width) as u64
    }
}

/// PNG encoder
pub struct PNGEncoder<W: Write> {
    w: W,
}

impl<W: Write> PNGEncoder<W> {
    /// Create a new encoder that writes its output to ```w```
    pub fn new(w: W) -> PNGEncoder<W> {
        PNGEncoder { w }
    }

    /// Encodes the image ```image```
    /// that has dimensions ```width``` and ```height```
    /// and ```ColorType``` ```c```
    pub fn encode(self, data: &[u8], width: u32, height: u32, color: ColorType) -> io::Result<()> {
        let (ct, bits) = match color {
            ColorType::L8 => (png::ColorType::Grayscale, png::BitDepth::Eight),
            ColorType::L16 => (png::ColorType::Grayscale,png::BitDepth::Sixteen),
            ColorType::La8 => (png::ColorType::GrayscaleAlpha, png::BitDepth::Eight),
            ColorType::La16 => (png::ColorType::GrayscaleAlpha,png::BitDepth::Sixteen),
            ColorType::Rgb8 => (png::ColorType::RGB, png::BitDepth::Eight),
            ColorType::Rgb16 => (png::ColorType::RGB,png::BitDepth::Sixteen),
            ColorType::Rgba8 => (png::ColorType::RGBA, png::BitDepth::Eight),
            ColorType::Rgba16 => (png::ColorType::RGBA,png::BitDepth::Sixteen),
            ColorType::Bgr8 => (png::ColorType::RGB, png::BitDepth::Eight),
            ColorType::Bgra8 => (png::ColorType::RGBA, png::BitDepth::Eight),
            _ => return Err(io::Error::new(io::ErrorKind::InvalidInput,
                                           "Unsupported color type".to_owned())),
        };

        let mut encoder = png::Encoder::new(self.w, width, height);
        encoder.set_color(ct);
        encoder.set_depth(bits);
        let mut writer = encoder.write_header()?;
        writer.write_image_data(data).map_err(|e| e.into())
    }
}

impl From<png::DecodingError> for ImageError {
    fn from(err: png::DecodingError) -> ImageError {
        use self::png::DecodingError::*;
        match err {
            IoError(err) => ImageError::IoError(err),
            Format(desc) => ImageError::FormatError(desc.into_owned()),
            InvalidSignature => ImageError::FormatError("invalid signature".into()),
            CrcMismatch { .. } => ImageError::FormatError("CRC error".into()),
            Other(desc) => ImageError::FormatError(desc.into_owned()),
            CorruptFlateStream => {
                ImageError::FormatError("compressed data stream corrupted".into())
            }
            LimitsExceeded => ImageError::InsufficientMemory,
        }
    }
}

#[cfg(test)]
mod tests {
    use image::ImageDecoder;
    use std::io::Read;
    use super::*;

    #[test]
    fn ensure_no_decoder_off_by_one() {
        let dec = PngDecoder::new(std::fs::File::open("tests/images/png/bugfixes/debug_triangle_corners_widescreen.png").unwrap())
            .expect("Unable to read PNG file (does it exist?)");

        assert_eq![(2000, 1000), dec.dimensions()];

        assert_eq![
            ColorType::Rgb8,
            dec.color_type(),
            "Image MUST have the Rgb8 format"
        ];

        let correct_bytes = dec
            .into_reader()
            .expect("Unable to read file")
            .bytes()
            .map(|x| x.expect("Unable to read byte"))
            .collect::<Vec<u8>>();

        assert_eq![6_000_000, correct_bytes.len()];
    }
}
