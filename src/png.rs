//! Decoding and Encoding of PNG Images
//!
//! PNG (Portable Network Graphics) is an image format that supports lossless compression.
//!
//! # Related Links
//! * <http://www.w3.org/TR/PNG/> - The PNG Specification
//!

extern crate png;

use self::png::HasParameters;

use std;
use std::io::{self, Read, Write};

use color::ColorType;
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
    
        while self.index + 1 >= self.buffer.len() {
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
pub struct PNGDecoder<R: Read> {
    colortype: ColorType,
    reader: png::Reader<R>,
}

impl<R: Read> PNGDecoder<R> {
    /// Creates a new decoder that decodes from the stream ```r```
    pub fn new(r: R) -> ImageResult<PNGDecoder<R>> {
        let limits = png::Limits {
            pixels: std::u64::MAX,
        };
        let decoder = png::Decoder::new_with_limits(r, limits);
        let (_, mut reader) = decoder.read_info()?;
        let colortype = reader.output_color_type().into();

        Ok(PNGDecoder { colortype, reader })
    }
}

impl<'a, R: 'a + Read> ImageDecoder<'a> for PNGDecoder<R> {
    type Reader = PNGReader<R>;

    fn dimensions(&self) -> (u64, u64) {
        let (w, h) = self.reader.info().size();
        (w as u64, h as u64)
    }

    fn colortype(&self) -> ColorType {
        self.colortype
    }

    fn into_reader(self) -> ImageResult<Self::Reader> {
        PNGReader::new(self.reader)
    }

    fn read_image(mut self) -> ImageResult<Vec<u8>> {
        // This should be slightly faster than the default implementation
        let mut data = vec![0; self.reader.output_buffer_size()];
        self.reader.next_frame(&mut data)?;
        Ok(data)
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
        let (ct, bits) = color.into();
        let mut encoder = png::Encoder::new(self.w, width, height);
        encoder.set(ct).set(bits);
        let mut writer = try!(encoder.write_header());
        writer.write_image_data(data).map_err(|e| e.into())
    }
}

impl From<(png::ColorType, png::BitDepth)> for ColorType {
    fn from((ct, bits): (png::ColorType, png::BitDepth)) -> ColorType {
        use self::png::ColorType::*;
        match (ct, bits as u8) {
            (Grayscale, 1) => ColorType::L1,
            (Grayscale, 8) => ColorType::L8,
            (Grayscale, 16) => ColorType::L16,
            (Grayscale, n) => ColorType::Unknown(n),
            (GrayscaleAlpha, 8) => ColorType::LA,
            (GrayscaleAlpha, 16) => ColorType::LA16,
            (GrayscaleAlpha, n) => ColorType::Unknown(n*2),
            (RGB, 8) => ColorType::RGB,
            (RGB, 16) => ColorType::RGB16,
            (RGB, n) => ColorType::Unknown(n*3),
            (RGBA, 8) => ColorType::RGBA,
            (RGBA, 16) => ColorType::RGBA16,
            (RGBA, n) => ColorType::Unknown(n*4),
            (Indexed, bits) => ColorType::Unknown(bits),
        }
    }
}

impl From<ColorType> for (png::ColorType, png::BitDepth) {
    fn from(ct: ColorType) -> (png::ColorType, png::BitDepth) {
        use self::png::ColorType::*;
        let (ct, bits) = match ct {
            ColorType::L1 => (Grayscale, 1),
            ColorType::L8 => (Grayscale, 8),
            ColorType::L16 => (Grayscale, 16),
            ColorType::LA => (GrayscaleAlpha, 8),
            ColorType::LA16 => (GrayscaleAlpha, 16),
            ColorType::RGB => (RGB, 8),
            ColorType::RGB16 => (RGB, 16),
            ColorType::RGBA => (RGBA, 8),
            ColorType::RGBA16 => (RGBA, 16),
            ColorType::BGR => (RGB, 8),
            ColorType::BGRA => (RGBA, 8),
            ColorType::Unknown(_) => unimplemented!(),
            ColorType::__Nonexhaustive => unreachable!(),
        };
        (ct, png::BitDepth::from_u8(bits).unwrap())
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
        }
    }
}
