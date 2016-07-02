
//! Decoding and Encoding of PNG Images
//!
//! PNG (Portable Network Graphics) is an image format that supports lossless compression.
//!
//! # Related Links
//! * http://www.w3.org/TR/PNG/ - The PNG Specification
//!

extern crate png;

use self::png::HasParameters;

use std::io::{self, Read, Write};

use image::{ImageError, ImageResult, DecodingResult, ImageDecoder};
use color::ColorType;

enum Either<T, U> {
    Left(T),
    Right(U)
}

/// PNG decoder
pub struct PNGDecoder<R: Read> {
    inner: Option<Either<png::Decoder<R>, png::Reader<R>>>
}

impl<R: Read> PNGDecoder<R> {
    /// Creates a new decoder that decodes from the stream ```r```
    pub fn new(r: R) -> PNGDecoder<R> {
        PNGDecoder {
            inner: Some(Either::Left(png::Decoder::new(r)))
        }
    }

    // Converts the inner decoder to a reader
    fn get_reader(&mut self) -> Result<&mut png::Reader<R>, png::DecodingError> {
        let inner = self.inner.take().unwrap();
        self.inner = Some(match inner {
            Either::Left(decoder) => {
                let (_, reader) = try!(decoder.read_info());
                Either::Right(reader)
            },
            Either::Right(reader) => Either::Right(reader)
        });
        match self.inner {
            Some(Either::Right(ref mut reader)) => Ok(reader),
            _ => unreachable!()
        }
    }
}

impl<R: Read> ImageDecoder for PNGDecoder<R> {
    fn dimensions(&mut self) -> ImageResult<(u32, u32)> {
        let reader = try!(self.get_reader());
        Ok(reader.info().size())
    }

    fn colortype(&mut self) -> ImageResult<ColorType> {
        let reader = try!(self.get_reader());
        Ok(reader.output_color_type().into())
    }

    fn row_len(&mut self) -> ImageResult<usize> {
        let reader = try!(self.get_reader());
        let width = reader.info().width;
        Ok(reader.output_line_size(width))
    }

    fn read_scanline(&mut self, buf: &mut [u8]) -> ImageResult<u32> {
        match try!(try!(self.get_reader()).next_row()) {
            Some(line) => {
                ::copy_memory(line, &mut buf[..line.len()]);
                Ok(line.len() as u32)
            },
            None => Err(ImageError::ImageEnd)
        }
    }

    fn read_image(&mut self) -> ImageResult<DecodingResult> {
        let reader = try!(self.get_reader());
        let mut data = vec![0; reader.output_buffer_size()];
        try!(reader.next_frame(&mut data));
        Ok(DecodingResult::U8(data))
    }
}

/// PNG encoder
pub struct PNGEncoder<W: Write> {
    w: W
}

impl<W: Write> PNGEncoder<W> {
    /// Create a new encoder that writes its output to ```w```
    pub fn new(w: W) -> PNGEncoder<W> {
        PNGEncoder {
            w: w
        }
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
        let bits = bits as u8;
        match ct {
            Grayscale => ColorType::Gray(bits),
            RGB => ColorType::RGB(bits),
            Indexed => ColorType::Palette(bits),
            GrayscaleAlpha => ColorType::GrayA(bits),
            RGBA => ColorType::RGBA(bits)
        }
    }
}

impl From<ColorType> for (png::ColorType, png::BitDepth) {
    fn from(ct: ColorType) -> (png::ColorType, png::BitDepth) {
        use self::png::ColorType::*;
        let (ct, bits) = match ct {
            ColorType::Gray(bits) => (Grayscale, bits),
            ColorType::RGB(bits) => (RGB, bits),
            ColorType::Palette(bits) => (Indexed, bits),
            ColorType::GrayA(bits) => (GrayscaleAlpha, bits),
            ColorType::RGBA(bits) => (RGBA, bits),
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
            CorruptFlateStream => ImageError::FormatError("compressed data stream corrupted".into())
        }
    }
}