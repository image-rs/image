//!  Decoding of GIF Images
//!
//!  GIF (Graphics Interchange Format) is an image format that supports lossless compression.
//!
//!  # Related Links
//!  * http://www.w3.org/Graphics/GIF/spec-gif89a.txt - The GIF Specification
//!
extern crate gif;

use std::io::{Read, Write};

pub use self::gif::Frame;
use self::gif::{SetParameter, ColorOutput};

use image::{ImageError, ImageResult, DecodingResult, ImageDecoder};
use color;

enum Either<T, U> {
    Left(T),
    Right(U)
}

/// GIF decoder
pub struct Decoder<R: Read> {
    inner: Option<Either<gif::Decoder<R>, gif::Reader<R>>>
}

impl<R: Read> Decoder<R> {
    /// Creates a new decoder that decodes the input steam ```r```
    pub fn new(r: R) -> Decoder<R> {
        let mut decoder = gif::Decoder::new(r);
        decoder.set(ColorOutput::RGBA);
        Decoder {
            inner: Some(Either::Left(decoder))
        }
    }

    // Converts the inner decoder to a reader
    fn get_reader(&mut self) -> Result<&mut gif::Reader<R>, gif::DecodingError> {
        let inner = self.inner.take().unwrap();
        self.inner = Some(match inner {
            Either::Left(decoder) => {
                let reader = try!(decoder.read_info());
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


impl<R: Read> ImageDecoder for Decoder<R> {
    fn dimensions(&mut self) -> ImageResult<(u32, u32)> {
        let reader = try!(self.get_reader());
        Ok((reader.width() as u32, reader.height() as u32))
    }

    fn colortype(&mut self) -> ImageResult<color::ColorType> {
        Ok(color::ColorType::RGBA(8))
    }

    fn row_len(&mut self) -> ImageResult<usize> {
        let reader = try!(self.get_reader());
        Ok(reader.line_length())
    }

    fn read_scanline(&mut self, buf: &mut [u8]) -> ImageResult<u32> {
        let reader = try!(self.get_reader());
        let len = reader.line_length();
        try!(reader.fill_buffer(&mut buf[..len]));
        Ok(len as u32)
    }

    fn read_image(&mut self) -> ImageResult<DecodingResult> {
        let reader = try!(self.get_reader());
        if let Some(_) = try!(reader.next_frame_info()) {
            let mut buf = vec![0; reader.buffer_size()];
            try!(reader.read_into_buffer(&mut buf));
            Ok(DecodingResult::U8(buf))
        } else {
            Err(ImageError::ImageEnd)
        }
    }
}

/// GIF encoder.
pub struct Encoder<W: Write> {
    w: W,
}

impl<W: Write> Encoder<W> {
    /// Creates a new GIF encoder.
    pub fn new(w: W) -> Encoder<W> {
        Encoder {
            w: w
        }
    }
    /// Encodes a frame.
    pub fn encode(self, frame: Frame) -> ImageResult<()> {
        let mut encoder = try!(
            gif::Encoder::new(self.w, frame.width, frame.height, &[])
        );
        encoder.write_frame(&frame).map_err(|err| err.into())
    }
}

impl From<gif::DecodingError> for ImageError {
    fn from(err: gif::DecodingError) -> ImageError {
        use self::gif::DecodingError::*;
        match err {
            Format(desc) => ImageError::FormatError(desc.into()),
            Internal(desc) => ImageError::FormatError(desc.into()),
            Io(io_err) => ImageError::IoError(io_err),
        }
    }
}
