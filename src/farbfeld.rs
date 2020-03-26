//! Decoding of farbfeld images
//!
//! farbfeld is a lossless image format which is easy to parse, pipe and compress.
//!
//! It has the following format:
//!
//! | Bytes  | Description                                             |
//! |--------|---------------------------------------------------------|
//! | 8      | "farbfeld" magic value                                  |
//! | 4      | 32-Bit BE unsigned integer (width)                      |
//! | 4      | 32-Bit BE unsigned integer (height)                     |
//! | [2222] | 4⋅16-Bit BE unsigned integers [RGBA] / pixel, row-major |
//!
//! The RGB-data should be sRGB for best interoperability and not alpha-premultiplied.
//!
//! # Related Links
//! * <https://tools.suckless.org/farbfeld/> - the farbfeld specification

use std::io::{self, Read, Write, BufReader, BufWriter};

use byteorder::{BigEndian, ByteOrder, NativeEndian};

use crate::color::ColorType;
use crate::error::{EncodingError, DecodingError, ImageError, ImageResult};
use crate::image::{ImageDecoder, ImageEncoder, ImageFormat};

/// farbfeld Reader
pub struct FarbfeldReader<R: Read> {
    width: u32,
    height: u32,
    inner: BufReader<R>,
    cached_byte: Option<u8>,
}

impl<R: Read> FarbfeldReader<R> {
    fn new(reader: R) -> ImageResult<FarbfeldReader<R>> {
        fn read_dimm<R: Read>(from: &mut R) -> ImageResult<u32> {
            let mut buf = [0u8; 4];
            from.read_exact(&mut buf).map_err(|err|
                ImageError::Decoding(DecodingError::new(
                    ImageFormat::Farbfeld.into(),
                    err,
                )))?;
            Ok(BigEndian::read_u32(&buf))
        }

        let mut inner = BufReader::new(reader);

        let mut magic = [0u8; 8];
        inner.read_exact(&mut magic).map_err(|err|
            ImageError::Decoding(DecodingError::new(
                ImageFormat::Farbfeld.into(),
                err,
            )))?;
        if &magic != b"farbfeld" {
            return Err(ImageError::Decoding(DecodingError::new(
                ImageFormat::Farbfeld.into(),
                format!("Invalid magic: {:02x?}", magic),
            )));
        }

        Ok(FarbfeldReader {
            width: read_dimm(&mut inner)?,
            height: read_dimm(&mut inner)?,
            inner,
            cached_byte: None,
        })
    }
}

impl<R: Read> Read for FarbfeldReader<R> {
    fn read(&mut self, mut buf: &mut [u8]) -> io::Result<usize> {
        fn consume_channel<R: Read>(from: &mut R, to: &mut [u8]) -> io::Result<()> {
            let mut ibuf = [0u8; 2];
            from.read_exact(&mut ibuf)?;
            NativeEndian::write_u16(to, BigEndian::read_u16(&ibuf));
            Ok(())
        }

        let mut bytes_written = 0;
        if let Some(byte) = self.cached_byte.take() {
            buf[0] = byte;
            buf = &mut buf[1..];
            bytes_written = 1;
        }

        if buf.len() == 1 {
            let mut obuf = [0u8; 2];
            consume_channel(&mut self.inner, &mut obuf)?;
            buf[0] = obuf[0];
            self.cached_byte = Some(obuf[1]);
            bytes_written += 1;
        } else {
            for channel_out in buf.chunks_exact_mut(2) {
                consume_channel(&mut self.inner, channel_out)?;
                bytes_written += 2;
            }
        }

        Ok(bytes_written)
    }
}

/// farbfeld decoder
pub struct FarbfeldDecoder<R: Read> {
    reader: FarbfeldReader<R>,
}

impl<R: Read> FarbfeldDecoder<R> {
    /// Creates a new decoder that decodes from the stream ```r```
    pub fn new(r: R) -> ImageResult<FarbfeldDecoder<R>> {
        Ok(FarbfeldDecoder { reader: FarbfeldReader::new(r)? })
    }
}

impl<'a, R: 'a + Read> ImageDecoder<'a> for FarbfeldDecoder<R> {
    type Reader = FarbfeldReader<R>;

    fn dimensions(&self) -> (u32, u32) {
        (self.reader.width, self.reader.height)
    }

    fn color_type(&self) -> ColorType {
        ColorType::Rgba16
    }

    fn into_reader(self) -> ImageResult<Self::Reader> {
        Ok(self.reader)
    }

    fn scanline_bytes(&self) -> u64 {
        2
    }
}

/// farbfeld encoder
pub struct FarbfeldEncoder<W: Write> {
    w: BufWriter<W>,
}

impl<W: Write> FarbfeldEncoder<W> {
    /// Create a new encoder that writes its output to ```w```
    pub fn new(w: W) -> FarbfeldEncoder<W> {
        FarbfeldEncoder { w: BufWriter::new(w) }
    }

    /// Encodes the image ```data``` (native endian)
    /// that has dimensions ```width``` and ```height```
    pub fn encode(self, data: &[u8], width: u32, height: u32) -> ImageResult<()> {
        self.encode_impl(data, width, height).map_err(|err|
            ImageError::Encoding(EncodingError::new(
                ImageFormat::Farbfeld.into(),
                err,
            )))
    }

    fn encode_impl(mut self, data: &[u8], width: u32, height: u32) -> io::Result<()> {
        self.w.write_all(b"farbfeld")?;

        let mut buf = [0u8; 4];
        BigEndian::write_u32(&mut buf, width);
        self.w.write_all(&buf)?;

        BigEndian::write_u32(&mut buf, height);
        self.w.write_all(&buf)?;

        for channel in data.chunks_exact(2) {
            BigEndian::write_u16(&mut buf, NativeEndian::read_u16(channel));
            self.w.write_all(&buf[..2])?;
        }

        Ok(())
    }
}

impl<W: Write> ImageEncoder for FarbfeldEncoder<W> {
    fn write_image(
        self,
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ColorType,
    ) -> ImageResult<()> {
        if color_type != ColorType::Rgba16 {
            return Err(ImageError::UnsupportedColor(color_type.into()));
        }

        self.encode(buf, width, height)
    }
}
