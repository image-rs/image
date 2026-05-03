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
//! | (2222) | 4⋅16-Bit BE unsigned integers (RGBA) / pixel, row-major |
//!
//! The RGB-data should be sRGB for best interoperability and not alpha-premultiplied.
//!
//! # Related Links
//! * <https://tools.suckless.org/farbfeld/> - the farbfeld specification

use std::io::{self, Read, Write};

use crate::color::ExtendedColorType;
use crate::error::{
    DecodingError, ImageError, ImageResult, UnsupportedError, UnsupportedErrorKind,
};
use crate::{ColorType, ImageDecoder, ImageEncoder, ImageFormat};

const MAGIC: &[u8] = b"farbfeld";
fn parse_header(r: &mut dyn Read) -> ImageResult<(u32, u32)> {
    let mut header = [0_u8; 16];
    r.read_exact(&mut header)?;

    let magic = &header[..8];
    if magic != MAGIC {
        return Err(ImageError::Decoding(DecodingError::new(
            ImageFormat::Farbfeld.into(),
            format!("Invalid magic: {magic:02x?}"),
        )));
    }

    let width = u32::from_be_bytes(header[8..12].try_into().unwrap());
    let height = u32::from_be_bytes(header[12..16].try_into().unwrap());

    if crate::utils::check_dimension_overflow(
        width, height, 8, // ExtendedColorType is always rgba16
    ) {
        return Err(ImageError::Unsupported(
            UnsupportedError::from_format_and_kind(
                ImageFormat::Farbfeld.into(),
                UnsupportedErrorKind::GenericFeature(format!(
                    "Image dimensions ({width}x{height}) are too large"
                )),
            ),
        ));
    }

    Ok((width, height))
}

fn u16_swap_be_ne(data: &mut [u8]) {
    #[cfg(target_endian = "little")]
    {
        for [low, high] in data.as_chunks_mut::<2>().0 {
            std::mem::swap(low, high);
        }
    }
}

/// farbfeld decoder
pub struct FarbfeldDecoder<R: Read> {
    width: u32,
    height: u32,
    reader: R,
}

impl<R: Read> FarbfeldDecoder<R> {
    /// Creates a new decoder that decodes from the stream ```r```
    pub fn new(mut r: R) -> ImageResult<Self> {
        let (width, height) = parse_header(&mut r)?;

        Ok(FarbfeldDecoder {
            width,
            height,
            reader: r,
        })
    }
}

impl<R: Read> ImageDecoder for FarbfeldDecoder<R> {
    fn dimensions(&self) -> (u32, u32) {
        (self.width, self.height)
    }

    fn color_type(&self) -> ColorType {
        ColorType::Rgba16
    }

    fn read_image(mut self, buf: &mut [u8]) -> ImageResult<()> {
        assert_eq!(u64::try_from(buf.len()), Ok(self.total_bytes()));
        self.reader.read_exact(buf)?;
        u16_swap_be_ne(buf);
        Ok(())
    }

    fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()> {
        (*self).read_image(buf)
    }
}

/// farbfeld encoder
pub struct FarbfeldEncoder<W: Write> {
    w: W,
}

impl<W: Write> FarbfeldEncoder<W> {
    /// Create a new encoder that writes its output to ```w```.
    pub fn new(w: W) -> FarbfeldEncoder<W> {
        FarbfeldEncoder { w }
    }

    /// Encodes the image `data` (native endian) that has dimensions `width` and `height`.
    ///
    /// # Panics
    ///
    /// Panics if `width * height * 8 != data.len()`.
    #[track_caller]
    pub fn encode(self, data: &[u8], width: u32, height: u32) -> ImageResult<()> {
        let expected_buffer_len = (u64::from(width) * u64::from(height)).saturating_mul(8);
        assert_eq!(
            expected_buffer_len,
            data.len() as u64,
            "Invalid buffer length: expected {expected_buffer_len} got {} for {width}x{height} image",
            data.len(),
        );
        self.encode_impl(data, width, height)?;
        Ok(())
    }

    fn encode_impl(mut self, data: &[u8], width: u32, height: u32) -> io::Result<()> {
        self.w.write_all(MAGIC)?;

        self.w.write_all(&width.to_be_bytes())?;
        self.w.write_all(&height.to_be_bytes())?;

        let mut buf = [0_u8; 4096];
        for chunk in data.chunks(buf.len()) {
            let buf = &mut buf[..chunk.len()];
            buf.copy_from_slice(chunk);
            u16_swap_be_ne(buf);
            self.w.write_all(buf)?;
        }

        Ok(())
    }
}

impl<W: Write> ImageEncoder for FarbfeldEncoder<W> {
    #[track_caller]
    fn write_image(
        self,
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ExtendedColorType,
    ) -> ImageResult<()> {
        if color_type != ExtendedColorType::Rgba16 {
            return Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Farbfeld.into(),
                    UnsupportedErrorKind::Color(color_type),
                ),
            ));
        }

        self.encode(buf, width, height)
    }
}

#[cfg(test)]
mod tests {
    use crate::codecs::farbfeld::FarbfeldDecoder;
    use std::io::Cursor;

    #[test]
    fn dimension_overflow() {
        let header = b"farbfeld\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF";

        assert!(FarbfeldDecoder::new(Cursor::new(header)).is_err());
    }
}
