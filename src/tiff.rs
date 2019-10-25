//! Decoding and Encoding of TIFF Images
//!
//! TIFF (Tagged Image File Format) is a versatile image format that supports
//! lossless and lossy compression.
//!
//! # Related Links
//! * <http://partners.adobe.com/public/developer/tiff/index.html> - The TIFF specification

extern crate tiff;

use std::convert::TryFrom;
use std::io::{self, Cursor, Read, Write, Seek};
use std::marker::PhantomData;
use std::mem;

use byteorder::{NativeEndian, ByteOrder};

use color::{ColorType, ExtendedColorType};
use image::{ImageDecoder, ImageResult, ImageError};
use utils::vec_u16_into_u8;

/// Decoder for TIFF images.
pub struct TiffDecoder<R>
    where R: Read + Seek
{
    dimensions: (u32, u32),
    color_type: ColorType,
    inner: tiff::decoder::Decoder<R>,
}

impl<R> TiffDecoder<R>
    where R: Read + Seek
{
    /// Create a new TiffDecoder.
    pub fn new(r: R) -> Result<TiffDecoder<R>, ImageError> {
        let mut inner = tiff::decoder::Decoder::new(r)?;
        let dimensions = inner.dimensions()?;
        let color_type = match inner.colortype()? {
            tiff::ColorType::Gray(8) => ColorType::L8,
            tiff::ColorType::Gray(16) => ColorType::L16,
            tiff::ColorType::GrayA(8) => ColorType::La8,
            tiff::ColorType::GrayA(16) => ColorType::La16,
            tiff::ColorType::RGB(8) => ColorType::Rgb8,
            tiff::ColorType::RGB(16) => ColorType::Rgb16,
            tiff::ColorType::RGBA(8) => ColorType::Rgba8,
            tiff::ColorType::RGBA(16) => ColorType::Rgba16,

            tiff::ColorType::Palette(n) | tiff::ColorType::Gray(n) =>
                return Err(ImageError::UnsupportedColor(ExtendedColorType::Unknown(n))),
            tiff::ColorType::GrayA(n) =>
                return Err(ImageError::UnsupportedColor(ExtendedColorType::Unknown(n*2))),
            tiff::ColorType::RGB(n) =>
                return Err(ImageError::UnsupportedColor(ExtendedColorType::Unknown(n*3))),
            tiff::ColorType::RGBA(n) | tiff::ColorType::CMYK(n) =>
                return Err(ImageError::UnsupportedColor(ExtendedColorType::Unknown(n*4))),
        };

        Ok(TiffDecoder {
            dimensions,
            color_type,
            inner,
        })
    }
}

impl From<tiff::TiffError> for ImageError {
    fn from(err: tiff::TiffError) -> ImageError {
        match err {
            tiff::TiffError::IoError(err) => ImageError::IoError(err),
            tiff::TiffError::FormatError(desc) => ImageError::FormatError(desc.to_string()),
            tiff::TiffError::UnsupportedError(desc) => ImageError::UnsupportedError(desc.to_string()),
            tiff::TiffError::LimitsExceeded => ImageError::InsufficientMemory,
        }
    }
}

/// Wrapper struct around a `Cursor<Vec<u8>>`
pub struct TiffReader<R>(Cursor<Vec<u8>>, PhantomData<R>);
impl<R> Read for TiffReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.0.read(buf)
    }
    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
        if self.0.position() == 0 && buf.is_empty() {
            mem::swap(buf, self.0.get_mut());
            Ok(buf.len())
        } else {
            self.0.read_to_end(buf)
        }
    }
}

impl<'a, R: 'a + Read + Seek> ImageDecoder<'a> for TiffDecoder<R> {
    type Reader = TiffReader<R>;

    fn dimensions(&self) -> (u32, u32) {
        self.dimensions
    }

    fn color_type(&self) -> ColorType {
        self.color_type
    }

    fn into_reader(mut self) -> ImageResult<Self::Reader> {
        let buf = match self.inner.read_image()? {
            tiff::decoder::DecodingResult::U8(v) => v,
            tiff::decoder::DecodingResult::U16(v) => vec_u16_into_u8(v),
        };

        Ok(TiffReader(Cursor::new(buf), PhantomData))
    }

    fn read_image(mut self, buf: &mut [u8]) -> ImageResult<()> {
        assert_eq!(u64::try_from(buf.len()), Ok(self.total_bytes()));
        match self.inner.read_image()? {
            tiff::decoder::DecodingResult::U8(v) => {
                buf.copy_from_slice(&v);
            }
            tiff::decoder::DecodingResult::U16(v) => {
                NativeEndian::write_u16_into(&v, buf);
            }
        }
        Ok(())
    }
}

/// Encoder for tiff images
pub struct TiffEncoder<W> {
    w: W,
}

impl<W: Write + Seek> TiffEncoder<W> {
    /// Create a new encoder that writes its output to `w`
    pub fn new(w: W) -> TiffEncoder<W> {
        TiffEncoder { w }
    }

    /// Encodes the image `image` that has dimensions `width` and `height` and `ColorType` `c`.
    ///
    /// 16-bit color types are not yet supported.
    pub fn encode(self, data: &[u8], width: u32, height: u32, color: ColorType) -> ImageResult<()> {
        // TODO: 16bit support
        let mut encoder = tiff::encoder::TiffEncoder::new(self.w)?;
        match color {
            ColorType::L8 => encoder.write_image::<tiff::encoder::colortype::Gray8>(width, height, data)?,
            ColorType::Rgb8 => encoder.write_image::<tiff::encoder::colortype::RGB8>(width, height, data)?,
            ColorType::Rgba8 => encoder.write_image::<tiff::encoder::colortype::RGBA8>(width, height, data)?,
            _ => return Err(ImageError::UnsupportedColor(color.into()))
        }

        Ok(())
    }
}
