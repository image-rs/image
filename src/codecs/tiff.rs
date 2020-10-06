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

use crate::color::{ColorType, ExtendedColorType};
use crate::error::{
    DecodingError, EncodingError, ImageError, ImageResult, LimitError, LimitErrorKind,
    ParameterError, ParameterErrorKind, UnsupportedError, UnsupportedErrorKind,
};
use crate::image::{ImageDecoder, ImageEncoder, ImageFormat};
use crate::utils;

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
        let mut inner = tiff::decoder::Decoder::new(r).map_err(ImageError::from_tiff_decode)?;
        let dimensions = inner.dimensions().map_err(ImageError::from_tiff_decode)?;
        let color_type = match inner.colortype().map_err(ImageError::from_tiff_decode)? {
            tiff::ColorType::Gray(8) => ColorType::L8,
            tiff::ColorType::Gray(16) => ColorType::L16,
            tiff::ColorType::GrayA(8) => ColorType::La8,
            tiff::ColorType::GrayA(16) => ColorType::La16,
            tiff::ColorType::RGB(8) => ColorType::Rgb8,
            tiff::ColorType::RGB(16) => ColorType::Rgb16,
            tiff::ColorType::RGBA(8) => ColorType::Rgba8,
            tiff::ColorType::RGBA(16) => ColorType::Rgba16,

            tiff::ColorType::Palette(n) | tiff::ColorType::Gray(n) => {
                return Err(err_unknown_color_type(n))
            }
            tiff::ColorType::GrayA(n) => return Err(err_unknown_color_type(n * 2)),
            tiff::ColorType::RGB(n) => return Err(err_unknown_color_type(n * 3)),
            tiff::ColorType::RGBA(n) | tiff::ColorType::CMYK(n) => {
                return Err(err_unknown_color_type(n * 4))
            }
        };

        Ok(TiffDecoder {
            dimensions,
            color_type,
            inner,
        })
    }
}

fn err_unknown_color_type(value: u8) -> ImageError {
    ImageError::Unsupported(UnsupportedError::from_format_and_kind(
        ImageFormat::Tiff.into(),
        UnsupportedErrorKind::Color(ExtendedColorType::Unknown(value)),
    ))
}

impl ImageError {
    fn from_tiff_decode(err: tiff::TiffError) -> ImageError {
        match err {
            tiff::TiffError::IoError(err) => ImageError::IoError(err),
            err @ tiff::TiffError::FormatError(_) | err @ tiff::TiffError::IntSizeError => {
                ImageError::Decoding(DecodingError::new(ImageFormat::Tiff.into(), err))
            }
            tiff::TiffError::UnsupportedError(desc) => {
                ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                    ImageFormat::Tiff.into(),
                    UnsupportedErrorKind::GenericFeature(desc.to_string()),
                ))
            }
            tiff::TiffError::LimitsExceeded => {
                ImageError::Limits(LimitError::from_kind(LimitErrorKind::InsufficientMemory))
            }
        }
    }

    fn from_tiff_encode(err: tiff::TiffError) -> ImageError {
        match err {
            tiff::TiffError::IoError(err) => ImageError::IoError(err),
            err @ tiff::TiffError::FormatError(_) | err @ tiff::TiffError::IntSizeError => {
                ImageError::Encoding(EncodingError::new(ImageFormat::Tiff.into(), err))
            }
            tiff::TiffError::UnsupportedError(desc) => {
                ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                    ImageFormat::Tiff.into(),
                    UnsupportedErrorKind::GenericFeature(desc.to_string()),
                ))
            }
            tiff::TiffError::LimitsExceeded => {
                ImageError::Limits(LimitError::from_kind(LimitErrorKind::InsufficientMemory))
            }
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
        let buf = match self
            .inner
            .read_image()
            .map_err(ImageError::from_tiff_decode)?
        {
            tiff::decoder::DecodingResult::U8(v) => v,
            tiff::decoder::DecodingResult::U16(v) => utils::vec_u16_into_u8(v),
            tiff::decoder::DecodingResult::U32(v) => utils::vec_u32_into_u8(v),
            tiff::decoder::DecodingResult::U64(v) => utils::vec_u64_into_u8(v),
        };

        Ok(TiffReader(Cursor::new(buf), PhantomData))
    }

    fn read_image(mut self, buf: &mut [u8]) -> ImageResult<()> {
        assert_eq!(u64::try_from(buf.len()), Ok(self.total_bytes()));
        match self
            .inner
            .read_image()
            .map_err(ImageError::from_tiff_decode)?
        {
            tiff::decoder::DecodingResult::U8(v) => {
                buf.copy_from_slice(&v);
            }
            tiff::decoder::DecodingResult::U16(v) => {
                buf.copy_from_slice(bytemuck::cast_slice(&v));
            }
            tiff::decoder::DecodingResult::U32(v) => {
                buf.copy_from_slice(bytemuck::cast_slice(&v));
            }
            tiff::decoder::DecodingResult::U64(v) => {
                buf.copy_from_slice(bytemuck::cast_slice(&v));
            }
        }
        Ok(())
    }
}

/// Encoder for tiff images
pub struct TiffEncoder<W> {
    w: W,
}

// Utility to simplify and deduplicate error handling during 16-bit encoding.
fn u8_slice_as_u16(buf: &[u8]) -> ImageResult<&[u16]> {
    bytemuck::try_cast_slice(buf).map_err(|err| {
        // If the buffer is not aligned or the correct length for a u16 slice, err.
        //
        // `bytemuck::PodCastError` of bytemuck-1.2.0 does not implement
        // `Error` and `Display` trait.
        // See <https://github.com/Lokathor/bytemuck/issues/22>.
        ImageError::Parameter(ParameterError::from_kind(ParameterErrorKind::Generic(
            format!("{:?}", err),
        )))
    })
}

impl<W: Write + Seek> TiffEncoder<W> {
    /// Create a new encoder that writes its output to `w`
    pub fn new(w: W) -> TiffEncoder<W> {
        TiffEncoder { w }
    }

    /// Encodes the image `image` that has dimensions `width` and `height` and `ColorType` `c`.
    ///
    /// 16-bit types assume the buffer is native endian.
    pub fn encode(self, data: &[u8], width: u32, height: u32, color: ColorType) -> ImageResult<()> {
        let mut encoder =
            tiff::encoder::TiffEncoder::new(self.w).map_err(ImageError::from_tiff_encode)?;
        match color {
            ColorType::L8 => encoder.write_image::<tiff::encoder::colortype::Gray8>(width, height, data),
            ColorType::Rgb8 => encoder.write_image::<tiff::encoder::colortype::RGB8>(width, height, data),
            ColorType::Rgba8 => encoder.write_image::<tiff::encoder::colortype::RGBA8>(width, height, data),
            ColorType::L16 => encoder.write_image::<tiff::encoder::colortype::Gray16>(width, height, &u8_slice_as_u16(data)?),
            ColorType::Rgb16 => encoder.write_image::<tiff::encoder::colortype::RGB16>(width, height, &u8_slice_as_u16(data)?),
            ColorType::Rgba16 => encoder.write_image::<tiff::encoder::colortype::RGBA16>(width, height, &u8_slice_as_u16(data)?),
            _ => {
                return Err(ImageError::Unsupported(
                    UnsupportedError::from_format_and_kind(
                        ImageFormat::Tiff.into(),
                        UnsupportedErrorKind::Color(color.into()),
                    ),
                ))
            }
        }
        .map_err(ImageError::from_tiff_encode)?;

        Ok(())
    }
}

impl<W: Write + Seek> ImageEncoder for TiffEncoder<W> {
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
