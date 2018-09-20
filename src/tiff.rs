//! Decoding and Encoding of TIFF Images
//!
//! TIFF (Tagged Image File Format) is a versatile image format that supports
//! lossless and lossy compression.
//!
//! # Related Links
//! * <http://partners.adobe.com/public/developer/tiff/index.html> - The TIFF specification

extern crate tiff;

use color::ColorType;
use image::{DecodingResult, ImageDecoder, ImageResult, ImageError};

use std::io::{Read, Seek};

/// Decoder for TIFF images.
pub struct TIFFDecoder<R>
    where R: Read + Seek
{
    inner: tiff::decoder::Decoder<R>
}

impl<R> TIFFDecoder<R>
    where R: Read + Seek
{
    /// Create a new TIFFDecoder.
    pub fn new(r: R) -> Result<TIFFDecoder<R>, ImageError> {
        Ok(TIFFDecoder { inner: tiff::decoder::Decoder::new(r)? })
    }
}

impl From<tiff::TiffError> for ImageError {
    fn from(err: tiff::TiffError) -> ImageError {
        match err {
            tiff::TiffError::IoError(err) => ImageError::IoError(err),
            tiff::TiffError::FormatError(desc) => ImageError::FormatError(desc.to_string()),
            tiff::TiffError::UnsupportedError(desc) => ImageError::UnsupportedError(desc.to_string()),
        }
    }
}

impl From<tiff::ColorType> for ColorType {
    fn from(ct: tiff::ColorType) -> ColorType {
        match ct {
            tiff::ColorType::Gray(depth) => ColorType::Gray(depth),
            tiff::ColorType::RGB(depth) => ColorType::RGB(depth),
            tiff::ColorType::Palette(depth) => ColorType::Palette(depth),
            tiff::ColorType::GrayA(depth) => ColorType::GrayA(depth),
            tiff::ColorType::RGBA(depth) => ColorType::RGBA(depth),
            tiff::ColorType::CMYK(_) => unimplemented!()
        }
    }
}

impl From<tiff::decoder::DecodingResult> for DecodingResult {
    fn from(res: tiff::decoder::DecodingResult) -> DecodingResult {
        match res {
            tiff::decoder::DecodingResult::U8(data) => DecodingResult::U8(data),
            tiff::decoder::DecodingResult::U16(data) => DecodingResult::U16(data),
        }
    }
}

impl<R: Read + Seek> ImageDecoder for TIFFDecoder<R> {
    fn dimensions(&mut self) -> ImageResult<(u32, u32)> {
        self.inner.dimensions().map_err(|e| e.into())
    }

    fn colortype(&mut self) -> ImageResult<ColorType> {
        Ok(self.inner.colortype()?.into())
    }

    fn row_len(&mut self) -> ImageResult<usize> {
        unimplemented!()
    }

    fn read_scanline(&mut self, _: &mut [u8]) -> ImageResult<u32> {
        unimplemented!()
    }

    fn read_image(&mut self) -> ImageResult<DecodingResult> {
        self.inner.read_image().map_err(|e| e.into()).map(|res| res.into())
    }
}
