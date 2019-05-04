//! Decoding of FLIF Images

extern crate flif;

use std::io::{Cursor, Read};

use color::ColorType;
use image::{ImageDecoder, ImageResult, ImageError};

/// Decoder for FLIF images.
pub struct FLIFDecoder<R>
    where R: Read
{
    dimensions: (u32, u32),
    colortype: ColorType,
    inner: flif::Decoder<R>,
}

impl<R> FLIFDecoder<R>
    where R: Read 
{
    /// Create a new FLIFDecoder.
    pub fn new(r: R) -> ImageResult<FLIFDecoder<R>> {
        let inner = flif::Decoder::new(r)?;
        let width = inner.info().header.width;
        let height = inner.info().header.height;

        let bits = match inner.info().header.bytes_per_channel {
            flif::components::BytesPerChannel::Custom =>
                return Err(ImageError::UnsupportedError("Custom number of bits per channel".to_owned())),
            flif::components::BytesPerChannel::One => 8,
            flif::components::BytesPerChannel::Two => 16,
        };

        let colortype = match inner.info().header.channels {
            flif::components::ColorSpace::Monochrome => ColorType::Gray(bits),
            flif::components::ColorSpace::RGB => ColorType::RGB(bits),
            flif::components::ColorSpace::RGBA => ColorType::RGBA(bits),
        };

        Ok(FLIFDecoder {
            dimensions: (width, height),
            colortype,
            inner,
        })
    }
}

impl From<flif::Error> for ImageError {
    fn from(err: flif::Error) -> ImageError {
        match err {
            flif::Error::Io(err) => ImageError::IoError(err),
            flif::Error::Unimplemented(..) => ImageError::UnsupportedError(err.to_string()),
            flif::Error::UnimplementedTransformation(..) => ImageError::UnsupportedError(err.to_string()),
            flif::Error::LimitViolation(..) => ImageError::FormatError(err.to_string()), // FIXME: should have a better error type
            err => ImageError::FormatError(err.to_string()),
        }
    }
}

impl<R: Read> ImageDecoder for FLIFDecoder<R> {
    type Reader = Cursor<Vec<u8>>;

    fn dimensions(&self) -> (u64, u64) {
        (self.dimensions.0 as u64, self.dimensions.1 as u64)
    }

    fn colortype(&self) -> ColorType {
        self.colortype
    }

    fn into_reader(self) -> ImageResult<Self::Reader> {
        Ok(Cursor::new(self.read_image()?))
    }

    fn read_image(self) -> ImageResult<Vec<u8>> {
        Ok(self.inner.decode_image()?
            .into_raw()
            .into_vec())
    }
}
