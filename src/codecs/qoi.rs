//! Decoding and encoding of QOI images

use alloc::boxed::Box;
use alloc::format;
use alloc::vec::Vec;

use crate::error::{DecodingError, EncodingError};
use crate::{
    ColorType, ExtendedColorType, ImageDecoder, ImageEncoder, ImageError, ImageFormat, ImageResult,
};

#[cfg(feature = "std")]
use std::io::{Read, Write};

/// QOI decoder
pub struct QoiDecoder<R> {
    inner: R,
}

impl<R> QoiDecoder<(R, qoi::Header)>
where
    R: AsRef<[u8]>,
{
    /// Creates a new decoder that decodes from the slice reference ```bytes```
    pub fn from_bytes(bytes: R) -> ImageResult<Self> {
        let header = qoi::decode_header(&bytes).map_err(decoding_error)?;
        let inner = (bytes, header);

        Ok(Self { inner })
    }
}

impl<R: AsRef<[u8]>> ImageDecoder for QoiDecoder<(R, qoi::Header)> {
    fn dimensions(&self) -> (u32, u32) {
        (self.inner.1.width, self.inner.1.height)
    }

    fn color_type(&self) -> ColorType {
        match self.inner.1.channels {
            qoi::Channels::Rgb => ColorType::Rgb8,
            qoi::Channels::Rgba => ColorType::Rgba8,
        }
    }

    fn read_image(self, buf: &mut [u8]) -> ImageResult<()> {
        qoi::decode_to_buf(buf, &self.inner.0).map_err(decoding_error)?;
        Ok(())
    }

    fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()> {
        (*self).read_image(buf)
    }
}

#[cfg(feature = "std")]
impl<R> QoiDecoder<qoi::Decoder<R>>
where
    R: Read,
{
    /// Creates a new decoder that decodes from the stream ```reader```
    pub fn new(reader: R) -> ImageResult<Self> {
        let inner = qoi::Decoder::from_stream(reader).map_err(decoding_error)?;

        Ok(Self { inner })
    }
}

#[cfg(feature = "std")]
impl<R: Read> ImageDecoder for QoiDecoder<qoi::Decoder<R>> {
    fn dimensions(&self) -> (u32, u32) {
        (self.inner.header().width, self.inner.header().height)
    }

    fn color_type(&self) -> ColorType {
        match self.inner.header().channels {
            qoi::Channels::Rgb => ColorType::Rgb8,
            qoi::Channels::Rgba => ColorType::Rgba8,
        }
    }

    fn read_image(mut self, buf: &mut [u8]) -> ImageResult<()> {
        self.inner.decode_to_buf(buf).map_err(decoding_error)?;
        Ok(())
    }

    fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()> {
        (*self).read_image(buf)
    }
}

/// Wrapper to implement [`Error`](core::error::Error) for [`qoi::Error`].
#[repr(transparent)]
struct QoiError(qoi::Error);

impl core::fmt::Debug for QoiError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        <qoi::Error as core::fmt::Debug>::fmt(&self.0, f)
    }
}

impl core::fmt::Display for QoiError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        <qoi::Error as core::fmt::Display>::fmt(&self.0, f)
    }
}

impl core::error::Error for QoiError {}

fn decoding_error(error: qoi::Error) -> ImageError {
    ImageError::Decoding(DecodingError::new(ImageFormat::Qoi.into(), QoiError(error)))
}

fn encoding_error(error: qoi::Error) -> ImageError {
    ImageError::Encoding(EncodingError::new(ImageFormat::Qoi.into(), QoiError(error)))
}

/// QOI encoder
pub struct QoiEncoder<W> {
    writer: W,
}

impl<W> QoiEncoder<W> {
    /// Creates a new encoder that writes its output to ```writer```
    pub fn new(writer: W) -> Self {
        Self { writer }
    }
}

#[cfg(feature = "std")]
impl<W: Write> ImageEncoder for QoiEncoder<W> {
    #[track_caller]
    fn write_image(
        mut self,
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ExtendedColorType,
    ) -> ImageResult<()> {
        let data = write_image_to_vec(buf, width, height, color_type)?;

        // Write data to buffer
        self.writer.write_all(&data[..])?;
        self.writer.flush()?;

        Ok(())
    }
}

// Note that ImageEncoder should only be implemented for QoiEncoder<W> where W would implement
// std::io::Write with the std feature enabled.
#[cfg(not(feature = "std"))]
impl ImageEncoder for QoiEncoder<Vec<u8>> {
    #[track_caller]
    fn write_image(
        mut self,
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ExtendedColorType,
    ) -> ImageResult<()> {
        let mut data = write_image_to_vec(buf, width, height, color_type)?;

        // Write data to buffer
        self.writer.append(&mut data);

        Ok(())
    }
}

#[track_caller]
fn write_image_to_vec(
    buf: &[u8],
    width: u32,
    height: u32,
    color_type: ExtendedColorType,
) -> ImageResult<Vec<u8>> {
    if !matches!(
        color_type,
        ExtendedColorType::Rgba8 | ExtendedColorType::Rgb8
    ) {
        return Err(ImageError::Encoding(EncodingError::new(
            ImageFormat::Qoi.into(),
            format!("unsupported color type {color_type:?}. Supported are Rgba8 and Rgb8."),
        )));
    }

    let expected_buffer_len = color_type.buffer_size(width, height);
    assert_eq!(
        expected_buffer_len,
        buf.len() as u64,
        "Invalid buffer length: expected {expected_buffer_len} got {} for {width}x{height} image",
        buf.len(),
    );

    // Encode data in QOI
    let data = qoi::encode_to_vec(buf, width, height).map_err(encoding_error)?;

    Ok(data)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::File;

    #[test]
    fn decode_test_image() {
        let decoder = QoiDecoder::new(File::open("tests/images/qoi/basic-test.qoi").unwrap())
            .expect("Unable to read QOI file");

        assert_eq!((5, 5), decoder.dimensions());
        assert_eq!(ColorType::Rgba8, decoder.color_type());
    }
}
