//! Decoding and encoding of QOI images

use crate::{
    error::{DecodingError, EncodingError},
    ColorType, ImageDecoder, ImageEncoder, ImageError, ImageFormat, ImageResult,
};
use std::io::{Cursor, Read, Write};

/// QOI decoder
pub struct QoiDecoder<R> {
    decoder: qoi::Decoder<R>,
}

impl<R> QoiDecoder<R>
where
    R: Read,
{
    /// Creates a new decoder that decodes from the stream ```reader```
    pub fn new(reader: R) -> ImageResult<Self> {
        let decoder = qoi::Decoder::from_stream(reader).map_err(decoding_error)?;
        Ok(Self { decoder })
    }
}

impl<'a, R: Read + 'a> ImageDecoder<'a> for QoiDecoder<R> {
    type Reader = Cursor<Vec<u8>>;

    fn dimensions(&self) -> (u32, u32) {
        (self.decoder.header().width, self.decoder.header().height)
    }

    fn color_type(&self) -> ColorType {
        match self.decoder.header().channels {
            qoi::Channels::Rgb => ColorType::Rgb8,
            qoi::Channels::Rgba => ColorType::Rgba8,
        }
    }

    fn into_reader(mut self) -> ImageResult<Self::Reader> {
        let buffer = self.decoder.decode_to_vec().map_err(decoding_error)?;
        Ok(Cursor::new(buffer))
    }
}

fn decoding_error(error: qoi::Error) -> ImageError {
    ImageError::Decoding(DecodingError::new(ImageFormat::Qoi.into(), error))
}

fn encoding_error(error: qoi::Error) -> ImageError {
    ImageError::Encoding(EncodingError::new(ImageFormat::Qoi.into(), error))
}

/// QOI encoder
pub struct QoiEncoder<W> {
    writer: W,
}

impl<W: Write> QoiEncoder<W> {
    /// Creates a new encoder that writes its output to ```writer```
    pub fn new(writer: W) -> Self {
        Self { writer }
    }
}

impl<W: Write> ImageEncoder for QoiEncoder<W> {
    fn write_image(
        mut self,
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ColorType,
    ) -> ImageResult<()> {
        if !matches!(color_type, ColorType::Rgba8 | ColorType::Rgb8) {
            return Err(ImageError::Encoding(EncodingError::new(
                ImageFormat::Qoi.into(),
                format!("unsupported color type {color_type:?}. Supported are Rgba8 and Rgb8."),
            )));
        }

        // Encode data in QOI
        let data = qoi::encode_to_vec(buf, width, height).map_err(encoding_error)?;

        // Write data to buffer
        self.writer.write_all(&data[..])?;
        self.writer.flush()?;

        Ok(())
    }
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
