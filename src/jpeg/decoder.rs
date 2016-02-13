extern crate jpeg_decoder;

use std::io::Read;

use color::{self, ColorType};
use image::{DecodingResult, ImageDecoder, ImageError, ImageResult};

/// JPEG decoder
pub struct JPEGDecoder<R> {
    decoder: jpeg_decoder::Decoder<R>,
    metadata: Option<jpeg_decoder::ImageInfo>,
}

impl<R: Read> JPEGDecoder<R> {
    /// Create a new decoder that decodes from the stream ```r```
    pub fn new(r: R) -> JPEGDecoder<R> {
        JPEGDecoder {
            decoder: jpeg_decoder::Decoder::new(r),
            metadata: None,
        }
    }

    fn metadata(&mut self) -> ImageResult<jpeg_decoder::ImageInfo> {
        match self.metadata {
            Some(metadata) => Ok(metadata),
            None => {
                try!(self.decoder.read_info());
                let mut metadata = self.decoder.info().unwrap();

                // We convert CMYK data to RGB before returning it to the user.
                if metadata.pixel_format == jpeg_decoder::PixelFormat::CMYK32 {
                    metadata.pixel_format = jpeg_decoder::PixelFormat::RGB24;
                }

                self.metadata = Some(metadata);
                Ok(metadata)
            },
        }
    }
}

impl<R: Read> ImageDecoder for JPEGDecoder<R> {
    fn dimensions(&mut self) -> ImageResult<(u32, u32)> {
        let metadata = try!(self.metadata());
        Ok((metadata.width as u32, metadata.height as u32))
    }

    fn colortype(&mut self) -> ImageResult<ColorType> {
        let metadata = try!(self.metadata());
        Ok(metadata.pixel_format.into())
    }

    fn row_len(&mut self) -> ImageResult<usize> {
        let metadata = try!(self.metadata());
        Ok(metadata.width as usize * color::num_components(metadata.pixel_format.into()))
    }

    fn read_scanline(&mut self, _buf: &mut [u8]) -> ImageResult<u32> {
        unimplemented!();
    }

    fn read_image(&mut self) -> ImageResult<DecodingResult> {
        let mut data = try!(self.decoder.decode());
        data = match self.decoder.info().unwrap().pixel_format {
            jpeg_decoder::PixelFormat::CMYK32 => cmyk_to_rgb(&data),
            _ => data,
        };

        Ok(DecodingResult::U8(data))
    }
}

fn cmyk_to_rgb(input: &[u8]) -> Vec<u8> {
    let size = input.len() - input.len() / 4;
    let mut output = Vec::with_capacity(size);

    for pixel in input.chunks(4) {
        let c = pixel[0] as f32 / 255.0;
        let m = pixel[1] as f32 / 255.0;
        let y = pixel[2] as f32 / 255.0;
        let k = pixel[3] as f32 / 255.0;

        // CMYK -> CMY
        let c = c * (1.0 - k) + k;
        let m = m * (1.0 - k) + k;
        let y = y * (1.0 - k) + k;

        // CMY -> RGB
        let r = (1.0 - c) * 255.0;
        let g = (1.0 - m) * 255.0;
        let b = (1.0 - y) * 255.0;

        output.push(r as u8);
        output.push(g as u8);
        output.push(b as u8);
    }

    output
}

impl From<jpeg_decoder::PixelFormat> for ColorType {
    fn from(pixel_format: jpeg_decoder::PixelFormat) -> ColorType {
        use self::jpeg_decoder::PixelFormat::*;
        match pixel_format {
            L8     => ColorType::Gray(8),
            RGB24  => ColorType::RGB(8),
            CMYK32 => panic!(),
        }
    }
}

impl From<jpeg_decoder::Error> for ImageError {
    fn from(err: jpeg_decoder::Error) -> ImageError {
        use self::jpeg_decoder::Error::*;
        match err {
            Format(desc)      => ImageError::FormatError(desc),
            Unsupported(desc) => ImageError::UnsupportedError(format!("{:?}", desc)),
            Io(err)           => ImageError::IoError(err),
            Internal(err)     => ImageError::FormatError(err.description().to_owned()),
        }
    }
}
