use std::convert::TryFrom;
use std::io::{self, Cursor, Read};
use std::marker::PhantomData;
use std::mem;

use crate::color::ColorType;
use crate::image::ImageDecoder;
use crate::error::{ImageError, ImageResult};

/// JPEG decoder
pub struct JpegDecoder<R> {
    decoder: jpeg::Decoder<R>,
    metadata: jpeg::ImageInfo,
}

impl<R: Read> JpegDecoder<R> {
    /// Create a new decoder that decodes from the stream ```r```
    pub fn new(r: R) -> ImageResult<JpegDecoder<R>> {
        let mut decoder = jpeg::Decoder::new(r);

        decoder.read_info().map_err(ImageError::from_jpeg)?;
        let mut metadata = decoder.info().unwrap();

        // We convert CMYK data to RGB before returning it to the user.
        if metadata.pixel_format == jpeg::PixelFormat::CMYK32 {
            metadata.pixel_format = jpeg::PixelFormat::RGB24;
        }

        Ok(JpegDecoder {
            decoder,
            metadata,
        })
    }
}

/// Wrapper struct around a `Cursor<Vec<u8>>`
pub struct JpegReader<R>(Cursor<Vec<u8>>, PhantomData<R>);
impl<R> Read for JpegReader<R> {
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

impl<'a, R: 'a + Read> ImageDecoder<'a> for JpegDecoder<R> {
    type Reader = JpegReader<R>;

    fn dimensions(&self) -> (u32, u32) {
        (u32::from(self.metadata.width), u32::from(self.metadata.height))
    }

    fn color_type(&self) -> ColorType {
        ColorType::from_jpeg(self.metadata.pixel_format)
    }

    fn into_reader(mut self) -> ImageResult<Self::Reader> {
        let mut data = self.decoder.decode().map_err(ImageError::from_jpeg)?;
        data = match self.decoder.info().unwrap().pixel_format {
            jpeg::PixelFormat::CMYK32 => cmyk_to_rgb(&data),
            _ => data,
        };

        Ok(JpegReader(Cursor::new(data), PhantomData))
    }

    fn read_image(mut self, buf: &mut [u8]) -> ImageResult<()> {
        assert_eq!(u64::try_from(buf.len()), Ok(self.total_bytes()));

        let mut data = self.decoder.decode().map_err(ImageError::from_jpeg)?;
        data = match self.decoder.info().unwrap().pixel_format {
            jpeg::PixelFormat::CMYK32 => cmyk_to_rgb(&data),
            _ => data,
        };

        buf.copy_from_slice(&data);
        Ok(())
    }
}

pub fn cmyk_to_rgb(input: &[u8]) -> Vec<u8> {
    let count = input.len()/4;
    let mut output = vec![0; 3*count];

    let in_pixels = input[..4*count].chunks_exact(4);
    let out_pixels = output[..3*count].chunks_exact_mut(3);

    let inv = 1.0/255.0;
    for (pixel, outp) in in_pixels.zip(out_pixels) {
        let c = 255 - u16::from(pixel[0]);
        let m = 255 - u16::from(pixel[1]);
        let y = 255 - u16::from(pixel[2]);
        let k = 255 - u16::from(pixel[3]);

        // CMY -> RGB
        let r = (k * c) as f32 * inv;
        let g = (k * m) as f32 * inv;
        let b = (k * y) as f32 * inv;

        outp[0] = r as u8;
        outp[1] = g as u8;
        outp[2] = b as u8;
    }

    output
}

impl ColorType {
    fn from_jpeg(pixel_format: jpeg::PixelFormat) -> ColorType {
        use jpeg::PixelFormat::*;
        match pixel_format {
            L8 => ColorType::L8,
            RGB24 => ColorType::Rgb8,
            CMYK32 => panic!(),
        }
    }
}

impl ImageError {
    fn from_jpeg(err: jpeg::Error) -> ImageError {
        use jpeg::Error::*;
        match err {
            Format(desc) => ImageError::FormatError(desc),
            Unsupported(desc) => ImageError::UnsupportedError(format!("{:?}", desc)),
            Io(err) => ImageError::IoError(err),
            Internal(err) => ImageError::FormatError(err.to_string()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::cmyk_to_rgb;

    #[test]
    fn cymk_to_rgb_correct() {
        // Based on R = 255 * (1-C/255) * (1-K/255)
        for c in 0..255 {
            for k in 0..255 {
                let r = (255.0 - f32::from(c)) * (255.0 - f32::from(k)) / 255.0;
                let r_u8 = r as u8;
                let convert_r = cmyk_to_rgb(&[c, 0, 0, k])[0];
                let convert_g = cmyk_to_rgb(&[0, c, 0, k])[0];
                let convert_b = cmyk_to_rgb(&[0, 0, c, k])[0];

                assert_eq!(convert_r, r_u8,
                           "c = {}, k = {}, cymk_to_rgb[0] = {}, should be {}", c, k, convert_r, r_u8);
                assert_eq!(convert_g, r_u8,
                           "m = {}, k = {}, cymk_to_rgb[0] = {}, should be {}", c, k, convert_g, r_u8);
                assert_eq!(convert_b, r_u8,
                           "y = {}, k = {}, cymk_to_rgb[0] = {}, should be {}", c, k, convert_b, r_u8);
            }
        }
    }
}

