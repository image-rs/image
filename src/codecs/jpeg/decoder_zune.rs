use std::io::{self, Cursor, Read};

use crate::{
    error::{DecodingError, LimitError, UnsupportedError, UnsupportedErrorKind},
    ColorType, ImageDecoder, ImageError, ImageFormat, ImageResult,
};

type ZuneColorSpace = zune_core::colorspace::ColorSpace;

pub struct ZuneJpegDecoder {
    input: Vec<u8>,
    orig_color_space: ZuneColorSpace,
    width: u16,
    height: u16,
}

impl ZuneJpegDecoder {
    pub fn new<R: Read>(mut r: R) -> ImageResult<ZuneJpegDecoder> {
        let mut input = Vec::new();
        r.read_to_end(&mut input)?;
        let mut decoder = zune_jpeg::JpegDecoder::new(&input);
        decoder
            .decode_headers()
            .map_err(ImageError::from_zune_jpeg)?;
        // now that we've decoded the headers we can `.unwrap()`
        // all these functions that only fail if called before decoding the headers
        let (width, height) = decoder.dimensions().unwrap();
        let orig_color_space = decoder.get_output_colorspace().unwrap();
        Ok(ZuneJpegDecoder {
            input,
            orig_color_space,
            width,
            height,
        })
    }
}

/// Wrapper struct around a `Cursor<Vec<u8>>`
pub struct ZuneJpegReader(Cursor<Vec<u8>>);
impl Read for ZuneJpegReader {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.0.read(buf)
    }
    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
        if self.0.position() == 0 && buf.is_empty() {
            std::mem::swap(buf, self.0.get_mut());
            Ok(buf.len())
        } else {
            self.0.read_to_end(buf)
        }
    }
}

impl<'a> ImageDecoder<'a> for ZuneJpegDecoder {
    type Reader = ZuneJpegReader;

    fn dimensions(&self) -> (u32, u32) {
        (u32::from(self.width), u32::from(self.height))
    }

    fn color_type(&self) -> ColorType {
        ColorType::from_zune_jpeg(self.orig_color_space)
    }

    fn icc_profile(&mut self) -> Option<Vec<u8>> {
        None // zune-jpeg doesn't support it
    }

    fn into_reader(self) -> ImageResult<Self::Reader> {
        let mut decoder = new_zune_decoder(&self.input, self.orig_color_space);
        let data = decoder.decode().map_err(ImageError::from_zune_jpeg)?;
        Ok(ZuneJpegReader(Cursor::new(data)))
    }

    fn read_image(self, buf: &mut [u8]) -> ImageResult<()> {
        let advertised_len = self.total_bytes();
        let actual_len = buf.len() as u64;

        if actual_len != advertised_len {
            return Err(ImageError::Decoding(DecodingError::new(
                ImageFormat::Jpeg.into(),
                format!(
                    "Length of the decoded data {actual_len}\
                    doesn't match the advertised dimensions of the image\
                    that imply length {advertised_len}"
                ),
            )));
        }

        let mut decoder = new_zune_decoder(&self.input, self.orig_color_space);
        decoder.decode_into(buf).map_err(ImageError::from_zune_jpeg)?;
        Ok(())
    }
}

impl ColorType {
    fn from_zune_jpeg(colorspace: ZuneColorSpace) -> ColorType {
        let colorspace = to_supported_color_space(colorspace);
        use zune_core::colorspace::ColorSpace::*;
        match colorspace {
            // As of zune-jpeg 0.3.13 the output is always 8-bit,
            // but support for 16-bit JPEG might be added in the future.
            RGB => ColorType::Rgb8,
            RGBA => ColorType::Rgba8,
            Luma => ColorType::L8,
            LumaA => ColorType::La8,
            // to_supported_color_space() doesn't return any of the other variants
            _ => unreachable!(),
        }
    }
}

fn to_supported_color_space(orig: ZuneColorSpace) -> ZuneColorSpace {
    use zune_core::colorspace::ColorSpace::*;
    match orig {
        RGB | RGBA | Luma | LumaA => orig,
        // the rest is not supported by `image` so it will be converted to RGB during decoding
        _ => RGB,
    }
}

fn new_zune_decoder(input: &[u8], orig_color_space: ZuneColorSpace) -> zune_jpeg::JpegDecoder {
    let target_color_space = to_supported_color_space(orig_color_space);
    let options =
        zune_core::options::DecoderOptions::default().jpeg_set_out_colorspace(target_color_space);
    zune_jpeg::JpegDecoder::new_with_options(options, &input)
}

impl ImageError {
    fn from_zune_jpeg(err: zune_jpeg::errors::DecodeErrors) -> ImageError {
        use zune_jpeg::errors::DecodeErrors::*;
        match err {
            Unsupported(desc) => ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                ImageFormat::Jpeg.into(),
                UnsupportedErrorKind::GenericFeature(format!("{:?}", desc)),
            )),
            LargeDimensions(_) => ImageError::Limits(LimitError::from_kind(
                crate::error::LimitErrorKind::DimensionError,
            )),
            err => ImageError::Decoding(DecodingError::new(ImageFormat::Jpeg.into(), err)),
        }
    }
}
