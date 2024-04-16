use std::io::{BufRead, Cursor, Seek};
use std::marker::PhantomData;

use zune_core::options::DecoderOptions;

use crate::color::ColorType;
use crate::error::{
    DecodingError, ImageError, ImageResult, LimitError, UnsupportedError, UnsupportedErrorKind,
};
use crate::image::{ImageDecoder, ImageFormat};
use crate::io::Limits;

type ZuneColorSpace = zune_core::colorspace::ColorSpace;

/// JPEG decoder
pub struct JpegDecoder<R: BufRead + Seek> {
    orig_color_space: ZuneColorSpace,
    width: u16,
    height: u16,
    limits: Limits,
    // For API compatibility with the previous jpeg_decoder wrapper.
    // Can be removed later, which would be an API break.
    phantom: PhantomData<R>,
    decoder: zune_jpeg::JpegDecoder<R>,
}

impl<R: BufRead + Seek> JpegDecoder<R> {
    /// Create a new decoder that decodes from the stream ```r```
    pub fn new(r: R) -> ImageResult<JpegDecoder<R>> {
        let options = DecoderOptions::default()
            .set_strict_mode(false)
            .set_max_width(usize::MAX)
            .set_max_height(usize::MAX);
        let mut decoder = zune_jpeg::JpegDecoder::new_with_options(r, options);
        decoder.decode_headers().map_err(ImageError::from_jpeg)?;
        // now that we've decoded the headers we can `.unwrap()`
        // all these functions that only fail if called before decoding the headers
        let (width, height) = decoder.dimensions().unwrap();
        // JPEG can only express dimensions up to 65535x65535, so this conversion cannot fail
        let width: u16 = width.try_into().unwrap();
        let height: u16 = height.try_into().unwrap();
        let orig_color_space = decoder.output_colorspace().unwrap();

        decoder.set_options(
            decoder
                .options()
                .to_owned()
                .jpeg_set_out_colorspace(orig_color_space),
        );
        // Limits are disabled by default in the constructor for all decoders
        let limits = Limits::no_limits();
        Ok(JpegDecoder {
            orig_color_space,
            width,
            height,
            limits,
            phantom: PhantomData,
            decoder,
        })
    }
}

impl<R: BufRead + Seek> ImageDecoder for JpegDecoder<R> {
    fn dimensions(&self) -> (u32, u32) {
        (u32::from(self.width), u32::from(self.height))
    }

    fn color_type(&self) -> ColorType {
        ColorType::from_jpeg(self.orig_color_space)
    }

    fn icc_profile(&mut self) -> ImageResult<Option<Vec<u8>>> {
        Ok(self.decoder.icc_profile())
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

        let options = DecoderOptions::default()
            .jpeg_set_out_colorspace(to_supported_color_space(self.orig_color_space))
            .set_strict_mode(false);

        let max_height = match self.limits.max_image_height {
            Some(max_height) => max_height as usize,
            None => usize::MAX,
        };

        let max_width = match self.limits.max_image_width {
            Some(max_width) => max_width as usize,
            None => usize::MAX,
        };

        options.set_max_height(max_height).set_max_width(max_width);

        let mut decoder =
            zune_jpeg::JpegDecoder::new_with_options(Cursor::new(buf.to_owned()), options);

        decoder.decode_into(buf).map_err(ImageError::from_jpeg)
    }

    fn set_limits(&mut self, limits: Limits) -> ImageResult<()> {
        limits.check_support(&crate::io::LimitSupport::default())?;
        let (width, height) = self.dimensions();
        limits.check_dimensions(width, height)?;
        self.limits = limits;
        Ok(())
    }

    fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()> {
        (*self).read_image(buf)
    }
}

impl ColorType {
    fn from_jpeg(colorspace: ZuneColorSpace) -> ColorType {
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

impl ImageError {
    fn from_jpeg(err: zune_jpeg::errors::DecodeErrors) -> ImageError {
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
