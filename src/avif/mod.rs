use std::io::Write;

use crate::{flat, ColorType, DynamicImage, ImageBuffer, ImageFormat, Rgba, Pixel};
use crate::{ImageError, ImageResult};
use crate::error::{EncodingError, ParameterError, ParameterErrorKind, UnsupportedError, UnsupportedErrorKind};

use ravif::{Img, ColorSpace, Config, RGBA8, encode_rgba};
use rgb::AsPixels;

pub struct AvifEncoder<W> {
    inner: W,
    fallback: Vec<RGBA8>,
}

impl<W: Write> AvifEncoder<W> {
    /// Create a new encoder that writes its output to `w`.
    pub fn new(w: W) -> Self {
        AvifEncoder { inner: w, fallback: vec![] }
    }

    pub fn encode(mut self, data: &[u8], width: u32, height: u32, color: ColorType) -> ImageResult<()> {
        let config = self.config(color);
        // `ravif` needs strongly typed data so let's convert. We can either use a temporarily
        // owned version in our own buffer or zero-copy if possible by using the input buffer.
        // This requires going through `rgb`.
        let buffer = self.encode_as_img(data, width, height, color)?;
        let (data, _color_size, _alpha_size) = encode_rgba(buffer, &config)
            .map_err(|err| ImageError::Encoding(
                EncodingError::new(ImageFormat::Avif.into(), err)
            ))?;
        self.inner.write_all(&data)?;
        Ok(())
    }

    fn config(&self, _color: ColorType) -> Config {
        Config {
            quality: 100,
            alpha_quality: 100,
            speed: 1,
            premultiplied_alpha: false,
            color_space: ColorSpace::RGB,
        }
    }

    fn encode_as_img<'buf>(&'buf mut self, data: &'buf [u8], width: u32, height: u32, color: ColorType)
        -> ImageResult<Img<&'buf [RGBA8]>>
    {
        fn try_from_raw<P: Pixel + 'static>(data: &[P::Subpixel], width: u32, height: u32)
            -> ImageResult<ImageBuffer<P, &[P::Subpixel]>>
        {
            ImageBuffer::from_raw(width, height, data).ok_or_else(|| {
                ImageError::Parameter(ParameterError::from_kind(ParameterErrorKind::DimensionMismatch))
            })
        };

        match color {
            ColorType::Rgba8 => {
                // ravif doesn't do any checks but has some asserts, so we do the checks.
                let img = try_from_raw::<Rgba<u8>>(data, width, height)?;
                // Now, internally ravif uses u32 but it takes usize. We could do some checked
                // conversion but instead we use that a non-empty image must be addressable.
                if img.pixels().len() == 0 {
                    return Err(ImageError::Parameter(ParameterError::from_kind(ParameterErrorKind::DimensionMismatch)));
                }

                Ok(Img::new(rgb::AsPixels::as_pixels(data), width as usize, height as usize))
            },
            // we need a separate buffer..
            ColorType::L8 | ColorType::La8 | ColorType::Rgb8 | ColorType::Bgr8 | ColorType::Bgra8 => {
                todo!()
            }
            // we need to really convert data..
            ColorType::L16 | ColorType::La16 | ColorType::Rgb16 | ColorType::Rgba16 => {
                todo!()
            }
            // for cases we do not support at all?
            _ => Err(ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                    ImageFormat::Avif.into(),
                    UnsupportedErrorKind::Color(color.into()),
                )))
        }
    }
}
