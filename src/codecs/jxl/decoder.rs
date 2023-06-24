//! Decoding of JPEG XL images
//!
//! TODO: animation handling, CMYK [ColorType]

use crate::{
    error::{DecodingError, UnsupportedError, UnsupportedErrorKind},
    image::decoder_to_vec,
    ColorType, ExtendedColorType, ImageDecoder, ImageError, ImageFormat, ImageResult,
    io::Limits,
};
use std::io::{Cursor, Read};

use jxl_oxide::{JxlImage, PixelFormat, Render};

/// JPEG XL decoder
pub struct JxlDecoder<R> {
    image: JxlImage<R>,
    bitdepth: BitDepth,
    pixfmt: PixelFormat,
    colortype: ColorType,
    keyframes: Vec<Render>,
}

impl<R: Read> JxlDecoder<R> {
    /// With limits
     
    pub fn new(r: R) -> ImageResult<JxlDecoder<R>> {
        Self::with_limits(r, Limits::default())
    }
    /// Creates a new decoder that decodes from the stream ```r```
    pub fn with_limits(r: R, limits: Limits) -> ImageResult<JxlDecoder<R>> {
        limits.check_support(&crate::io::LimitSupport::default())?;
        //limits.max_image_width = Some(7680); //TODO: hard coded to 8k for testing
        //limits.max_image_height = Some(4320);
        let mut image = JxlImage::from_reader(r).map_err(|_| {
            ImageError::Decoding(DecodingError::new(
                ImageFormat::Jxl.into(),
                "Failed to parse image",
            ))
        })?;
        let metadata = &image.image_header().metadata;
        let bits_per_sample = metadata.bit_depth.bits_per_sample();
        let bitdepth = BitDepth::new(bits_per_sample);
        limits.check_dimensions(image.image_header().size.width, image.image_header().size.width)?;
        let mut keyframes = Vec::new();
        let mut renderer = image.renderer();
        //TODO: check max bytes
        //let max_bytes = usize::try_from(limits.max_alloc.unwrap_or(u64::MAX)).unwrap_or(usize::MAX);
        loop {
            let result = renderer.render_next_frame().map_err(|e| {
                ImageError::Decoding(DecodingError::new(ImageFormat::Jxl.into(), e))
            })?;
            match result {
                jxl_oxide::RenderResult::Done(frame) => keyframes.push(frame),
                jxl_oxide::RenderResult::NeedMoreData => {
                    return Err(ImageError::Decoding(DecodingError::new(
                        ImageFormat::Jxl.into(),
                        "Unexpected end of file",
                    )))
                }
                jxl_oxide::RenderResult::NoMoreFrames => break,
            }
        }
        let pixfmt = renderer.pixel_format();

        let colortype = match (pixfmt, bitdepth) {
            (PixelFormat::Gray, BitDepth::Eight) => ColorType::L8,
            (PixelFormat::Gray, BitDepth::Sixteen) => ColorType::L16,
            //imagers doens't support 32bit gray
            (PixelFormat::Gray, BitDepth::ThirtyTwo) => ColorType::L16,
            //
            (PixelFormat::Graya, BitDepth::Eight) => ColorType::La8,
            (PixelFormat::Graya, BitDepth::Sixteen) => ColorType::La16,
            //imagers doens't support 32bit gray
            (PixelFormat::Graya, BitDepth::ThirtyTwo) => ColorType::La16,
            //
            (PixelFormat::Rgb, BitDepth::Eight) => ColorType::Rgb8,
            (PixelFormat::Rgb, BitDepth::Sixteen) => ColorType::Rgb16,
            (PixelFormat::Rgb, BitDepth::ThirtyTwo) => ColorType::Rgb32F,
            //
            (PixelFormat::Rgba, BitDepth::Eight) => ColorType::Rgba8,
            (PixelFormat::Rgba, BitDepth::Sixteen) => ColorType::Rgba16,
            (PixelFormat::Rgba, BitDepth::ThirtyTwo) => ColorType::Rgba32F,
            //
            _ => {
                return Err(unsupported_color(ExtendedColorType::Unknown(
                    bits_per_sample as u8,
                )))
            }
        };

        Ok(Self {
            image,
            bitdepth: BitDepth::new(bits_per_sample),
            pixfmt,
            colortype,
            keyframes,
        })
    }
}

fn unsupported_color(ect: ExtendedColorType) -> ImageError {
    ImageError::Unsupported(UnsupportedError::from_format_and_kind(
        ImageFormat::Jxl.into(),
        UnsupportedErrorKind::Color(ect),
    ))
}

impl<'a, R: 'a + Read> ImageDecoder<'a> for JxlDecoder<R> {
    type Reader = Cursor<Vec<u8>>;

    fn dimensions(&self) -> (u32, u32) {
        let size = &self.image.image_header().size;
        (size.width, size.height)
    }

    fn color_type(&self) -> ColorType {
        self.colortype
    }

    fn into_reader(self) -> ImageResult<Self::Reader> {
            let data = decoder_to_vec(self)?;
            Ok(Cursor::new(data))
    }

    fn icc_profile(&mut self) -> Option<Vec<u8>> {
        Option::from(self.image.rendered_icc())
    }

    fn read_image(self, buf: &mut [u8]) -> ImageResult<()> {
        let fb = self
            .keyframes
            .get(0)
            .ok_or_else(|| {
                ImageError::Decoding(DecodingError::new(
                    ImageFormat::Jxl.into(),
                    "No keyframes found",
                ))
            })?
            .image();
        let mut bdepth = self.bitdepth;
        if self.pixfmt == PixelFormat::Gray || self.pixfmt == PixelFormat::Graya {
            if bdepth == BitDepth::ThirtyTwo {
                //manually cast 32bit gray down to 16bit as a workaround
                 bdepth = BitDepth::Sixteen
            }
        };
        match bdepth {
            BitDepth::Eight => {
                for (b, s) in buf.iter_mut().zip(fb.buf()) {
                    *b = (*s * 255.0 + 0.5).clamp(0.0, 255.0) as u8;
                }
            }
            BitDepth::Sixteen => {
                for (b, s) in buf.chunks_exact_mut(2).zip(fb.buf()) {
                    let w = (*s * 65535.0 + 0.5).clamp(0.0, 65535.0) as u16;
                    let [b0, b1] = w.to_ne_bytes();
                    b[0] = b0;
                    b[1] = b1;
                }
            }
            BitDepth::ThirtyTwo => {
                buf.clone_from_slice(bytemuck::cast_slice(fb.buf()));
            }
        };
        Ok(())
    }
}


#[derive(Debug, PartialEq)]
enum BitDepth {
    Eight,
    Sixteen,
    ThirtyTwo,
}

impl BitDepth {
    fn new(bits_per_sample: u32) -> Self {
        match bits_per_sample {
            17.. => Self::ThirtyTwo,
            9.. => Self::Sixteen,
            _ => Self::Eight,
      }
    }
}