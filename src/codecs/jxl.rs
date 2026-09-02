//! Decoding of JPEG XL images.

use std::io::{BufRead, BufReader, Read, Seek};
use std::num::NonZeroU32;
use std::time::Duration;

use jxl::api::{
    states, Endianness, JxlBitDepth, JxlColorType, JxlDataFormat, JxlDecoder as InnerDecoder,
    JxlDecoderOptions, JxlOutputBuffer, JxlPixelFormat, ProcessingResult,
};
use jxl::headers::extra_channels::ExtraChannel;

use crate::error::{DecodingError, ImageError, LimitError, LimitErrorKind};
use crate::io::{
    DecodedAnimationAttributes, DecodedImageAttributes, DecodedMetadataHint, DecoderPreparedImage,
    FormatAttributes, SequenceControl,
};
use crate::metadata::LoopCount;
use crate::{ColorType, ImageDecoder, ImageFormat, ImageResult, Limits};

/// JPEG XL decoder.
pub struct JxlDecoder<R: Read + Seek> {
    input: BufReader<R>,
    decoder: Option<InnerDecoder<states::WithImageInfo>>,
    width: u32,
    height: u32,
    color_type: ColorType,
    icc_profile: Option<Vec<u8>>,
    animation: Option<DecodedAnimationAttributes>,
    limits: Limits,
}

impl<R: Read + Seek> JxlDecoder<R> {
    /// Creates a new decoder that decodes from `reader`.
    pub fn new(reader: R) -> ImageResult<Self> {
        let mut input = BufReader::new(reader);
        let mut decoder = InnerDecoder::<states::Initialized>::new(JxlDecoderOptions::default());
        let mut decoder = loop {
            match decoder.process(&mut input, None).map_err(decoding_error)? {
                ProcessingResult::Complete { result } => break result,
                ProcessingResult::NeedsMoreInput { fallback, .. } => {
                    decoder = fallback;
                    if input.fill_buf()?.is_empty() {
                        return Err(truncated_error());
                    }
                }
            }
        };

        let info = decoder.basic_info().clone();
        let width = u32::try_from(info.size.0).map_err(|_| dimension_error())?;
        let height = u32::try_from(info.size.1).map_err(|_| dimension_error())?;
        let has_alpha = info
            .extra_channels
            .iter()
            .any(|channel| channel.ec_type == ExtraChannel::Alpha);
        let grayscale = decoder.current_pixel_format().color_type.is_grayscale();

        let (data_format, color_type) = match info.bit_depth {
            JxlBitDepth::Int { bits_per_sample } if bits_per_sample <= 8 => (
                JxlDataFormat::U8 { bit_depth: 8 },
                image_color_type(grayscale, has_alpha, 8),
            ),
            JxlBitDepth::Int { .. } => (
                JxlDataFormat::U16 {
                    endianness: Endianness::native(),
                    bit_depth: 16,
                },
                image_color_type(grayscale, has_alpha, 16),
            ),
            JxlBitDepth::Float { .. } => (
                JxlDataFormat::F32 {
                    endianness: Endianness::native(),
                },
                image_color_type(grayscale, has_alpha, 32),
            ),
        };

        let pixel_format = JxlPixelFormat {
            color_type: match (grayscale, has_alpha) {
                (true, false) => JxlColorType::Grayscale,
                (true, true) => JxlColorType::GrayscaleAlpha,
                (false, false) => JxlColorType::Rgb,
                (false, true) => JxlColorType::Rgba,
            },
            color_data_format: Some(data_format),
            extra_channel_format: vec![None; info.extra_channels.len()],
        };
        decoder.set_pixel_format(pixel_format);

        let icc_profile = decoder
            .output_color_profile()
            .try_as_icc()
            .map(|profile| profile.into_owned());
        let animation = info.animation.map(|animation| DecodedAnimationAttributes {
            loop_count: match NonZeroU32::new(animation.num_loops) {
                Some(count) => LoopCount::Finite(count),
                None => LoopCount::Infinite,
            },
        });

        Ok(Self {
            input,
            decoder: Some(decoder),
            width,
            height,
            color_type,
            icc_profile,
            animation,
            limits: Limits::no_limits(),
        })
    }

    fn layout(&self) -> DecoderPreparedImage {
        DecoderPreparedImage::new(self.width, self.height, self.color_type)
    }
}

impl<R: Read + Seek> ImageDecoder for JxlDecoder<R> {
    fn format_attributes(&self) -> FormatAttributes {
        FormatAttributes {
            supports_animation: true,
            icc: DecodedMetadataHint::InHeader,
            ..FormatAttributes::default()
        }
    }

    fn animation_attributes(&mut self) -> Option<DecodedAnimationAttributes> {
        self.animation.clone()
    }

    fn prepare_image(&mut self) -> ImageResult<DecoderPreparedImage> {
        if self.decoder.is_none() {
            return Err(ImageError::Decoding(DecodingError::new(
                ImageFormat::JpegXl.into(),
                "no more JPEG XL frames",
            )));
        }
        Ok(self.layout())
    }

    fn read_image(&mut self, buf: &mut [u8]) -> ImageResult<DecodedImageAttributes> {
        let layout = self.prepare_image()?;
        assert_eq!(buf.len() as u64, layout.total_bytes());

        let mut decoder = self.decoder.take().expect("checked by prepare_image");
        let frame_decoder = loop {
            match decoder
                .process(&mut self.input, None)
                .map_err(decoding_error)?
            {
                ProcessingResult::Complete { result } => break result,
                ProcessingResult::NeedsMoreInput { fallback, .. } => {
                    decoder = fallback;
                    if self.input.fill_buf()?.is_empty() {
                        return Err(truncated_error());
                    }
                }
            }
        };
        let frame_header = frame_decoder.frame_header();

        let decoder = match self.color_type {
            ColorType::L8 | ColorType::La8 | ColorType::Rgb8 | ColorType::Rgba8 => {
                decode_frame(&mut self.input, frame_decoder, buf, self.height)?
            }
            ColorType::L16 | ColorType::La16 | ColorType::Rgb16 | ColorType::Rgba16 => {
                self.limits.reserve_usize(buf.len())?;
                let mut aligned = vec![0u16; buf.len() / 2];
                self.limits.free_usize(buf.len());
                let decoder = decode_frame(
                    &mut self.input,
                    frame_decoder,
                    bytemuck::cast_slice_mut(&mut aligned),
                    self.height,
                )?;
                buf.copy_from_slice(bytemuck::cast_slice(&aligned));
                decoder
            }
            ColorType::L32F | ColorType::La32F | ColorType::Rgb32F | ColorType::Rgba32F => {
                self.limits.reserve_usize(buf.len())?;
                let mut aligned = vec![0f32; buf.len() / 4];
                self.limits.free_usize(buf.len());
                let decoder = decode_frame(
                    &mut self.input,
                    frame_decoder,
                    bytemuck::cast_slice_mut(&mut aligned),
                    self.height,
                )?;
                buf.copy_from_slice(bytemuck::cast_slice(&aligned));
                decoder
            }
        };

        let has_more_frames = decoder.has_more_frames();
        self.decoder = has_more_frames.then_some(decoder);

        Ok(DecodedImageAttributes {
            delay: frame_header.duration.map(|milliseconds| {
                crate::Delay::from_saturating_duration(Duration::from_secs_f64(
                    milliseconds / 1000.0,
                ))
            }),
            ..DecodedImageAttributes::default()
        })
    }

    fn icc_profile(&mut self) -> ImageResult<Option<Vec<u8>>> {
        Ok(self.icc_profile.clone())
    }

    fn more_images(&self) -> SequenceControl {
        if self.decoder.is_some() {
            SequenceControl::MaybeMore
        } else {
            SequenceControl::None
        }
    }

    fn set_limits(&mut self, limits: Limits) -> ImageResult<()> {
        limits.check_support(&crate::LimitSupport::default())?;
        limits.check_layout_dimensions(&self.layout())?;
        self.limits = limits;
        Ok(())
    }
}

fn decode_frame<R: Read + Seek>(
    input: &mut BufReader<R>,
    mut decoder: InnerDecoder<states::WithFrameInfo>,
    buf: &mut [u8],
    height: u32,
) -> ImageResult<InnerDecoder<states::WithImageInfo>> {
    let mut output = JxlOutputBuffer::new(buf, height as usize, buf.len() / height as usize);
    loop {
        match decoder
            .process(input, std::slice::from_mut(&mut output), None)
            .map_err(decoding_error)?
        {
            ProcessingResult::Complete { result } => break Ok(result),
            ProcessingResult::NeedsMoreInput { fallback, .. } => {
                decoder = fallback;
                if input.fill_buf()?.is_empty() {
                    break Err(truncated_error());
                }
            }
        }
    }
}

fn image_color_type(grayscale: bool, alpha: bool, bits: u8) -> ColorType {
    match (grayscale, alpha, bits) {
        (true, false, 8) => ColorType::L8,
        (true, true, 8) => ColorType::La8,
        (false, false, 8) => ColorType::Rgb8,
        (false, true, 8) => ColorType::Rgba8,
        (true, false, 16) => ColorType::L16,
        (true, true, 16) => ColorType::La16,
        (false, false, 16) => ColorType::Rgb16,
        (false, true, 16) => ColorType::Rgba16,
        (true, false, 32) => ColorType::L32F,
        (true, true, 32) => ColorType::La32F,
        (false, false, 32) => ColorType::Rgb32F,
        (false, true, 32) => ColorType::Rgba32F,
        _ => unreachable!(),
    }
}

fn decoding_error(error: jxl::error::Error) -> ImageError {
    ImageError::Decoding(DecodingError::new(ImageFormat::JpegXl.into(), error))
}

fn truncated_error() -> ImageError {
    ImageError::Decoding(DecodingError::new(
        ImageFormat::JpegXl.into(),
        "truncated JPEG XL image",
    ))
}

fn dimension_error() -> ImageError {
    ImageError::Limits(LimitError::from_kind(LimitErrorKind::DimensionError))
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use super::*;

    const RGB_LOSSLESS: &[u8] = include_bytes!("../../tests/images/jxl/rgb-lossless.jxl");
    const RGB16_LOSSLESS: &[u8] = include_bytes!("../../tests/images/jxl/rgb16-lossless.jxl");
    const ANIMATION: &[u8] = include_bytes!("../../tests/images/jxl/animation.jxl");

    #[test]
    fn decode_rgb_lossless() {
        let mut decoder = JxlDecoder::new(Cursor::new(RGB_LOSSLESS)).unwrap();
        let layout = decoder.prepare_image().unwrap();
        assert_eq!(layout.layout.dimensions(), (2, 1));
        assert_eq!(layout.layout.color, ColorType::Rgb8);

        let mut pixels = vec![0; layout.total_bytes() as usize];
        decoder.read_image(&mut pixels).unwrap();
        assert_eq!(pixels, [255, 0, 0, 0, 255, 0]);
    }

    #[test]
    fn decode_rgb16_to_unaligned_buffer() {
        let mut decoder = JxlDecoder::new(Cursor::new(RGB16_LOSSLESS)).unwrap();
        let layout = decoder.prepare_image().unwrap();
        assert_eq!(layout.layout.color, ColorType::Rgb16);

        let mut storage = vec![0; layout.total_bytes() as usize + 1];
        let pixels = &mut storage[1..];
        decoder.read_image(pixels).unwrap();
        let expected: Vec<u8> = [u16::MAX, 0, 0, 32768, u16::MAX, 0]
            .into_iter()
            .flat_map(u16::to_ne_bytes)
            .collect();
        assert_eq!(pixels, expected);
    }

    #[test]
    fn decode_animation() {
        let reader =
            crate::ImageReaderOptions::with_format(Cursor::new(ANIMATION), ImageFormat::JpegXl)
                .into_reader()
                .unwrap();
        let frames = reader.into_frames().collect_frames().unwrap();
        assert_eq!(frames.len(), 2);
        assert_eq!(frames[0].delay().as_millis(), 100);
        assert_eq!(frames[1].delay().as_millis(), 200);
    }

    #[test]
    fn high_level_decode_uses_magic_bytes() {
        assert_eq!(
            crate::guess_format(RGB_LOSSLESS).unwrap(),
            ImageFormat::JpegXl
        );
        let image = crate::load_from_memory(RGB_LOSSLESS).unwrap();
        assert_eq!(image.into_rgb8().into_raw(), [255, 0, 0, 0, 255, 0]);
    }
}
