use crate::{ImageError, ColorType, error::{DecodingError, UnsupportedErrorKind, UnsupportedError, LimitError}, ImageFormat, color};

type ZuneColorSpace = zune_core::colorspace::ColorSpace;

pub struct ZuneJpegDecoder<R> {
    decoder: jpeg::Decoder<R>,
    metadata: jpeg::ImageInfo,
}

impl ColorType {
    fn from_zune_jpeg(colorspace: ZuneColorSpace) -> ColorType {
        let colorspace = to_supported_color_space(colorspace);
        use zune_core::colorspace::ColorSpace::*;
        match colorspace {
            // TODO: are these always 8-bit? I've asked at
            // https://github.com/etemesi254/zune-image/discussions/99
            RGB => ColorType::Rgb8,
            RGBA => ColorType::Rgba8,
            Luma => ColorType::L8,
            LumaA => ColorType::La8,
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
    fn from_zune_jpeg(err: zune_jpeg::errors::DecodeErrors) -> ImageError {
        use zune_jpeg::errors::DecodeErrors::*;
        match err {
            Unsupported(desc) => ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                ImageFormat::Jpeg.into(),
                UnsupportedErrorKind::GenericFeature(format!("{:?}", desc)),
            )),
            LargeDimensions(_) => ImageError::Limits(LimitError::from_kind(crate::error::LimitErrorKind::DimensionError)),
            err => {
                ImageError::Decoding(DecodingError::new(ImageFormat::Jpeg.into(), err))
            }
        }
    }
}