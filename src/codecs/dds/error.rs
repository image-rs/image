use std::{error, fmt};

use crate::error::{DecodingError, ImageError};
use crate::image::ImageFormat;

use super::header::ResourceDimension;

/// Errors that can occur during decoding and parsing a DDS image
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
#[allow(clippy::enum_variant_names)]
pub(crate) enum DecoderError {
    /// Wrong DDS channel width
    PixelFormatSizeInvalid(u32),
    /// Wrong DDS header size
    HeaderSizeInvalid(u32),
    /// Wrong DDS header flags
    HeaderFlagsInvalid(u32),
    /// Wrong DDS caps
    HeaderCapsInvalid(u32),
    /// Wrong DDS caps2
    HeaderCaps2Invalid(u32),

    /// Invalid DXGI format in DX10 header
    DxgiFormatInvalid(u32),
    /// Invalid resource dimension
    ResourceDimensionInvalid(u32),
    /// Unsupported resource dimension
    ResourceDimensionUnsupported(ResourceDimension),
    /// Invalid flags in DX10 header
    Dx10FlagsInvalid(u32),
    /// Invalid array size in DX10 header
    Dx10ArraySizeInvalid(u32),

    /// DDS "DDS " signature invalid or missing
    DdsSignatureInvalid,
}

impl fmt::Display for DecoderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DecoderError::PixelFormatSizeInvalid(s) => {
                f.write_fmt(format_args!("Invalid DDS PixelFormat size: {}", s))
            }
            DecoderError::HeaderSizeInvalid(s) => {
                f.write_fmt(format_args!("Invalid DDS header size: {}", s))
            }
            DecoderError::HeaderFlagsInvalid(fs) => {
                f.write_fmt(format_args!("Invalid DDS header flags: {:#010X}", fs))
            }
            DecoderError::HeaderCapsInvalid(caps) => {
                f.write_fmt(format_args!("Invalid DDS caps: {:#010X}", caps))
            }
            DecoderError::HeaderCaps2Invalid(caps) => {
                f.write_fmt(format_args!("Invalid DDS caps2: {:#010X}", caps))
            }
            DecoderError::DxgiFormatInvalid(df) => {
                f.write_fmt(format_args!("Invalid DDS DXGI format: {}", df))
            }
            DecoderError::ResourceDimensionInvalid(d) => {
                f.write_fmt(format_args!("Invalid DDS resource dimension: {}", d))
            }
            DecoderError::ResourceDimensionUnsupported(d) => {
                f.write_fmt(format_args!("Unsupported DDS resource dimension: {:?}", d))
            }
            DecoderError::Dx10FlagsInvalid(fs) => {
                f.write_fmt(format_args!("Invalid DDS DX10 header flags: {:#010X}", fs))
            }
            DecoderError::Dx10ArraySizeInvalid(s) => {
                f.write_fmt(format_args!("Invalid DDS DX10 array size: {}", s))
            }
            DecoderError::DdsSignatureInvalid => f.write_str("DDS signature not found"),
        }
    }
}

impl From<DecoderError> for ImageError {
    fn from(e: DecoderError) -> ImageError {
        ImageError::Decoding(DecodingError::new(ImageFormat::Dds.into(), e))
    }
}

impl error::Error for DecoderError {}
