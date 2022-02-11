use std::{error, fmt};
use std::io::Read;

use crate::{ImageResult, ImageError};
use crate::image::ImageFormat;
use crate::Rgba;
use crate::error::DecodingError;
use byteorder::{ReadBytesExt, LittleEndian, BigEndian};

#[derive(Debug, Clone, Copy)]
enum ExtendedWebPDecoderError {
    HeaderInvalid,
}

impl fmt::Display for ExtendedWebPDecoderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExtendedWebPDecoderError::HeaderInvalid => {
                f.write_str("Invalid Header")
            }
        }
    }
}

impl From<ExtendedWebPDecoderError> for ImageError {
    fn from(e: ExtendedWebPDecoderError) -> ImageError {
        ImageError::Decoding(DecodingError::new(ImageFormat::WebP.into(), e))
    }
}

impl error::Error for ExtendedWebPDecoderError {}

#[derive(Debug, Copy, Clone)]
pub(crate) struct WebPExtendedInfo {
    icc_profile: bool,
    alpha: bool,
    exif_metadata: bool,
    xmp_metadata: bool,
    animation: bool,
    canvas_width: u32,
    canvas_height: u32,
}

struct WebPExtendedFrame {

}

struct WebPAnimInfo {
    background_color: Rgba<u8>,
    loop_count: u16,
}

pub(crate) fn read_extended_header<R: Read>(reader: &mut R) -> ImageResult<WebPExtendedInfo> {
    let chunk_flags = reader.read_u8()?;

    let reserved_first = chunk_flags & 0b11000000;
    let icc_profile = chunk_flags & 0b00100000 != 0;
    let alpha = chunk_flags & 0b00010000 != 0;
    let exif_metadata = chunk_flags & 0b00001000 != 0;
    let xmp_metadata = chunk_flags & 0b00000100 != 0;
    let animation = chunk_flags & 0b00000010 != 0;
    let reserved_second = chunk_flags & 0b00000001;

    let reserved_third = reader.read_u8()?;
    let reserved_fourth = reader.read_u16::<BigEndian>()?;

    if reserved_first != 0 || reserved_second != 0 || reserved_third != 0 || reserved_fourth != 0 {
        return Err(ExtendedWebPDecoderError::HeaderInvalid.into());
    }

    let value1 = reader.read_u32::<BigEndian>()?;
    let value2 = u32::from(reader.read_u16::<BigEndian>()?);

    let canvas_width = value1 & 0b11111111_11111111_11111111_00000000;
    let canvas_height = ((value1 & 0b00000000_00000000_00000000_11111111) << 16) | value2;

    let info = WebPExtendedInfo {
        icc_profile,
        alpha,
        exif_metadata,
        xmp_metadata,
        animation,
        canvas_width,
        canvas_height,
    };

    return Ok(info);
}

fn read_extended_chunks(info: WebPExtendedInfo) -> WebPExtendedFrame {
    todo!()
    //iccp chunk
}