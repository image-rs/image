use std::{error, fmt};
use std::io::{self, Read};

use crate::{ImageResult, ImageError};
use crate::image::ImageFormat;
use crate::Rgba;
use crate::error::DecodingError;
use super::vp8::{Vp8Decoder, Frame as VP8Frame};
use super::lossless::{LosslessDecoder, LosslessFrame};
use byteorder::{ReadBytesExt, LittleEndian, BigEndian};

#[derive(Debug, Clone, Copy)]
enum ExtendedWebPDecoderError {
    HeaderInvalid,
}

enum SimpleFrame {
    Lossy(VP8Frame),
    Lossless(LosslessFrame),
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

pub(crate) struct WebPExtendedImage {
    info: WebPExtendedInfo,
    frame: ExtendedFrame,
}

enum ExtendedFrame {
    Animation(WebPAnimated),
    Static(WebPStatic),
}

struct WebPStatic {
    alpha: Option<AlphaChunk>,
    frame: SimpleFrame,
}

struct WebPAnimated {
    background_color: Rgba<u8>,
    loop_count: u16,
    frames: Vec<WebPStatic>,
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

    let mut reserved_third: [u8; 3] = [0; 3];
    reader.read_exact(&mut reserved_third)?;

    if reserved_first != 0 || reserved_second != 0 || reserved_third != [0, 0, 0] {
        return Err(ExtendedWebPDecoderError::HeaderInvalid.into());
    }

    let canvas_width = read_3_bytes(reader)?;
    let canvas_height = read_3_bytes(reader)?;

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

pub(crate) fn read_extended_chunks<R: Read>(info: WebPExtendedInfo, reader: &mut R) -> ImageResult<WebPExtendedFrame> {
    if info.icc_profile {
        //ignore iccp
        ignore_chunk(reader, b"ICCP")?;
        println!("ignored iccp");
    }

    if info.animation {
        //read animation
        read_animation(reader)?;
    }

    //image data

    if info.exif_metadata {
        //ignore exif metadat
        ignore_chunk(reader, b"EXIF")?;
        println!("ignored exif");
    }

    if info.xmp_metadata {
        //ignore xmp metadata
        ignore_chunk(reader, b"XMP ")?;
        println!("ignored xmp");
    }

    todo!()
}

fn ignore_chunk<R: Read>(reader: &mut R, chunk_fourcc: &[u8; 4]) -> ImageResult<()> {
    let mut chunk: [u8; 4] = [0; 4];
    reader.read_exact(&mut chunk)?;

    if &chunk != chunk_fourcc {
        return Err(ExtendedWebPDecoderError::HeaderInvalid.into());
    }

    let mut len = u64::from(reader.read_u32::<LittleEndian>()?);

    if len % 2 != 0 {
        // RIFF chunks containing an uneven number of bytes append
        // an extra 0x00 at the end of the chunk
        //
        // The addition cannot overflow since we have a u64 that was created from a u32
        len += 1;
    }
    
    io::copy(&mut reader.by_ref().take(len), &mut io::sink())?;

    Ok(())
}

fn read_animation<R: Read>(reader: &mut R) -> ImageResult<()> {
    //read anim chunk
    let mut chunk: [u8; 4] = [0; 4];

    reader.read_exact(&mut chunk)?;

    if &chunk != b"ANIM" {
        return Err(ExtendedWebPDecoderError::HeaderInvalid.into());
    }

    let _len = reader.read_u32::<LittleEndian>()?;

    let _background_color = reader.read_u32::<LittleEndian>()?;
    
    let loop_count = reader.read_u16::<LittleEndian>()?;

    read_anim_frame(reader)?;


    Ok(())
}

fn read_anim_frame<R: Read>(reader: &mut R) -> ImageResult<SimpleFrame> {
    let mut chunk: [u8; 4] = [0; 4];
    reader.read_exact(&mut chunk)?;

    if &chunk != b"ANMF" {
        return Err(ExtendedWebPDecoderError::HeaderInvalid.into());
    }

    let frame_x = read_3_bytes(reader)?;
    let frame_y = read_3_bytes(reader)?;

    let frame_width = read_3_bytes(reader)? + 1;
    let frame_height = read_3_bytes(reader)? + 1;
    let duration = read_3_bytes(reader)?;

    let frame_info = reader.read_u8()?;
    let reserved = frame_info & 0b11111100;
    if reserved != 0 {
        return Err(ExtendedWebPDecoderError::HeaderInvalid.into());
    }
    let use_alpha_blending = frame_info & 0b00000010 == 0;
    let dispose = frame_info & 0b00000001 != 0;

    println!("{frame_x}, {frame_y}, {frame_width}, {frame_height}, {duration}");

    //read normal bitstream now
}

fn read_3_bytes<R: Read>(reader: &mut R) -> ImageResult<u32> {
    let mut buffer: [u8; 3] = [0; 3];
    reader.read_exact(&mut buffer)?;
    let value: u32 = (u32::from(buffer[0]) << 16) | (u32::from(buffer[1]) << 8) | u32::from(buffer[2]);
    Ok(value)
}

fn read_image<R: Read>(reader: &mut R) -> WebPStatic {
    let mut chunk = [0; 4];
    reader.read_exact(&mut chunk)?;

    match &chunk {
        b"VP8 " => {
            let len = reader.read_u32::<LittleEndian>()?;

            let mut vp8_decoder = Vp8Decoder::new(reader);
            let frame = vp8_decoder.decode_frame()?;

            let frame = SimpleFrame::Lossy(frame.clone());

            let img = WebPStatic {
                alpha: None,
                frame,
            };

            Ok(img)
        }
        b"VP8L" => {
            let len = reader.read_u32::<LittleEndian>()?;

            let mut lossless_decoder = LosslessDecoder::new(reader);
            let frame = lossless_decoder.decode_frame()?;

            let frame = SimpleFrame::Lossless(frame.clone());

            let img = WebPStatic {
                alpha: None,
                frame,
            };

            Ok(img)
        }
        b"ALPH" => {
            let alpha = read_alpha_chunk(reader);

            let mut chunk = [0; 4];
            reader.read_exact(&mut chunk)?;

            if chunk != b"VP8 " {
                return Err(ExtendedWebPDecoderError::HeaderInvalid.into());
            }

            let len = reader.read_u32::<LittleEndian>()?;

            let mut vp8_decoder = Vp8Decoder::new(reader);
            let frame = vp8_decoder.decode_frame()?;

            let frame = SimpleFrame::Lossy(frame.clone());

            let img = WebPStatic {
                alpha,
                frame,
            };

            Ok(img)
        }
        _ => Err(ExtendedWebPDecoderError::HeaderInvalid.into())
    }
}

struct AlphaChunk {
    preprocessing: bool,
    filtering_method: FilteringMethod,
    lossless_compression: bool,
    data: Vec<u8>,
}

enum FilteringMethod {
    None,
    Horizontal,
    Vertical,
    Gradient,
}

impl FilteringMethod {
    fn from_num(num: u8) -> Self {
        match num {
            0 => FilteringMethod::None,
            1 => FilteringMethod::Horizontal,
            2 => FilteringMethod::Vertical,
            3 => FilteringMethod::Gradient,
            _ => panic!("Invalid filter for alpha chunk in WebP image, should be unreachable"),
        }
    }
}

//note only for VP8 frames
fn read_alpha_chunk<R: Read>(reader: &mut R, width: u32, height: u32) -> ImageResult<AlphaChunk> {

    //TODO: if odd, add 1
    let len = reader.read_u32::<LittleEndian>()?;

    let info_byte = reader.read_u8()?;

    let reserved = info_byte & 0b11000000;
    let preprocessing = (info_byte & 0b00110000) >> 4;
    let filtering = (info_byte & 0b00001100) >> 2;
    let compression = info_byte & 0b00000011;

    if reserved != 0 {
        return Err(ExtendedWebPDecoderError::HeaderInvalid.into());
    }

    let preprocessing = match preprocessing {
        0 => false,
        1 => true,
        _ => return Err(ExtendedWebPDecoderError::HeaderInvalid.into()),
    };

    let filtering_method = FilteringMethod::from_num(filtering);

    let lossless_compression = match compression {
        0 => false,
        1 => true,
        _ => return Err(ExtendedWebPDecoderError::HeaderInvalid.into()),
    };

    let mut framedata = Vec::new();
    reader.take(len as u64).read_to_end(&mut framedata)?;
    
    let data = if lossless_compression {
        let cursor = std::io::Cursor::new(framedata);

        let mut decoder = LosslessDecoder::new(cursor);
        let frame = decoder.decode_frame_implicit_dims()?;

        //TODO: change to proper cast
        let mut data = vec![0; (width * height) as usize];

        frame.fill_green(&mut data)

        frame
    } else {
        framedata
    };

    let chunk = AlphaChunk {
        preprocessing,
        filtering_method,
        lossless_compression,
        data,
    }

    Ok(chunk)
}