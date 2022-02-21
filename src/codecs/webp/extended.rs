use std::{error, fmt};
use std::convert::TryInto;
use std::io::{self, Read};

use crate::{ImageResult, ImageError};
use crate::image::ImageFormat;
use crate::image::GenericImageView;
use crate::color;
use crate::color::Blend;
use crate::Rgba;
use crate::Frames;
use crate::Frame;
use crate::Delay;
use crate::ImageBuffer;
use crate::RgbaImage;
use crate::error::DecodingError;
use super::vp8::{Vp8Decoder, Frame as VP8Frame};
use super::lossless::{LosslessDecoder, LosslessFrame};
use super::decoder::read_len_cursor;
use byteorder::{ReadBytesExt, LittleEndian, BigEndian};

#[derive(Debug, Clone, Copy)]
enum ExtendedWebPDecoderError {
    HeaderInvalid,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub(crate) struct ExtendedImage {
    info: WebPExtendedInfo,
    image: ExtendedImageData,
}

impl ExtendedImage {
    pub(crate) fn dimensions(&self) -> (u32, u32) {
        (self.info.canvas_width, self.info.canvas_height)
    }

    pub(crate) fn color_type(&self) -> color::ColorType {
        if self.info.alpha {
            color::ColorType::Rgba8
        } else {
            color::ColorType::Rgb8
        }
    }

    pub(crate) fn into_frames<'a>(self) -> Frames<'a> {
        struct FrameIterator {
            image: ExtendedImage,
            index: usize,
            width: u32,
            height: u32,
            canvas: RgbaImage,
        }

        impl Iterator for FrameIterator {
            type Item = ImageResult<Frame>;

            fn next(&mut self) -> Option<Self::Item> {
                if let ExtendedImageData::Animation(animated) = &self.image.image {
                    let frame = animated.frames.get(self.index);
                    match frame {
                        Some(anim_image) => {

                            let mut buffer = vec![0; (anim_image.width * anim_image.height * 4) as usize];
                            anim_image.image.fill_buf(&mut buffer);

                            for x in 0..anim_image.width {
                                for y in 0..anim_image.height {
                                    let canvas_index = (x + anim_image.offset_x * 2, y + anim_image.offset_y * 2);
                                    let index = (y * 4 * anim_image.width + x * 4) as usize;
                                    self.canvas[canvas_index] = if anim_image.use_alpha_blending {
                                        let canvas = self.canvas[canvas_index];
                                        let canvas_alpha = f64::from(canvas[3]);
                                        let buffer_alpha = f64::from(buffer[index + 3]);
                                        let blend_alpha_f64 = buffer_alpha + canvas_alpha * (1.0 - buffer_alpha / 255.0);
                                        let blend_alpha = blend_alpha_f64 as u8;

                                        let blend_rgb: [u8; 3] = if blend_alpha == 0 {
                                            [0, 0, 0]
                                        } else {
                                            let mut rgb = [0; 3];
                                            for i in 0..3 {
                                                let canvas_f64 = f64::from(canvas[i]);
                                                let buffer_f64 = f64::from(buffer[index + i]);

                                                let val = (buffer_f64 * buffer_alpha + canvas_f64 * canvas_alpha * (1.0 - buffer_alpha / 255.0)) / blend_alpha_f64;
                                                rgb[i] = val as u8;
                                            }
                                            
                                            rgb
                                        };

                                        Rgba([blend_rgb[0], blend_rgb[1], blend_rgb[2], blend_alpha])
                                    } else {
                                        Rgba([buffer[index], buffer[index + 1], buffer[index + 2], buffer[index + 3]])
                                    };
                                }
                            }


                            let delay = Delay::from_numer_denom_ms(anim_image.duration, 1);
                            let img = self.canvas.clone();
                            let frame = Frame::from_parts(img, 0, 0, delay);

                            if anim_image.dispose {
                                for x in 0..anim_image.width {
                                    for y in 0..anim_image.height {
                                        let canvas_index = (x + anim_image.offset_x * 2, y + anim_image.offset_y * 2);
                                        self.canvas[canvas_index] = animated.background_color;
                                    }
                                }
                            }

                            self.index += 1;
                            
                            Some(Ok(frame))
                        },
                        None => None,
                    }
                } else {
                    None
                }
            }
        }

        let width = self.info.canvas_width;
        let height = self.info.canvas_height;
        let background_color = if let ExtendedImageData::Animation(ref animated) = self.image {
            animated.background_color
        } else {
            Rgba([0, 0, 0, 255])
        };

        let frame_iter = FrameIterator {
            image: self,
            index: 0,
            width,
            height,
            canvas: RgbaImage::from_pixel(width, height, background_color),
        };

        Frames::new(Box::new(frame_iter))
    }
}

fn blend_subimage(image: &mut RgbaImage, anim_frame: &AnimatedFrame) {
    for y in 0..anim_frame.height {
        for x in 0..anim_frame.width {
            let index = y * anim_frame.width + x;
            //TODO: fix
            let rgba = anim_frame.image.get_at_pos(index.try_into().unwrap());

            let img_index = (x + anim_frame.offset_x, y + anim_frame.offset_y);
            image[img_index].blend(&rgba);
        }
    }
}

#[derive(Debug)]
enum ExtendedImageData {
    Animation(WebPAnimated),
    Static(WebPStatic),
}

#[derive(Debug)]
struct WebPStatic {
    alpha: Option<AlphaChunk>,
    frame: SimpleFrame,
}

impl WebPStatic {
    //note if alpha, assumes rgba buffer, else rgb buffer
    pub(crate) fn fill_buf(&self, buf: &mut [u8]) {
        match &self.frame {
            SimpleFrame::Lossy(lossy) => {
                match &self.alpha {
                    Some(alpha_chunk) => {
                        for (index, (val, alpha)) in buf.chunks_exact_mut(4).zip(alpha_chunk.data.iter()).enumerate() {
                            let (r, g, b) = lossy.get_rgb(index);
                            val[0] = r;
                            val[1] = g;
                            val[2] = b;
                            val[3] = *alpha;
                        }
                    },
                    None => lossy.fill_rgb(buf),
                }
            }
            SimpleFrame::Lossless(lossless) => {
                lossless.fill_rgba(buf);
            }
        }
    }

    pub(crate) fn get_at_pos(&self, index: usize) -> Rgba<u8> {
        match &self.frame {
            SimpleFrame::Lossy(lossy) => {
                let (r, g, b) = lossy.get_rgb(index);
                let alpha = match &self.alpha {
                    Some(alpha_chunk) => alpha_chunk.data[index],
                    None => 0,
                };

                Rgba([r, g, b, alpha])
            }
            SimpleFrame::Lossless(lossless) => {
                lossless.get_rgba(index)
            }
        }
    }
}

#[derive(Debug)]
struct WebPAnimated {
    background_color: Rgba<u8>,
    loop_count: u16,
    frames: Vec<AnimatedFrame>,
}

#[derive(Debug)]
struct AnimatedFrame {
    offset_x: u32,
    offset_y: u32,
    width: u32,
    height: u32,
    duration: u32,
    use_alpha_blending: bool,
    dispose: bool,
    image: WebPStatic,
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

    let canvas_width = read_3_bytes(reader)? + 1;
    let canvas_height = read_3_bytes(reader)? + 1;

    println!("width: {canvas_width}, height: {canvas_height}");

    let info = WebPExtendedInfo {
        icc_profile,
        alpha,
        exif_metadata,
        xmp_metadata,
        animation,
        canvas_width,
        canvas_height,
    };

    println!("info: {info:?}");

    return Ok(info);
}

pub(crate) fn read_extended_chunks<R: Read>(info: WebPExtendedInfo, reader: &mut R) -> ImageResult<ExtendedImage> {
    if info.icc_profile {
        //ignore iccp
        ignore_chunk(reader, b"ICCP")?;
    }

    let image = if info.animation {
        //read animation
        let anim = read_animation(reader)?;
        ExtendedImageData::Animation(anim)
    } else if info.alpha {
        todo!()
    } else {
        todo!()
    };

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

    let image = ExtendedImage {
        info,
        image,
    };

    Ok(image)
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

fn read_animation<R: Read>(reader: &mut R) -> ImageResult<WebPAnimated> {
    //read anim chunk
    let mut chunk: [u8; 4] = [0; 4];

    reader.read_exact(&mut chunk)?;

    if &chunk != b"ANIM" {
        println!("anim chunk invalid: {:?}", chunk);
        return Err(ExtendedWebPDecoderError::HeaderInvalid.into());
    }

    let _len = reader.read_u32::<LittleEndian>()?;

    let mut background_color: [u8; 4] = [0; 4];
    reader.read_exact(&mut background_color)?;

    let background_color = Rgba(background_color);
    
    let loop_count = reader.read_u16::<LittleEndian>()?;

    let mut frames = Vec::new();

    loop {
        let mut chunk: [u8; 4] = [0; 4];
        let _ = reader.read_exact(&mut chunk);

        if &chunk == b"ANMF" {
            let len = reader.read_u32::<LittleEndian>()?;
            println!("ANMF len: {len}");
            frames.push(read_anim_frame(reader)?);
        } else {
            println!("num of frames: {}", frames.len());
            let animated = WebPAnimated {
                background_color,
                loop_count,
                frames,
            };

            return Ok(animated);
        }
    }
}

fn read_anim_frame<R: Read>(reader: &mut R) -> ImageResult<AnimatedFrame> {

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

    //read normal bitstream now
    let static_image = read_image(reader, frame_width, frame_height)?;

    let frame = AnimatedFrame {
        offset_x: frame_x,
        offset_y: frame_y,
        width: frame_width,
        height: frame_height,
        duration,
        use_alpha_blending,
        dispose,
        image: static_image,
    };

    println!("frame: x: {}, y: {}, width: {}, height: {}, duration: {}, alpha_blending: {}, dispose: {}", frame_x, frame_y, frame_width, frame_height, duration, use_alpha_blending, dispose);

    Ok(frame)
}

fn read_3_bytes<R: Read>(reader: &mut R) -> ImageResult<u32> {
    let mut buffer: [u8; 3] = [0; 3];
    reader.read_exact(&mut buffer)?;
    let value: u32 = (u32::from(buffer[2]) << 16) | (u32::from(buffer[1]) << 8) | u32::from(buffer[0]);
    Ok(value)
}

fn read_image<R: Read>(reader: &mut R, width: u32, height: u32) -> ImageResult<WebPStatic> {
    let mut chunk = [0; 4];
    reader.read_exact(&mut chunk)?;

    match &chunk {
        b"VP8 " => {
            let len = reader.read_u32::<LittleEndian>()?;

            println!("vp8 len: {len}");

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
            let cursor = read_len_cursor(reader)?;

            let mut lossless_decoder = LosslessDecoder::new(cursor);
            let frame = lossless_decoder.decode_frame()?;

            let frame = SimpleFrame::Lossless(frame.clone());

            let img = WebPStatic {
                alpha: None,
                frame,
            };

            Ok(img)
        }
        b"ALPH" => {
            let alpha = read_alpha_chunk(reader, width, height)?;

            let mut chunk = [0; 4];
            reader.read_exact(&mut chunk)?;

            if &chunk != b"VP8 " {
                return Err(ExtendedWebPDecoderError::HeaderInvalid.into());
            }

            let len = reader.read_u32::<LittleEndian>()?;

            println!("vp8 with alpha len: {len}");

            let mut vp8_decoder = Vp8Decoder::new(reader);
            let frame = vp8_decoder.decode_frame()?;

            let frame = SimpleFrame::Lossy(frame.clone());

            let img = WebPStatic {
                alpha: Some(alpha),
                frame,
            };

            Ok(img)
        }
        _ => {
            println!("this chunk invalid: {:?}", chunk);
            Err(ExtendedWebPDecoderError::HeaderInvalid.into())
        }
    }
}

#[derive(Debug)]
struct AlphaChunk {
    preprocessing: bool,
    filtering_method: FilteringMethod,
    lossless_compression: bool,
    data: Vec<u8>,
}

#[derive(Debug)]
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

    println!("alph len: {len}");

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
        //fix both casts
        let frame = decoder.decode_frame_implicit_dims(width as u16, height as u16)?;

        //TODO: change to proper cast
        let mut data = vec![0; (width * height) as usize];

        frame.fill_green(&mut data);

        data
    } else {
        framedata
    };

    let chunk = AlphaChunk {
        preprocessing,
        filtering_method,
        lossless_compression,
        data,
    };

    Ok(chunk)
}