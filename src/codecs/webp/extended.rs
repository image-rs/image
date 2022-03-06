use std::{error, fmt};
use std::convert::TryInto;
use std::io::{self, Read, Error};

use crate::{ImageResult, ImageError};
use crate::image::ImageFormat;
use crate::color;
use crate::color::Blend;
use crate::Rgba;
use crate::Frames;
use crate::Frame;
use crate::Delay;
use crate::RgbaImage;
use crate::error::DecodingError;
use super::vp8::{Vp8Decoder, Frame as VP8Frame};
use super::lossless::{LosslessDecoder, LosslessFrame};
use super::decoder::{read_len_cursor, read_chunk, WebPRiffChunk};
use byteorder::{ReadBytesExt, LittleEndian};

#[derive(Debug, Clone, Copy)]
enum ExtendedWebPDecoderError {
    HeaderInvalid,
}

#[derive(Debug)]
enum WebPStatic {
    Lossy {
        frame: VP8Frame,
        alpha: Option<AlphaChunk>,
    },
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
enum ExtendedImageData {
    Animation {
        frames: Vec<AnimatedFrame>,
        anim_info: WebPAnimatedInfo,
    },
    Static(WebPStatic),
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
                if let ExtendedImageData::Animation{ frames, anim_info } = &self.image.image {
                    let frame = frames.get(self.index);
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
                                        self.canvas[canvas_index] = anim_info.background_color;
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
        let background_color = if let ExtendedImageData::Animation{ ref anim_info, .. } = self.image {
            anim_info.background_color
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

    pub(crate) fn read_extended_chunks<R: Read>(reader: &mut R, info: WebPExtendedInfo) -> ImageResult<ExtendedImage> {
        let mut anim_info: Option<WebPAnimatedInfo> = None;
        let mut anim_frames: Vec<AnimatedFrame> = Vec::new();
        let mut static_frame: Option<WebPStatic> = None;

        //go until end of file
        while let Some((mut cursor, chunk)) = read_chunk(reader)? {
            match chunk {
                WebPRiffChunk::ICCP |
                WebPRiffChunk::EXIF |
                WebPRiffChunk::XMP => {
                    //ignore these chunks
                }
                WebPRiffChunk::ANIM => {
                    if anim_info.is_none() {
                        anim_info = Some(Self::read_anim_info(&mut cursor)?);
                    }
                }
                WebPRiffChunk::ANMF => {
                    let frame = read_anim_frame(cursor)?;
                    anim_frames.push(frame);
                }
                WebPRiffChunk::ALPH => {
                    if static_frame.is_none() {
                        //reads alpha chunk
                        let alpha_chunk = read_alpha_chunk(&mut cursor, info.canvas_width, info.canvas_height)?;

                        let vp8_frame = read_lossy(reader)?;

                        let img = WebPStatic::Lossy {
                            alpha: Some(alpha_chunk),
                            frame: vp8_frame,
                        };

                        static_frame = Some(img);
                    }
                }
                _ => return Err(ExtendedWebPDecoderError::HeaderInvalid.into()),
            }
        }

        let image = if let Some(info) = anim_info {
            ExtendedImageData::Animation {
                frames: anim_frames,
                anim_info: info,
            }
        } else if let Some(frame) = static_frame {
            ExtendedImageData::Static(frame)
        } else {
            return Err(ExtendedWebPDecoderError::HeaderInvalid.into());
        };

        let image = ExtendedImage {
            image,
            info,
        };

        Ok(image)
    }

    fn read_anim_info<R: Read>(reader: &mut R) -> ImageResult<WebPAnimatedInfo> {
        let mut background_color: [u8; 4] = [0; 4];
        reader.read_exact(&mut background_color)?;

        let background_color = Rgba(background_color);
        
        let loop_count = reader.read_u16::<LittleEndian>()?;

        let info = WebPAnimatedInfo {
            background_color,
            loop_count,
        };

        Ok(info)
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

impl WebPStatic {

    //note if alpha, assumes rgba buffer, else rgb buffer
    pub(crate) fn fill_buf(&self, buf: &mut [u8]) {
        match self {
            WebPStatic::Lossy{ frame, alpha } => {
                match alpha {
                    Some(alpha_chunk) => {
                        for (index, (val, alpha)) in buf.chunks_exact_mut(4).zip(alpha_chunk.data.iter()).enumerate() {
                            let (r, g, b) = frame.get_rgb(index);
                            val[0] = r;
                            val[1] = g;
                            val[2] = b;
                            val[3] = *alpha;
                        }
                    },
                    None => frame.fill_rgb(buf),
                }
            }
            WebPStatic::Lossless(lossless) => {
                lossless.fill_rgba(buf);
            }
        }
    }

    pub(crate) fn get_at_pos(&self, index: usize) -> Rgba<u8> {
        match self {
            WebPStatic::Lossy{ frame, alpha } => {
                let (r, g, b) = frame.get_rgb(index);
                let alpha = match alpha {
                    Some(alpha_chunk) => alpha_chunk.data[index],
                    None => 0,
                };

                Rgba([r, g, b, alpha])
            }
            WebPStatic::Lossless(lossless) => {
                lossless.get_rgba(index)
            }
        }
    }
}

#[derive(Debug)]
struct WebPAnimatedInfo {
    background_color: Rgba<u8>,
    loop_count: u16,
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

fn read_anim_frame<R: Read>(mut reader: R) -> ImageResult<AnimatedFrame> {

    let frame_x = read_3_bytes(&mut reader)?;
    let frame_y = read_3_bytes(&mut reader)?;

    let frame_width = read_3_bytes(&mut reader)? + 1;
    let frame_height = read_3_bytes(&mut reader)? + 1;
    let duration = read_3_bytes(&mut reader)?;

    let frame_info = reader.read_u8()?;
    let reserved = frame_info & 0b11111100;
    if reserved != 0 {
        return Err(ExtendedWebPDecoderError::HeaderInvalid.into());
    }
    let use_alpha_blending = frame_info & 0b00000010 == 0;
    let dispose = frame_info & 0b00000001 != 0;

    //read normal bitstream now
    let static_image = read_image(&mut reader, frame_width, frame_height)?;

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

    Ok(frame)
}

fn read_3_bytes<R: Read>(reader: &mut R) -> ImageResult<u32> {
    let mut buffer: [u8; 3] = [0; 3];
    reader.read_exact(&mut buffer)?;
    let value: u32 = (u32::from(buffer[2]) << 16) | (u32::from(buffer[1]) << 8) | u32::from(buffer[0]);
    Ok(value)
}

fn read_lossy<R: Read>(reader: &mut R) -> ImageResult<VP8Frame> {

    let (cursor, chunk) = read_chunk(reader)?.ok_or(Error::from(io::ErrorKind::UnexpectedEof))?;

    if chunk != WebPRiffChunk::VP8 {
        return Err(ExtendedWebPDecoderError::HeaderInvalid.into());
    }

    let mut vp8_decoder = Vp8Decoder::new(cursor);
    let frame = vp8_decoder.decode_frame()?;

    Ok(frame.clone())
}

fn read_image<R: Read>(reader: &mut R, width: u32, height: u32) -> ImageResult<WebPStatic> {
    let chunk = read_chunk(reader)?;

    match chunk {
        Some((cursor, WebPRiffChunk::VP8)) => {
            let mut vp8_decoder = Vp8Decoder::new(cursor);
            let frame = vp8_decoder.decode_frame()?;

            let img = WebPStatic::Lossy {
                frame: frame.clone(),
                alpha: None,
            };

            Ok(img)
        }
        Some((cursor, WebPRiffChunk::VP8L)) => {
            let mut lossless_decoder = LosslessDecoder::new(cursor);
            let frame = lossless_decoder.decode_frame()?;

            let img = WebPStatic::Lossless(frame.clone());

            Ok(img)
        }
        Some((mut cursor, WebPRiffChunk::ALPH)) => {
            let alpha_chunk = read_alpha_chunk(&mut cursor, width, height)?;

            let vp8_frame = read_lossy(reader)?;

            let img = WebPStatic::Lossy {
                alpha: Some(alpha_chunk),
                frame: vp8_frame,
            };

            Ok(img)
        }
        _ => {
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
    reader.read_to_end(&mut framedata)?;
    
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