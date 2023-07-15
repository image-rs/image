use std::convert::TryInto;
use std::io::{self, Cursor, Error, Read};
use std::{error, fmt};

use super::decoder::{
    read_chunk, read_fourcc, read_len_cursor, DecoderError::ChunkHeaderInvalid, WebPRiffChunk,
};
use super::lossless::{LosslessDecoder, LosslessFrame};
use super::vp8::{Frame as VP8Frame, Vp8Decoder};
use crate::error::{DecodingError, ParameterError, ParameterErrorKind};
use crate::image::ImageFormat;
use crate::{
    ColorType, Delay, Frame, Frames, ImageError, ImageResult, Rgb, RgbImage, Rgba, RgbaImage,
};
use byteorder::{LittleEndian, ReadBytesExt};

//all errors that can occur while parsing extended chunks in a WebP file
#[derive(Debug, Clone, Copy)]
enum DecoderError {
    // Some bits were invalid
    InfoBitsInvalid { name: &'static str, value: u32 },
    // Alpha chunk doesn't match the frame's size
    AlphaChunkSizeMismatch,
    // Image is too large, either for the platform's pointer size or generally
    ImageTooLarge,
    // Frame would go out of the canvas
    FrameOutsideImage,
}

impl fmt::Display for DecoderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DecoderError::InfoBitsInvalid { name, value } => f.write_fmt(format_args!(
                "Info bits `{}` invalid, received value: {}",
                name, value
            )),
            DecoderError::AlphaChunkSizeMismatch => {
                f.write_str("Alpha chunk doesn't match the size of the frame")
            }
            DecoderError::ImageTooLarge => f.write_str("Image is too large to be decoded"),
            DecoderError::FrameOutsideImage => {
                f.write_str("Frame is too large and would go outside the image")
            }
        }
    }
}

impl From<DecoderError> for ImageError {
    fn from(e: DecoderError) -> ImageError {
        ImageError::Decoding(DecodingError::new(ImageFormat::WebP.into(), e))
    }
}

impl error::Error for DecoderError {}

#[derive(Debug, Clone)]
pub(crate) struct WebPExtendedInfo {
    _icc_profile: bool,
    _alpha: bool,
    _exif_metadata: bool,
    _xmp_metadata: bool,
    _animation: bool,
    canvas_width: u32,
    canvas_height: u32,
    icc_profile: Option<Vec<u8>>,
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

    pub(crate) fn has_animation(&self) -> bool {
        self.info._animation
    }

    pub(crate) fn icc_profile(&self) -> Option<Vec<u8>> {
        self.info.icc_profile.clone()
    }

    pub(crate) fn color_type(&self) -> ColorType {
        match &self.image {
            ExtendedImageData::Animation { frames, .. } => &frames[0].image,
            ExtendedImageData::Static(image) => image,
        }
        .color_type()
    }

    pub(crate) fn into_frames<'a>(self) -> Frames<'a> {
        struct FrameIterator {
            image: ExtendedImage,
            index: usize,
            canvas: RgbaImage,
        }

        impl Iterator for FrameIterator {
            type Item = ImageResult<Frame>;

            fn next(&mut self) -> Option<Self::Item> {
                if let ExtendedImageData::Animation { frames, anim_info } = &self.image.image {
                    let frame = frames.get(self.index);
                    match frame {
                        Some(anim_image) => {
                            self.index += 1;
                            ExtendedImage::draw_subimage(
                                &mut self.canvas,
                                anim_image,
                                anim_info.background_color,
                            )
                        }
                        None => None,
                    }
                } else {
                    None
                }
            }
        }

        let width = self.info.canvas_width;
        let height = self.info.canvas_height;
        let background_color =
            if let ExtendedImageData::Animation { ref anim_info, .. } = self.image {
                anim_info.background_color
            } else {
                Rgba([0, 0, 0, 0])
            };

        let frame_iter = FrameIterator {
            image: self,
            index: 0,
            canvas: RgbaImage::from_pixel(width, height, background_color),
        };

        Frames::new(Box::new(frame_iter))
    }

    pub(crate) fn read_extended_chunks<R: Read>(
        reader: &mut R,
        mut info: WebPExtendedInfo,
    ) -> ImageResult<ExtendedImage> {
        let mut anim_info: Option<WebPAnimatedInfo> = None;
        let mut anim_frames: Vec<AnimatedFrame> = Vec::new();
        let mut static_frame: Option<WebPStatic> = None;
        //go until end of file and while chunk headers are valid
        while let Some((mut cursor, chunk)) = read_extended_chunk(reader)? {
            match chunk {
                WebPRiffChunk::EXIF | WebPRiffChunk::XMP => {
                    //ignore these chunks
                }
                WebPRiffChunk::ANIM => {
                    if anim_info.is_none() {
                        anim_info = Some(Self::read_anim_info(&mut cursor)?);
                    }
                }
                WebPRiffChunk::ANMF => {
                    let frame = read_anim_frame(cursor, info.canvas_width, info.canvas_height)?;
                    anim_frames.push(frame);
                }
                WebPRiffChunk::ALPH => {
                    if static_frame.is_none() {
                        let alpha_chunk =
                            read_alpha_chunk(&mut cursor, info.canvas_width, info.canvas_height)?;

                        let vp8_frame = read_lossy_with_chunk(reader)?;

                        let img = WebPStatic::from_alpha_lossy(alpha_chunk, vp8_frame)?;

                        static_frame = Some(img);
                    }
                }
                WebPRiffChunk::ICCP => {
                    let mut icc_profile = Vec::new();
                    cursor.read_to_end(&mut icc_profile)?;
                    info.icc_profile = Some(icc_profile);
                }
                WebPRiffChunk::VP8 => {
                    if static_frame.is_none() {
                        let vp8_frame = read_lossy(cursor)?;

                        let img = WebPStatic::from_lossy(vp8_frame)?;

                        static_frame = Some(img);
                    }
                }
                WebPRiffChunk::VP8L => {
                    if static_frame.is_none() {
                        let mut lossless_decoder = LosslessDecoder::new(cursor);
                        let frame = lossless_decoder.decode_frame()?;
                        let image = WebPStatic::Lossless(frame.clone());

                        static_frame = Some(image);
                    }
                }
                _ => return Err(ChunkHeaderInvalid(chunk.to_fourcc()).into()),
            }
        }

        let image = if let Some(info) = anim_info {
            if anim_frames.is_empty() {
                return Err(ImageError::IoError(Error::from(
                    io::ErrorKind::UnexpectedEof,
                )));
            }
            ExtendedImageData::Animation {
                frames: anim_frames,
                anim_info: info,
            }
        } else if let Some(frame) = static_frame {
            ExtendedImageData::Static(frame)
        } else {
            //reached end of file too early before image data was reached
            return Err(ImageError::IoError(Error::from(
                io::ErrorKind::UnexpectedEof,
            )));
        };

        let image = ExtendedImage { image, info };

        Ok(image)
    }

    fn read_anim_info<R: Read>(reader: &mut R) -> ImageResult<WebPAnimatedInfo> {
        let mut colors: [u8; 4] = [0; 4];
        reader.read_exact(&mut colors)?;

        //background color is [blue, green, red, alpha]
        let background_color = Rgba([colors[2], colors[1], colors[0], colors[3]]);

        let loop_count = reader.read_u16::<LittleEndian>()?;

        let info = WebPAnimatedInfo {
            background_color,
            _loop_count: loop_count,
        };

        Ok(info)
    }

    fn draw_subimage(
        canvas: &mut RgbaImage,
        anim_image: &AnimatedFrame,
        background_color: Rgba<u8>,
    ) -> Option<ImageResult<Frame>> {
        let mut buffer = vec![0; anim_image.image.get_buf_size()];
        anim_image.image.fill_buf(&mut buffer);
        let has_alpha = anim_image.image.has_alpha();
        let pixel_len: u32 = anim_image.image.color_type().bytes_per_pixel().into();

        'x: for x in 0..anim_image.width {
            for y in 0..anim_image.height {
                let canvas_index: (u32, u32) = (x + anim_image.offset_x, y + anim_image.offset_y);
                // Negative offsets are not possible due to unsigned ints
                // If we go out of bounds by height, still continue by x
                if canvas_index.1 >= canvas.height() {
                    continue 'x;
                }
                // If we go out of bounds by width, it doesn't make sense to continue at all
                if canvas_index.0 >= canvas.width() {
                    break 'x;
                }
                let index: usize = ((y * anim_image.width + x) * pixel_len).try_into().unwrap();
                canvas[canvas_index] = if anim_image.use_alpha_blending && has_alpha {
                    let buffer: [u8; 4] = buffer[index..][..4].try_into().unwrap();
                    ExtendedImage::do_alpha_blending(buffer, canvas[canvas_index])
                } else {
                    Rgba([
                        buffer[index],
                        buffer[index + 1],
                        buffer[index + 2],
                        if has_alpha { buffer[index + 3] } else { 255 },
                    ])
                };
            }
        }

        let delay = Delay::from_numer_denom_ms(anim_image.duration, 1);
        let img = canvas.clone();
        let frame = Frame::from_parts(img, 0, 0, delay);

        if anim_image.dispose {
            for x in 0..anim_image.width {
                for y in 0..anim_image.height {
                    let canvas_index = (x + anim_image.offset_x, y + anim_image.offset_y);
                    canvas[canvas_index] = background_color;
                }
            }
        }

        Some(Ok(frame))
    }

    fn do_alpha_blending(buffer: [u8; 4], canvas: Rgba<u8>) -> Rgba<u8> {
        let canvas_alpha = f64::from(canvas[3]);
        let buffer_alpha = f64::from(buffer[3]);
        let blend_alpha_f64 = buffer_alpha + canvas_alpha * (1.0 - buffer_alpha / 255.0);
        //value should be between 0 and 255, this truncates the fractional part
        let blend_alpha: u8 = blend_alpha_f64 as u8;

        let blend_rgb: [u8; 3] = if blend_alpha == 0 {
            [0, 0, 0]
        } else {
            let mut rgb = [0u8; 3];
            for i in 0..3 {
                let canvas_f64 = f64::from(canvas[i]);
                let buffer_f64 = f64::from(buffer[i]);

                let val = (buffer_f64 * buffer_alpha
                    + canvas_f64 * canvas_alpha * (1.0 - buffer_alpha / 255.0))
                    / blend_alpha_f64;
                //value should be between 0 and 255, this truncates the fractional part
                rgb[i] = val as u8;
            }

            rgb
        };

        Rgba([blend_rgb[0], blend_rgb[1], blend_rgb[2], blend_alpha])
    }

    pub(crate) fn fill_buf(&self, buf: &mut [u8]) {
        match &self.image {
            // will always have at least one frame
            ExtendedImageData::Animation { frames, anim_info } => {
                let first_frame = &frames[0];
                let (canvas_width, canvas_height) = self.dimensions();
                if canvas_width == first_frame.width && canvas_height == first_frame.height {
                    first_frame.image.fill_buf(buf);
                } else {
                    let bg_color = match &self.info._alpha {
                        true => Rgba::from([0, 0, 0, 0]),
                        false => anim_info.background_color,
                    };
                    let mut canvas = RgbaImage::from_pixel(canvas_width, canvas_height, bg_color);
                    let _ = ExtendedImage::draw_subimage(&mut canvas, first_frame, bg_color)
                        .unwrap()
                        .unwrap();
                    buf.copy_from_slice(canvas.into_raw().as_slice());
                }
            }
            ExtendedImageData::Static(image) => {
                image.fill_buf(buf);
            }
        }
    }

    pub(crate) fn get_buf_size(&self) -> usize {
        match &self.image {
            // will always have at least one frame
            ExtendedImageData::Animation { frames, .. } => &frames[0].image,
            ExtendedImageData::Static(image) => image,
        }
        .get_buf_size()
    }

    pub(crate) fn set_background_color(&mut self, color: Rgba<u8>) -> ImageResult<()> {
        match &mut self.image {
            ExtendedImageData::Animation { anim_info, .. } => {
                anim_info.background_color = color;
                Ok(())
            }
            _ => Err(ImageError::Parameter(ParameterError::from_kind(
                ParameterErrorKind::Generic(
                    "Background color can only be set on animated webp".to_owned(),
                ),
            ))),
        }
    }
}

#[derive(Debug)]
enum WebPStatic {
    LossyWithAlpha(RgbaImage),
    LossyWithoutAlpha(RgbImage),
    Lossless(LosslessFrame),
}

impl WebPStatic {
    pub(crate) fn from_alpha_lossy(
        alpha: AlphaChunk,
        vp8_frame: VP8Frame,
    ) -> ImageResult<WebPStatic> {
        if alpha.data.len() != usize::from(vp8_frame.width) * usize::from(vp8_frame.height) {
            return Err(DecoderError::AlphaChunkSizeMismatch.into());
        }

        let size = usize::from(vp8_frame.width).checked_mul(usize::from(vp8_frame.height) * 4);
        let mut image_vec = match size {
            Some(size) => vec![0u8; size],
            None => return Err(DecoderError::ImageTooLarge.into()),
        };

        vp8_frame.fill_rgba(&mut image_vec);

        for y in 0..vp8_frame.height {
            for x in 0..vp8_frame.width {
                let predictor: u8 = WebPStatic::get_predictor(
                    x.into(),
                    y.into(),
                    vp8_frame.width.into(),
                    alpha.filtering_method,
                    &image_vec,
                );
                let predictor = u16::from(predictor);

                let alpha_index = usize::from(y) * usize::from(vp8_frame.width) + usize::from(x);
                let alpha_val = alpha.data[alpha_index];
                let alpha: u8 = ((predictor + u16::from(alpha_val)) % 256)
                    .try_into()
                    .unwrap();

                let alpha_index = alpha_index * 4 + 3;
                image_vec[alpha_index] = alpha;
            }
        }

        let image = RgbaImage::from_vec(vp8_frame.width.into(), vp8_frame.height.into(), image_vec)
            .unwrap();

        Ok(WebPStatic::LossyWithAlpha(image))
    }

    fn get_predictor(
        x: usize,
        y: usize,
        width: usize,
        filtering_method: FilteringMethod,
        image_slice: &[u8],
    ) -> u8 {
        match filtering_method {
            FilteringMethod::None => 0,
            FilteringMethod::Horizontal => {
                if x == 0 && y == 0 {
                    0
                } else if x == 0 {
                    let index = (y - 1) * width + x;
                    image_slice[index * 4 + 3]
                } else {
                    let index = y * width + x - 1;
                    image_slice[index * 4 + 3]
                }
            }
            FilteringMethod::Vertical => {
                if x == 0 && y == 0 {
                    0
                } else if y == 0 {
                    let index = y * width + x - 1;
                    image_slice[index * 4 + 3]
                } else {
                    let index = (y - 1) * width + x;
                    image_slice[index * 4 + 3]
                }
            }
            FilteringMethod::Gradient => {
                let (left, top, top_left) = match (x, y) {
                    (0, 0) => (0, 0, 0),
                    (0, y) => {
                        let above_index = (y - 1) * width + x;
                        let val = image_slice[above_index * 4 + 3];
                        (val, val, val)
                    }
                    (x, 0) => {
                        let before_index = y * width + x - 1;
                        let val = image_slice[before_index * 4 + 3];
                        (val, val, val)
                    }
                    (x, y) => {
                        let left_index = y * width + x - 1;
                        let left = image_slice[left_index * 4 + 3];
                        let top_index = (y - 1) * width + x;
                        let top = image_slice[top_index * 4 + 3];
                        let top_left_index = (y - 1) * width + x - 1;
                        let top_left = image_slice[top_left_index * 4 + 3];

                        (left, top, top_left)
                    }
                };

                let combination = i16::from(left) + i16::from(top) - i16::from(top_left);
                i16::clamp(combination, 0, 255).try_into().unwrap()
            }
        }
    }

    pub(crate) fn from_lossy(vp8_frame: VP8Frame) -> ImageResult<WebPStatic> {
        let mut image = RgbImage::from_pixel(
            vp8_frame.width.into(),
            vp8_frame.height.into(),
            Rgb([0, 0, 0]),
        );

        vp8_frame.fill_rgb(&mut image);

        Ok(WebPStatic::LossyWithoutAlpha(image))
    }

    pub(crate) fn fill_buf(&self, buf: &mut [u8]) {
        match self {
            WebPStatic::LossyWithAlpha(image) => {
                buf.copy_from_slice(image);
            }
            WebPStatic::LossyWithoutAlpha(image) => {
                buf.copy_from_slice(image);
            }
            WebPStatic::Lossless(lossless) => {
                lossless.fill_rgba(buf);
            }
        }
    }

    pub(crate) fn get_buf_size(&self) -> usize {
        match self {
            WebPStatic::LossyWithAlpha(rgb_image) => rgb_image.len(),
            WebPStatic::LossyWithoutAlpha(rgba_image) => rgba_image.len(),
            WebPStatic::Lossless(lossless) => lossless.get_buf_size(),
        }
    }

    pub(crate) fn color_type(&self) -> ColorType {
        if self.has_alpha() {
            ColorType::Rgba8
        } else {
            ColorType::Rgb8
        }
    }

    pub(crate) fn has_alpha(&self) -> bool {
        match self {
            Self::LossyWithAlpha(..) | Self::Lossless(..) => true,
            Self::LossyWithoutAlpha(..) => false,
        }
    }
}

#[derive(Debug)]
struct WebPAnimatedInfo {
    background_color: Rgba<u8>,
    _loop_count: u16,
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

/// Reads a chunk, but silently ignores unknown chunks at the end of a file
fn read_extended_chunk<R>(r: &mut R) -> ImageResult<Option<(Cursor<Vec<u8>>, WebPRiffChunk)>>
where
    R: Read,
{
    let mut unknown_chunk = Ok(());

    while let Some(chunk) = read_fourcc(r)? {
        let cursor = read_len_cursor(r)?;
        match chunk {
            Ok(chunk) => return unknown_chunk.and(Ok(Some((cursor, chunk)))),
            Err(err) => unknown_chunk = unknown_chunk.and(Err(err)),
        }
    }

    Ok(None)
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

    let reserved_third = read_3_bytes(reader)?;

    if reserved_first != 0 || reserved_second != 0 || reserved_third != 0 {
        let value: u32 = if reserved_first != 0 {
            reserved_first.into()
        } else if reserved_second != 0 {
            reserved_second.into()
        } else {
            reserved_third
        };
        return Err(DecoderError::InfoBitsInvalid {
            name: "reserved",
            value,
        }
        .into());
    }

    let canvas_width = read_3_bytes(reader)? + 1;
    let canvas_height = read_3_bytes(reader)? + 1;

    //product of canvas dimensions cannot be larger than u32 max
    if u32::checked_mul(canvas_width, canvas_height).is_none() {
        return Err(DecoderError::ImageTooLarge.into());
    }

    let info = WebPExtendedInfo {
        _icc_profile: icc_profile,
        _alpha: alpha,
        _exif_metadata: exif_metadata,
        _xmp_metadata: xmp_metadata,
        _animation: animation,
        canvas_width,
        canvas_height,
        icc_profile: None,
    };

    Ok(info)
}

fn read_anim_frame<R: Read>(
    mut reader: R,
    canvas_width: u32,
    canvas_height: u32,
) -> ImageResult<AnimatedFrame> {
    //offsets for the frames are twice the values
    let frame_x = read_3_bytes(&mut reader)? * 2;
    let frame_y = read_3_bytes(&mut reader)? * 2;

    let frame_width = read_3_bytes(&mut reader)? + 1;
    let frame_height = read_3_bytes(&mut reader)? + 1;

    if frame_x + frame_width > canvas_width || frame_y + frame_height > canvas_height {
        return Err(DecoderError::FrameOutsideImage.into());
    }

    let duration = read_3_bytes(&mut reader)?;

    let frame_info = reader.read_u8()?;
    let reserved = frame_info & 0b11111100;
    if reserved != 0 {
        return Err(DecoderError::InfoBitsInvalid {
            name: "reserved",
            value: reserved.into(),
        }
        .into());
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
    let value: u32 =
        (u32::from(buffer[2]) << 16) | (u32::from(buffer[1]) << 8) | u32::from(buffer[0]);
    Ok(value)
}

fn read_lossy_with_chunk<R: Read>(reader: &mut R) -> ImageResult<VP8Frame> {
    let (cursor, chunk) =
        read_chunk(reader)?.ok_or_else(|| Error::from(io::ErrorKind::UnexpectedEof))?;

    if chunk != WebPRiffChunk::VP8 {
        return Err(ChunkHeaderInvalid(chunk.to_fourcc()).into());
    }

    read_lossy(cursor)
}

fn read_lossy(cursor: Cursor<Vec<u8>>) -> ImageResult<VP8Frame> {
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

            let img = WebPStatic::from_lossy(frame.clone())?;

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

            let vp8_frame = read_lossy_with_chunk(reader)?;

            let img = WebPStatic::from_alpha_lossy(alpha_chunk, vp8_frame)?;

            Ok(img)
        }
        None => Err(ImageError::IoError(Error::from(
            io::ErrorKind::UnexpectedEof,
        ))),
        Some((_, chunk)) => Err(ChunkHeaderInvalid(chunk.to_fourcc()).into()),
    }
}

#[derive(Debug)]
struct AlphaChunk {
    _preprocessing: bool,
    filtering_method: FilteringMethod,
    data: Vec<u8>,
}

#[derive(Debug, Copy, Clone)]
enum FilteringMethod {
    None,
    Horizontal,
    Vertical,
    Gradient,
}

fn read_alpha_chunk<R: Read>(reader: &mut R, width: u32, height: u32) -> ImageResult<AlphaChunk> {
    let info_byte = reader.read_u8()?;

    let reserved = info_byte & 0b11000000;
    let preprocessing = (info_byte & 0b00110000) >> 4;
    let filtering = (info_byte & 0b00001100) >> 2;
    let compression = info_byte & 0b00000011;

    if reserved != 0 {
        return Err(DecoderError::InfoBitsInvalid {
            name: "reserved",
            value: reserved.into(),
        }
        .into());
    }

    let preprocessing = match preprocessing {
        0 => false,
        1 => true,
        _ => {
            return Err(DecoderError::InfoBitsInvalid {
                name: "reserved",
                value: preprocessing.into(),
            }
            .into())
        }
    };

    let filtering_method = match filtering {
        0 => FilteringMethod::None,
        1 => FilteringMethod::Horizontal,
        2 => FilteringMethod::Vertical,
        3 => FilteringMethod::Gradient,
        _ => unreachable!(),
    };

    let lossless_compression = match compression {
        0 => false,
        1 => true,
        _ => {
            return Err(DecoderError::InfoBitsInvalid {
                name: "lossless compression",
                value: compression.into(),
            }
            .into())
        }
    };

    let mut framedata = Vec::new();
    reader.read_to_end(&mut framedata)?;

    let data = if lossless_compression {
        let cursor = io::Cursor::new(framedata);

        let mut decoder = LosslessDecoder::new(cursor);
        //this is a potential problem for large images; would require rewriting lossless decoder to use u32 for width and height
        let width: u16 = width
            .try_into()
            .map_err(|_| ImageError::from(DecoderError::ImageTooLarge))?;
        let height: u16 = height
            .try_into()
            .map_err(|_| ImageError::from(DecoderError::ImageTooLarge))?;
        let frame = decoder.decode_frame_implicit_dims(width, height)?;

        let mut data = vec![0u8; usize::from(width) * usize::from(height)];

        frame.fill_green(&mut data);

        data
    } else {
        framedata
    };

    let chunk = AlphaChunk {
        _preprocessing: preprocessing,
        filtering_method,
        data,
    };

    Ok(chunk)
}
