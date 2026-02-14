//!  Decoding of GIF Images
//!
//!  GIF (Graphics Interchange Format) is an image format that supports lossless compression.
//!
//!  # Related Links
//!  * <http://www.w3.org/Graphics/GIF/spec-gif89a.txt> - The GIF Specification
//!
//! # Examples
//! ```rust,no_run
//! use image::codecs::gif::{GifDecoder, GifEncoder};
//! use image::ImageReader;
//! use std::fs::File;
//! use std::io::BufReader;
//!
//! # fn main() -> std::io::Result<()> {
//! // Decode a gif into frames
//! let file_in = BufReader::new(File::open("foo.gif")?);
//! let mut decoder = Box::new(GifDecoder::new(file_in).unwrap());
//!
//! let frames = ImageReader::from_decoder(decoder).into_frames();
//! let frames = frames.collect_frames().expect("error decoding gif");
//!
//! // Encode frames into a gif and save to a file
//! let mut file_out = File::open("out.gif")?;
//! let mut encoder = GifEncoder::new(file_out);
//! encoder.encode_frames(frames.into_iter());
//! # Ok(())
//! # }
//! ```
#![allow(clippy::while_let_loop)]

use std::io::{self, BufRead, Cursor, Read, Seek, Write};
use std::marker::PhantomData;
use std::mem;
use std::num::NonZeroU32;

use gif::ColorOutput;
use gif::{DisposalMethod, Frame};

use crate::animation::{self, Ratio};
use crate::color::{ColorType, Rgba};
use crate::error::{
    DecodingError, EncodingError, ImageError, ImageResult, LimitError, LimitErrorKind,
    ParameterError, ParameterErrorKind, UnsupportedError, UnsupportedErrorKind,
};
use crate::io::{
    DecodedAnimationAttributes, DecodedImageAttributes, DecodedMetadataHint, DecoderAttributes,
};
use crate::metadata::LoopCount;
use crate::traits::Pixel;
use crate::{ExtendedColorType, ImageBuffer, ImageDecoder, ImageEncoder, ImageFormat, Limits};

/// GIF decoder
pub struct GifDecoder<R: Read> {
    options: gif::DecodeOptions,
    reader: Option<R>,
    decoder: Option<gif::Decoder<R>>,
    non_disposed_frame: Option<ImageBuffer<Rgba<u8>, Vec<u8>>>,
    limits: Limits,
}

const COLOR: ColorType = ColorType::Rgba8;

impl<R: Read> GifDecoder<R> {
    /// Creates a new decoder that decodes the input steam `r`
    pub fn new(r: R) -> ImageResult<GifDecoder<R>> {
        let mut options = gif::DecodeOptions::new();
        options.set_color_output(ColorOutput::RGBA);

        Ok(GifDecoder {
            options,
            reader: Some(r),
            decoder: None,
            non_disposed_frame: None,
            limits: Limits::no_limits(),
        })
    }

    // We're manipulating the lifetime. The early return must not borrow from `self.decoder` for
    // the whole scope of the function thus this check does not work with if-let patterns until at
    // least the next generation borrow checker (as of 1.89).
    //
    // FIXME: would be nice to have a sub-object for these two attributes or an enum for the state
    // machine so that we can `ensure_decoder` without borrowing the whole `GifDecoder` type.
    #[allow(clippy::unnecessary_unwrap)]
    fn ensure_decoder(&mut self) -> ImageResult<&mut gif::Decoder<R>> {
        if self.decoder.is_some() {
            return Ok(self.decoder.as_mut().unwrap());
        }

        let Some(reader) = self.reader.take() else {
            return Err(ImageError::Parameter(ParameterError::from_kind(
                ParameterErrorKind::FailedAlready,
            )));
        };

        let decoder = self
            .options
            .clone()
            .read_info(reader)
            .map_err(ImageError::from_decoding)?;

        Ok(self.decoder.insert(decoder))
    }

    fn layout_from_decoder(decoder: &gif::Decoder<R>) -> crate::ImageLayout {
        crate::ImageLayout::new(
            decoder.width().into(),
            decoder.height().into(),
            ColorType::Rgba8,
        )
    }
}

/// Wrapper struct around a `Cursor<Vec<u8>>`
#[allow(dead_code)]
#[deprecated]
pub struct GifReader<R>(Cursor<Vec<u8>>, PhantomData<R>);

#[allow(deprecated)]
impl<R> Read for GifReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.0.read(buf)
    }

    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
        if self.0.position() == 0 && buf.is_empty() {
            mem::swap(buf, self.0.get_mut());
            Ok(buf.len())
        } else {
            self.0.read_to_end(buf)
        }
    }
}

impl<R: BufRead + Seek> ImageDecoder for GifDecoder<R> {
    fn attributes(&self) -> DecoderAttributes {
        DecoderAttributes {
            xmp: DecodedMetadataHint::AfterFinish,
            icc: DecodedMetadataHint::AfterFinish,
            iptc: DecodedMetadataHint::None,
            exif: DecodedMetadataHint::None,
            is_animated: true,
            ..DecoderAttributes::default()
        }
    }

    fn animation_attributes(&mut self) -> Option<DecodedAnimationAttributes> {
        let decoder = self.ensure_decoder().ok()?;
        let loop_count = match decoder.repeat() {
            gif::Repeat::Finite(n) => LoopCount::Finite(
                NonZeroU32::new(n.into()).expect("Repeat::Finite should be non-zero"),
            ),
            gif::Repeat::Infinite => LoopCount::Infinite,
        };

        Some(DecodedAnimationAttributes { loop_count })
    }

    fn peek_layout(&mut self) -> ImageResult<crate::ImageLayout> {
        let decoder = self.ensure_decoder()?;
        Ok(Self::layout_from_decoder(decoder))
    }

    fn set_limits(&mut self, limits: Limits) -> ImageResult<()> {
        limits.check_support(&crate::LimitSupport::default())?;

        let (width, height) = self.peek_layout()?.dimensions();
        limits.check_dimensions(width, height)?;

        self.limits = limits;

        Ok(())
    }

    fn read_image(&mut self, buf: &mut [u8]) -> ImageResult<DecodedImageAttributes> {
        let decoder = self.ensure_decoder()?;

        let layout @ crate::ImageLayout {
            width,
            height,
            color,
            ..
        } = Self::layout_from_decoder(decoder);

        // Allocate the buffer for the previous frame.
        // This is done here and not in the constructor because
        // the constructor cannot return an error when the allocation limit is exceeded.
        if self.non_disposed_frame.is_none() {
            self.limits.reserve_buffer(width, height, color)?;
            self.non_disposed_frame =
                Some(ImageBuffer::from_pixel(width, height, Rgba([0, 0, 0, 0])));
        }

        // Initialized from `ensure_decoder` above, re-acquired for borrow checker.
        let decoder = self.decoder.as_mut().unwrap();
        assert_eq!(u64::try_from(buf.len()), Ok(layout.total_bytes()));

        let frame = match decoder
            .next_frame_info()
            .map_err(ImageError::from_decoding)?
        {
            Some(frame) => FrameInfo::new_from_frame(frame),
            None => {
                return Err(ImageError::Parameter(ParameterError::from_kind(
                    ParameterErrorKind::NoMoreData,
                )))
            }
        };

        let frame_start_len = if (frame.left, frame.width) == (0, width)
            && (u64::from(frame.top) + u64::from(frame.height) <= u64::from(height))
        {
            // If the frame matches the logical screen, or, as a more general case,
            // fits into it and touches its left and right borders, then
            // we can directly write it into the buffer without causing line wraparound.
            let line_length = usize::try_from(width)
                .unwrap()
                .checked_mul(COLOR.bytes_per_pixel() as usize)
                .unwrap();

            let frame_start = line_length.checked_mul(frame.top as usize).unwrap();
            let frame_len = line_length.checked_mul(frame.height as usize).unwrap();
            Some((frame_start, frame_len))
        } else {
            None
        };

        if let Some((frame_start, frame_len)) = frame_start_len {
            // isolate the portion of the buffer to read the frame data into.
            // the chunks above and below it are going to be zeroed.
            let (blank_top, rest) = buf.split_at_mut(frame_start);
            let (buf, blank_bottom) = rest.split_at_mut(frame_len);

            debug_assert_eq!(buf.len(), decoder.buffer_size());

            // this is only necessary in case the buffer is not zeroed
            for b in blank_top {
                *b = 0;
            }

            // fill the middle section with the frame data
            decoder
                .read_into_buffer(buf)
                .map_err(ImageError::from_decoding)?;

            // this is only necessary in case the buffer is not zeroed
            for b in blank_bottom {
                *b = 0;
            }
        } else {
            // If the frame does not match the logical screen, read into an extra buffer
            // and 'insert' the frame from left/top to logical screen width/height.
            let buffer_size = (frame.width as usize)
                .checked_mul(frame.height as usize)
                .and_then(|s| s.checked_mul(4))
                .ok_or(ImageError::Limits(LimitError::from_kind(
                    LimitErrorKind::InsufficientMemory,
                )))?;

            self.limits.reserve_usize(buffer_size)?;
            let mut frame_buffer = vec![0; buffer_size];
            self.limits.free_usize(buffer_size);

            let decoder = self.ensure_decoder()?;

            decoder
                .read_into_buffer(&mut frame_buffer[..])
                .map_err(ImageError::from_decoding)?;

            let frame_buffer = ImageBuffer::from_raw(frame.width, frame.height, frame_buffer);
            let image_buffer = ImageBuffer::from_raw(width, height, &mut *buf);

            // `buffer_size` uses wrapping arithmetic, thus might not report the
            // correct storage requirement if the result does not fit in `usize`.
            // `ImageBuffer::from_raw` detects overflow and reports by returning `None`.
            if frame_buffer.is_none() || image_buffer.is_none() {
                return Err(ImageError::Unsupported(
                    UnsupportedError::from_format_and_kind(
                        ImageFormat::Gif.into(),
                        UnsupportedErrorKind::GenericFeature(format!(
                            "Image dimensions ({}, {}) are too large",
                            frame.width, frame.height
                        )),
                    ),
                ));
            }

            let frame_buffer = frame_buffer.unwrap();
            let mut image_buffer = image_buffer.unwrap();

            for (x, y, pixel) in image_buffer.enumerate_pixels_mut() {
                let frame_x = x.wrapping_sub(frame.left);
                let frame_y = y.wrapping_sub(frame.top);

                if frame_x < frame.width && frame_y < frame.height {
                    *pixel = *frame_buffer.get_pixel(frame_x, frame_y);
                } else {
                    // this is only necessary in case the buffer is not zeroed
                    *pixel = Rgba([0, 0, 0, 0]);
                }
            }
        }

        // Bind to a variable to avoid repeated `.unwrap()` calls
        let non_disposed_frame = self.non_disposed_frame.as_mut().unwrap();

        // blend and dispose into the current non-disposed frame.
        if let Some((frame_start, frame_len)) = frame_start_len {
            // We can blend pixels in a fully contiguous region instead of row-by-row.
            let non_disposed_data =
                &mut non_disposed_frame.inner_pixels_mut()[frame_start..][..frame_len];
            let frame_data = &mut buf[frame_start..][..frame_len];
            blend_and_dispose_region(frame.disposal_method, non_disposed_data, frame_data);
        } else {
            // We have validated bounds already so no checked math.
            let effective_left = frame.left.min(width);
            let effective_width = (width - effective_left).min(frame.width);

            let row_len = width as usize * COLOR.bytes_per_pixel() as usize;
            let data_len = effective_width as usize * COLOR.bytes_per_pixel() as usize;
            let row_skip = effective_left as usize * COLOR.bytes_per_pixel() as usize;

            // process rows before, within and after the frame. Everything not in bounds is copied
            // as if by `DisposalMethod::Previous`.
            for y in 0..frame.top {
                if y >= height {
                    break;
                }

                let start = y as usize * row_len;
                let non_disposed_data =
                    &mut non_disposed_frame.inner_pixels_mut()[start..][..row_len];
                let frame_data = &mut buf[start..][..row_len];
                frame_data.copy_from_slice(non_disposed_data);
            }

            for y in frame.top..(frame.top + frame.height) {
                if y >= height {
                    break;
                }

                let start = y as usize * row_len;

                let non_disposed_data =
                    &mut non_disposed_frame.inner_pixels_mut()[start..][..row_len];
                let frame_data = &mut buf[start..][..row_len];

                non_disposed_data[..row_skip].copy_from_slice(&frame_data[..row_skip]);

                blend_and_dispose_region(
                    frame.disposal_method,
                    &mut non_disposed_data[row_skip..][..data_len],
                    &mut frame_data[row_skip..][..data_len],
                );

                let after_frame = row_skip + data_len;
                non_disposed_data[after_frame..].copy_from_slice(&frame_data[after_frame..]);
            }

            for y in (frame.top + frame.height)..height {
                if y >= height {
                    break;
                }

                let start = y as usize * row_len;
                let non_disposed_data =
                    &mut non_disposed_frame.inner_pixels_mut()[start..][..row_len];
                let frame_data = &mut buf[start..][..row_len];
                frame_data.copy_from_slice(non_disposed_data);
            }
        }

        Ok(DecodedImageAttributes {
            delay: Some(frame.delay),
            ..Default::default()
        })
    }

    fn icc_profile(&mut self) -> ImageResult<Option<Vec<u8>>> {
        let decoder = self.ensure_decoder()?;
        // Similar to XMP metadata
        Ok(decoder.icc_profile().map(Vec::from))
    }

    fn xmp_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        let decoder = self.ensure_decoder()?;
        // XMP metadata must be part of the header which is read with `read_info`.
        Ok(decoder.xmp_metadata().map(Vec::from))
    }
}

fn blend_and_dispose_region(
    dispose: DisposalMethod,
    non_disposed_data: &mut [u8],
    frame_data: &mut [u8],
) {
    for (disposed, pixel) in non_disposed_data
        .chunks_exact_mut(4)
        .zip(frame_data.chunks_exact_mut(4))
    {
        // FIXME: internal dispatch on disposal method may be slow, investigate if this is
        // properly and reliably vectorized.
        let disposed = Rgba::<u8>::from_slice_mut(disposed);
        let pixel = Rgba::<u8>::from_slice_mut(pixel);
        blend_and_dispose_pixel(dispose, disposed, pixel);
    }
}

// blend the current frame with the non-disposed frame, then update
// the non-disposed frame according to the disposal method.
fn blend_and_dispose_pixel(
    dispose: DisposalMethod,
    previous: &mut Rgba<u8>,
    current: &mut Rgba<u8>,
) {
    let pixel_alpha = current.channels()[3];
    if pixel_alpha == 0 {
        *current = *previous;
    }

    match dispose {
        DisposalMethod::Any | DisposalMethod::Keep => {
            // do not dispose
            // (keep pixels from this frame)
            // note: the `Any` disposal method is underspecified in the GIF
            // spec, but most viewers treat it identically to `Keep`
            *previous = *current;
        }
        DisposalMethod::Background => {
            // restore to background color
            // (background shows through transparent pixels in the next frame)
            *previous = Rgba([0, 0, 0, 0]);
        }
        DisposalMethod::Previous => {
            // restore to previous
            // (dispose frames leaving the last none disposal frame)
        }
    }
}

struct FrameInfo {
    left: u32,
    top: u32,
    width: u32,
    height: u32,
    disposal_method: DisposalMethod,
    delay: animation::Delay,
}

impl FrameInfo {
    fn new_from_frame(frame: &Frame) -> FrameInfo {
        FrameInfo {
            left: u32::from(frame.left),
            top: u32::from(frame.top),
            width: u32::from(frame.width),
            height: u32::from(frame.height),
            disposal_method: frame.dispose,
            // frame.delay is in units of 10ms so frame.delay*10 is in ms
            delay: animation::Delay::from_ratio(Ratio::new(u32::from(frame.delay) * 10, 1)),
        }
    }
}

/// Number of repetitions for a GIF animation
#[derive(Clone, Copy, Debug)]
pub enum Repeat {
    /// Finite number of repetitions
    Finite(u16),
    /// Looping GIF
    Infinite,
}

impl Repeat {
    pub(crate) fn to_gif_enum(self) -> gif::Repeat {
        match self {
            Repeat::Finite(n) => gif::Repeat::Finite(n),
            Repeat::Infinite => gif::Repeat::Infinite,
        }
    }
}

/// GIF encoder.
pub struct GifEncoder<W: Write> {
    w: Option<W>,
    gif_encoder: Option<gif::Encoder<W>>,
    speed: i32,
    repeat: Option<Repeat>,
}

impl<W: Write> GifEncoder<W> {
    /// Creates a new GIF encoder with a speed of 10. This provides a good balance between quality and encoding speed.
    pub fn new(w: W) -> GifEncoder<W> {
        Self::new_with_speed(w, 10)
    }

    /// Create a new GIF encoder, and has the speed parameter `speed`. See
    /// [`Frame::from_rgba_speed`](https://docs.rs/gif/latest/gif/struct.Frame.html#method.from_rgba_speed)
    /// for more information.
    pub fn new_with_speed(w: W, speed: i32) -> GifEncoder<W> {
        assert!(
            (1..=30).contains(&speed),
            "speed needs to be in the range [1, 30]"
        );
        GifEncoder {
            w: Some(w),
            gif_encoder: None,
            speed,
            repeat: None,
        }
    }

    /// Set the repeat behaviour of the encoded GIF
    pub fn set_repeat(&mut self, repeat: Repeat) -> ImageResult<()> {
        if let Some(ref mut encoder) = self.gif_encoder {
            encoder
                .set_repeat(repeat.to_gif_enum())
                .map_err(ImageError::from_encoding)?;
        }
        self.repeat = Some(repeat);
        Ok(())
    }

    /// Encode a single image.
    pub fn encode(
        &mut self,
        data: &[u8],
        width: u32,
        height: u32,
        color: ExtendedColorType,
    ) -> ImageResult<()> {
        let (width, height) = self.gif_dimensions(width, height)?;
        match color {
            ExtendedColorType::Rgb8 => {
                self.encode_gif(Frame::from_rgb_speed(width, height, data, self.speed))
            }
            ExtendedColorType::Rgba8 => self.encode_gif(Frame::from_rgba_speed(
                width,
                height,
                &mut data.to_owned(),
                self.speed,
            )),
            ExtendedColorType::L8 => {
                let palette: Vec<u8> = (0..=255).flat_map(|i| [i, i, i]).collect();

                self.encode_gif(Frame::from_palette_pixels(
                    width, height, data, palette, None,
                ))
            }
            ExtendedColorType::La8 => {
                self.encode_gif(Frame::from_grayscale_with_alpha(width, height, data))
            }
            _ => Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Gif.into(),
                    UnsupportedErrorKind::Color(color),
                ),
            )),
        }
    }

    /// Encode one frame of animation.
    pub fn encode_frame(&mut self, img_frame: animation::Frame) -> ImageResult<()> {
        let frame = self.convert_frame(img_frame)?;
        self.encode_gif(frame)
    }

    /// Encodes Frames.
    /// Consider using `try_encode_frames` instead to encode an `animation::Frames` like iterator.
    pub fn encode_frames<F>(&mut self, frames: F) -> ImageResult<()>
    where
        F: IntoIterator<Item = animation::Frame>,
    {
        for img_frame in frames {
            self.encode_frame(img_frame)?;
        }
        Ok(())
    }

    /// Try to encode a collection of `ImageResult<animation::Frame>` objects.
    /// Use this function to encode an `animation::Frames` like iterator.
    /// Whenever an `Err` item is encountered, that value is returned without further actions.
    pub fn try_encode_frames<F>(&mut self, frames: F) -> ImageResult<()>
    where
        F: IntoIterator<Item = ImageResult<animation::Frame>>,
    {
        for img_frame in frames {
            self.encode_frame(img_frame?)?;
        }
        Ok(())
    }

    pub(crate) fn convert_frame(
        &mut self,
        img_frame: animation::Frame,
    ) -> ImageResult<Frame<'static>> {
        // get the delay before converting img_frame
        let frame_delay = img_frame.delay().into_ratio().to_integer();
        // convert img_frame into RgbaImage
        let mut rbga_frame = img_frame.into_buffer();
        let (width, height) = self.gif_dimensions(rbga_frame.width(), rbga_frame.height())?;

        // Create the gif::Frame from the animation::Frame
        let mut frame = Frame::from_rgba_speed(width, height, &mut rbga_frame, self.speed);
        // Saturate the conversion to u16::MAX instead of returning an error as that
        // would require a new special cased variant in ParameterErrorKind which most
        // likely couldn't be reused for other cases. This isn't a bad trade-off given
        // that the current algorithm is already lossy.
        frame.delay = (frame_delay / 10).try_into().unwrap_or(u16::MAX);

        Ok(frame)
    }

    fn gif_dimensions(&self, width: u32, height: u32) -> ImageResult<(u16, u16)> {
        fn inner_dimensions(width: u32, height: u32) -> Option<(u16, u16)> {
            let width = u16::try_from(width).ok()?;
            let height = u16::try_from(height).ok()?;
            Some((width, height))
        }

        // TODO: this is not very idiomatic yet. Should return an EncodingError.
        inner_dimensions(width, height).ok_or_else(|| {
            ImageError::Parameter(ParameterError::from_kind(
                ParameterErrorKind::DimensionMismatch,
            ))
        })
    }

    pub(crate) fn encode_gif(&mut self, mut frame: Frame) -> ImageResult<()> {
        let gif_encoder;
        if let Some(ref mut encoder) = self.gif_encoder {
            gif_encoder = encoder;
        } else {
            let writer = self.w.take().unwrap();
            let mut encoder = gif::Encoder::new(writer, frame.width, frame.height, &[])
                .map_err(ImageError::from_encoding)?;
            if let Some(ref repeat) = self.repeat {
                encoder
                    .set_repeat(repeat.to_gif_enum())
                    .map_err(ImageError::from_encoding)?;
            }
            self.gif_encoder = Some(encoder);
            gif_encoder = self.gif_encoder.as_mut().unwrap();
        }

        frame.dispose = DisposalMethod::Background;

        gif_encoder
            .write_frame(&frame)
            .map_err(ImageError::from_encoding)
    }
}
impl<W: Write> ImageEncoder for GifEncoder<W> {
    fn write_image(
        mut self,
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ExtendedColorType,
    ) -> ImageResult<()> {
        self.encode(buf, width, height, color_type)
    }
}

impl ImageError {
    fn from_decoding(err: gif::DecodingError) -> ImageError {
        use gif::DecodingError::*;
        match err {
            Io(io_err) => ImageError::IoError(io_err),
            other => ImageError::Decoding(DecodingError::new(ImageFormat::Gif.into(), other)),
        }
    }

    fn from_encoding(err: gif::EncodingError) -> ImageError {
        use gif::EncodingError::*;
        match err {
            Io(io_err) => ImageError::IoError(io_err),
            other => ImageError::Encoding(EncodingError::new(ImageFormat::Gif.into(), other)),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn frames_exceeding_logical_screen_size() {
        // This is a gif with 10x10 logical screen, but a 16x16 frame + 6px offset inside.
        let data = vec![
            0x47, 0x49, 0x46, 0x38, 0x39, 0x61, 0x0A, 0x00, 0x0A, 0x00, 0xF0, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x0E, 0xFF, 0x1F, 0x21, 0xF9, 0x04, 0x09, 0x64, 0x00, 0x00, 0x00, 0x2C,
            0x06, 0x00, 0x06, 0x00, 0x10, 0x00, 0x10, 0x00, 0x00, 0x02, 0x23, 0x84, 0x8F, 0xA9,
            0xBB, 0xE1, 0xE8, 0x42, 0x8A, 0x0F, 0x50, 0x79, 0xAE, 0xD1, 0xF9, 0x7A, 0xE8, 0x71,
            0x5B, 0x48, 0x81, 0x64, 0xD5, 0x91, 0xCA, 0x89, 0x4D, 0x21, 0x63, 0x89, 0x4C, 0x09,
            0x77, 0xF5, 0x6D, 0x14, 0x00, 0x3B,
        ];

        let mut decoder = GifDecoder::new(Cursor::new(data)).unwrap();
        let layout = decoder.peek_layout().unwrap();

        let mut buf = vec![0u8; layout.total_bytes() as usize];
        assert!(decoder.read_image(&mut buf).is_ok());
    }
}
