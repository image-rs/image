//!  Decoding of GIF Images
//!
//!  GIF (Graphics Interchange Format) is an image format that supports lossless compression.
//!
//!  # Related Links
//!  * <http://www.w3.org/Graphics/GIF/spec-gif89a.txt> - The GIF Specification
//!
//! # Examples
//! ```rust,no_run
//! use image::gif::{GifDecoder, Encoder};
//! use image::{ImageDecoder, AnimationDecoder};
//! use std::fs::File;
//! # fn main() -> std::io::Result<()> {
//! // Decode a gif into frames
//! let file_in = File::open("foo.gif")?;
//! let mut decoder = GifDecoder::new(file_in).unwrap();
//! let frames = decoder.into_frames();
//! let frames = frames.collect_frames().expect("error decoding gif");
//!
//! // Encode frames into a gif and save to a file
//! let mut file_out = File::open("out.gif")?;
//! let mut encoder = Encoder::new(file_out);
//! encoder.encode_frames(frames.into_iter());
//! # Ok(())
//! # }
//! ```
#![allow(clippy::while_let_loop)]

use std::convert::TryInto;
use std::convert::TryFrom;
use std::io::{self, Cursor, Read, Write};
use std::marker::PhantomData;
use std::mem;

use gif::{ColorOutput, SetParameter};
use gif::{DisposalMethod, Frame};
use num_rational::Ratio;

use crate::animation;
use crate::ImageBuffer;
use crate::color::{ColorType, Rgba};
use crate::error::{DecodingError, ImageError, ImageResult, ParameterError, ParameterErrorKind, UnsupportedError, UnsupportedErrorKind};
use crate::image::{self, AnimationDecoder, ImageDecoder, ImageFormat};
use crate::traits::Pixel;

/// GIF decoder
pub struct GifDecoder<R: Read> {
    reader: gif::Reader<R>,
}

impl<R: Read> GifDecoder<R> {
    /// Creates a new decoder that decodes the input steam ```r```
    pub fn new(r: R) -> ImageResult<GifDecoder<R>> {
        let mut decoder = gif::Decoder::new(r);
        decoder.set(ColorOutput::RGBA);

        Ok(GifDecoder {
            reader: decoder.read_info().map_err(ImageError::from_gif)?,
        })
    }
}

/// Wrapper struct around a `Cursor<Vec<u8>>`
pub struct GifReader<R>(Cursor<Vec<u8>>, PhantomData<R>);
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

impl<'a, R: 'a + Read> ImageDecoder<'a> for GifDecoder<R> {
    type Reader = GifReader<R>;

    fn dimensions(&self) -> (u32, u32) {
        (u32::from(self.reader.width()), u32::from(self.reader.height()))
    }

    fn color_type(&self) -> ColorType {
        ColorType::Rgba8
    }

    fn into_reader(self) -> ImageResult<Self::Reader> {
        Ok(GifReader(Cursor::new(image::decoder_to_vec(self)?), PhantomData))
    }

    fn read_image(mut self, buf: &mut [u8]) -> ImageResult<()> {
        assert_eq!(u64::try_from(buf.len()), Ok(self.total_bytes()));

        let (f_width, f_height, left, top);

        if let Some(frame) = self.reader.next_frame_info().map_err(ImageError::from_gif)? {
            left = u32::from(frame.left);
            top = u32::from(frame.top);
            f_width = u32::from(frame.width);
            f_height = u32::from(frame.height);
        } else {
            return Err(ImageError::Parameter(
                ParameterError::from_kind(ParameterErrorKind::NoMoreData)
            ));
        }

        self.reader.read_into_buffer(buf).map_err(ImageError::from_gif)?;

        let (width, height) = (u32::from(self.reader.width()), u32::from(self.reader.height()));
        if (left, top) != (0, 0) || (width, height) != (f_width, f_height) {
            // This is somewhat of an annoying case. The image we read into `buf` doesn't take up
            // the whole buffer and now we need to properly insert borders. For simplicity this code
            // currently takes advantage of the `ImageBuffer::from_fn` function to make a second
            // ImageBuffer that is properly positioned, and then copies it back into `buf`.
            //
            // TODO: Implement this without any allocation.

            // Recover the full image
            let image_buffer = {
                // See the comments inside `<GifFrameIterator as Iterator>::next` about
                // the error handling of `from_raw`.
                let image = ImageBuffer::from_raw(f_width, f_height, &mut *buf).ok_or_else(
                    || ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                        ImageFormat::Gif.into(),
                        UnsupportedErrorKind::GenericFeature(format!("Image dimensions ({}, {}) are too large", f_width, f_height)))))?;

                ImageBuffer::from_fn(width, height, |x, y| {
                    let x = x.wrapping_sub(left);
                    let y = y.wrapping_sub(top);
                    if x < image.width() && y < image.height() {
                        *image.get_pixel(x, y)
                    } else {
                        Rgba([0, 0, 0, 0])
                    }
                })
            };
            buf.copy_from_slice(&mut image_buffer.into_raw());
        }
        Ok(())
    }
}

struct GifFrameIterator<R: Read> {
    reader: gif::Reader<R>,

    width: u32,
    height: u32,

    non_disposed_frame: ImageBuffer<Rgba<u8>, Vec<u8>>,
}


impl<R: Read> GifFrameIterator<R> {
    fn new(decoder: GifDecoder<R>) -> GifFrameIterator<R> {
        let (width, height) = decoder.dimensions();

        // TODO: Avoid this cast
        let (width, height) = (width as u32, height as u32);

        // intentionally ignore the background color for web compatibility

        // create the first non disposed frame
        let non_disposed_frame = ImageBuffer::from_pixel(width, height, Rgba([0, 0, 0, 0]));

        GifFrameIterator {
            reader: decoder.reader,
            width,
            height,
            non_disposed_frame,
        }
    }
}


impl<R: Read> Iterator for GifFrameIterator<R> {
    type Item = ImageResult<animation::Frame>;

    fn next(&mut self) -> Option<ImageResult<animation::Frame>> {
        // begin looping over each frame
        let (left, top, delay, dispose, f_width, f_height);

        match self.reader.next_frame_info() {
            Ok(frame_info) => {
                if let Some(frame) = frame_info {
                    left = u32::from(frame.left);
                    top = u32::from(frame.top);
                    f_width = u32::from(frame.width);
                    f_height = u32::from(frame.height);

                    // frame.delay is in units of 10ms so frame.delay*10 is in ms
                    delay = Ratio::new(u32::from(frame.delay) * 10, 1);
                    dispose = frame.dispose;
                } else {
                    // no more frames
                    return None;
                }
            },
            Err(err) => return Some(Err(ImageError::from_gif(err))),
        }

        let mut vec = vec![0; self.reader.buffer_size()];
        if let Err(err) = self.reader.read_into_buffer(&mut vec) {
            return Some(Err(ImageError::from_gif(err)));
        }

        // create the image buffer from the raw frame.
        // `buffer_size` uses wrapping arithmetics, thus might not report the
        // correct storage requirement if the result does not fit in `usize`.
        // on the other hand, `ImageBuffer::from_raw` detects overflow and
        // reports by returning `None`.
        let mut frame_buffer = match ImageBuffer::from_raw(f_width, f_height, vec) {
            Some(frame_buffer) => frame_buffer,
            None => {
                return Some(Err(ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                    ImageFormat::Gif.into(),
                    UnsupportedErrorKind::GenericFeature(format!("Image dimensions ({}, {}) are too large", f_width, f_height)),
                ))))
            }
        };

        // blend the current frame with the non-disposed frame, then update
        // the non-disposed frame according to the disposal method.
        fn blend_and_dispose_pixel(dispose: DisposalMethod,
                previous: &mut Rgba<u8>, current: &mut Rgba<u8>) {
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

        // if `frame_buffer`'s frame exactly matches the entire image, then
        // use it directly, else create a new buffer to hold the composited
        // image.
        let image_buffer = if (left, top) == (0, 0)
                && (self.width, self.height) == frame_buffer.dimensions() {
            for (x, y, pixel) in frame_buffer.enumerate_pixels_mut() {
                let previous_pixel = self.non_disposed_frame.get_pixel_mut(x, y);
                blend_and_dispose_pixel(dispose, previous_pixel, pixel);
            }
            frame_buffer
        } else {
            ImageBuffer::from_fn(self.width, self.height, |x, y| {
                let frame_x = x.wrapping_sub(left);
                let frame_y = y.wrapping_sub(top);
                let previous_pixel = self.non_disposed_frame.get_pixel_mut(x, y);

                if frame_x < frame_buffer.width() && frame_y < frame_buffer.height() {
                    let mut pixel = *frame_buffer.get_pixel(frame_x, frame_y);
                    blend_and_dispose_pixel(dispose, previous_pixel, &mut pixel);
                    pixel
                } else {
                    // out of bounds, return pixel from previous frame
                    *previous_pixel
                }
            })
        };

        Some(Ok(animation::Frame::from_parts(
            image_buffer, 0, 0, animation::Delay::from_ratio(delay),
        )))
    }
}

impl<'a, R: Read + 'a> AnimationDecoder<'a> for GifDecoder<R> {
    fn into_frames(self) -> animation::Frames<'a> {
        animation::Frames::new(Box::new(GifFrameIterator::new(self)))
    }
}

/// GIF encoder.
pub struct Encoder<W: Write> {
    w: Option<W>,
    gif_encoder: Option<gif::Encoder<W>>,
}

impl<W: Write> Encoder<W> {
    /// Creates a new GIF encoder.
    pub fn new(w: W) -> Encoder<W> {
        Encoder {
            w: Some(w),
            gif_encoder: None,
        }
    }

    /// Encode a single image.
    pub fn encode(
        &mut self,
        data: &[u8],
        width: u32,
        height: u32,
        color: ColorType,
    ) -> ImageResult<()> {
        let (width, height) = self.gif_dimensions(width, height)?;
        match color {
            ColorType::Rgb8 => self.encode_gif(Frame::from_rgb(width, height, data)),
            ColorType::Rgba8 => {
                self.encode_gif(Frame::from_rgba(width, height, &mut data.to_owned()))
            },
            _ => Err(ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                ImageFormat::Gif.into(),
                UnsupportedErrorKind::Color(color.into())
            ))),
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
        F: IntoIterator<Item = animation::Frame>
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
        F: IntoIterator<Item = ImageResult<animation::Frame>>
    {
        for img_frame in frames {
            self.encode_frame(img_frame?)?;
        }
        Ok(())
    }

    pub(crate) fn convert_frame(&mut self, img_frame: animation::Frame)
        -> ImageResult<Frame<'static>>
    {
        // get the delay before converting img_frame
        let frame_delay = img_frame.delay().into_ratio().to_integer();
        // convert img_frame into RgbaImage
        let mut rbga_frame = img_frame.into_buffer();
        let (width, height) = self.gif_dimensions(
            rbga_frame.width(),
            rbga_frame.height())?;

        // Create the gif::Frame from the animation::Frame
        let mut frame = Frame::from_rgba(width, height, &mut *rbga_frame);
        // Saturate the conversion to u16::MAX instead of returning an error as that
        // would require a new special cased variant in ParameterErrorKind which most
        // likely couldn't be reused for other cases. This isn't a bad trade-off given
        // that the current algorithm is already lossy.
        frame.delay = (frame_delay / 10).try_into().unwrap_or(std::u16::MAX);

        Ok(frame)
    }

    fn gif_dimensions(&self, width: u32, height: u32) -> ImageResult<(u16, u16)> {
        fn inner_dimensions(width: u32, height: u32) -> Option<(u16, u16)> {
            let width = u16::try_from(width).ok()?;
            let height = u16::try_from(height).ok()?;
            Some((width, height))
        }

        // TODO: this is not very idiomatic yet. Should return an EncodingError.
        inner_dimensions(width, height).ok_or(ImageError::Parameter(ParameterError::from_kind(
            ParameterErrorKind::DimensionMismatch
        )))
    }

    pub(crate) fn encode_gif(&mut self, frame: Frame) -> ImageResult<()> {
        let gif_encoder;
        if let Some(ref mut encoder) = self.gif_encoder {
            gif_encoder = encoder;
        } else {
            let writer = self.w.take().unwrap();
            let encoder = gif::Encoder::new(writer, frame.width, frame.height, &[])?;
            self.gif_encoder = Some(encoder);
            gif_encoder = self.gif_encoder.as_mut().unwrap()
        }

        gif_encoder.write_frame(&frame).map_err(|err| err.into())
    }
}

impl ImageError {
    fn from_gif(err: gif::DecodingError) -> ImageError {
        use gif::DecodingError::*;
        match err {
            err @ Format(_) | err @ Internal(_) => ImageError::Decoding(DecodingError::new(ImageFormat::Gif.into(), err)),
            Io(io_err) => ImageError::IoError(io_err),
        }
    }
}
