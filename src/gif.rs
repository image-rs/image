//!  Decoding of GIF Images
//!
//!  GIF (Graphics Interchange Format) is an image format that supports lossless compression.
//!
//!  # Related Links
//!  * <http://www.w3.org/Graphics/GIF/spec-gif89a.txt> - The GIF Specification
//!
//! # Examples
//! ```rust,no_run
//! use image::gif::{Decoder, Encoder};
//! use image::ImageDecoder;
//! use std::fs::File;
//! # fn main() -> std::io::Result<()> {
//! // Decode a gif into frames
//! let file_in = File::open("foo.gif")?;
//! let mut decoder = Decoder::new(file_in);
//! let frames = decoder.into_frames().expect("error decoding gif");
//!
//! // Encode frames into a gif and save to a file
//! let mut file_out = File::open("out.gif")?;
//! let mut encoder = Encoder::new(file_out);
//! encoder.encode_frames(frames);
//! # Ok(())
//! # }
//! ```
#![cfg_attr(feature = "cargo-clippy", allow(while_let_loop))]

extern crate gif;
extern crate num_rational;

use std::clone::Clone;
use std::io::{Read, Write};
use std::vec::Vec;

use self::gif::{ColorOutput, SetParameter};
pub use self::gif::{DisposalMethod, Frame};

use animation;
use buffer::{ImageBuffer, Pixel};
use color;
use color::Rgba;
use image::{DecodingResult, ImageDecoder, ImageError, ImageResult};
use num_rational::Ratio;

struct GifFrameIterator<R: Read> {
    reader: gif::Reader<R>,
    width: u32,
    height: u32,

    background_img: ImageBuffer<Rgba<u8>, Vec<u8>>,
    non_disposed_frame: ImageBuffer<Rgba<u8>, Vec<u8>>,

    left: u32,
    top: u32,
    delay: Ratio<u16>,
    dispose: DisposalMethod,
}

impl<R: Read> GifFrameIterator<R> {
    fn new(reader: gif::Reader<R>) -> GifFrameIterator<R> {
        let width = u32::from(reader.width());
        let height = u32::from(reader.height());

        // set the background color to be either the bg_color defined in the gif
        // or a transparent pixel.
        let background_color_option = reader.bg_color();
        let mut background_color = vec![0; 4];
        let background_pixel = {
            let global_palette = reader.global_palette();
            match background_color_option {
                Some(index) => {
                    // find the color by looking in the global palette
                    match global_palette {
                        // take the color from the palette that is at the index defined
                        // by background_color_option
                        Some(slice) => {
                            background_color.clone_from_slice(&slice[index..(index + 4)]);
                            Rgba::from_slice(&background_color)
                        }
                        // if there is no global palette, assign the background color to be
                        // transparent
                        None => Rgba::from_slice(&[0, 0, 0, 0]),
                    }
                }
                // return a transparent background color
                None => Rgba::from_slice(&[0, 0, 0, 0]),
            }
        };

        // create the background image to use later
        let background_img = ImageBuffer::from_pixel(width, height, *background_pixel);

        // the background image is the first non disposed frame
        let non_disposed_frame = background_img.clone();

        GifFrameIterator {
            reader,
            width,
            height,
            background_img,
            non_disposed_frame,
            left: 0,
            top: 0,
            delay: Ratio::new(0, 1),
            dispose: DisposalMethod::Any
        }
    }
}

impl<R: Read> Iterator for GifFrameIterator<R> {
    type Item = ImageResult<animation::Frame>;
    fn next(&mut self) -> Option<ImageResult<animation::Frame>> {
        match self.reader.next_frame_info() {
            Ok(frame) => {
                if let Some(frame) = frame {
                    self.left = u32::from(frame.left);
                    self.top = u32::from(frame.top);

                    // frame.delay is in units of 10ms so frame.delay*10 is in ms
                    self.delay = Ratio::new(frame.delay * 10, 1);
                    self.dispose = frame.dispose;
                } else {
                    // no more frames
                    return None;
                }
            },
            Err(err) => return Some(Err(ImageError::from(err))),
        }

        let mut vec = vec![0; self.reader.buffer_size()];
        if let Err(err) = self.reader.fill_buffer(&mut vec) {
            return Some(Err(ImageError::from(err)));
        }

        let mut result = None;

        // create the image buffer from the raw frame
        if let Some(mut image_buffer) = ImageBuffer::from_raw(self.width, self.height, vec) {
            // loop over all pixels, checking if any pixels from the non disposed
            // frame need to be used
            for (x, y, pixel) in image_buffer.enumerate_pixels_mut() {
                let previous_img_buffer = &self.non_disposed_frame;
                let mut adjusted_pixel: &mut Rgba<u8> = pixel;
                let previous_pixel: &Rgba<u8> = previous_img_buffer.get_pixel(x, y);

                let pixel_alpha = adjusted_pixel.channels()[3];

                // If a pixel is not visible then we show the non disposed frame pixel instead
                if pixel_alpha == 0 {
                    adjusted_pixel.blend(previous_pixel);
                }
            }

            let frame = animation::Frame::from_parts(image_buffer.clone(), self.left, self.top, self.delay);
            result = Some(Ok(frame));

            match self.dispose {
                DisposalMethod::Any => {
                    // do nothing
                    // (completely replace this frame with the next)
                }
                DisposalMethod::Keep => {
                    // do not dispose
                    // (keep pixels from this frame)
                    self.non_disposed_frame = image_buffer;
                }
                DisposalMethod::Background => {
                    // restore to background color
                    // (background shows through transparent pixels in the next frame)
                    self.non_disposed_frame = self.background_img.clone();
                }
                DisposalMethod::Previous => {
                    // restore to previous
                    // (dispose frames leaving the last none disposal frame)
                }
            };
        }

        result
    }
}

enum Either<T, U> {
    Left(T),
    Right(U),
}

/// GIF decoder
pub struct Decoder<R: Read> {
    inner: Option<Either<gif::Decoder<R>, gif::Reader<R>>>,
}

impl<R: Read> Decoder<R> {
    /// Creates a new decoder that decodes the input steam ```r```
    pub fn new(r: R) -> Decoder<R> {
        let mut decoder = gif::Decoder::new(r);
        decoder.set(ColorOutput::RGBA);
        Decoder {
            inner: Some(Either::Left(decoder)),
        }
    }

    // Converts the inner decoder to a reader
    fn get_reader(&mut self) -> Result<&mut gif::Reader<R>, gif::DecodingError> {
        let inner = self.inner.take().unwrap();
        self.inner = Some(match inner {
            Either::Left(decoder) => {
                let reader = try!(decoder.read_info());
                Either::Right(reader)
            }
            Either::Right(reader) => Either::Right(reader),
        });
        match self.inner {
            Some(Either::Right(ref mut reader)) => Ok(reader),
            _ => unreachable!(),
        }
    }

    fn take_reader(mut self) -> Result<gif::Reader<R>, gif::DecodingError> {
        let inner = self.inner.take().unwrap();
        self.inner = Some(match inner {
            Either::Left(decoder) => {
                let reader = try!(decoder.read_info());
                Either::Right(reader)
            }
            Either::Right(reader) => Either::Right(reader),
        });
        match self.inner {
            Some(Either::Right(reader)) => Ok(reader),
            _ => unreachable!(),
        }
    }
}

impl<'a, R: Read + 'a> ImageDecoder<'a> for Decoder<R> {
    fn dimensions(&mut self) -> ImageResult<(u32, u32)> {
        let reader = try!(self.get_reader());
        Ok((u32::from(reader.width()), u32::from(reader.height())))
    }

    fn colortype(&mut self) -> ImageResult<color::ColorType> {
        Ok(color::ColorType::RGBA(8))
    }

    fn row_len(&mut self) -> ImageResult<usize> {
        let reader = try!(self.get_reader());
        Ok(reader.line_length())
    }

    fn read_scanline(&mut self, buf: &mut [u8]) -> ImageResult<u32> {
        let reader = try!(self.get_reader());
        let len = reader.line_length();
        try!(reader.fill_buffer(&mut buf[..len]));
        Ok(len as u32)
    }

    fn read_image(&mut self) -> ImageResult<DecodingResult> {
        let reader = try!(self.get_reader());
        if try!(reader.next_frame_info()).is_some() {
            let mut buf = vec![0; reader.buffer_size()];
            try!(reader.read_into_buffer(&mut buf));
            Ok(DecodingResult::U8(buf))
        } else {
            Err(ImageError::ImageEnd)
        }
    }

    fn is_animated(&mut self) -> ImageResult<bool> {
        Ok(true)
    }

    fn into_frames(self) -> ImageResult<animation::FrameIterator<'a>> {
        let reader = try!(self.take_reader());
        Ok(animation::FrameIterator::new(Box::new(GifFrameIterator::new(reader))))
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
    /// Encodes a frame.
    pub fn encode(&mut self, frame: &Frame) -> ImageResult<()> {
        let result;
        if let Some(ref mut encoder) = self.gif_encoder {
            result = encoder.write_frame(frame).map_err(|err| err.into());
        } else {
            let writer = self.w.take().unwrap();
            let mut encoder = try!(gif::Encoder::new(writer, frame.width, frame.height, &[]));
            result = encoder.write_frame(&frame).map_err(|err| err.into());
            self.gif_encoder = Some(encoder);
        }
        result
    }
    /// Encodes Frames.
    pub fn encode_frames<Frames>(&mut self, frames: Frames) -> ImageResult<()>
    where
        Frames: Iterator<Item = ImageResult<animation::Frame>>
    {
        for frame_result in frames {
            match frame_result {
                Ok(img_frame) => {
                    // get the delay before coverting img_frame
                    let frame_delay = img_frame.delay().to_integer();
                    // convert img_frame into RgbaImage
                    let rbga_frame = img_frame.into_buffer();

                    // Create the gif::Frame from the animation::Frame
                    let mut frame = Frame::from_rgba(
                        rbga_frame.width() as u16,
                        rbga_frame.height() as u16,
                        &mut rbga_frame.into_raw()
                    );
                    frame.delay = frame_delay;

                    // encode the gif::Frame
                    if let Err(e) = self.encode(&frame) {
                        return Err(e);
                    }
                },
                Err(err) => return Err(err),
            }
        }
        Ok(())
    }
}

impl From<gif::DecodingError> for ImageError {
    fn from(err: gif::DecodingError) -> ImageError {
        use self::gif::DecodingError::*;
        match err {
            Format(desc) | Internal(desc) => ImageError::FormatError(desc.into()),
            Io(io_err) => ImageError::IoError(io_err),
        }
    }
}
