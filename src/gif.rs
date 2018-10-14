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
//! use image::{ImageDecoder, AnimationDecoder};
//! use std::fs::File;
//! # fn main() -> std::io::Result<()> {
//! // Decode a gif into frames
//! let file_in = File::open("foo.gif")?;
//! let mut decoder = Decoder::new(file_in).unwrap();
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
use std::io::{Cursor, Read, Write};

use self::gif::{ColorOutput, SetParameter};
pub use self::gif::{DisposalMethod, Frame};

use animation;
use buffer::{ImageBuffer, Pixel};
use color;
use color::Rgba;
use image::{AnimationDecoder, ImageDecoder, ImageError, ImageResult};
use num_rational::Ratio;

/// GIF decoder
pub struct Decoder<R: Read> {
    reader: gif::Reader<R>,
}

impl<R: Read> Decoder<R> {
    /// Creates a new decoder that decodes the input steam ```r```
    pub fn new(r: R) -> ImageResult<Decoder<R>> {
        let mut decoder = gif::Decoder::new(r);
        decoder.set(ColorOutput::RGBA);

        Ok(Decoder {
            reader: decoder.read_info()?,
        })
    }
}

impl<R: Read> ImageDecoder for Decoder<R> {
    type Reader = Cursor<Vec<u8>>;

    fn dimensions(&self) -> (u64, u64) {
        (self.reader.width() as u64, self.reader.height() as u64)
    }

    fn colortype(&self) -> color::ColorType {
        color::ColorType::RGBA(8)
    }

    // fn read_scanline(&mut self, buf: &mut [u8]) -> ImageResult<u32> {
    //     let reader = try!(self.get_reader());
    //     let len = reader.line_length();
    //     try!(reader.fill_buffer(&mut buf[..len]));
    //     Ok(len as u32)
    // }

    fn into_reader(self) -> ImageResult<Self::Reader> {
        Ok(Cursor::new(self.read_image()?))
    }

    fn read_image(mut self) -> ImageResult<Vec<u8>> {
        if self.reader.next_frame_info()?.is_some() {
            let mut buf = vec![0; self.reader.buffer_size()];
            self.reader.read_into_buffer(&mut buf)?;
            Ok(buf)
        } else {
            Err(ImageError::ImageEnd)
        }
    }
}

impl<R: Read> AnimationDecoder for Decoder<R> {
    fn into_frames(mut self) -> ImageResult<animation::Frames> {
        let (width, height) = self.dimensions();

        // TODO: Avoid this cast
        let (width, height) = (width as u32, height as u32);

        // variable to hold all the image frames
        let mut frames = Vec::new();

        // set the background color to be either the bg_color defined in the gif
        // or a transparent pixel.
        let background_color_option = self.reader.bg_color();
        let mut background_color = vec![0; 4];
        let background_pixel = {
            let global_palette = self.reader.global_palette();
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
        let mut non_disposed_frame = background_img.clone();

        // define the variables used by each frame
        let mut left: u32;
        let mut top: u32;
        let mut delay: Ratio<u16>;
        let mut dispose: DisposalMethod;

        // begin looping over each frame
        loop {
            if let Some(frame) = try!(self.reader.next_frame_info()) {
                left = u32::from(frame.left);
                top = u32::from(frame.top);

                // frame.delay is in units of 10ms so frame.delay*10 is in ms
                delay = Ratio::new(frame.delay * 10, 1);
                dispose = frame.dispose;
            } else {
                // no more frames, so end the loop here
                break;
            }

            let mut vec = vec![0; self.reader.buffer_size()];
            try!(self.reader.fill_buffer(&mut vec));

            // create the image buffer from the raw frame
            if let Some(mut image_buffer) = ImageBuffer::from_raw(width, height, vec) {
                let previous_img_buffer = non_disposed_frame.clone();

                // loop over all pixels, checking if any pixels from the non disposed
                // frame need to be used
                for (x, y, pixel) in image_buffer.enumerate_pixels_mut() {
                    let mut adjusted_pixel: &mut Rgba<u8> = pixel;
                    let previous_pixel: &Rgba<u8> = previous_img_buffer.get_pixel(x, y);

                    let pixel_alpha = adjusted_pixel.channels()[3];

                    // If a pixel is not visible then we show the non disposed frame pixel instead
                    if pixel_alpha == 0 {
                        adjusted_pixel.blend(previous_pixel);
                    }
                }

                let frame = animation::Frame::from_parts(image_buffer.clone(), left, top, delay);
                frames.push(frame);

                match dispose {
                    DisposalMethod::Any => {
                        // do nothing
                        // (completely replace this frame with the next)
                    }
                    DisposalMethod::Keep => {
                        // do not dispose
                        // (keep pixels from this frame)
                        non_disposed_frame = image_buffer;
                    }
                    DisposalMethod::Background => {
                        // restore to background color
                        // (background shows through transparent pixels in the next frame)
                        non_disposed_frame = background_img.clone();
                    }
                    DisposalMethod::Previous => {
                        // restore to previous
                        // (dispose frames leaving the last none disposal frame)
                    }
                };
            };
        }
        Ok(animation::Frames::new(frames))
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
    pub fn encode_frames(&mut self, frames: animation::Frames) -> ImageResult<()> {
        for img_frame in frames {
            // get the delay before coverting img_frame
            let frame_delay = img_frame.delay().to_integer();
            // convert img_frame into RgbaImage
            let rbga_frame = img_frame.into_buffer();

            // Create the gif::Frame from the animation::Frame
            let mut frame = Frame::from_rgba(rbga_frame.width() as u16, rbga_frame.height() as u16, &mut rbga_frame.into_raw());
            frame.delay = frame_delay;

            // encode the gif::Frame
            if let Err(e) = self.encode(&frame) {
                return Err(e);
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
