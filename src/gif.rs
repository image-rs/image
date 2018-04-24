//!  Decoding of GIF Images
//!
//!  GIF (Graphics Interchange Format) is an image format that supports lossless compression.
//!
//!  # Related Links
//!  * <http://www.w3.org/Graphics/GIF/spec-gif89a.txt> - The GIF Specification
//!

extern crate gif;
extern crate num_rational;

use std::clone::Clone;
use std::io::{Read, Write};

use self::gif::{ColorOutput, SetParameter};
pub use self::gif::{DisposalMethod, Frame};

use animation::{Frame as Image_Frame, Frames as Image_Frames};
use buffer::{ImageBuffer, Pixel};
use color;
use color::Rgba;
use image::{DecodingResult, ImageDecoder, ImageError, ImageResult};
use num_rational::Ratio;

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
}

impl<R: Read> ImageDecoder for Decoder<R> {
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

    fn into_frames(mut self) -> ImageResult<Image_Frames> {
        let reader = try!(self.get_reader());
        let width = u32::from(reader.width());
        let height = u32::from(reader.height());

        // variable to hold all the image frames
        let mut frames = Vec::new();

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
        let mut non_disposed_frame = background_img.clone();

        // define the variables used by each frame
        let mut left: u32;
        let mut top: u32;
        let mut delay: Ratio<u16>;
        let mut dispose: DisposalMethod;

        // begin looping over each frame
        loop {
            if let Some(frame) = try!(reader.next_frame_info()) {
                left = u32::from(frame.left);
                top = u32::from(frame.top);

                // frame.delay is in units of 10ms so frame.delay*10 is in ms
                delay = Ratio::new(frame.delay * 10, 1);
                dispose = frame.dispose;
            } else {
                // no more frames, so end the loop here
                break;
            }

            let mut vec = vec![0; reader.buffer_size()];
            try!(reader.fill_buffer(&mut vec));

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

                let frame = Image_Frame::from_parts(image_buffer.clone(), left, top, delay);
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
        Ok(Image_Frames::new(frames))
    }
}

/// GIF encoder.
pub struct Encoder<W: Write> {
    w: W,
}

impl<W: Write> Encoder<W> {
    /// Creates a new GIF encoder.
    pub fn new(w: W) -> Encoder<W> {
        Encoder { w }
    }
    /// Encodes a frame.
    pub fn encode(self, frame: Frame) -> ImageResult<()> {
        let mut encoder = try!(gif::Encoder::new(self.w, frame.width, frame.height, &[]));
        encoder.write_frame(&frame).map_err(|err| err.into())
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
