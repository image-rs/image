use std::slice;
use std::io::MemReader;
use std::default::Default;

use image;
use image::ImageResult;
use image::ImageDecoder;

use color;

use super::vp8::Frame;
use super::vp8::VP8Decoder;



/// A Representation of a Webp Image format decoder.
pub struct WebpDecoder<R> {
    r: R,
    frame: Frame,
    have_frame: bool,
    decoded_rows: u32,
}

impl<R: Reader> WebpDecoder<R> {
    /// Create a new WebpDecoder from the Reader ```r```.
    /// This function takes ownership of the Reader.
    pub fn new(r: R) -> WebpDecoder<R> {
        let f: Frame = Default::default();

        WebpDecoder {
            r: r,
            have_frame: false,
            frame: f,
            decoded_rows: 0
        }
    }

    fn read_riff_header(&mut self) -> ImageResult<u32> {
        let riff = try!(self.r.read_exact(4));
        let size = try!(self.r.read_le_u32());
        let webp = try!(self.r.read_exact(4));

        if &riff[] != "RIFF".as_bytes() {
            return Err(image::ImageError::FormatError("Invalid RIFF signature.".to_string()))
        }

        if &webp[] != "WEBP".as_bytes() {
            return Err(image::ImageError::FormatError("Invalid WEBP signature.".to_string()))
        }

        Ok(size)
    }

    fn read_vp8_header(&mut self) -> ImageResult<()> {
        let vp8 = try!(self.r.read_exact(4));

        if &vp8[] != "VP8 ".as_bytes() {
            return Err(image::ImageError::FormatError("Invalid VP8 signature.".to_string()))
        }

        let _len = try!(self.r.read_le_u32());

        Ok(())
    }

    fn read_frame(&mut self) -> ImageResult<()> {
        let framedata = try!(self.r.read_to_end());
        let m = MemReader::new(framedata);

        let mut v = VP8Decoder::new(m);
        let frame = try!(v.decode_frame());

        self.frame = frame.clone();

        Ok(())
    }

    fn read_metadata(&mut self) -> ImageResult<()> {
        if !self.have_frame {
            let _ = try!(self.read_riff_header());
            let _ = try!(self.read_vp8_header());
            let _ = try!(self.read_frame());

            self.have_frame = true;
        }

        Ok(())
    }
}

impl<R: Reader> ImageDecoder for WebpDecoder<R> {
    fn dimensions(&mut self) -> ImageResult<(u32, u32)> {
        let _ = try!(self.read_metadata());

        Ok((self.frame.width as u32, self.frame.height as u32))
    }

    fn colortype(&mut self) -> ImageResult<color::ColorType> {
        Ok(color::ColorType::Grey(8))
    }

    fn row_len(&mut self) -> ImageResult<usize> {
        let _ = try!(self.read_metadata());

        Ok(self.frame.width as usize)
    }

    fn read_scanline(&mut self, buf: &mut [u8]) -> ImageResult<u32> {
        let _ = try!(self.read_metadata());

        if self.decoded_rows > self.frame.height as u32 {
            return Err(image::ImageError::ImageEnd)
        }

        let rlen  = buf.len();
        let slice = &self.frame.ybuf[
            self.decoded_rows as usize * rlen..
            self.decoded_rows as usize * rlen + rlen
        ];

        slice::bytes::copy_memory(buf, slice);
        self.decoded_rows += 1;

        Ok(self.decoded_rows)
    }

    fn read_image(&mut self) -> ImageResult<image::DecodingResult> {
        let _ = try!(self.read_metadata());

        Ok(image::DecodingResult::U8(self.frame.ybuf.clone()))
    }
}
