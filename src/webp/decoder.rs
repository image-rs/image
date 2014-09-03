use std::slice;
use std::io::MemReader;
use std::default::Default;

use image;
use image::ImageResult;
use image::ImageDecoder;

use color;

use super::vp8::Frame;
use super::vp8::VP8Decoder;

macro_rules! io_try(
    ($e: expr) => (
        match $e {
            Ok(e) => e,
            Err(err) => return Err(image::IoError(err))
        }
    )
)

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
        let riff = io_try!(self.r.read_exact(4));
        let size = io_try!(self.r.read_le_u32());
        let webp = io_try!(self.r.read_exact(4));

        if riff.as_slice() != "RIFF".as_bytes() {
            return Err(image::FormatError("Invalid RIFF signature.".to_string()))
        }

        if webp.as_slice() != "WEBP".as_bytes() {
            return Err(image::FormatError("Invalid WEBP signature.".to_string()))
        }

        Ok(size)
    }

    fn read_vp8_header(&mut self) -> ImageResult<()> {
        let vp8 = io_try!(self.r.read_exact(4));

        if vp8.as_slice() != "VP8 ".as_bytes() {
            return Err(image::FormatError("Invalid VP8 signature.".to_string()))
        }

        let _len = io_try!(self.r.read_le_u32());

        Ok(())
    }

    fn read_frame(&mut self) -> ImageResult<()> {
        let framedata = io_try!(self.r.read_to_end());
        let m = MemReader::new(framedata);

        let mut v = VP8Decoder::new(m);
        let frame = io_try!(v.decode_frame());

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
        Ok(color::Grey(8))
    }

    fn row_len(&mut self) -> ImageResult<uint> {
        let _ = try!(self.read_metadata());

        Ok(self.frame.width as uint)
    }

    fn read_scanline(&mut self, buf: &mut [u8]) -> ImageResult<u32> {
        let _ = try!(self.read_metadata());

        if self.decoded_rows > self.frame.height as u32 {
            return Err(image::ImageEnd)
        }

        let rlen  = buf.len();
        let slice = self.frame.ybuf.slice(
            self.decoded_rows as uint * rlen,
            self.decoded_rows as uint * rlen + rlen
        );

        slice::bytes::copy_memory(buf, slice);
        self.decoded_rows += 1;

        Ok(self.decoded_rows)
    }

    fn read_image(&mut self) -> ImageResult<Vec<u8>> {
        let _ = try!(self.read_metadata());

        Ok(self.frame.ybuf.clone())
    }
}