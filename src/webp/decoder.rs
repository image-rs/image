use std::io;
use std::io::Read;
use std::default::Default;
use byteorder::{ReadBytesExt, LittleEndian};

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

impl<R: Read> WebpDecoder<R> {
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
        let mut riff = Vec::with_capacity(4);
        try!(self.r.by_ref().take(4).read_to_end(&mut riff));
        let size = try!(self.r.read_u32::<LittleEndian>());
        let mut webp = Vec::with_capacity(4);
        try!(self.r.by_ref().take(4).read_to_end(&mut webp));

        if &*riff != "RIFF".as_bytes() {
            return Err(image::ImageError::FormatError("Invalid RIFF signature.".to_string()))
        }

        if &*webp != "WEBP".as_bytes() {
            return Err(image::ImageError::FormatError("Invalid WEBP signature.".to_string()))
        }

        Ok(size)
    }

    fn read_vp8_header(&mut self) -> ImageResult<()> {
        let mut vp8 = Vec::with_capacity(4);
        try!(self.r.by_ref().take(4).read_to_end(&mut vp8));

        if &*vp8 != "VP8 ".as_bytes() {
            return Err(image::ImageError::FormatError("Invalid VP8 signature.".to_string()))
        }

        let _len = try!(self.r.read_u32::<LittleEndian>());

        Ok(())
    }

    fn read_frame(&mut self) -> ImageResult<()> {
        let mut framedata = Vec::new();
        try!(self.r.read_to_end(&mut framedata));
        let m = io::Cursor::new(framedata);

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

impl<R: Read> ImageDecoder for WebpDecoder<R> {
    fn dimensions(&mut self) -> ImageResult<(u32, u32)> {
        let _ = try!(self.read_metadata());

        Ok((self.frame.width as u32, self.frame.height as u32))
    }

    fn colortype(&mut self) -> ImageResult<color::ColorType> {
        Ok(color::ColorType::Gray(8))
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

        ::copy_memory(slice, buf);
        self.decoded_rows += 1;

        Ok(self.decoded_rows)
    }

    fn read_image(&mut self) -> ImageResult<image::DecodingResult> {
        let _ = try!(self.read_metadata());

        Ok(image::DecodingResult::U8(self.frame.ybuf.clone()))
    }
}
