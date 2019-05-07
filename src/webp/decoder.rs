use byteorder::{LittleEndian, ReadBytesExt};
use std::default::Default;
use std::io::{self, Cursor, Read};
use std::marker::PhantomData;
use std::mem;

use image;
use image::ImageDecoder;
use image::ImageResult;

use color;

use super::vp8::Frame;
use super::vp8::VP8Decoder;

/// Webp Image format decoder. Currently only supportes the luma channel (meaning that decoded
/// images will be grayscale).
pub struct WebpDecoder<R> {
    r: R,
    frame: Frame,
    have_frame: bool,
}

impl<R: Read> WebpDecoder<R> {
    /// Create a new WebpDecoder from the Reader ```r```.
    /// This function takes ownership of the Reader.
    pub fn new(r: R) -> ImageResult<WebpDecoder<R>> {
        let f: Frame = Default::default();

        let mut decoder = WebpDecoder {
            r,
            have_frame: false,
            frame: f,
        };
        decoder.read_metadata()?;
        Ok(decoder)
    }

    fn read_riff_header(&mut self) -> ImageResult<u32> {
        let mut riff = Vec::with_capacity(4);
        try!(self.r.by_ref().take(4).read_to_end(&mut riff));
        let size = try!(self.r.read_u32::<LittleEndian>());
        let mut webp = Vec::with_capacity(4);
        try!(self.r.by_ref().take(4).read_to_end(&mut webp));

        if &*riff != b"RIFF" {
            return Err(image::ImageError::FormatError(
                "Invalid RIFF signature.".to_string(),
            ));
        }

        if &*webp != b"WEBP" {
            return Err(image::ImageError::FormatError(
                "Invalid WEBP signature.".to_string(),
            ));
        }

        Ok(size)
    }

    fn read_vp8_header(&mut self) -> ImageResult<()> {
        let mut vp8 = Vec::with_capacity(4);
        try!(self.r.by_ref().take(4).read_to_end(&mut vp8));

        if &*vp8 != b"VP8 " {
            return Err(image::ImageError::FormatError(
                "Invalid VP8 signature.".to_string(),
            ));
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
            try!(self.read_riff_header());
            try!(self.read_vp8_header());
            try!(self.read_frame());

            self.have_frame = true;
        }

        Ok(())
    }
}

/// Wrapper struct around a `Cursor<Vec<u8>>`
pub struct WebpReader<R>(Cursor<Vec<u8>>, PhantomData<R>);
impl<R> Read for WebpReader<R> {
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

impl<'a, R: 'a + Read> ImageDecoder<'a> for WebpDecoder<R> {
    type Reader = WebpReader<R>;

    fn dimensions(&self) -> (u64, u64) {
        (self.frame.width as u64, self.frame.height as u64)
    }

    fn colortype(&self) -> color::ColorType {
        color::ColorType::RGB(8)
    }

    fn into_reader(self) -> ImageResult<Self::Reader> {
        Ok(WebpReader(Cursor::new(self.read_image()?), PhantomData))
    }

    fn read_image(self) -> ImageResult<Vec<u8>> {
        let mut res = Vec::with_capacity(self.frame.ybuf.len() * 3);
        for i in 0..self.frame.ybuf.len() {
            let y = self.frame.ybuf[i] as f32;
            let u = self.frame.ubuf[i] as f32;
            let v = self.frame.vbuf[i] as f32;

            let r = 298.082 * y / 256. + 408.583 * u / 256. - 222.291;
            let g = 298.082 * y / 256. - 100.291* v / 256. - 208.120 * u / 256. + 135.576;
            let b = 298.082 * y / 256. + 516.412 * v / 256. - 276.836;
            res.push(::math::utils::clamp(r as i32, 0, 255) as u8);
            res.push(::math::utils::clamp(g as i32, 0, 255) as u8);
            res.push(::math::utils::clamp(b as i32, 0, 255) as u8);
        }
        Ok(res)
    }
}
