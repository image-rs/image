//! Encoding of PPM Images

use std::old_io::*;
use std::fmt;

use color;
use color::ColorType:: {
    Gray,
    Palette,
    GrayA,
    RGB,
    RGBA
};

/// A representation of a PPM encoder.
pub struct PPMEncoder<'a, W: 'a> {
    w: &'a mut W
}

impl<'a, W: Writer> PPMEncoder<'a, W> {
    /// Create a new PPMEncoder from the Writer ```w```.
    /// This function takes ownership of the Writer.
    pub fn new(w: &mut W) -> PPMEncoder<W> {
        PPMEncoder { w: w }
    }

    /// Encode the buffer ```im``` as a PPM image.
    /// ```width``` and ```height``` are the dimensions of the buffer.
    /// ```color``` is the buffers ColorType.
    pub fn encode(&mut self, im: &[u8], width: u32, height: u32, color: color::ColorType) -> IoResult<()> {
        let _ = try!(self.write_magic_number());
        let _ = try!(self.write_metadata(width, height, color));

        self.write_image(im, color, width, height)
    }

    fn write_magic_number(&mut self) -> IoResult<()> {
        self.w.write_str("P6\n")
    }

    fn write_metadata(&mut self, width: u32, height: u32, pixel_type: color::ColorType) -> IoResult<()> {
        let w = fmt::radix(width, 10);
        let h = fmt::radix(height, 10);
        let m = max_pixel_value(pixel_type);

        self.w.write_str(&format!("{0} {1}\n{2}\n", w, h, m))
    }

    fn write_image(
        &mut self,
        buf: &[u8],
        pixel_type: color::ColorType,
        width: u32,
        height: u32) -> IoResult<()> {

        assert!(buf.len() > 0);
        match pixel_type {
            Gray(8) => {
                for i in (0..(width * height) as usize) {
                    let _ = try!(self.w.write_u8(buf[i]));
                    let _ = try!(self.w.write_u8(buf[i]));
                    let _ = try!(self.w.write_u8(buf[i]));
                }
            }

            RGB(8)  => try!(self.w.write_all(buf)),
            RGB(16) => try!(self.w.write_all(buf)),
            RGBA(8) => {
                for x in buf.chunks(4) {

                    let _ = try!(self.w.write_u8(x[0]));

                    let _ = try!(self.w.write_u8(x[1]));

                    let _ = try!(self.w.write_u8(x[2]));
                }
            }

            a => panic!(format!("not implemented: {:?}", a))
        }

        Ok(())
    }
}

fn max_pixel_value(pixel_type: color::ColorType) -> u16 {
    use std::num::Int;

    match pixel_type {
        Gray(n)    => 2u16.pow(n as u32) - 1,
        RGB(n)     => 2u16.pow(n as u32) - 1,
        Palette(n) => 2u16.pow(n as u32) - 1,
        GrayA(n)   => 2u16.pow(n as u32) - 1,
        RGBA(n)    => 2u16.pow(n as u32) - 1
    }
}
