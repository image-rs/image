//! Encoding of PPM Images

use std::io::IoResult;
use std::fmt;

use color;
use color::ColorType:: {
    Grey,
    Palette,
    GreyA,
    RGB,
    RGBA
};

/// A representation of a PPM encoder.
pub struct PPMEncoder<W> {
    w: W
}

impl<W: Writer> PPMEncoder<W> {
    /// Create a new PPMEncoder from the Writer ```w```.
    /// This function takes ownership of the Writer.
    pub fn new(w: W) -> PPMEncoder<W> {
        PPMEncoder {w: w}
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

        self.w.write_str(format!("{0} {1}\n{2}\n", w, h, m).as_slice())
    }

    fn write_image(
        &mut self,
        buf: &[u8],
        pixel_type: color::ColorType,
        width: u32,
        height: u32) -> IoResult<()> {

        assert!(buf.len() > 0);
        match pixel_type {
            Grey(8) => {
                for i in range(0, (width * height) as uint) {
                    let _ = try!(self.w.write_u8(buf[i]));
                    let _ = try!(self.w.write_u8(buf[i]));
                    let _ = try!(self.w.write_u8(buf[i]));
                }
            }

            RGB(8)  => try!(self.w.write(buf)),
            RGB(16) => try!(self.w.write(buf)),
            RGBA(8) => {
                for x in buf.chunks(4) {

                    let _ = try!(self.w.write_u8(x[0]));

                    let _ = try!(self.w.write_u8(x[1]));

                    let _ = try!(self.w.write_u8(x[2]));
                }
            }

            a => panic!(format!("not implemented: {}", a))
        }

        Ok(())
    }
}

fn max_pixel_value(pixel_type: color::ColorType) -> u16 {
    use std::num;

    match pixel_type {
        Grey(n)    => num::pow(2, n as uint) - 1,
        RGB(n)     => num::pow(2, n as uint) - 1,
        Palette(n) => num::pow(2, n as uint) - 1,
        GreyA(n)   => num::pow(2, n as uint) - 1,
        RGBA(n)    => num::pow(2, n as uint) - 1
    }
}
