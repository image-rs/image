//! Encoding of PPM Images

use std::io;
use std::io::Write;

use color;
use color::ColorType:: {
    Gray,
    Palette,
    GrayA,
    RGB,
    RGBA,
};

/// A representation of a PPM encoder.
pub struct PPMEncoder<'a, W: 'a> {
    w: &'a mut W
}

impl<'a, W: Write> PPMEncoder<'a, W> {
    /// Create a new PPMEncoder from the Writer ```w```.
    pub fn new(w: &mut W) -> PPMEncoder<W> {
        PPMEncoder { w: w }
    }

    /// Encode the buffer ```im``` as a PPM image.
    /// ```width``` and ```height``` are the dimensions of the buffer.
    /// ```color``` is the buffers ColorType.
    pub fn encode(&mut self, im: &[u8], width: u32, height: u32, color: color::ColorType) -> io::Result<()> {
        try!(self.write_magic_number());
        try!(self.write_metadata(width, height, color));

        self.write_image(im, color, width, height)
    }

    fn write_magic_number(&mut self) -> io::Result<()> {
        write!(self.w, "P6\n")
    }

    fn write_metadata(&mut self, width: u32, height: u32, pixel_type: color::ColorType) -> io::Result<()> {
        let m = max_pixel_value(pixel_type);
        write!(self.w, "{0} {1}\n{2}\n", width, height, m)
    }

    fn write_image(
        &mut self,
        buf: &[u8],
        pixel_type: color::ColorType,
        width: u32,
        height: u32) -> io::Result<()> {

        assert!(!buf.is_empty());
        match pixel_type {
            Gray(8) => {
                for i in 0..(width * height) as usize {
                    try!(self.w.write_all(&[buf[i]]));
                    try!(self.w.write_all(&[buf[i]]));
                    try!(self.w.write_all(&[buf[i]]));
                }
            }
            RGB(8) | RGB(16) => try!(self.w.write_all(buf)),
            RGBA(8) => {
                for x in buf.chunks(4) {
                    try!(self.w.write_all(&[x[0]]));
                    try!(self.w.write_all(&[x[1]]));
                    try!(self.w.write_all(&[x[2]]));
                }
            }
            a => panic!(format!("not implemented: {:?}", a))
        }

        Ok(())
    }
}

fn max_pixel_value(pixel_type: color::ColorType) -> u16 {
    let max = match pixel_type {
        Gray(n) | RGB(n) | Palette(n) | GrayA(n) | RGBA(n) => 2u32.pow(n as u32) - 1,
    };

    if max > 65535 {
      panic!("PPM: Trying to encode image with more than 16bit per pixel");
    } else {
      max as u16
    }
}
