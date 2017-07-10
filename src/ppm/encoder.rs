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
    BGR,
    BGRA,
};

/// A representation of a PPM encoder.
pub struct PPMEncoder<'a, W: 'a> {
    w: &'a mut W
}

impl<'a, W: Write> PPMEncoder<'a, W> {
    /// Create a new PPMEncoder from the Writer ```w```.
    /// This function takes ownership of the Writer.
    pub fn new(w: &mut W) -> PPMEncoder<W> {
        PPMEncoder { w: w }
    }

    /// Encode the buffer ```im``` as a PPM image.
    /// ```width``` and ```height``` are the dimensions of the buffer.
    /// ```color``` is the buffers ColorType.
    pub fn encode(&mut self, im: &[u8], width: u32, height: u32, color: color::ColorType) -> io::Result<()> {
        try!(self.write_magic_number());
        try!(self.write_metadata(width, height, color));

        self.write_image(im, color)
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
        pixel_type: color::ColorType) -> io::Result<()> {

        assert!(buf.len() > 0);
        match pixel_type {
            Gray(8) => {
                for &x in buf {
                    try!(self.w.write_all(&[x, x, x]));
                }
            }
            GrayA(8) => {
                for x in buf.chunks(2) {
                    try!(self.w.write_all(&[x[0], x[0], x[0]]));
                }
            }
            RGB(8)  => try!(self.w.write_all(buf)),
            RGB(16) => try!(self.w.write_all(buf)),
            RGBA(8) => {
                for x in buf.chunks(4) {
                    try!(self.w.write_all(&[x[0], x[1], x[2]]));
                }
            }
            BGR(8) => {
                for x in buf.chunks(3) {
                    try!(self.w.write_all(&[x[2], x[1], x[0]]));
                }
            }
            BGRA(8) => {
                for x in buf.chunks(4) {
                    try!(self.w.write_all(&[x[2], x[1], x[0]]));
                }
            }
            _ => unimplemented!()
        }

        Ok(())
    }
}

fn max_pixel_value(pixel_type: color::ColorType) -> u16 {
    let max = match pixel_type {
        Gray(n)    => 2u32.pow(n as u32) - 1,
        RGB(n)     => 2u32.pow(n as u32) - 1,
        Palette(n) => 2u32.pow(n as u32) - 1,
        GrayA(n)   => 2u32.pow(n as u32) - 1,
        RGBA(n)    => 2u32.pow(n as u32) - 1,
        BGR(n)     => 2u32.pow(n as u32) - 1,
        BGRA(n)    => 2u32.pow(n as u32) - 1,
    };

    if max > 65535 {
        panic!("PPM: Trying to encode image with more than 16bit per pixel");
    }

    max as u16
}
