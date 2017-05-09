use std::io::{self, Write};
use byteorder::{WriteBytesExt, LittleEndian};

use color;

/// The representation of a BMP encoder.
pub struct BMPEncoder<'a, W: 'a> {
    writer: &'a mut W,
}

impl<'a, W: Write + 'a> BMPEncoder<'a, W> {
    /// Create a new encoder that writes its output to ```w```.
    pub fn new(w: &'a mut W) -> Self {
        BMPEncoder {
            writer: w,
        }
    }

    /// Encodes the image ```image```
    /// that has dimensions ```width``` and ```height```
    /// and ```ColorType``` ```c```.
    pub fn encode(&mut self,
                  image: &[u8],
                  width: u32,
                  height: u32,
                  c: color::ColorType) -> io::Result<()> {

        let bmp_header_size = 14;
        let dib_header_size = 40;

        let row_pad_size = width % 4; // each row must be padded to a multiple of 4 bytes

        // always encoded as 24bpp RGB
        let image_size = width * height * 3 + (height * row_pad_size);
        let file_size = bmp_header_size + dib_header_size + image_size;

        // write BMP header
        try!(self.writer.write_u8('B' as u8));
        try!(self.writer.write_u8('M' as u8));
        try!(self.writer.write_u32::<LittleEndian>(file_size)); // file size
        try!(self.writer.write_u16::<LittleEndian>(0)); // reserved 1
        try!(self.writer.write_u16::<LittleEndian>(0)); // reserved 2
        try!(self.writer.write_u32::<LittleEndian>(bmp_header_size + dib_header_size)); // image data offset

        // write DIB header
        try!(self.writer.write_u32::<LittleEndian>(dib_header_size));
        try!(self.writer.write_i32::<LittleEndian>(width as i32));
        try!(self.writer.write_i32::<LittleEndian>(height as i32));
        try!(self.writer.write_u16::<LittleEndian>(1)); // color planes
        try!(self.writer.write_u16::<LittleEndian>(24)); // 24 bpp
        try!(self.writer.write_u32::<LittleEndian>(0)); // compression method - no compression
        try!(self.writer.write_u32::<LittleEndian>(image_size));
        try!(self.writer.write_i32::<LittleEndian>(0)); // horizontal ppm
        try!(self.writer.write_i32::<LittleEndian>(0)); // vertical ppm
        try!(self.writer.write_u32::<LittleEndian>(0)); // color palette size - no palette
        try!(self.writer.write_u32::<LittleEndian>(0)); // all colors are important

        // write image data
        let x_stride = match c {
            color::ColorType::RGB(8) => 3,
            _  => return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                &format!("Unsupported color type {:?}. Use 8 bit per channel RGB instead.", c)[..],
            )),
        };
        let y_stride = height * x_stride;

        for row in 0..height {
            // from the bottom up
            let row_start = ((height - row - 1) * y_stride) as usize;
            for col in 0..width {
                let pixel_start = row_start + (col * x_stride) as usize;
                let r = image[pixel_start];
                let g = image[pixel_start + 1];
                let b = image[pixel_start + 2];
                // written as BGR
                try!(self.writer.write_u8(b));
                try!(self.writer.write_u8(g));
                try!(self.writer.write_u8(r));
            }

            for _ in 0..row_pad_size {
                try!(self.writer.write_u8(0));
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;
    use super::BMPEncoder;
    use super::super::BMPDecoder;
    use color::ColorType;
    use image::{ImageDecoder, DecodingResult};

    #[test]
    fn round_trip_single_pixel() {
        // write single red pixel
        let img = [255u8, 0, 0];
        let mut encoded = Vec::new();
        {
            let mut encoder = BMPEncoder::new(&mut encoded);
            encoder.encode(&img, 1, 1, ColorType::RGB(8)).expect("could not encode image");
        }

        // decode from memory
        let mut decoder = BMPDecoder::new(Cursor::new(&encoded));
        match decoder.read_image().expect("failed to decode") {
            DecodingResult::U8(decoded) => {
                assert_eq!(3, decoded.len());
                assert_eq!(255, decoded[0]);
                assert_eq!(0, decoded[1]);
                assert_eq!(0, decoded[2]);
            },
            _ => panic!("image did not decode as 24bpp"),
        }
    }
}
