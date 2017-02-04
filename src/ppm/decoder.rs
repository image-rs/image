use std::io::Read;
use std::io::BufReader;

use color::{ColorType};
use image::{DecodingResult, ImageDecoder, ImageResult, ImageError};

/// PPM decoder
pub struct PPMDecoder<R> {
    reader: BufReader<R>,
    width: u32,
    height: u32,
}

impl<R: Read> PPMDecoder<R> {
    /// Create a new decoder that decodes from the stream ```r```
    pub fn new(r: R) -> ImageResult<PPMDecoder<R>> {
        let mut buf = BufReader::new(r);
        try!(PPMDecoder::read_next_string(&mut buf)); // Skip P6
        let width = try!(PPMDecoder::read_next_u32(&mut buf));
        let height = try!(PPMDecoder::read_next_u32(&mut buf));
        try!(PPMDecoder::read_next_u32(&mut buf)); // Skip maxwhite
        Ok(PPMDecoder {
            reader: buf,
            width: width,
            height: height,
        })
    }

  fn read_next_string(reader: &mut BufReader<R>) -> ImageResult<String> {
      let mut bytes = Vec::new();
      loop {
          let mut buffer: [u8;1] = [0];
          try!(reader.read_exact(&mut buffer));
          match buffer[0] {
              b'\n' | b' ' | b'\r' => {
                  if bytes.len() > 0 {
                    break // We're done as we already have some content
                  }
              },
              _ => {
                  bytes.push(buffer[0]);
              },
          }
      }

      match String::from_utf8(bytes) {
          Ok(s) => Ok(s),
          Err(_) => Err(ImageError::FormatError("Couldn't read preamble".to_string())),
      }
  }

  fn read_next_u32(reader: &mut BufReader<R>) -> ImageResult<u32> {
      let s = try!(PPMDecoder::read_next_string(reader));
      match s.parse::<u32>() {
          Ok(v) => Ok(v),
          Err(_) => Err(ImageError::FormatError("Couldn't read preamble".to_string())),
      }
  }
}

impl<R: Read> ImageDecoder for PPMDecoder<R> {
    fn dimensions(&mut self) -> ImageResult<(u32, u32)> {
        Ok((self.width, self.height))
    }

    fn colortype(&mut self) -> ImageResult<ColorType> {
        Ok(ColorType::RGB(8))
    }

    fn row_len(&mut self) -> ImageResult<usize> {
        Ok((self.width*3) as usize)
    }

    fn read_scanline(&mut self, _buf: &mut [u8]) -> ImageResult<u32> {
        unimplemented!();
    }

    fn read_image(&mut self) -> ImageResult<DecodingResult> {
        let mut data = vec![0 as u8; (self.width*self.height*3) as usize];
        match self.reader.read_exact(&mut data) {
            Ok(_) => {},
            Err(e) => return Err(ImageError::IoError(e)),
        };

        Ok(DecodingResult::U8(data))
    }
}
