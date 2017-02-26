use std::io::Read;
use std::io::BufReader;

use color::{ColorType};
use image::{DecodingResult, ImageDecoder, ImageResult, ImageError};
extern crate byteorder;
use self::byteorder::{BigEndian, ByteOrder};


/// PPM decoder
pub struct PPMDecoder<R> {
    reader: BufReader<R>,
    width: u32,
    height: u32,
    maxwhite: u32,
    row: u32,
}

impl<R: Read> PPMDecoder<R> {
    /// Create a new decoder that decodes from the stream ```r```
    pub fn new(r: R) -> ImageResult<PPMDecoder<R>> {
        let mut buf = BufReader::new(r);
        try!(PPMDecoder::read_next_string(&mut buf)); // Skip P6
        let width = try!(PPMDecoder::read_next_u32(&mut buf));
        let height = try!(PPMDecoder::read_next_u32(&mut buf));
        let maxwhite = try!(PPMDecoder::read_next_u32(&mut buf));
        Ok(PPMDecoder {
            reader: buf,
            width: width,
            height: height,
            maxwhite: maxwhite,
            row: 0,
        })
    }

  fn read_next_string(reader: &mut BufReader<R>) -> ImageResult<String> {
      let mut bytes = Vec::new();
      for byte in reader.bytes() {
          match byte {
              Ok(b'\n') | Ok(b' ') | Ok(b'\r') | Ok(b'\t') => {
                  if bytes.len() > 0 {
                    break // We're done as we already have some content
                  }
              },
              Ok(byte) => {
                  bytes.push(byte);
              },
              Err(_) => break,
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
        match self.bytewidth() {
            1 => Ok(ColorType::RGB(8)),
            2 => Ok(ColorType::RGB(16)),
            _ => Err(ImageError::FormatError("Don't know how to decode PPM with more than 16 bits".to_string())),
        }
    }

    fn row_len(&mut self) -> ImageResult<usize> {
        Ok((self.width*3*self.bytewidth()) as usize)
    }

    fn read_scanline(&mut self, buf: &mut [u8]) -> ImageResult<u32> {
        let row_len = try!(self.row_len());
        match self.reader.read_exact(&mut buf[0..row_len]) {
            Ok(_) => {self.row += 1; Ok(self.row-1)},
            Err(e) => Err(ImageError::IoError(e)),
        }
    }

    fn read_image(&mut self) -> ImageResult<DecodingResult> {
        let mut data = vec![0 as u8; (self.width*self.height*3*self.bytewidth()) as usize];
        for line in data.chunks_mut(try!(self.row_len())) {
          try!(self.read_scanline(line));
        }

        if self.bytewidth() == 1 {
            Ok(DecodingResult::U8(data))
        } else {
            let mut out = vec![0 as u16; (self.width*self.height*3) as usize];
            for (o, i) in out.chunks_mut(1).zip(data.chunks(2)) {
                o[0] = BigEndian::read_u16(i);
            }
            Ok(DecodingResult::U16(out))
        }
    }
}

impl<R: Read> PPMDecoder<R> {
    fn bytewidth(&self) -> u32 {
        if self.maxwhite < 256 { 1 } else { 2 }
    }
}
