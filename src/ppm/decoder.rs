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
        })
    }

    /// Reads a string as well as a single whitespace after it, ignoring comments
    fn read_next_string(reader: &mut BufReader<R>) -> ImageResult<String> {
        let mut bytes = Vec::new();
        let mut comment = false;

        for byte in reader.bytes() {
            match byte {
                Ok(b'\n') | Ok(b'\r') if comment => {
                    // End of comment, this does not end tokens (http://netpbm.sourceforge.net/doc/ppm.html)
                    comment = false;
                },
                Ok(_) if comment => {
                    // Ignore all other bytes in comments
                },
                Ok(b'\n') | Ok(b' ') | Ok(b'\r') | Ok(b'\t') => {
                    if !bytes.is_empty() {
                        break // We're done as we already have some content
                    }
                },
                Ok(b'#') => {
                    comment = true;
                },
                Ok(byte) => {
                    bytes.push(byte);
                },
                Err(_) => break,
            }
        }

        if comment && bytes.is_empty() {
            return Err(ImageError::FormatError("Unexpected eof".to_string()))
        }

        String::from_utf8(bytes).map_err(|_| ImageError::FormatError("Couldn't read preamble".to_string()))
    }

    fn read_next_u32(reader: &mut BufReader<R>) -> ImageResult<u32> {
        let s = try!(PPMDecoder::read_next_string(reader));
        s.parse::<u32>().map_err(|_| ImageError::FormatError("Couldn't read preamble".to_string()))
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

    fn read_scanline(&mut self, _buf: &mut [u8]) -> ImageResult<u32> {
        unimplemented!();
    }

    fn read_image(&mut self) -> ImageResult<DecodingResult> {
        let opt_size = self.width.checked_mul(self.height)
            .map_or(None, |v| v.checked_mul(3))
            .map_or(None, |v| v.checked_mul(self.bytewidth()));

        let size = match opt_size {
            Some(v) => v,
            None => return Err(ImageError::DimensionError),
        };

        let mut data = vec![0 as u8; size as usize];

        match self.reader.read_exact(&mut data) {
            Ok(_) => {},
            Err(e) => return Err(ImageError::IoError(e)),
        };

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

/// Tests parsing binary buffers were written based on and validated against `identify` from
/// ImageMagick (https://www.imagemagick.org/script/identify.php).
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn minimal_form() {
        // Violates current specification (October 2016 ) but accepted by both netpbm and ImageMagick
        decode_minimal_image(&b"P61 1 255 \x01\x02\x03"[..]);
        decode_minimal_image(&b"P6 1 1 255 123"[..]);
        decode_minimal_image(&b"P6 1 1 255 123\xFF"[..]); // Too long should not be an issue
    }

    #[test]
    fn comment_in_token() {
        decode_minimal_image(&b"P6 1 1 2#comment\n55 123"[..]); // Terminating LF
        decode_minimal_image(&b"P6 1 1 2#comment\r55 123"[..]); // Terminating CR
        decode_minimal_image(&b"P6 1 1 255#comment\n 123"[..]); // Comment after token
        decode_minimal_image(&b"P6 1 1 #comment\n255 123"[..]); // Comment before token
        decode_minimal_image(&b"P6#comment\n 1 1 255 123"[..]); // Begin of header
        decode_minimal_image(&b"P6 1 1 255#comment\n 123"[..]); // End of header
    }

    #[test]
    fn whitespace() {
        decode_minimal_image(&b"P6\x091\x091\x09255\x09123"[..]); // TAB
        decode_minimal_image(&b"P6\x0a1\x0a1\x0a255\x0a123"[..]); // LF
        decode_minimal_image(&b"P6\x0b1\x0b1\x0b255\x0b123"[..]); // VT
        decode_minimal_image(&b"P6\x0c1\x0c1\x0c255\x0c123"[..]); // FF
        decode_minimal_image(&b"P6\x0d1\x0d1\x0d255\x0d123"[..]); // CR
        // Spaces tested before
        decode_minimal_image(&b"P61\x09\x0a\x0b\x0c\x0d1 255 123"[..]); // All whitespace, combined
    }

    /// Tests for decoding error, assuming `encoded` is ppm encoding for the very simplistic image
    /// containing a single pixel with one byte values (1, 2, 3).
    fn decode_minimal_image(encoded: &[u8]) {
        let content = vec![49 as u8, 50, 51];
        let mut decoder = super::PPMDecoder::new(encoded).unwrap();

        assert_eq!(decoder.dimensions().unwrap(), (1, 1));
        assert_eq!(decoder.colortype().unwrap(), ColorType::RGB(8));
        assert_eq!(decoder.row_len().unwrap(), 3);
        assert_eq!(decoder.bytewidth(), 1);

        match decoder.read_image().unwrap() {
            DecodingResult::U8(image) => assert_eq!(image, content),
            _ => assert!(false),
        }
    }
}
