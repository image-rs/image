use std::io::Read;

use color::{ColorType};
use image::{DecodingResult, ImageDecoder, ImageResult, ImageError};
use pnm::{PNMDecoder, PNMSubtype};

/// PPM decoder, restriction pnm type to ppm
pub struct PPMDecoder<R>(PNMDecoder<R>);

impl<R: Read> PPMDecoder<R> {
    /// Create a new pnm decoder and asserts it is ppm
    pub fn new(read: R) -> ImageResult<PPMDecoder<R>> {
        let pnm = PNMDecoder::new(read)?;
        match pnm.subtype() {
            PNMSubtype::Pixmap => {},
            _ => return Err(ImageError::FormatError("Expected pixmap magic constant (P3 or P6)".to_string())),
        }
        Ok(PPMDecoder(pnm))
    }
}

impl<R: Read> ImageDecoder for PPMDecoder<R> {
    fn dimensions(&mut self) -> ImageResult<(u32, u32)> {
        self.0.dimensions()
    }

    fn colortype(&mut self) -> ImageResult<ColorType> {
        self.0.colortype()
    }

    fn row_len(&mut self) -> ImageResult<usize> {
        self.0.row_len()
    }

    fn read_scanline(&mut self, mut buf: &mut [u8]) -> ImageResult<u32> {
        self.0.read_scanline(&mut buf)
    }

    fn read_image(&mut self) -> ImageResult<DecodingResult> {
        self.0.read_image()
    }
}

/// Tests parsing binary buffers were written based on and validated against `pamfile` from
/// netpbm (http://netpbm.sourceforge.net/).
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn minimal_form() {
        // Violates current specification (October 2016 ) but accepted by both netpbm and ImageMagick
        decode_minimal_image(&b"P61 1 255 123"[..]);
        decode_minimal_image(&b"P6 1 1 255 123"[..]);
        decode_minimal_image(&b"P6 1 1 255 123\xFF"[..]); // Too long should not be an issue
    }

    #[test]
    fn comment_in_token() {
        decode_minimal_image(&b"P6 1 1 2#comment\n55 123"[..]); // Terminating LF
        decode_minimal_image(&b"P6 1 1 2#comment\r55 123"[..]); // Terminating CR
        decode_minimal_image(&b"P6 1 1#comment\n 255 123"[..]); // Comment after token
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

    #[test]
    fn ascii_encoded() {
        decode_minimal_image(&b"P31 1 255 49 50 51"[..]);
        assert!(PPMDecoder::new(&b"P31 1 65535 65535 65535 65535"[..]).unwrap()
            .read_image().is_ok()); // Maximum sample size
        decode_minimal_image(&b"P31 1 255  49 50 51"[..]); // Whitespace after header
        decode_minimal_image(&b"P31 1 255 49\n\t 50\r\x0b\x0c51"[..]); // All forms of whitespace
    }

    /// Tests for decoding error, assuming `encoded` is ppm encoding for the very simplistic image
    /// containing a single pixel with one byte values (1, 2, 3).
    fn decode_minimal_image(encoded: &[u8]) {
        let content = vec![49 as u8, 50, 51];
        let mut decoder = PPMDecoder::new(encoded).unwrap();

        assert_eq!(decoder.dimensions().unwrap(), (1, 1));
        assert_eq!(decoder.colortype().unwrap(), ColorType::RGB(8));
        assert_eq!(decoder.row_len().unwrap(), 3);

        match decoder.read_image().unwrap() {
            DecodingResult::U8(image) => assert_eq!(image, content),
            _ => assert!(false),
        }
    }

    #[test]
    fn wrong_tag() {
        assert!(PPMDecoder::new(&b"P5 1 1 255 1"[..]).is_err());
    }

    #[test]
    fn invalid_characters() {
        assert!(PPMDecoder::new(&b"P6 1chars1 255 1"[..]).is_err()); // No text outside of comments
        assert!(PPMDecoder::new(&b"P6 1\xFF1 255 1"[..]).is_err()); // No invalid ascii chars
        assert!(PPMDecoder::new(&b"P6 0x01 1 255 1"[..]).is_err()); // Numbers only as decimal
    }

    /// These violate the narrow specification of ppm but are commonly supported in other programs.
    /// Fail fast and concise is important here as these might be received as input files.
    #[test]
    fn unsupported_extensions() {
        assert!(PPMDecoder::new(&b"P6 1 1 65536 1"[..]).is_err()); // No bitwidth above 16
    }
}
