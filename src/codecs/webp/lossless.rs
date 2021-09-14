use std::{error, fmt, io::Read};

use byteorder::{LittleEndian, ReadBytesExt};

use crate::{ImageError, ImageFormat, ImageResult, error::DecodingError};

#[derive(Debug, Clone, Copy)]
enum DecoderError {
    /// Signature of 0x2f not found
    LosslessSignatureInvalid(u8),
}

impl fmt::Display for DecoderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DecoderError::LosslessSignatureInvalid(sig) => 
                f.write_fmt(format_args!("Invalid lossless signature: {:#04X?}", sig)),
        }
    }
}

impl From<DecoderError> for ImageError {
    fn from(e: DecoderError) -> ImageError {
        ImageError::Decoding(DecodingError::new(ImageFormat::WebP.into(), e))
    }
}

impl error::Error for DecoderError {}

//Decodes lossless WebP images
pub(crate) struct LosslessDecoder<R> {
    r: R,
    bit_reader: BitReader,
}

impl<R: Read> LosslessDecoder<R> {
    /// Create a new decoder
    pub(crate) fn new(r: R) -> LosslessDecoder<R> {

        LosslessDecoder {
            r,
            bit_reader: BitReader::new(),
        }
    }

    /// Reads the frame
    pub(crate) fn decode_frame(&mut self) -> ImageResult<()> {
        let len = self.r.read_u32::<LittleEndian>()?;
        let signature = self.r.read_u8()?;

        if signature != 0x2f {
            return Err(DecoderError::LosslessSignatureInvalid(signature).into());
        }

        let mut buf = Vec::new();
        self.r.read_to_end(&mut buf)?;
        self.bit_reader.init(buf);

        Ok(())
    }
}

#[derive(Debug, Clone)]
struct BitReader {
    buf: Vec<u8>,
    index: usize,
    bit_count: u8,
}

impl BitReader {
    fn new() -> BitReader {
        BitReader {
            buf: Vec::new(),
            index: 0,
            bit_count: 7,
        }
    }

    fn init(&mut self, buf: Vec<u8>) {
        self.buf = buf;
    }

    fn read_bits(&mut self, num: u8) -> u32 {
        let mut value = 0u32;
        
        for i in 0..num {
            let bit_true = self.buf[self.index] & (1 << self.bit_count) != 0;
            value += u32::from(bit_true) << i;
            self.bit_count = if self.bit_count == 0 {
                self.index += 1;
                7
            } else {
                self.bit_count - 1
            };
        }

        value

    }
}

#[derive(Debug, Clone, Default)]
struct LosslessFrame {
    width: u16,
    height: u16,
}

#[cfg(test)]
mod test {

    use super::BitReader;

    #[test]
    fn bit_read_test() {
        let mut bit_reader = BitReader::new();

        //10011100 01000001 11100001
        let buf = vec![0x9C, 0x41, 0xE1];

        for val in buf.iter() {
            println!("{:#b}", val);
        }

        bit_reader.init(buf);

        assert_eq!(bit_reader.read_bits(3), 1); //100
        assert_eq!(bit_reader.read_bits(2), 3); //11
        assert_eq!(bit_reader.read_bits(6), 17); //100010
        assert_eq!(bit_reader.read_bits(10), 240); //0000111100
        assert_eq!(bit_reader.read_bits(3), 4); //001
    }
}