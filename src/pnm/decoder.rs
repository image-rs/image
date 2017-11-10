use std::io::{Read, BufReader};
use std::ascii::AsciiExt;

use color::{ColorType};
use image::{DecodingResult, ImageDecoder, ImageResult, ImageError};
extern crate byteorder;
use self::byteorder::{BigEndian, ByteOrder};

enum DecodeStrategy {
    Bytes,
    Ascii,
}

enum TupleType {
    RGB,
    Grayscale,
    Bit,
}

/// Denotes the category of the magic number
#[derive(Clone, Copy)]
pub enum PNMSubtype {
    /// Magic numbers P1 and P4
    Bitmap,
    /// Magic numbers P2 and P5
    Graymap,
    /// Magic numbers P3 and P6
    Pixmap,
    /// Magic number P7
    ArbitraryMap,
}

/// PNM decoder
pub struct PNMDecoder<R> {
    reader: BufReader<R>,
    width: u32,
    height: u32,
    maxwhite: u32,
    tuple: TupleType,
    decoder: DecodeStrategy,
    subtype: PNMSubtype,
}

impl<R: Read> PNMDecoder<R> {
    /// Create a new decoder that decodes from the stream ```r```
    pub fn new(read: R) -> ImageResult<PNMDecoder<R>> {
        let mut buf = BufReader::new(read);
        let mut magic: [u8; 2] = [0, 0];
        try!(buf.read_exact(&mut magic[..])); // Skip magic constant
        if magic[0] != b'P' {
            return Err(ImageError::FormatError("Expected magic constant for ppm, P3 or P6".to_string()));
        }

        let decoder = match magic[1] {
            b'3' => DecodeStrategy::Ascii,
            b'6' => DecodeStrategy::Bytes,
            _ => return Err(ImageError::FormatError("Expected magic constant for ppm, P3 or P6".to_string())),
        };

        let width = try!(PNMDecoder::read_next_u32(&mut buf));
        let height = try!(PNMDecoder::read_next_u32(&mut buf));
        let maxwhite = try!(PNMDecoder::read_next_u32(&mut buf));

        if !(maxwhite <= u16::max_value() as u32) {
            return Err(ImageError::FormatError("Image maxval is not less or equal to 65535".to_string()))
        }

        Ok(PNMDecoder {
            reader: buf,
            width: width,
            height: height,
            maxwhite: maxwhite,
            tuple: TupleType::RGB,
            decoder: decoder,
            subtype: PNMSubtype::Pixmap,
        })
    }

    /// Reads a string as well as a single whitespace after it, ignoring comments
    fn read_next_string(reader: &mut BufReader<R>) -> ImageResult<String> {
        let mut bytes = Vec::new();

        // pair input bytes with a bool mask to remove comments
        let mark_comments = reader
            .bytes()
            .scan(true, |partof, read| {
                let byte = match read {
                    Err(err) => return Some((*partof, Err(err))),
                    Ok(byte) => byte,
                };
                let cur_enabled = *partof && byte != b'#';
                let next_enabled = cur_enabled || (byte == b'\r' || byte == b'\n');
                *partof = next_enabled;
                return Some((cur_enabled, Ok(byte)));
            });

        for (_, byte) in mark_comments.filter(|ref e| e.0) {
            match byte {
                Ok(b'\t') | Ok(b'\n') | Ok(b'\x0b') | Ok(b'\x0c') | Ok(b'\r') | Ok(b' ') => {
                    if !bytes.is_empty() {
                        break // We're done as we already have some content
                    }
                },
                Ok(byte) => {
                    bytes.push(byte);
                },
                Err(_) => break,
            }
        }

        if bytes.is_empty() {
            return Err(ImageError::FormatError("Unexpected eof".to_string()))
        }

        if !bytes.as_slice().is_ascii() {
            return Err(ImageError::FormatError("Non ascii character in preamble".to_string()))
        }

        String::from_utf8(bytes).map_err(|_| ImageError::FormatError("Couldn't read preamble".to_string()))
    }

    fn read_next_u32(reader: &mut BufReader<R>) -> ImageResult<u32> {
        let s = try!(PNMDecoder::read_next_string(reader));
        s.parse::<u32>().map_err(|_| ImageError::FormatError("Invalid number in preamble".to_string()))
    }
}

impl<R: Read> ImageDecoder for PNMDecoder<R> {
    fn dimensions(&mut self) -> ImageResult<(u32, u32)> {
        Ok((self.width, self.height))
    }

    fn colortype(&mut self) -> ImageResult<ColorType> {
        match self.tuple {
            TupleType::Grayscale if self.maxwhite < 256 => Ok(ColorType::Gray(8)),
            TupleType::Grayscale if self.maxwhite < 65536 => Ok(ColorType::Gray(16)),
            TupleType::RGB if self.maxwhite < 256 => Ok(ColorType::RGB(8)),
            TupleType::RGB if self.maxwhite < 65536 => Ok(ColorType::RGB(16)),
            TupleType::Bit => Ok(ColorType::Gray(8)),
            _ => Err(ImageError::FormatError("Can't determine color type".to_string()))
        }
    }

    fn row_len(&mut self) -> ImageResult<usize> {
        self.rowlen()
    }

    fn read_scanline(&mut self, _buf: &mut [u8]) -> ImageResult<u32> {
        unimplemented!();
    }

    fn read_image(&mut self) -> ImageResult<DecodingResult> {
        self.read()
    }
}

impl<R: Read> PNMDecoder<R> {
    fn rowlen(&self) -> ImageResult<usize> {
        match self.tuple {
            TupleType::Bit => Bit::bytelen(self.width, 1, 1),
            TupleType::RGB if self.maxwhite < 256 => u8::bytelen(self.width, 1, 3),
            TupleType::RGB if self.maxwhite < 65536 => u16::bytelen(self.width, 1, 3),
            TupleType::Grayscale if self.maxwhite < 256 => u8::bytelen(self.width, 1, 1),
            TupleType::Grayscale if self.maxwhite < 65536 => u16::bytelen(self.width, 1, 1),
            _ => return Err(ImageError::FormatError("Invalid sample types for row length".to_string()))
        }
    }

    fn read(&mut self) -> ImageResult<DecodingResult> {
        match self.tuple {
            TupleType::Bit => self.read_samples::<Bit>(1),
            TupleType::RGB if self.maxwhite < 256 => self.read_samples::<u8>(3),
            TupleType::RGB if self.maxwhite < 65536 => self.read_samples::<u16>(3),
            TupleType::Grayscale if self.maxwhite < 256 => self.read_samples::<u8>(1),
            TupleType::Grayscale if self.maxwhite < 65536 => self.read_samples::<u16>(1),
            _ => return Err(ImageError::FormatError("Invalid sample types for row length".to_string()))
        }
    }

    fn read_samples<S: SampleType>(&mut self, components: u32) -> ImageResult<DecodingResult> where
        Vec<S::T>: Into<DecodingResult> {
        match self.decoder {
            DecodeStrategy::Bytes => {
                    let bytecount = S::bytelen(self.width, self.height, components)?;
                    let mut bytes = vec![0 as u8; bytecount];
                    (&mut self.reader).read_exact(&mut bytes)?;
                    let samples = S::from_bytes(&bytes, self.width, self.height, components)?;
                    Ok(samples.into())
                },
            DecodeStrategy::Ascii => {
                    let samples = self.read_ascii::<S>(components)?;
                    Ok(samples.into())
                }
        }
    }

    fn read_ascii<Basic: SampleType>(&mut self, components: u32) -> ImageResult<Vec<Basic::T>> {
        let mut buffer = Vec::new();
        for _ in 0 .. (self.width * self.height * components) {
            let value = self.read_ascii_sample()?;
            let sample = Basic::from_unsigned(value)?;
            buffer.push(sample);
        }
        Ok(buffer)
    }

    fn read_ascii_sample(&mut self) -> ImageResult<u32> {
        let istoken = |v: &Result<u8, _>| match v {
                &Err(_) => false,
                &Ok(b'\t') | &Ok(b'\n') | &Ok(b'\x0b') | &Ok(b'\x0c') | &Ok(b'\r') | &Ok(b' ') => false,
                _ => true,
            };
        let token = (&mut self.reader).bytes()
            .skip_while(|v| !istoken(v))
            .take_while(&istoken)
            .collect::<Result<Vec<u8>, _>>()?;
        if !token.is_ascii() {
            return Err(ImageError::FormatError("Non ascii character where sample value was expected".to_string()))
        }
        let string = String::from_utf8(token).map_err(|_| ImageError::FormatError("Error parsing sample".to_string()))?;
        string.parse::<u32>().map_err(|_| ImageError::FormatError("Error parsing sample value".to_string()))
    }

    /// Get the pnm subtype, depending on the magic constant contained in the header
    pub fn subtype(&self) -> PNMSubtype {
        self.subtype
    }
}

trait SampleType {
    type T;
    fn bytelen(width: u32, height: u32, samples: u32) -> ImageResult<usize>;
    /// It is guaranteed that `bytes.len() == bytelen(width, height, samples)`
    fn from_bytes(bytes: &[u8], width: u32, height: u32, samples: u32) -> ImageResult<Vec<Self::T>>;
    fn from_unsigned(u32) -> ImageResult<Self::T>;
}

impl SampleType for u8 {
    type T = u8;
    fn bytelen(width: u32, height: u32, samples: u32) -> ImageResult<usize> {
        Ok((width * height * samples) as usize)
    }
    fn from_bytes(bytes: &[u8], _width: u32, _height: u32, _samples: u32) -> ImageResult<Vec<Self::T>> {
        let mut buffer = Vec::new();
        buffer.resize(bytes.len(), 0 as u8);
        buffer.copy_from_slice(bytes);
        Ok(buffer)
    }
    fn from_unsigned(val: u32) -> ImageResult<Self::T> {
        if val > u8::max_value() as u32 {
            Err(ImageError::FormatError("Sample value outside of bounds".to_string()))
        } else {
            Ok(val as u8)
        }
    }
}

impl SampleType for u16 {
    type T = u16;
    fn bytelen(width: u32, height: u32, samples: u32) -> ImageResult<usize> {
        Ok((width * height * samples * 2) as usize)
    }
    fn from_bytes(bytes: &[u8], width: u32, height: u32, samples: u32) -> ImageResult<Vec<Self::T>> {
        let mut buffer = Vec::new();
        buffer.resize((width * height * samples) as usize, 0 as u16);
        BigEndian::read_u16_into(bytes, &mut buffer);
        Ok(buffer)
    }
    fn from_unsigned(val: u32) -> ImageResult<Self::T> {
        if val > u16::max_value() as u32 {
            Err(ImageError::FormatError("Sample value outside of bounds".to_string()))
        } else {
            Ok(val as u16)
        }
    }
}

struct Bit;
impl SampleType for Bit {
    type T = u8;
    fn bytelen(width: u32, height: u32, samples: u32) -> ImageResult<usize> {
        let count = width * samples;
        let linelen = (count/8) + (count % 8 == 0) as u32;
        Ok((linelen * height) as usize)
    }
    fn from_bytes(bytes: &[u8], width: u32, height: u32, samples: u32) -> ImageResult<Vec<Self::T>> {
        let mut buffer = Vec::new();
        let linecount = width * samples;
        buffer.resize((width * height * samples) as usize, 0 as u8);
        for line in 0..height {
            for samplei in 0..linecount {
                let byteindex = (samplei/8) as usize;
                let inindex = 7 - samplei % 8;
                let indicator = (bytes[byteindex] >> inindex) & 0x01;
                let bufferindex = (linecount*line + samplei) as usize;
                buffer[bufferindex] = if indicator == 0 { 0 } else { 255 };
            }
        }
        Ok(buffer)
    }
    fn from_unsigned(val: u32) -> ImageResult<Self::T> {
        if val > 1 {
            Err(ImageError::FormatError("Sample value outside of bounds".to_string()))
        } else if val == 1 {
            Ok(255 as u8)
        } else {
            Ok(0 as u8)
        }
    }
}

impl Into<DecodingResult> for Vec<u8> {
    fn into(self) -> DecodingResult {
        DecodingResult::U8(self)
    }
}

impl Into<DecodingResult> for Vec<u16> {
    fn into(self) -> DecodingResult {
        DecodingResult::U16(self)
    }
}
