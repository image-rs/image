use std::io::{Read, BufRead, BufReader};
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
        let magic = try!(buf.read_magic_constant());
        if magic[0] != b'P' {
            return Err(ImageError::FormatError("Expected magic constant for pnm, P1 through P7".to_string()));
        }

        let (subtype, decoder) = match magic[1] {
            b'1' => (PNMSubtype::Bitmap, DecodeStrategy::Ascii),
            b'2' => (PNMSubtype::Graymap, DecodeStrategy::Ascii),
            b'3' => (PNMSubtype::Pixmap, DecodeStrategy::Ascii),
            b'4' => (PNMSubtype::Bitmap, DecodeStrategy::Bytes),
            b'5' => (PNMSubtype::Graymap, DecodeStrategy::Bytes),
            b'6' => (PNMSubtype::Pixmap, DecodeStrategy::Bytes),
            b'7' => (PNMSubtype::ArbitraryMap, DecodeStrategy::Bytes),
            _ => return Err(ImageError::FormatError("Expected magic constant for ppm, P1 through P7".to_string())),
        };

        let (width, height, maxwhite, tuple) = match subtype {
            PNMSubtype::Bitmap => PNMDecoder::read_bitmap_header(&mut buf)?,
            PNMSubtype::Graymap => PNMDecoder::read_graymap_header(&mut buf)?,
            PNMSubtype::Pixmap => PNMDecoder::read_pixmap_header(&mut buf)?,
            PNMSubtype::ArbitraryMap => PNMDecoder::read_arbitrary_header(&mut buf)?,
        };

        if !(maxwhite <= u16::max_value() as u32) {
            return Err(ImageError::FormatError("Image maxval is not less or equal to 65535".to_string()))
        }

        Ok(PNMDecoder {
            reader: buf,
            width: width,
            height: height,
            maxwhite: maxwhite,
            tuple: tuple,
            decoder: decoder,
            subtype: subtype,
        })
    }

    fn read_bitmap_header(reader: &mut BufReader<R>) -> ImageResult<(u32, u32, u32, TupleType)> {
        let (w, h) = reader.read_bitmap_header()?;
        Ok((w, h, 1, TupleType::Bit))
    }

    fn read_graymap_header(reader: &mut BufReader<R>) -> ImageResult<(u32, u32, u32, TupleType)> {
        let (w, h, m) = reader.read_graymap_header()?;
        Ok((w, h, m, TupleType::Grayscale))
    }

    fn read_pixmap_header(reader: &mut BufReader<R>) -> ImageResult<(u32, u32, u32, TupleType)> {
        let (w, h, m) = reader.read_pixmap_header()?;
        Ok((w, h, m, TupleType::RGB))
    }

    fn read_arbitrary_header(_reader: &mut BufReader<R>) -> ImageResult<(u32, u32, u32, TupleType)> {
        Err(ImageError::FormatError("PAM is not (yet) supported".to_string()))
    }
}

#[allow(unused)]
struct ArbitraryHeader {
    height: u32,
    width: u32,
    depth: u32,
    maxval: u32,
    tupltype: String,
}

trait HeaderReader: BufRead {
    /// Reads the two magic constant bytes
    fn read_magic_constant(&mut self) -> ImageResult<[u8; 2]> {
        let mut magic: [u8; 2] = [0, 0];
        self.read_exact(&mut magic).map_err(|_| ImageError::NotEnoughData)?;
        Ok(magic)
    }

    /// Reads a string as well as a single whitespace after it, ignoring comments
    fn read_next_string(&mut self) -> ImageResult<String> {
        let mut bytes = Vec::new();

        // pair input bytes with a bool mask to remove comments
        let mark_comments = self
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

    /// Read the next line
    fn read_next_line(&mut self) -> ImageResult<String> {
        let mut buffer = String::new();
        self.read_line(&mut buffer).map_err(|_| ImageError::FormatError("Line not properly formatted".to_string()))?;
        Ok(buffer)
    }

    fn read_next_u32(&mut self) -> ImageResult<u32> {
        let s = try!(self.read_next_string());
        s.parse::<u32>().map_err(|_| ImageError::FormatError("Invalid number in preamble".to_string()))
    }

    fn read_bitmap_header(&mut self) -> ImageResult<(u32, u32)> {
        let width = try!(self.read_next_u32());
        let height = try!(self.read_next_u32());
        Ok((width, height))
    }

    fn read_graymap_header(&mut self) -> ImageResult<(u32, u32, u32)> {
        self.read_pixmap_header()
    }

    fn read_pixmap_header(&mut self) -> ImageResult<(u32, u32, u32)> {
        let width = try!(self.read_next_u32());
        let height = try!(self.read_next_u32());
        let maxwhite = try!(self.read_next_u32());
        Ok((width, height, maxwhite))
    }

    fn read_arbitrary_header(&mut self) -> ImageResult<ArbitraryHeader> {
        match self.bytes().next() {
            None => return Err(ImageError::FormatError("Input too short".to_string())),
            Some(Err(io)) => return Err(ImageError::IoError(io)),
            Some(Ok(b'\n')) => (),
            _ => return Err(ImageError::FormatError("Expected newline after P7".to_string())),
        }

        let mut line = String::new();
        let mut height: Option<u32> = None;
        let mut width: Option<u32> = None;
        let mut depth: Option<u32> = None;
        let mut maxval: Option<u32> = None;
        let mut tupltype: Option<String> = None;
        loop {
            line.truncate(0);
            self.read_line(&mut line).map_err(|io| ImageError::IoError(io))?;
            if line.as_bytes()[0] == b'#' {
                continue;
            }
            if !line.is_ascii() {
                return Err(ImageError::FormatError("Only ascii characters allowed in pam header".to_string()));
            }
            let (identifier, rest) = line.trim_left().split_at(line.find(char::is_whitespace).unwrap_or(line.len()));
            match identifier {
                "ENDHDR" => break,
                "HEIGHT" => if height.is_some() {
                        return Err(ImageError::FormatError("Duplicate HEIGHT line".to_string()))
                    } else {
                        let h = rest.trim().parse::<u32>().map_err(|_| ImageError::FormatError("Invalid height".to_string()))?;
                        height = Some(h);
                    },
                "WIDTH" => if width.is_some() {
                        return Err(ImageError::FormatError("Duplicate WIDTH line".to_string()))
                    } else {
                        let w = rest.trim().parse::<u32>().map_err(|_| ImageError::FormatError("Invalid width".to_string()))?;
                        width = Some(w);
                    },
                "DEPTH" => if depth.is_some() {
                        return Err(ImageError::FormatError("Duplicate DEPTH line".to_string()))
                    } else {
                        let d = rest.trim().parse::<u32>().map_err(|_| ImageError::FormatError("Invalid depth".to_string()))?;
                        depth = Some(d);
                    },
                "MAXVAL" => if maxval.is_some() {
                        return Err(ImageError::FormatError("Duplicate MAXVAL line".to_string()))
                    } else {
                        let m = rest.trim().parse::<u32>().map_err(|_| ImageError::FormatError("Invalid maxval".to_string()))?;
                        maxval = Some(m);
                    },
                "TUPLTYPE" => {
                        let identifier = rest.trim();
                        if tupltype.is_some() {
                            let appended = tupltype.take().map(|mut v| { v.push(' '); v.push_str(identifier); v });
                            tupltype = appended;
                        } else {
                            tupltype = Some(identifier.to_string());
                        }
                    },
                _ => return Err(ImageError::FormatError("Unknown header line".to_string())),
            }
        }
        let (h, w, d, m) = match (height, width, depth, maxval) {
            (None, _, _, _) => return Err(ImageError::FormatError("Expected one HEIGHT line".to_string())),
            (_, None, _, _) => return Err(ImageError::FormatError("Expected one WIDTH line".to_string())),
            (_, _, None, _) => return Err(ImageError::FormatError("Expected one DEPTH line".to_string())),
            (_, _, _, None) => return Err(ImageError::FormatError("Expected one MAXVAL line".to_string())),
            (Some(h), Some(w), Some(d), Some(m)) => (h, w, d, m),
        };
        Ok(ArbitraryHeader{
            height: h,
            width: w,
            depth: d,
            maxval: m,
            tupltype: tupltype.unwrap_or("".to_string()),
        })
    }
}

impl<R: Read> HeaderReader for BufReader<R> { }

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
                    (&mut self.reader).read_exact(&mut bytes).map_err(|_| ImageError::NotEnoughData)?;
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
