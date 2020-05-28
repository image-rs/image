use std::convert::TryFrom;
use std::error;
use std::io::{self, BufRead, BufReader, Cursor, Read};
use std::str::{self, FromStr};
use std::fmt::{self, Display};
use std::marker::PhantomData;
use std::mem;
use std::num::ParseIntError;

use super::{ArbitraryHeader, ArbitraryTuplType, BitmapHeader, GraymapHeader, PixmapHeader};
use super::{HeaderRecord, PNMHeader, PNMSubtype, SampleEncoding};
use crate::color::{ColorType, ExtendedColorType};
use crate::error::{
    DecodingError, ImageError, ImageResult, UnsupportedError, UnsupportedErrorKind,
};
use crate::image::{self, ImageDecoder, ImageFormat};
use crate::utils;

use byteorder::{BigEndian, ByteOrder, NativeEndian};

/// All errors that can occur when attempting to parse a PNM
#[derive(Debug, Clone)]
enum DecoderError {
    /// PNM's "P[123456]" signature wrong or missing
    PnmMagicInvalid([u8; 2]),
    /// Couldn't parse the specified string as an integer from the specified source
    UnparsableValue(ErrorDataSource, String, ParseIntError),

    /// More than the exactly one allowed plane specified by the format
    NonAsciiByteInHeader(u8),
    /// The PAM header contained a non-ASCII byte
    NonAsciiLineInPamHeader,
    /// A sample string contained a non-ASCII byte
    NonAsciiSample,

    /// The byte after the P7 magic was not 0x0A NEWLINE
    NotNewlineAfterP7Magic(u8),
    /// The PNM header had too few lines
    UnexpectedPnmHeaderEnd,

    /// The specified line was specified twice
    HeaderLineDuplicated(PnmHeaderLine),
    /// The line with the specified ID was not understood
    HeaderLineUnknown(String),
    /// At least one of the required lines were missing from the header (are `None` here)
    ///
    /// Same names as [`PnmHeaderLine`](enum.PnmHeaderLine.html)
    #[allow(missing_docs)]
    HeaderLineMissing {
        height: Option<u32>,
        width: Option<u32>,
        depth: Option<u32>,
        maxval: Option<u32>,
    },

    /// Not enough data was provided to the Decoder to decode the image
    InputTooShort,
    /// Sample raster contained unexpected byte
    UnexpectedByteInRaster(u8),
    /// Specified sample was out of bounds (e.g. >1 in B&W)
    SampleOutOfBounds(u8),
    /// The image's maxval exceeds 0xFFFF
    MaxvalTooBig(u32),

    /// The specified tuple type supports restricted depths and maxvals, those restrictions were not met
    InvalidDepthOrMaxval {
        tuple_type: ArbitraryTuplType,
        depth: u32,
        maxval: u32,
    },
    /// The specified tuple type supports restricted depths, those restrictions were not met
    InvalidDepth {
        tuple_type: ArbitraryTuplType,
        depth: u32,
    },
    /// The tuple type was not recognised by the parser
    TupleTypeUnrecognised,
}

impl Display for DecoderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DecoderError::PnmMagicInvalid(magic) =>
                f.write_fmt(format_args!("Expected magic constant for PNM: P1..P7, got [{:#04X?}, {:#04X?}]", magic[0], magic[1])),
            DecoderError::UnparsableValue(src, data, err) =>
                f.write_fmt(format_args!("Error parsing {:?} as {}: {}", data, src, err)),

            DecoderError::NonAsciiByteInHeader(c) =>
                f.write_fmt(format_args!("Non-ASCII character {:#04X?} in header", c)),
            DecoderError::NonAsciiLineInPamHeader =>
                f.write_str("Non-ASCII line in PAM header"),
            DecoderError::NonAsciiSample =>
                f.write_str("Non-ASCII character where sample value was expected"),

            DecoderError::NotNewlineAfterP7Magic(c) =>
                f.write_fmt(format_args!("Expected newline after P7 magic, got {:#04X?}", c)),
            DecoderError::UnexpectedPnmHeaderEnd =>
                f.write_str("Unexpected end of PNM header"),

            DecoderError::HeaderLineDuplicated(line) =>
                f.write_fmt(format_args!("Duplicate {} line", line)),
            DecoderError::HeaderLineUnknown(identifier) =>
                f.write_fmt(format_args!("Unknown header line with identifier {:?}", identifier)),
            DecoderError::HeaderLineMissing { height, width, depth, maxval } =>
                f.write_fmt(format_args!("Missing header line: have height={:?}, width={:?}, depth={:?}, maxval={:?}", height, width, depth, maxval)),

            DecoderError::InputTooShort =>
                f.write_str("Not enough data was provided to the Decoder to decode the image"),
            DecoderError::UnexpectedByteInRaster(c) =>
                f.write_fmt(format_args!("Unexpected character {:#04X?} within sample raster", c)),
            DecoderError::SampleOutOfBounds(val) =>
                f.write_fmt(format_args!("Sample value {} outside of bounds", val)),
            DecoderError::MaxvalTooBig(maxval) =>
                f.write_fmt(format_args!("Image MAXVAL exceeds {}: {}", 0xFFFF, maxval)),

            DecoderError::InvalidDepthOrMaxval { tuple_type, depth, maxval } =>
                f.write_fmt(format_args!("Invalid depth ({}) or maxval ({}) for tuple type {}", depth, maxval, tuple_type.name())),
            DecoderError::InvalidDepth { tuple_type, depth } =>
                f.write_fmt(format_args!("Invalid depth ({}) for tuple type {}", depth, tuple_type.name())),
            DecoderError::TupleTypeUnrecognised =>
                f.write_str("Tuple type not recognized"),
        }
    }
}

/// Note: should `pnm` be extracted into a separate crate,
/// this will need to be hidden until that crate hits version `1.0`.
impl From<DecoderError> for ImageError {
    fn from(e: DecoderError) -> ImageError {
        ImageError::Decoding(DecodingError::new(ImageFormat::Pnm.into(), e))
    }
}

impl error::Error for DecoderError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            DecoderError::UnparsableValue(_, _, err) => Some(err),
            _ => None,
        }
    }
}

/// Single-value lines in a PNM header
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum PnmHeaderLine {
    /// "HEIGHT"
    Height,
    /// "WIDTH"
    Width,
    /// "DEPTH"
    Depth,
    /// "MAXVAL", a.k.a. `maxwhite`
    Maxval,
}

impl Display for PnmHeaderLine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            PnmHeaderLine::Height => "HEIGHT",
            PnmHeaderLine::Width => "WIDTH",
            PnmHeaderLine::Depth => "DEPTH",
            PnmHeaderLine::Maxval => "MAXVAL",
        })
    }
}

/// Single-value lines in a PNM header
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum ErrorDataSource {
    /// One of the header lines
    Line(PnmHeaderLine),
    /// Value in the preamble
    Preamble,
    /// Sample/pixel data
    Sample,
}

impl Display for ErrorDataSource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorDataSource::Line(l) => l.fmt(f),
            ErrorDataSource::Preamble => f.write_str("number in preamble"),
            ErrorDataSource::Sample => f.write_str("sample"),
        }
    }
}

/// Dynamic representation, represents all decodable (sample, depth) combinations.
#[derive(Clone, Copy)]
enum TupleType {
    PbmBit,
    BWBit,
    GrayU8,
    GrayU16,
    RGBU8,
    RGBU16,
}

trait Sample {
    fn bytelen(width: u32, height: u32, samples: u32) -> ImageResult<usize>;

    /// It is guaranteed that `bytes.len() == bytelen(width, height, samples)`
    fn from_bytes(bytes: &[u8], width: u32, height: u32, samples: u32)
        -> ImageResult<Vec<u8>>;

    fn from_ascii(reader: &mut dyn Read, width: u32, height: u32, samples: u32)
        -> ImageResult<Vec<u8>>;
}

struct U8;
struct U16;
struct PbmBit;
struct BWBit;

trait DecodableImageHeader {
    fn tuple_type(&self) -> ImageResult<TupleType>;
}

/// PNM decoder
pub struct PnmDecoder<R> {
    reader: BufReader<R>,
    header: PNMHeader,
    tuple: TupleType,
}

impl<R: Read> PnmDecoder<R> {
    /// Create a new decoder that decodes from the stream ```read```
    pub fn new(read: R) -> ImageResult<PnmDecoder<R>> {
        let mut buf = BufReader::new(read);
        let magic = buf.read_magic_constant()?;

        let subtype = match magic {
            [b'P', b'1'] => PNMSubtype::Bitmap(SampleEncoding::Ascii),
            [b'P', b'2'] => PNMSubtype::Graymap(SampleEncoding::Ascii),
            [b'P', b'3'] => PNMSubtype::Pixmap(SampleEncoding::Ascii),
            [b'P', b'4'] => PNMSubtype::Bitmap(SampleEncoding::Binary),
            [b'P', b'5'] => PNMSubtype::Graymap(SampleEncoding::Binary),
            [b'P', b'6'] => PNMSubtype::Pixmap(SampleEncoding::Binary),
            [b'P', b'7'] => PNMSubtype::ArbitraryMap,
            _ => Err(DecoderError::PnmMagicInvalid(magic))?,
        };

        match subtype {
            PNMSubtype::Bitmap(enc) => PnmDecoder::read_bitmap_header(buf, enc),
            PNMSubtype::Graymap(enc) => PnmDecoder::read_graymap_header(buf, enc),
            PNMSubtype::Pixmap(enc) => PnmDecoder::read_pixmap_header(buf, enc),
            PNMSubtype::ArbitraryMap => PnmDecoder::read_arbitrary_header(buf),
        }
    }

    /// Extract the reader and header after an image has been read.
    pub fn into_inner(self) -> (R, PNMHeader) {
        (self.reader.into_inner(), self.header)
    }

    fn read_bitmap_header(
        mut reader: BufReader<R>,
        encoding: SampleEncoding,
    ) -> ImageResult<PnmDecoder<R>> {
        let header = reader.read_bitmap_header(encoding)?;
        Ok(PnmDecoder {
            reader,
            tuple: TupleType::PbmBit,
            header: PNMHeader {
                decoded: HeaderRecord::Bitmap(header),
                encoded: None,
            },
        })
    }

    fn read_graymap_header(
        mut reader: BufReader<R>,
        encoding: SampleEncoding,
    ) -> ImageResult<PnmDecoder<R>> {
        let header = reader.read_graymap_header(encoding)?;
        let tuple_type = header.tuple_type()?;
        Ok(PnmDecoder {
            reader,
            tuple: tuple_type,
            header: PNMHeader {
                decoded: HeaderRecord::Graymap(header),
                encoded: None,
            },
        })
    }

    fn read_pixmap_header(
        mut reader: BufReader<R>,
        encoding: SampleEncoding,
    ) -> ImageResult<PnmDecoder<R>> {
        let header = reader.read_pixmap_header(encoding)?;
        let tuple_type = header.tuple_type()?;
        Ok(PnmDecoder {
            reader,
            tuple: tuple_type,
            header: PNMHeader {
                decoded: HeaderRecord::Pixmap(header),
                encoded: None,
            },
        })
    }

    fn read_arbitrary_header(mut reader: BufReader<R>) -> ImageResult<PnmDecoder<R>> {
        let header = reader.read_arbitrary_header()?;
        let tuple_type = header.tuple_type()?;
        Ok(PnmDecoder {
            reader,
            tuple: tuple_type,
            header: PNMHeader {
                decoded: HeaderRecord::Arbitrary(header),
                encoded: None,
            },
        })
    }
}

trait HeaderReader: BufRead {
    /// Reads the two magic constant bytes
    fn read_magic_constant(&mut self) -> ImageResult<[u8; 2]> {
        let mut magic: [u8; 2] = [0, 0];
        self.read_exact(&mut magic)?;
        Ok(magic)
    }

    /// Reads a string as well as a single whitespace after it, ignoring comments
    fn read_next_string(&mut self) -> ImageResult<String> {
        let mut bytes = Vec::new();

        // pair input bytes with a bool mask to remove comments
        let mark_comments = self.bytes().scan(true, |partof, read| {
            let byte = match read {
                Err(err) => return Some((*partof, Err(err))),
                Ok(byte) => byte,
            };
            let cur_enabled = *partof && byte != b'#';
            let next_enabled = cur_enabled || (byte == b'\r' || byte == b'\n');
            *partof = next_enabled;
            Some((cur_enabled, Ok(byte)))
        });

        for (_, byte) in mark_comments.filter(|ref e| e.0) {
            match byte {
                Ok(b'\t') | Ok(b'\n') | Ok(b'\x0b') | Ok(b'\x0c') | Ok(b'\r') | Ok(b' ') => {
                    if !bytes.is_empty() {
                        break; // We're done as we already have some content
                    }
                }
                Ok(byte) if !byte.is_ascii() => Err(DecoderError::NonAsciiByteInHeader(byte))?,
                Ok(byte) => {
                    bytes.push(byte);
                },
                Err(_) => break,
            }
        }

        if bytes.is_empty() {
            return Err(ImageError::IoError(io::ErrorKind::UnexpectedEof.into()));
        }

        if !bytes.as_slice().is_ascii() {
            // We have only filled the buffer with characters for which `byte.is_ascii()` holds.
            unreachable!("Non-ASCII character should have returned sooner")
        }

        let string = String::from_utf8(bytes)
            // We checked the precondition ourselves a few lines before, `bytes.as_slice().is_ascii()`.
            .unwrap_or_else(|_| unreachable!("Only ASCII characters should be decoded"));

        Ok(string)
    }

    /// Read the next line
    fn read_next_line(&mut self) -> ImageResult<String> {
        let mut buffer = String::new();
        self.read_line(&mut buffer)?;
        Ok(buffer)
    }

    fn read_next_u32(&mut self) -> ImageResult<u32> {
        let s = self.read_next_string()?;
        s.parse::<u32>()
            .map_err(|err| DecoderError::UnparsableValue(ErrorDataSource::Preamble, s, err).into())
    }

    fn read_bitmap_header(&mut self, encoding: SampleEncoding) -> ImageResult<BitmapHeader> {
        let width = self.read_next_u32()?;
        let height = self.read_next_u32()?;
        Ok(BitmapHeader {
            encoding,
            width,
            height,
        })
    }

    fn read_graymap_header(&mut self, encoding: SampleEncoding) -> ImageResult<GraymapHeader> {
        self.read_pixmap_header(encoding).map(
            |PixmapHeader {
                 encoding,
                 width,
                 height,
                 maxval,
             }| GraymapHeader {
                encoding,
                width,
                height,
                maxwhite: maxval,
            },
        )
    }

    fn read_pixmap_header(&mut self, encoding: SampleEncoding) -> ImageResult<PixmapHeader> {
        let width = self.read_next_u32()?;
        let height = self.read_next_u32()?;
        let maxval = self.read_next_u32()?;
        Ok(PixmapHeader {
            encoding,
            width,
            height,
            maxval,
        })
    }

    fn read_arbitrary_header(&mut self) -> ImageResult<ArbitraryHeader> {
        fn parse_single_value_line(line_val: &mut Option<u32>, rest: &str, line: PnmHeaderLine) -> ImageResult<()> {
            if line_val.is_some() {
                return Err(DecoderError::HeaderLineDuplicated(line).into());
            } else {
                let v = rest.trim().parse().map_err(|err| DecoderError::UnparsableValue(ErrorDataSource::Line(line), rest.to_owned(), err))?;
                *line_val = Some(v);
                Ok(())
            }
        }

        match self.bytes().next() {
            None => return Err(ImageError::IoError(io::ErrorKind::UnexpectedEof.into())),
            Some(Err(io)) => return Err(ImageError::IoError(io)),
            Some(Ok(b'\n')) => (),
            Some(Ok(c)) => return Err(DecoderError::NotNewlineAfterP7Magic(c).into()),
        }

        let mut line = String::new();
        let mut height: Option<u32> = None;
        let mut width: Option<u32> = None;
        let mut depth: Option<u32> = None;
        let mut maxval: Option<u32> = None;
        let mut tupltype: Option<String> = None;
        loop {
            line.truncate(0);
            let len = self.read_line(&mut line)?;
            if len == 0 {
                return Err(DecoderError::UnexpectedPnmHeaderEnd.into());
            }
            if line.as_bytes()[0] == b'#' {
                continue;
            }
            if !line.is_ascii() {
                return Err(DecoderError::NonAsciiLineInPamHeader.into());
            }
            #[allow(deprecated)]
            let (identifier, rest) = line.trim_left()
                .split_at(line.find(char::is_whitespace).unwrap_or_else(|| line.len()));
            match identifier {
                "ENDHDR" => break,
                "HEIGHT" => parse_single_value_line(&mut height, rest, PnmHeaderLine::Height)?,
                "WIDTH" => parse_single_value_line(&mut width, rest, PnmHeaderLine::Width)?,
                "DEPTH" => parse_single_value_line(&mut depth, rest, PnmHeaderLine::Depth)?,
                "MAXVAL" => parse_single_value_line(&mut maxval, rest, PnmHeaderLine::Maxval)?,
                "TUPLTYPE" => {
                    let identifier = rest.trim();
                    if tupltype.is_some() {
                        let appended = tupltype.take().map(|mut v| {
                            v.push(' ');
                            v.push_str(identifier);
                            v
                        });
                        tupltype = appended;
                    } else {
                        tupltype = Some(identifier.to_string());
                    }
                }
                _ => return Err(DecoderError::HeaderLineUnknown(identifier.to_string()).into()),
            }
        }

        let (h, w, d, m) = match (height, width, depth, maxval) {
            (Some(h), Some(w), Some(d), Some(m)) => (h, w, d, m),
            _ => return Err(DecoderError::HeaderLineMissing { height, width, depth, maxval }.into()),
        };

        let tupltype = match tupltype {
            None => None,
            Some(ref t) if t == "BLACKANDWHITE" => Some(ArbitraryTuplType::BlackAndWhite),
            Some(ref t) if t == "BLACKANDWHITE_ALPHA" => Some(ArbitraryTuplType::BlackAndWhiteAlpha),
            Some(ref t) if t == "GRAYSCALE" => Some(ArbitraryTuplType::Grayscale),
            Some(ref t) if t == "GRAYSCALE_ALPHA" => Some(ArbitraryTuplType::GrayscaleAlpha),
            Some(ref t) if t == "RGB" => Some(ArbitraryTuplType::RGB),
            Some(ref t) if t == "RGB_ALPHA" => Some(ArbitraryTuplType::RGBAlpha),
            Some(other) => Some(ArbitraryTuplType::Custom(other)),
        };

        Ok(ArbitraryHeader {
            height: h,
            width: w,
            depth: d,
            maxval: m,
            tupltype,
        })
    }
}

impl<R: Read> HeaderReader for BufReader<R> {}

/// Wrapper struct around a `Cursor<Vec<u8>>`
pub struct PnmReader<R>(Cursor<Vec<u8>>, PhantomData<R>);
impl<R> Read for PnmReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.0.read(buf)
    }
    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
        if self.0.position() == 0 && buf.is_empty() {
            mem::swap(buf, self.0.get_mut());
            Ok(buf.len())
        } else {
            self.0.read_to_end(buf)
        }
    }
}

impl<'a, R: 'a + Read> ImageDecoder<'a> for PnmDecoder<R> {
    type Reader = PnmReader<R>;

    fn dimensions(&self) -> (u32, u32) {
        (self.header.width(), self.header.height())
    }

    fn color_type(&self) -> ColorType {
        match self.tuple {
            TupleType::PbmBit => ColorType::L8,
            TupleType::BWBit => ColorType::L8,
            TupleType::GrayU8 => ColorType::L8,
            TupleType::GrayU16 => ColorType::L16,
            TupleType::RGBU8 => ColorType::Rgb8,
            TupleType::RGBU16 => ColorType::Rgb16,
        }
    }

    fn original_color_type(&self) -> ExtendedColorType {
        match self.tuple {
            TupleType::PbmBit => ExtendedColorType::L1,
            TupleType::BWBit => ExtendedColorType::L1,
            TupleType::GrayU8 => ExtendedColorType::L8,
            TupleType::GrayU16 => ExtendedColorType::L16,
            TupleType::RGBU8 => ExtendedColorType::Rgb8,
            TupleType::RGBU16 => ExtendedColorType::Rgb16,
        }
    }

    fn into_reader(self) -> ImageResult<Self::Reader> {
        Ok(PnmReader(Cursor::new(image::decoder_to_vec(self)?), PhantomData))
    }

    fn read_image(mut self, buf: &mut [u8]) -> ImageResult<()> {
        assert_eq!(u64::try_from(buf.len()), Ok(self.total_bytes()));
        buf.copy_from_slice(&match self.tuple {
            TupleType::PbmBit => self.read_samples::<PbmBit>(1),
            TupleType::BWBit => self.read_samples::<BWBit>(1),
            TupleType::RGBU8 => self.read_samples::<U8>(3),
            TupleType::RGBU16 => self.read_samples::<U16>(3),
            TupleType::GrayU8 => self.read_samples::<U8>(1),
            TupleType::GrayU16 => self.read_samples::<U16>(1),
        }?);
        Ok(())
    }
}

impl<R: Read> PnmDecoder<R> {
    fn read_samples<S: Sample>(&mut self, components: u32) -> ImageResult<Vec<u8>> {
        match self.subtype().sample_encoding() {
            SampleEncoding::Binary => {
                let width = self.header.width();
                let height = self.header.height();
                let bytecount = S::bytelen(width, height, components)?;
                let mut bytes = vec![];

                self.reader
                    .by_ref()
                    // This conversion is potentially lossy but unlikely and in that case we error
                    // later anyways.
                    .take(bytecount as u64)
                    .read_to_end(&mut bytes)?;

                if bytes.len() != bytecount {
                    return Err(DecoderError::InputTooShort.into());
                }

                let samples = S::from_bytes(&bytes, width, height, components)?;
                Ok(samples)
            }
            SampleEncoding::Ascii => {
                let samples = self.read_ascii::<S>(components)?;
                Ok(samples)
            }
        }
    }

    fn read_ascii<Basic: Sample>(&mut self, components: u32) -> ImageResult<Vec<u8>> {
        Basic::from_ascii(&mut self.reader, self.header.width(), self.header.height(), components)
    }

    /// Get the pnm subtype, depending on the magic constant contained in the header
    pub fn subtype(&self) -> PNMSubtype {
        self.header.subtype()
    }
}

fn read_separated_ascii<T: FromStr<Err = ParseIntError>>(reader: &mut dyn Read) -> ImageResult<T>
    where T::Err: Display
{
    let is_separator = |v: &u8| match *v {
        b'\t' | b'\n' | b'\x0b' | b'\x0c' | b'\r' | b' ' => true,
        _ => false,
    };

    let token = reader
        .bytes()
        .skip_while(|v| v.as_ref().ok().map(&is_separator).unwrap_or(false))
        .take_while(|v| v.as_ref().ok().map(|c| !is_separator(c)).unwrap_or(false))
        .collect::<Result<Vec<u8>, _>>()?;

    if !token.is_ascii() {
        return Err(DecoderError::NonAsciiSample.into());
    }

    let string = str::from_utf8(&token)
        // We checked the precondition ourselves a few lines before with `token.is_ascii()`.
        .unwrap_or_else(|_| unreachable!("Only ASCII characters should be decoded"));

    string.parse()
          .map_err(|err| DecoderError::UnparsableValue(ErrorDataSource::Sample, string.to_owned(), err).into())
}

impl Sample for U8 {
    fn bytelen(width: u32, height: u32, samples: u32) -> ImageResult<usize> {
        Ok((width * height * samples) as usize)
    }

    fn from_bytes(
        bytes: &[u8],
        width: u32,
        height: u32,
        samples: u32,
    ) -> ImageResult<Vec<u8>> {
        assert_eq!(bytes.len(), Self::bytelen(width, height, samples).unwrap());
        Ok(bytes.to_vec())
    }

    fn from_ascii(
        reader: &mut dyn Read,
        width: u32,
        height: u32,
        samples: u32,
    ) -> ImageResult<Vec<u8>> {
        (0..width*height*samples)
            .map(|_| read_separated_ascii(reader))
            .collect()
    }
}

impl Sample for U16 {
    fn bytelen(width: u32, height: u32, samples: u32) -> ImageResult<usize> {
        Ok((width * height * samples * 2) as usize)
    }

    fn from_bytes(
        bytes: &[u8],
        width: u32,
        height: u32,
        samples: u32,
    ) -> ImageResult<Vec<u8>> {
        assert_eq!(bytes.len(), Self::bytelen(width, height, samples).unwrap());

        let mut buffer = bytes.to_vec();
        for chunk in buffer.chunks_mut(2) {
            let v = BigEndian::read_u16(chunk);
            NativeEndian::write_u16(chunk, v);
        }
        Ok(buffer)
    }

    fn from_ascii(
        reader: &mut dyn Read,
        width: u32,
        height: u32,
        samples: u32,
    ) -> ImageResult<Vec<u8>> {
        let mut buffer = vec![0; (width * height * samples * 2) as usize];
        for i in 0..(width*height*samples) as usize {
            let v = read_separated_ascii::<u16>(reader)?;
            NativeEndian::write_u16(&mut buffer[2*i..][..2], v);
        }
        Ok(buffer)
    }
}

// The image is encoded in rows of bits, high order bits first. Any bits beyond the row bits should
// be ignored. Also, contrary to rgb, black pixels are encoded as a 1 while white is 0. This will
// need to be reversed for the grayscale output.
impl Sample for PbmBit {
    fn bytelen(width: u32, height: u32, samples: u32) -> ImageResult<usize> {
        let count = width * samples;
        let linelen = (count / 8) + ((count % 8) != 0) as u32;
        Ok((linelen * height) as usize)
    }

    fn from_bytes(
        bytes: &[u8],
        width: u32,
        height: u32,
        samples: u32,
    ) -> ImageResult<Vec<u8>> {
        assert_eq!(bytes.len(), Self::bytelen(width, height, samples).unwrap());

        let mut expanded = utils::expand_bits(1, width * samples, bytes);
        for b in expanded.iter_mut() {
            *b = !*b;
        }
        Ok(expanded)
    }

    fn from_ascii(
        reader: &mut dyn Read,
        width: u32,
        height: u32,
        samples: u32,
    ) -> ImageResult<Vec<u8>> {
        let count = (width*height*samples) as usize;
        let raw_samples = reader.bytes()
            .filter_map(|ascii| match ascii {
                Ok(b'0') => Some(Ok(255)),
                Ok(b'1') => Some(Ok(0)),
                Err(err) => Some(Err(ImageError::IoError(err))),
                Ok(b'\t')
                | Ok(b'\n')
                | Ok(b'\x0b')
                | Ok(b'\x0c')
                | Ok(b'\r')
                | Ok(b' ') => None,
                Ok(c) => Some(Err(DecoderError::UnexpectedByteInRaster(c).into())),
            })
            .take(count)
            .collect::<ImageResult<Vec<u8>>>()?;

        if raw_samples.len() < count {
            return Err(DecoderError::InputTooShort.into());
        }

        Ok(raw_samples)
    }
}

// Encoded just like a normal U8 but we check the values.
impl Sample for BWBit {
    fn bytelen(width: u32, height: u32, samples: u32) -> ImageResult<usize> {
        U8::bytelen(width, height, samples)
    }

    fn from_bytes(
        bytes: &[u8],
        width: u32,
        height: u32,
        samples: u32,
    ) -> ImageResult<Vec<u8>> {
        assert_eq!(bytes.len(), Self::bytelen(width, height, samples).unwrap());

        let values = U8::from_bytes(bytes, width, height, samples)?;
        if let Some(val) = values.iter().find(|&val| *val > 1) {
            return Err(DecoderError::SampleOutOfBounds(*val).into());
        }
        Ok(values)
    }

    fn from_ascii(
        _reader: &mut dyn Read,
        _width: u32,
        _height: u32,
        _samples: u32,
    ) -> ImageResult<Vec<u8>> {
        unreachable!("BW bits from anymaps are never encoded as ASCII")
    }
}

impl DecodableImageHeader for BitmapHeader {
    fn tuple_type(&self) -> ImageResult<TupleType> {
        Ok(TupleType::PbmBit)
    }
}

impl DecodableImageHeader for GraymapHeader {
    fn tuple_type(&self) -> ImageResult<TupleType> {
        match self.maxwhite {
            v if v <= 0xFF => Ok(TupleType::GrayU8),
            v if v <= 0xFFFF => Ok(TupleType::GrayU16),
            _ => Err(DecoderError::MaxvalTooBig(self.maxwhite).into()),
        }
    }
}

impl DecodableImageHeader for PixmapHeader {
    fn tuple_type(&self) -> ImageResult<TupleType> {
        match self.maxval {
            v if v <= 0xFF => Ok(TupleType::RGBU8),
            v if v <= 0xFFFF => Ok(TupleType::RGBU16),
            _ => Err(DecoderError::MaxvalTooBig(self.maxval).into()),
        }
    }
}

impl DecodableImageHeader for ArbitraryHeader {
    fn tuple_type(&self) -> ImageResult<TupleType> {
        match self.tupltype {
            None if self.depth == 1 => Ok(TupleType::GrayU8),
            None if self.depth == 2 => Err(ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                ImageFormat::Pnm.into(),
                UnsupportedErrorKind::Color(ExtendedColorType::La8),
            ))),
            None if self.depth == 3 => Ok(TupleType::RGBU8),
            None if self.depth == 4 => Err(ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                ImageFormat::Pnm.into(),
                UnsupportedErrorKind::Color(ExtendedColorType::Rgba8),
            ))),

            Some(ArbitraryTuplType::BlackAndWhite) if self.maxval == 1 && self.depth == 1 => {
                Ok(TupleType::BWBit)
            }
            Some(ArbitraryTuplType::BlackAndWhite) => Err(DecoderError::InvalidDepthOrMaxval {
                tuple_type: ArbitraryTuplType::BlackAndWhite,
                maxval: self.maxval,
                depth: self.depth,
            }.into()),

            Some(ArbitraryTuplType::Grayscale) if self.depth == 1 && self.maxval <= 0xFF => {
                Ok(TupleType::GrayU8)
            }
            Some(ArbitraryTuplType::Grayscale) if self.depth <= 1 && self.maxval <= 0xFFFF => {
                Ok(TupleType::GrayU16)
            }
            Some(ArbitraryTuplType::Grayscale) => Err(DecoderError::InvalidDepthOrMaxval {
                tuple_type: ArbitraryTuplType::Grayscale,
                maxval: self.maxval,
                depth: self.depth,
            }.into()),

            Some(ArbitraryTuplType::RGB) if self.depth == 3 && self.maxval <= 0xFF => {
                Ok(TupleType::RGBU8)
            }
            Some(ArbitraryTuplType::RGB) if self.depth == 3 && self.maxval <= 0xFFFF => {
                Ok(TupleType::RGBU16)
            }
            Some(ArbitraryTuplType::RGB) => Err(DecoderError::InvalidDepth {
                tuple_type: ArbitraryTuplType::RGB,
                depth: self.depth,
            }.into()),

            Some(ArbitraryTuplType::BlackAndWhiteAlpha) => {
                Err(ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                    ImageFormat::Pnm.into(),
                    UnsupportedErrorKind::GenericFeature(
                        format!("Color type {}", ArbitraryTuplType::BlackAndWhiteAlpha.name())),
                )))
            }
            Some(ArbitraryTuplType::GrayscaleAlpha) => Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Pnm.into(),
                    UnsupportedErrorKind::Color(ExtendedColorType::La8),
                ),
            )),
            Some(ArbitraryTuplType::RGBAlpha) => Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Pnm.into(),
                    UnsupportedErrorKind::Color(ExtendedColorType::Rgba8),
                ),
            )),
            Some(ArbitraryTuplType::Custom(ref custom)) => Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Pnm.into(),
                    UnsupportedErrorKind::GenericFeature(format!(
                        "Tuple type {:?}",
                        custom
                    )),
                ),
            )),
            None => Err(DecoderError::TupleTypeUnrecognised.into()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    /// Tests reading of a valid blackandwhite pam
    #[test]
    fn pam_blackandwhite() {
        let pamdata = b"P7
WIDTH 4
HEIGHT 4
DEPTH 1
MAXVAL 1
TUPLTYPE BLACKANDWHITE
# Comment line
ENDHDR
\x01\x00\x00\x01\x01\x00\x00\x01\x01\x00\x00\x01\x01\x00\x00\x01";
        let decoder = PnmDecoder::new(&pamdata[..]).unwrap();
        assert_eq!(decoder.color_type(), ColorType::L8);
        assert_eq!(decoder.original_color_type(), ExtendedColorType::L1);
        assert_eq!(decoder.dimensions(), (4, 4));
        assert_eq!(decoder.subtype(), PNMSubtype::ArbitraryMap);

        let mut image = vec![0; decoder.total_bytes() as usize];
        decoder.read_image(&mut image).unwrap();
        assert_eq!(
            image,
            vec![0x01, 0x00, 0x00, 0x01, 0x01, 0x00, 0x00, 0x01, 0x01, 0x00, 0x00, 0x01, 0x01, 0x00,
                 0x00, 0x01]
        );
        match PnmDecoder::new(&pamdata[..]).unwrap().into_inner() {
            (
                _,
                PNMHeader {
                    decoded:
                        HeaderRecord::Arbitrary(ArbitraryHeader {
                            width: 4,
                            height: 4,
                            maxval: 1,
                            depth: 1,
                            tupltype: Some(ArbitraryTuplType::BlackAndWhite),
                        }),
                    encoded: _,
                },
            ) => (),
            _ => panic!("Decoded header is incorrect"),
        }
    }

    /// Tests reading of a valid grayscale pam
    #[test]
    fn pam_grayscale() {
        let pamdata = b"P7
WIDTH 4
HEIGHT 4
DEPTH 1
MAXVAL 255
TUPLTYPE GRAYSCALE
# Comment line
ENDHDR
\xde\xad\xbe\xef\xde\xad\xbe\xef\xde\xad\xbe\xef\xde\xad\xbe\xef";
        let decoder = PnmDecoder::new(&pamdata[..]).unwrap();
        assert_eq!(decoder.color_type(), ColorType::L8);
        assert_eq!(decoder.dimensions(), (4, 4));
        assert_eq!(decoder.subtype(), PNMSubtype::ArbitraryMap);

        let mut image = vec![0; decoder.total_bytes() as usize];
        decoder.read_image(&mut image).unwrap();
        assert_eq!(
            image,
            vec![0xde, 0xad, 0xbe, 0xef, 0xde, 0xad, 0xbe, 0xef, 0xde, 0xad, 0xbe, 0xef, 0xde, 0xad,
                 0xbe, 0xef]
        );
        match PnmDecoder::new(&pamdata[..]).unwrap().into_inner() {
            (
                _,
                PNMHeader {
                    decoded:
                        HeaderRecord::Arbitrary(ArbitraryHeader {
                            width: 4,
                            height: 4,
                            depth: 1,
                            maxval: 255,
                            tupltype: Some(ArbitraryTuplType::Grayscale),
                        }),
                    encoded: _,
                },
            ) => (),
            _ => panic!("Decoded header is incorrect"),
        }
    }

    /// Tests reading of a valid rgb pam
    #[test]
    fn pam_rgb() {
        let pamdata = b"P7
# Comment line
MAXVAL 255
TUPLTYPE RGB
DEPTH 3
WIDTH 2
HEIGHT 2
ENDHDR
\xde\xad\xbe\xef\xde\xad\xbe\xef\xde\xad\xbe\xef";
        let decoder = PnmDecoder::new(&pamdata[..]).unwrap();
        assert_eq!(decoder.color_type(), ColorType::Rgb8);
        assert_eq!(decoder.dimensions(), (2, 2));
        assert_eq!(decoder.subtype(), PNMSubtype::ArbitraryMap);

        let mut image = vec![0; decoder.total_bytes() as usize];
        decoder.read_image(&mut image).unwrap();
        assert_eq!(image,
                   vec![0xde, 0xad, 0xbe, 0xef, 0xde, 0xad, 0xbe, 0xef, 0xde, 0xad, 0xbe, 0xef]);
        match PnmDecoder::new(&pamdata[..]).unwrap().into_inner() {
            (
                _,
                PNMHeader {
                    decoded:
                        HeaderRecord::Arbitrary(ArbitraryHeader {
                            maxval: 255,
                            tupltype: Some(ArbitraryTuplType::RGB),
                            depth: 3,
                            width: 2,
                            height: 2,
                        }),
                    encoded: _,
                },
            ) => (),
            _ => panic!("Decoded header is incorrect"),
        }
    }

    #[test]
    fn pbm_binary() {
        // The data contains two rows of the image (each line is padded to the full byte). For
        // comments on its format, see documentation of `impl SampleType for PbmBit`.
        let pbmbinary = [&b"P4 6 2\n"[..], &[0b01101100 as u8, 0b10110111]].concat();
        let decoder = PnmDecoder::new(&pbmbinary[..]).unwrap();
        assert_eq!(decoder.color_type(), ColorType::L8);
        assert_eq!(decoder.original_color_type(), ExtendedColorType::L1);
        assert_eq!(decoder.dimensions(), (6, 2));
        assert_eq!(
            decoder.subtype(),
            PNMSubtype::Bitmap(SampleEncoding::Binary)
        );
        let mut image = vec![0; decoder.total_bytes() as usize];
        decoder.read_image(&mut image).unwrap();
        assert_eq!(image, vec![255, 0, 0, 255, 0, 0, 0, 255, 0, 0, 255, 0]);
        match PnmDecoder::new(&pbmbinary[..]).unwrap().into_inner() {
            (
                _,
                PNMHeader {
                    decoded:
                        HeaderRecord::Bitmap(BitmapHeader {
                            encoding: SampleEncoding::Binary,
                            width: 6,
                            height: 2,
                        }),
                    encoded: _,
                },
            ) => (),
            _ => panic!("Decoded header is incorrect"),
        }
    }

    /// A previous inifite loop.
    #[test]
    fn pbm_binary_ascii_termination() {
        use std::io::{Cursor, Error, ErrorKind, Read, Result};
        struct FailRead(Cursor<&'static [u8]>);

        impl Read for FailRead {
            fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
                match self.0.read(buf) {
                    Ok(n) if n > 0 => Ok(n),
                    _ => Err(Error::new(
                        ErrorKind::BrokenPipe,
                        "Simulated broken pipe error"
                    )),
                }
            }
        }

        let pbmbinary = FailRead(Cursor::new(b"P1 1 1\n"));

        let decoder = PnmDecoder::new(pbmbinary).unwrap();
        let mut image = vec![0; decoder.total_bytes() as usize];
        decoder.read_image(&mut image).expect_err("Image is malformed");
    }

    #[test]
    fn pbm_ascii() {
        // The data contains two rows of the image (each line is padded to the full byte). For
        // comments on its format, see documentation of `impl SampleType for PbmBit`.  Tests all
        // whitespace characters that should be allowed (the 6 characters according to POSIX).
        let pbmbinary = b"P1 6 2\n 0 1 1 0 1 1\n1 0 1 1 0\t\n\x0b\x0c\r1";
        let decoder = PnmDecoder::new(&pbmbinary[..]).unwrap();
        assert_eq!(decoder.color_type(), ColorType::L8);
        assert_eq!(decoder.original_color_type(), ExtendedColorType::L1);
        assert_eq!(decoder.dimensions(), (6, 2));
        assert_eq!(decoder.subtype(), PNMSubtype::Bitmap(SampleEncoding::Ascii));

        let mut image = vec![0; decoder.total_bytes() as usize];
        decoder.read_image(&mut image).unwrap();
        assert_eq!(image, vec![255, 0, 0, 255, 0, 0, 0, 255, 0, 0, 255, 0]);
        match PnmDecoder::new(&pbmbinary[..]).unwrap().into_inner() {
            (
                _,
                PNMHeader {
                    decoded:
                        HeaderRecord::Bitmap(BitmapHeader {
                            encoding: SampleEncoding::Ascii,
                            width: 6,
                            height: 2,
                        }),
                    encoded: _,
                },
            ) => (),
            _ => panic!("Decoded header is incorrect"),
        }
    }

    #[test]
    fn pbm_ascii_nospace() {
        // The data contains two rows of the image (each line is padded to the full byte). Notably,
        // it is completely within specification for the ascii data not to contain separating
        // whitespace for the pbm format or any mix.
        let pbmbinary = b"P1 6 2\n011011101101";
        let decoder = PnmDecoder::new(&pbmbinary[..]).unwrap();
        assert_eq!(decoder.color_type(), ColorType::L8);
        assert_eq!(decoder.original_color_type(), ExtendedColorType::L1);
        assert_eq!(decoder.dimensions(), (6, 2));
        assert_eq!(decoder.subtype(), PNMSubtype::Bitmap(SampleEncoding::Ascii));

        let mut image = vec![0; decoder.total_bytes() as usize];
        decoder.read_image(&mut image).unwrap();
        assert_eq!(image, vec![255, 0, 0, 255, 0, 0, 0, 255, 0, 0, 255, 0]);
        match PnmDecoder::new(&pbmbinary[..]).unwrap().into_inner() {
            (
                _,
                PNMHeader {
                    decoded:
                        HeaderRecord::Bitmap(BitmapHeader {
                            encoding: SampleEncoding::Ascii,
                            width: 6,
                            height: 2,
                        }),
                    encoded: _,
                },
            ) => (),
            _ => panic!("Decoded header is incorrect"),
        }
    }

    #[test]
    fn pgm_binary() {
        // The data contains two rows of the image (each line is padded to the full byte). For
        // comments on its format, see documentation of `impl SampleType for PbmBit`.
        let elements = (0..16).collect::<Vec<_>>();
        let pbmbinary = [&b"P5 4 4 255\n"[..], &elements].concat();
        let decoder = PnmDecoder::new(&pbmbinary[..]).unwrap();
        assert_eq!(decoder.color_type(), ColorType::L8);
        assert_eq!(decoder.dimensions(), (4, 4));
        assert_eq!(
            decoder.subtype(),
            PNMSubtype::Graymap(SampleEncoding::Binary)
        );
        let mut image = vec![0; decoder.total_bytes() as usize];
        decoder.read_image(&mut image).unwrap();
        assert_eq!(image, elements);
        match PnmDecoder::new(&pbmbinary[..]).unwrap().into_inner() {
            (
                _,
                PNMHeader {
                    decoded:
                        HeaderRecord::Graymap(GraymapHeader {
                            encoding: SampleEncoding::Binary,
                            width: 4,
                            height: 4,
                            maxwhite: 255,
                        }),
                    encoded: _,
                },
            ) => (),
            _ => panic!("Decoded header is incorrect"),
        }
    }

    #[test]
    fn pgm_ascii() {
        // The data contains two rows of the image (each line is padded to the full byte). For
        // comments on its format, see documentation of `impl SampleType for PbmBit`.
        let pbmbinary = b"P2 4 4 255\n 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15";
        let decoder = PnmDecoder::new(&pbmbinary[..]).unwrap();
        assert_eq!(decoder.color_type(), ColorType::L8);
        assert_eq!(decoder.dimensions(), (4, 4));
        assert_eq!(
            decoder.subtype(),
            PNMSubtype::Graymap(SampleEncoding::Ascii)
        );
        let mut image = vec![0; decoder.total_bytes() as usize];
        decoder.read_image(&mut image).unwrap();
        assert_eq!(image, (0..16).collect::<Vec<_>>());
        match PnmDecoder::new(&pbmbinary[..]).unwrap().into_inner() {
            (
                _,
                PNMHeader {
                    decoded:
                        HeaderRecord::Graymap(GraymapHeader {
                            encoding: SampleEncoding::Ascii,
                            width: 4,
                            height: 4,
                            maxwhite: 255,
                        }),
                    encoded: _,
                },
            ) => (),
            _ => panic!("Decoded header is incorrect"),
        }
    }
}
