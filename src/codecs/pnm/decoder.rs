use std::error;
use std::fmt::{self, Display};
use std::io::{self, Read};
use std::mem::size_of;
use std::str;

use super::{ArbitraryHeader, ArbitraryTuplType, BitmapHeader, GraymapHeader, PixmapHeader};
use super::{HeaderRecord, PnmHeader, PnmSubtype, SampleEncoding};
use crate::color::{ColorType, ExtendedColorType};
use crate::error::{
    DecodingError, ImageError, ImageResult, UnsupportedError, UnsupportedErrorKind,
};
use crate::{utils, ImageDecoder, ImageFormat};

/// A limit on the maximum length of the TUPLTYPE field in a PAM image that the
/// PnmDecoder will accept.
///
/// This exactly matches the hard limit of 255 bytes + 1 null on the TUPLTYPE
/// from libnetpbm itself. Imposing this hard limit makes it possible to parse
/// PAM without requiring any configurable memory limits, and reduces the risk
/// that users of `image` may create files incompatible with the reference
/// implementation of the format.
const MAX_TUPLE_TYPE_LENGTH: usize = 255;

/// All errors that can occur when attempting to parse a PNM
#[derive(Debug, Clone)]
enum DecoderError {
    /// PNM's "P[123456]" signature wrong or missing
    PnmMagicInvalid([u8; 2]),

    /// More than the exactly one allowed plane specified by the format
    NonAsciiByteInHeader(u8),
    /// The PAM header contained a non-ASCII byte
    NonAsciiLineInPamHeader,
    /// Couldn't parse an integer: expected but did not get an ASCII digit
    InvalidDigit(ErrorDataSource),

    /// The byte after the P7 magic was not 0x0A NEWLINE
    NotNewlineAfterP7Magic(u8),
    /// The PNM header had too few lines
    UnexpectedPnmHeaderEnd,

    /// The specified line was specified twice
    HeaderLineDuplicated(PnmHeaderLine),
    /// The line with the specified identifier was not understood
    HeaderLineUnknown { token: String, too_long: bool },
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
    /// ENDHDR is not alone on its line
    InvalidEndHeader,
    /// PAM Header line only has a first token and no value
    HeaderLineMissingValue,

    /// Not enough data was provided to the Decoder to decode the image
    InputTooShort,
    /// Sample raster contained unexpected byte
    UnexpectedByteInRaster(u8),
    /// Specified sample was out of bounds (e.g. >1 in B&W)
    SampleOutOfBounds(u8),
    /// The image's maxval is zero
    MaxvalZero,
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
    /// A tuple type row in the PAM header did not contain a non-whitespace value
    TupleTypeEmpty,
    /// The tuple type was not too long
    TupleTypeTooLong,
    /// The tuple type was not recognised by the parser
    TupleTypeUnrecognised,

    /// Overflowed the specified value when parsing
    Overflow(ErrorDataSource),
}

impl Display for DecoderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DecoderError::PnmMagicInvalid(magic) => f.write_fmt(format_args!(
                "Expected magic constant for PNM: P1..P7, got [{:#04X?}, {:#04X?}]",
                magic[0], magic[1]
            )),

            DecoderError::NonAsciiByteInHeader(c) => {
                f.write_fmt(format_args!("Non-ASCII character {c:#04X?} in header"))
            }
            DecoderError::NonAsciiLineInPamHeader => f.write_str("Non-ASCII line in PAM header"),
            DecoderError::InvalidDigit(src) => {
                f.write_fmt(format_args!("Non-ASCII-digit character when parsing number in {src}"))
            }

            DecoderError::NotNewlineAfterP7Magic(c) => f.write_fmt(format_args!(
                "Expected newline after P7 magic, got {c:#04X?}"
            )),
            DecoderError::UnexpectedPnmHeaderEnd => f.write_str("Unexpected end of PNM header"),

            DecoderError::HeaderLineDuplicated(line) => {
                f.write_fmt(format_args!("Duplicate {line} line"))
            }
            DecoderError::HeaderLineUnknown{ token, too_long } => f.write_fmt(format_args!(
                "Unknown header line with identifier {token:?}{}", if *too_long {
                    "... (truncated)"
                } else {
                    ""
                }
            )),
            DecoderError::HeaderLineMissing {
                height,
                width,
                depth,
                maxval,
            } => f.write_fmt(format_args!(
                "Missing header line: have height={height:?}, width={width:?}, depth={depth:?}, maxval={maxval:?}"
            )),
            DecoderError::InvalidEndHeader => f.write_str("Unexpected token after ENDHDR"),
            DecoderError::HeaderLineMissingValue => f.write_str("Missing second token in PAM header line"),
            DecoderError::InputTooShort => {
                f.write_str("Not enough data was provided to the Decoder to decode the image")
            }
            DecoderError::UnexpectedByteInRaster(c) => f.write_fmt(format_args!(
                "Unexpected character {c:#04X?} within sample raster"
            )),
            DecoderError::SampleOutOfBounds(val) => {
                f.write_fmt(format_args!("Sample value {val} outside of bounds"))
            }
            DecoderError::MaxvalZero => f.write_str("Image MAXVAL is zero"),
            DecoderError::MaxvalTooBig(maxval) => {
                f.write_fmt(format_args!("Image MAXVAL exceeds {}: {}", 0xFFFF, maxval))
            }

            DecoderError::InvalidDepthOrMaxval {
                tuple_type,
                depth,
                maxval,
            } => f.write_fmt(format_args!(
                "Invalid depth ({}) or maxval ({}) for tuple type {}",
                depth,
                maxval,
                tuple_type.name()
            )),
            DecoderError::InvalidDepth { tuple_type, depth } => f.write_fmt(format_args!(
                "Invalid depth ({}) for tuple type {}",
                depth,
                tuple_type.name()
            )),
            DecoderError::TupleTypeEmpty => f.write_str("The PAM tuple type row did not contain a value"),
            DecoderError::TupleTypeTooLong => f.write_fmt(format_args!(
                "The PAM tuple type name was longer than {MAX_TUPLE_TYPE_LENGTH} ASCII letters"
            )),
            DecoderError::TupleTypeUnrecognised => f.write_str("Tuple type not recognized"),
            DecoderError::Overflow(src) => f.write_fmt(format_args!(
                "Overflow when parsing integer in {src}"
            ))
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

impl error::Error for DecoderError {}

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
    BWAlphaBit,
    GrayU8,
    GrayAlphaU8,
    GrayU16,
    GrayAlphaU16,
    RGBU8,
    RGBAlphaU8,
    RGBU16,
    RGBAlphaU16,
}

trait Sample {
    type Representation;

    /// Representation size in bytes
    fn sample_size() -> u32 {
        size_of::<Self::Representation>() as u32
    }
    fn from_bytes(
        reader: &mut dyn Read,
        output_buf: &mut [u8],
        width: u32,
        height: u32,
        components: u32,
    ) -> ImageResult<()>;
    fn from_ascii(reader: &mut dyn Read, output_buf: &mut [u8]) -> ImageResult<()>;
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
    reader: R,
    header: PnmHeader,
    tuple: TupleType,
}

impl<R: Read> PnmDecoder<R> {
    /// Create a new decoder that decodes from the stream ```read```
    pub fn new(mut buffered_read: R) -> ImageResult<PnmDecoder<R>> {
        let magic = buffered_read.read_magic_constant()?;

        let subtype = match magic {
            [b'P', b'1'] => PnmSubtype::Bitmap(SampleEncoding::Ascii),
            [b'P', b'2'] => PnmSubtype::Graymap(SampleEncoding::Ascii),
            [b'P', b'3'] => PnmSubtype::Pixmap(SampleEncoding::Ascii),
            [b'P', b'4'] => PnmSubtype::Bitmap(SampleEncoding::Binary),
            [b'P', b'5'] => PnmSubtype::Graymap(SampleEncoding::Binary),
            [b'P', b'6'] => PnmSubtype::Pixmap(SampleEncoding::Binary),
            [b'P', b'7'] => PnmSubtype::ArbitraryMap,
            _ => return Err(DecoderError::PnmMagicInvalid(magic).into()),
        };

        let decoder = match subtype {
            PnmSubtype::Bitmap(enc) => PnmDecoder::read_bitmap_header(buffered_read, enc),
            PnmSubtype::Graymap(enc) => PnmDecoder::read_graymap_header(buffered_read, enc),
            PnmSubtype::Pixmap(enc) => PnmDecoder::read_pixmap_header(buffered_read, enc),
            PnmSubtype::ArbitraryMap => PnmDecoder::read_arbitrary_header(buffered_read),
        }?;

        if utils::check_dimension_overflow(
            decoder.dimensions().0,
            decoder.dimensions().1,
            decoder.color_type().bytes_per_pixel(),
        ) {
            return Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Pnm.into(),
                    UnsupportedErrorKind::GenericFeature(format!(
                        "Image dimensions ({}x{}) are too large",
                        decoder.dimensions().0,
                        decoder.dimensions().1
                    )),
                ),
            ));
        }

        Ok(decoder)
    }

    /// Get the header of the decoded image.
    pub fn header(&self) -> &PnmHeader {
        &self.header
    }

    /// Extract the reader and header after an image has been read.
    pub fn into_inner(self) -> (R, PnmHeader) {
        (self.reader, self.header)
    }

    fn read_bitmap_header(mut reader: R, encoding: SampleEncoding) -> ImageResult<PnmDecoder<R>> {
        let header = reader.read_bitmap_header(encoding)?;
        Ok(PnmDecoder {
            reader,
            tuple: TupleType::PbmBit,
            header: PnmHeader {
                decoded: HeaderRecord::Bitmap(header),
                encoded: None,
            },
        })
    }

    fn read_graymap_header(mut reader: R, encoding: SampleEncoding) -> ImageResult<PnmDecoder<R>> {
        let header = reader.read_graymap_header(encoding)?;
        let tuple_type = header.tuple_type()?;
        Ok(PnmDecoder {
            reader,
            tuple: tuple_type,
            header: PnmHeader {
                decoded: HeaderRecord::Graymap(header),
                encoded: None,
            },
        })
    }

    fn read_pixmap_header(mut reader: R, encoding: SampleEncoding) -> ImageResult<PnmDecoder<R>> {
        let header = reader.read_pixmap_header(encoding)?;
        let tuple_type = header.tuple_type()?;
        Ok(PnmDecoder {
            reader,
            tuple: tuple_type,
            header: PnmHeader {
                decoded: HeaderRecord::Pixmap(header),
                encoded: None,
            },
        })
    }

    fn read_arbitrary_header(mut reader: R) -> ImageResult<PnmDecoder<R>> {
        let header = reader.read_arbitrary_header()?;
        let tuple_type = header.tuple_type()?;
        Ok(PnmDecoder {
            reader,
            tuple: tuple_type,
            header: PnmHeader {
                decoded: HeaderRecord::Arbitrary(header),
                encoded: None,
            },
        })
    }
}

trait HeaderReader: Read {
    /// Reads the two magic constant bytes
    fn read_magic_constant(&mut self) -> ImageResult<[u8; 2]> {
        let mut magic: [u8; 2] = [0, 0];
        self.read_exact(&mut magic)?;
        Ok(magic)
    }

    /// Reads an integer as well as a single whitespace after it, ignoring comments
    /// and leading whitespace
    fn read_next_u32(&mut self) -> ImageResult<u32> {
        // pair input bytes with a bool mask to remove comments
        #[allow(clippy::unbuffered_bytes)]
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

        // Streaming parse of the integer. To match Netpbm, this accepts values
        // with leading zeros, like 000005, but no leading + or -
        let mut value: u32 = 0;
        let mut found_digit = false;

        for (_, byte) in mark_comments.filter(|e| e.0) {
            match byte {
                Ok(b'\t' | b'\n' | b'\x0b' | b'\x0c' | b'\r' | b' ') => {
                    if found_digit {
                        break; // We're done as we already have some content
                    }
                }
                Ok(byte) if !byte.is_ascii() => {
                    return Err(DecoderError::NonAsciiByteInHeader(byte).into())
                }
                Ok(byte) => {
                    let digit = match byte {
                        b'0'..=b'9' => u32::from(byte - b'0'),
                        _ => {
                            return Err(DecoderError::InvalidDigit(ErrorDataSource::Preamble).into())
                        }
                    };
                    value = value
                        .checked_mul(10)
                        .ok_or(DecoderError::Overflow(ErrorDataSource::Preamble))?;
                    value = value
                        .checked_add(digit)
                        .ok_or(DecoderError::Overflow(ErrorDataSource::Preamble))?;
                    found_digit = true;
                }
                Err(_) => break,
            }
        }

        if !found_digit {
            return Err(ImageError::IoError(io::ErrorKind::UnexpectedEof.into()));
        }

        Ok(value)
    }

    fn read_bitmap_header(&mut self, encoding: SampleEncoding) -> ImageResult<BitmapHeader> {
        let width = self.read_next_u32()?;
        let height = self.read_next_u32()?;
        Ok(BitmapHeader {
            encoding,
            height,
            width,
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
            height,
            width,
            maxval,
        })
    }

    /// Read the next byte, returning an error on EOF or if the byte is not valid ASCII
    fn read_ascii_char(&mut self) -> ImageResult<u8> {
        let mut buf = [0];
        if self.read(&mut buf)? == 0 {
            return Err(DecoderError::UnexpectedPnmHeaderEnd.into());
        }
        let [byte] = buf;
        if !byte.is_ascii() {
            return Err(DecoderError::NonAsciiLineInPamHeader.into());
        }
        Ok(byte)
    }

    /// Read through the header until a nonempty noncomment line is reached, store
    /// the identifier in the provided buffer, and return its length and whether
    /// the identifier is the only thing on the line. The stored identifier will
    /// be valid ASCII text.
    fn read_next_pam_identifier(&mut self, identifier: &mut [u8]) -> ImageResult<(usize, bool)> {
        // Scan until an identifier starts
        let mut in_comment = false;
        loop {
            match self.read_ascii_char()? {
                b'\n' => {
                    // Empty line or end of comment
                    in_comment = false;
                }
                b'#' => {
                    in_comment = true;
                }
                // Skip whitespace before the identifier token
                b'\t' | b'\x0b' | b'\x0c' | b'\r' | b' ' => (),
                b => {
                    if !in_comment {
                        // Start of an identifier
                        identifier[0] = b;
                        break;
                    }
                }
            }
        }
        let mut id_len = 1;

        // Scan the identifier
        loop {
            match self.read_ascii_char()? {
                b'\t' | b'\x0b' | b'\x0c' | b'\r' | b' ' => {
                    return Ok((id_len, false));
                }
                b'\n' => {
                    return Ok((id_len, true));
                }
                b => {
                    if id_len >= identifier.len() {
                        return Err(DecoderError::HeaderLineUnknown {
                            token: str::from_utf8(identifier).expect("expected ascii").into(),
                            too_long: true,
                        }
                        .into());
                    }
                    identifier[id_len] = b;
                    id_len += 1;
                }
            }
        }
    }

    /// Read a whitespace gap within a line and return the first non-whitespace
    /// character following it. Will only return ASCII characters.
    fn read_pam_intraline_whitespace(&mut self) -> ImageResult<u8> {
        loop {
            match self.read_ascii_char()? {
                b'\t' | b'\x0b' | b'\x0c' | b'\r' | b' ' => (),
                b => {
                    return Ok(b);
                }
            }
        }
    }

    /// Read the ASCII decimal value at the end of a PAM header line and following
    /// whitespace characters up to and including the trailing \n. Similarly to
    /// the PNM parsers, this accepts leading zeros. This writes the value into
    /// `line_val` or errors if `*line_val` was not None.
    fn read_pam_decimal_value(
        &mut self,
        line_val: &mut Option<u32>,
        line: PnmHeaderLine,
    ) -> ImageResult<()> {
        if line_val.is_some() {
            return Err(DecoderError::HeaderLineDuplicated(line).into());
        }

        let first_char = self.read_pam_intraline_whitespace()?;

        let mut value = match first_char {
            b'0'..=b'9' => u32::from(first_char - b'0'),
            _ => return Err(DecoderError::InvalidDigit(ErrorDataSource::Line(line)).into()),
        };

        loop {
            match self.read_ascii_char()? {
                b'\t' | b'\x0b' | b'\x0c' | b'\r' | b' ' => {
                    break;
                }
                b'\n' => {
                    *line_val = Some(value);
                    return Ok(());
                }
                b @ b'0'..=b'9' => {
                    let digit = u32::from(b - b'0');
                    value = value
                        .checked_mul(10)
                        .ok_or(DecoderError::Overflow(ErrorDataSource::Line(line)))?;
                    value = value
                        .checked_add(digit)
                        .ok_or(DecoderError::Overflow(ErrorDataSource::Line(line)))?;
                }
                _ => return Err(DecoderError::InvalidDigit(ErrorDataSource::Line(line)).into()),
            }
        }

        let end = self.read_pam_intraline_whitespace()?;
        if end != b'\n' {
            // In this case, have whitespace inside the decimal
            return Err(DecoderError::InvalidDigit(ErrorDataSource::Line(line)).into());
        }
        *line_val = Some(value);
        Ok(())
    }

    /// Read until the end of the line, storing the tupltype in the provided buffer,
    /// and reading following whitespace characters up to and including the trailing \n.
    fn read_pam_tupltype_value(&mut self, tupltype: &mut [u8]) -> ImageResult<usize> {
        let first_char = self.read_pam_intraline_whitespace()?;
        if first_char == b'\n' {
            return Err(DecoderError::TupleTypeEmpty.into());
        }

        tupltype[0] = first_char;
        // The token can include whitespace inside it, but excludes the whitespace
        // at the end of the line. `len_all` counts all bytes recorded; `len_token`
        // just counts the number of bytes until the last whitespace run.
        let mut len_all = 1;
        let mut len_token = len_all;

        // Scan forward: include tupltype, but exclude trailing whitespace.
        loop {
            match self.read_ascii_char()? {
                b'\n' => {
                    return Ok(len_token);
                }
                b @ (b'\t' | b'\x0b' | b'\x0c' | b'\r' | b' ') => {
                    if len_all >= tupltype.len() {
                        // Now trying to parse trailing whitespace
                        break;
                    }
                    tupltype[len_all] = b;
                    len_all += 1;
                }
                b => {
                    if len_all >= tupltype.len() {
                        return Err(DecoderError::TupleTypeTooLong.into());
                    }
                    tupltype[len_all] = b;
                    len_all += 1;
                    len_token = len_all;
                }
            }
        }

        let end = self.read_pam_intraline_whitespace()?;
        if end != b'\n' {
            // In this case, have a too long stretch of whitespace inside the name
            return Err(DecoderError::TupleTypeTooLong.into());
        }
        Ok(len_token)
    }

    /// Decode the Portable Arbitrary Map header, starting from just after the P7 magic
    /// constant bytes and reading up to and including the \n at the header end.
    fn read_arbitrary_header(&mut self) -> ImageResult<ArbitraryHeader> {
        #[allow(clippy::unbuffered_bytes)]
        match self.bytes().next() {
            None => return Err(ImageError::IoError(io::ErrorKind::UnexpectedEof.into())),
            Some(Err(io)) => return Err(ImageError::IoError(io)),
            Some(Ok(b'\n')) => (),
            Some(Ok(c)) => return Err(DecoderError::NotNewlineAfterP7Magic(c).into()),
        }

        let mut height: Option<u32> = None;
        let mut width: Option<u32> = None;
        let mut depth: Option<u32> = None;
        let mut maxval: Option<u32> = None;
        let mut tuple_type_buf = [0u8; MAX_TUPLE_TYPE_LENGTH];
        let mut tuple_type_len: usize = 0;

        loop {
            // The longest header line token is TUPLTYPE, 8 bytes
            let mut identifier_buf = [0u8; 16];

            let (identifier_len, eol) = self.read_next_pam_identifier(&mut identifier_buf)?;
            let identifier = &identifier_buf[..identifier_len];
            if eol && identifier != b"ENDHDR" {
                return Err(DecoderError::HeaderLineMissingValue.into());
            }

            match identifier {
                b"ENDHDR" => {
                    if !eol {
                        let b = self.read_pam_intraline_whitespace()?;
                        if b != b'\n' {
                            return Err(DecoderError::InvalidEndHeader.into());
                        }
                    }
                    break;
                }
                b"HEIGHT" => self.read_pam_decimal_value(&mut height, PnmHeaderLine::Height)?,
                b"WIDTH" => self.read_pam_decimal_value(&mut width, PnmHeaderLine::Width)?,
                b"DEPTH" => self.read_pam_decimal_value(&mut depth, PnmHeaderLine::Depth)?,
                b"MAXVAL" => self.read_pam_decimal_value(&mut maxval, PnmHeaderLine::Maxval)?,
                b"TUPLTYPE" => {
                    if tuple_type_len >= tuple_type_buf.len() - 1 {
                        return Err(DecoderError::TupleTypeTooLong.into());
                    }
                    // Concatenate together multiple TUPLTYPE fields.
                    if tuple_type_len > 0 {
                        tuple_type_buf[tuple_type_len] = b' ';
                        tuple_type_len += 1;
                    }
                    let added_len =
                        self.read_pam_tupltype_value(&mut tuple_type_buf[tuple_type_len..])?;
                    tuple_type_len += added_len;
                }
                other => {
                    return Err(DecoderError::HeaderLineUnknown {
                        token: str::from_utf8(other).expect("expected ascii").into(),
                        too_long: false,
                    }
                    .into())
                }
            }
        }

        let (Some(h), Some(w), Some(d), Some(m)) = (height, width, depth, maxval) else {
            return Err(DecoderError::HeaderLineMissing {
                height,
                width,
                depth,
                maxval,
            }
            .into());
        };

        let tupltype = match &tuple_type_buf[..tuple_type_len] {
            b"BLACKANDWHITE" => Some(ArbitraryTuplType::BlackAndWhite),
            b"BLACKANDWHITE_ALPHA" => Some(ArbitraryTuplType::BlackAndWhiteAlpha),
            b"GRAYSCALE" => Some(ArbitraryTuplType::Grayscale),
            b"GRAYSCALE_ALPHA" => Some(ArbitraryTuplType::GrayscaleAlpha),
            b"RGB" => Some(ArbitraryTuplType::RGB),
            b"RGB_ALPHA" => Some(ArbitraryTuplType::RGBAlpha),
            // Note: per the PAM spec, if there are no TUPLTYPE lines, then the
            // tuple type is the empty string, which is encoded here as None
            // to preserve existing behavior.
            b"" => None,
            other => Some(ArbitraryTuplType::Custom(
                str::from_utf8(other).expect("expected ascii").into(),
            )),
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

impl<R> HeaderReader for R where R: Read {}

impl<R: Read> ImageDecoder for PnmDecoder<R> {
    fn dimensions(&self) -> (u32, u32) {
        (self.header.width(), self.header.height())
    }

    fn color_type(&self) -> ColorType {
        match self.tuple {
            TupleType::PbmBit => ColorType::L8,
            TupleType::BWBit => ColorType::L8,
            TupleType::BWAlphaBit => ColorType::La8,
            TupleType::GrayU8 => ColorType::L8,
            TupleType::GrayAlphaU8 => ColorType::La8,
            TupleType::GrayU16 => ColorType::L16,
            TupleType::GrayAlphaU16 => ColorType::La16,
            TupleType::RGBU8 => ColorType::Rgb8,
            TupleType::RGBAlphaU8 => ColorType::Rgba8,
            TupleType::RGBU16 => ColorType::Rgb16,
            TupleType::RGBAlphaU16 => ColorType::Rgba16,
        }
    }

    fn original_color_type(&self) -> ExtendedColorType {
        match self.tuple {
            TupleType::PbmBit => ExtendedColorType::L1,
            TupleType::BWBit => ExtendedColorType::L1,
            TupleType::BWAlphaBit => ExtendedColorType::La1,
            TupleType::GrayU8 => ExtendedColorType::L8,
            TupleType::GrayAlphaU8 => ExtendedColorType::La8,
            TupleType::GrayU16 => ExtendedColorType::L16,
            TupleType::GrayAlphaU16 => ExtendedColorType::La16,
            TupleType::RGBU8 => ExtendedColorType::Rgb8,
            TupleType::RGBAlphaU8 => ExtendedColorType::Rgba8,
            TupleType::RGBU16 => ExtendedColorType::Rgb16,
            TupleType::RGBAlphaU16 => ExtendedColorType::Rgba16,
        }
    }

    fn read_image(mut self, buf: &mut [u8]) -> ImageResult<()> {
        assert_eq!(u64::try_from(buf.len()), Ok(self.total_bytes()));
        match self.tuple {
            TupleType::PbmBit => self.read_samples::<PbmBit>(1, buf),
            TupleType::BWBit => self.read_samples::<BWBit>(1, buf),
            TupleType::BWAlphaBit => self.read_samples::<BWBit>(2, buf),
            TupleType::RGBU8 => self.read_samples::<U8>(3, buf),
            TupleType::RGBAlphaU8 => self.read_samples::<U8>(4, buf),
            TupleType::RGBU16 => self.read_samples::<U16>(3, buf),
            TupleType::RGBAlphaU16 => self.read_samples::<U16>(4, buf),
            TupleType::GrayU8 => self.read_samples::<U8>(1, buf),
            TupleType::GrayAlphaU8 => self.read_samples::<U8>(2, buf),
            TupleType::GrayU16 => self.read_samples::<U16>(1, buf),
            TupleType::GrayAlphaU16 => self.read_samples::<U16>(2, buf),
        }
    }

    fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()> {
        (*self).read_image(buf)
    }
}

impl<R: Read> PnmDecoder<R> {
    fn read_samples<S: Sample>(&mut self, components: u32, buf: &mut [u8]) -> ImageResult<()> {
        match self.subtype().sample_encoding() {
            SampleEncoding::Binary => {
                S::from_bytes(
                    &mut self.reader,
                    buf,
                    self.header.width(),
                    self.header.height(),
                    components,
                )?;
            }
            SampleEncoding::Ascii => {
                S::from_ascii(&mut self.reader, buf)?;
            }
        }

        // Scale samples if 8bit or 16bit is not saturated
        let current_sample_max = self.header.maximal_sample();
        let target_sample_max = 256_u32.pow(S::sample_size()) - 1;

        if current_sample_max != target_sample_max {
            let factor = target_sample_max as f32 / current_sample_max as f32;

            if S::sample_size() == 1 {
                for v in buf.iter_mut() {
                    *v = (f32::from(*v) * factor).round() as u8;
                }
            } else if S::sample_size() == 2 {
                for chunk in buf.as_chunks_mut::<2>().0.iter_mut() {
                    let v = (f32::from(u16::from_ne_bytes(*chunk)) * factor).round() as u16;
                    chunk.copy_from_slice(&v.to_ne_bytes());
                }
            }
        }

        Ok(())
    }

    /// Get the pnm subtype, depending on the magic constant contained in the header
    pub fn subtype(&self) -> PnmSubtype {
        self.header.subtype()
    }
}

fn read_separated_ascii<T: TryFrom<u16>>(reader: &mut dyn Read) -> ImageResult<T> {
    let is_separator = |v: &u8| matches!(*v, b'\t' | b'\n' | b'\x0b' | b'\x0c' | b'\r' | b' ');

    let mut v: u16 = 0;
    let mut had_any = false;
    #[allow(clippy::unbuffered_bytes)]
    for rc in reader
        .bytes()
        .skip_while(|v| v.as_ref().ok().is_some_and(is_separator))
        .take_while(|v| v.as_ref().ok().is_some_and(|c| !is_separator(c)))
    {
        let c = rc?;
        let digit = match c {
            b'0'..=b'9' => u16::from(c - b'0'),
            _ => return Err(DecoderError::InvalidDigit(ErrorDataSource::Sample).into()),
        };
        v = v
            .checked_mul(10)
            .ok_or(DecoderError::Overflow(ErrorDataSource::Sample))?;
        v = v
            .checked_add(digit)
            .ok_or(DecoderError::Overflow(ErrorDataSource::Sample))?;
        had_any = true;
    }

    if !had_any {
        return Err(DecoderError::InputTooShort.into());
    }

    Ok(T::try_from(v).or(Err(DecoderError::Overflow(ErrorDataSource::Sample)))?)
}

impl Sample for U8 {
    type Representation = u8;
    fn from_bytes(
        reader: &mut dyn Read,
        output_buf: &mut [u8],
        _width: u32,
        _height: u32,
        _components: u32,
    ) -> ImageResult<()> {
        reader.read_exact(output_buf)?;
        Ok(())
    }

    fn from_ascii(reader: &mut dyn Read, output_buf: &mut [u8]) -> ImageResult<()> {
        for b in output_buf {
            *b = read_separated_ascii(reader)?;
        }
        Ok(())
    }
}

impl Sample for U16 {
    type Representation = u16;

    fn from_bytes(
        reader: &mut dyn Read,
        output_buf: &mut [u8],
        _width: u32,
        _height: u32,
        _components: u32,
    ) -> ImageResult<()> {
        reader.read_exact(output_buf)?;
        for chunk in output_buf.as_chunks_mut::<2>().0.iter_mut() {
            let v = u16::from_be_bytes(*chunk);
            chunk.copy_from_slice(&v.to_ne_bytes());
        }
        Ok(())
    }

    fn from_ascii(reader: &mut dyn Read, output_buf: &mut [u8]) -> ImageResult<()> {
        for chunk in output_buf.as_chunks_mut::<2>().0.iter_mut() {
            let v = read_separated_ascii::<u16>(reader)?;
            chunk.copy_from_slice(&v.to_ne_bytes());
        }
        Ok(())
    }
}

// The image is encoded in rows of bits, high order bits first. Any bits beyond the row bits should
// be ignored. Also, contrary to rgb, black pixels are encoded as a 1 while white is 0. This will
// need to be reversed for the grayscale output.
impl Sample for PbmBit {
    type Representation = u8;

    fn from_bytes(
        reader: &mut dyn Read,
        output_buf: &mut [u8],
        width: u32,
        height: u32,
        components: u32,
    ) -> ImageResult<()> {
        assert!(components == 1);

        let width: usize = width
            .try_into()
            .map_err(|_| DecoderError::Overflow(ErrorDataSource::Sample))?;
        let height: usize = height
            .try_into()
            .map_err(|_| DecoderError::Overflow(ErrorDataSource::Sample))?;
        assert!(width.checked_mul(height) == Some(output_buf.len()));

        let linelen = width.div_ceil(8);
        let bytecount = height
            .checked_mul(linelen)
            .filter(|l| *l <= output_buf.len())
            .expect("PBM packed data is never longer than unpacked");

        reader.read_exact(&mut output_buf[..bytecount])?;

        // Expand the PBM data in place. This can be done byte by byte with a single
        // backwards pass over the image. At all times, the position being read
        // from will not be after the position being written to, so no data is lost.

        for y in (0..height).rev() {
            for x in (0..width).rev() {
                let shift = 7 - (x % 8);
                let v = (output_buf[y * linelen + x / 8] >> shift) & 0x1;
                output_buf[y * width + x] = 1 - v;
            }
        }
        Ok(())
    }

    fn from_ascii(reader: &mut dyn Read, output_buf: &mut [u8]) -> ImageResult<()> {
        #[allow(clippy::unbuffered_bytes)]
        let mut bytes = reader.bytes();
        for b in output_buf {
            loop {
                let byte = bytes
                    .next()
                    .ok_or_else::<ImageError, _>(|| DecoderError::InputTooShort.into())??;
                match byte {
                    b'\t' | b'\n' | b'\x0b' | b'\x0c' | b'\r' | b' ' => continue,
                    b'0' => *b = 255,
                    b'1' => *b = 0,
                    c => return Err(DecoderError::UnexpectedByteInRaster(c).into()),
                }
                break;
            }
        }

        Ok(())
    }
}

// Encoded just like a normal U8 but we check the values.
impl Sample for BWBit {
    type Representation = u8;

    fn from_bytes(
        reader: &mut dyn Read,
        output_buf: &mut [u8],
        width: u32,
        height: u32,
        components: u32,
    ) -> ImageResult<()> {
        U8::from_bytes(reader, output_buf, width, height, components)?;
        if let Some(val) = output_buf.iter().find(|&val| *val > 1) {
            return Err(DecoderError::SampleOutOfBounds(*val).into());
        }
        Ok(())
    }

    fn from_ascii(_reader: &mut dyn Read, _output_buf: &mut [u8]) -> ImageResult<()> {
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
            0 => Err(DecoderError::MaxvalZero.into()),
            v if v <= 0xFF => Ok(TupleType::GrayU8),
            v if v <= 0xFFFF => Ok(TupleType::GrayU16),
            _ => Err(DecoderError::MaxvalTooBig(self.maxwhite).into()),
        }
    }
}

impl DecodableImageHeader for PixmapHeader {
    fn tuple_type(&self) -> ImageResult<TupleType> {
        match self.maxval {
            0 => Err(DecoderError::MaxvalZero.into()),
            v if v <= 0xFF => Ok(TupleType::RGBU8),
            v if v <= 0xFFFF => Ok(TupleType::RGBU16),
            _ => Err(DecoderError::MaxvalTooBig(self.maxval).into()),
        }
    }
}

impl DecodableImageHeader for ArbitraryHeader {
    fn tuple_type(&self) -> ImageResult<TupleType> {
        match self.tupltype {
            _ if self.maxval == 0 => Err(DecoderError::MaxvalZero.into()),
            None if self.depth == 1 => Ok(TupleType::GrayU8),
            None if self.depth == 2 => Ok(TupleType::GrayAlphaU8),
            None if self.depth == 3 => Ok(TupleType::RGBU8),
            None if self.depth == 4 => Ok(TupleType::RGBAlphaU8),

            Some(ArbitraryTuplType::BlackAndWhite) if self.maxval == 1 && self.depth == 1 => {
                Ok(TupleType::BWBit)
            }
            Some(ArbitraryTuplType::BlackAndWhite) => Err(DecoderError::InvalidDepthOrMaxval {
                tuple_type: ArbitraryTuplType::BlackAndWhite,
                maxval: self.maxval,
                depth: self.depth,
            }
            .into()),

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
            }
            .into()),

            Some(ArbitraryTuplType::RGB) if self.depth == 3 && self.maxval <= 0xFF => {
                Ok(TupleType::RGBU8)
            }
            Some(ArbitraryTuplType::RGB) if self.depth == 3 && self.maxval <= 0xFFFF => {
                Ok(TupleType::RGBU16)
            }
            Some(ArbitraryTuplType::RGB) => Err(DecoderError::InvalidDepth {
                tuple_type: ArbitraryTuplType::RGB,
                depth: self.depth,
            }
            .into()),

            Some(ArbitraryTuplType::BlackAndWhiteAlpha) if self.depth == 2 && self.maxval == 1 => {
                Ok(TupleType::BWAlphaBit)
            }
            Some(ArbitraryTuplType::BlackAndWhiteAlpha) => {
                Err(DecoderError::InvalidDepthOrMaxval {
                    tuple_type: ArbitraryTuplType::BlackAndWhiteAlpha,
                    maxval: self.maxval,
                    depth: self.depth,
                }
                .into())
            }

            Some(ArbitraryTuplType::GrayscaleAlpha) if self.depth == 2 && self.maxval <= 0xFF => {
                Ok(TupleType::GrayAlphaU8)
            }
            Some(ArbitraryTuplType::GrayscaleAlpha) if self.depth == 2 && self.maxval <= 0xFFFF => {
                Ok(TupleType::GrayAlphaU16)
            }
            Some(ArbitraryTuplType::GrayscaleAlpha) => Err(DecoderError::InvalidDepth {
                tuple_type: ArbitraryTuplType::GrayscaleAlpha,
                depth: self.depth,
            }
            .into()),

            Some(ArbitraryTuplType::RGBAlpha) if self.depth == 4 && self.maxval <= 0xFF => {
                Ok(TupleType::RGBAlphaU8)
            }
            Some(ArbitraryTuplType::RGBAlpha) if self.depth == 4 && self.maxval <= 0xFFFF => {
                Ok(TupleType::RGBAlphaU16)
            }
            Some(ArbitraryTuplType::RGBAlpha) => Err(DecoderError::InvalidDepth {
                tuple_type: ArbitraryTuplType::RGBAlpha,
                depth: self.depth,
            }
            .into()),

            Some(ArbitraryTuplType::Custom(ref custom)) => Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Pnm.into(),
                    UnsupportedErrorKind::GenericFeature(format!("Tuple type {custom:?}")),
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
        assert_eq!(decoder.subtype(), PnmSubtype::ArbitraryMap);

        let mut image = vec![0; decoder.total_bytes() as usize];
        decoder.read_image(&mut image).unwrap();
        assert_eq!(
            image,
            vec![
                0xFF, 0x00, 0x00, 0xFF, 0xFF, 0x00, 0x00, 0xFF, 0xFF, 0x00, 0x00, 0xFF, 0xFF, 0x00,
                0x00, 0xFF
            ]
        );
        match PnmDecoder::new(&pamdata[..]).unwrap().into_inner() {
            (
                _,
                PnmHeader {
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

    /// Tests reading of a valid blackandwhite_alpha pam
    #[test]
    fn pam_blackandwhite_alpha() {
        let pamdata = b"P7
WIDTH 2
HEIGHT 2
DEPTH 2
MAXVAL 1
TUPLTYPE BLACKANDWHITE_ALPHA
# Comment line
ENDHDR
\x01\x00\x00\x01\x01\x00\x00\x01";
        let decoder = PnmDecoder::new(&pamdata[..]).unwrap();
        assert_eq!(decoder.color_type(), ColorType::La8);
        assert_eq!(decoder.original_color_type(), ExtendedColorType::La1);
        assert_eq!(decoder.dimensions(), (2, 2));
        assert_eq!(decoder.subtype(), PnmSubtype::ArbitraryMap);

        let mut image = vec![0; decoder.total_bytes() as usize];
        decoder.read_image(&mut image).unwrap();
        assert_eq!(image, vec![0xFF, 0x00, 0x00, 0xFF, 0xFF, 0x00, 0x00, 0xFF,]);
        match PnmDecoder::new(&pamdata[..]).unwrap().into_inner() {
            (
                _,
                PnmHeader {
                    decoded:
                        HeaderRecord::Arbitrary(ArbitraryHeader {
                            width: 2,
                            height: 2,
                            maxval: 1,
                            depth: 2,
                            tupltype: Some(ArbitraryTuplType::BlackAndWhiteAlpha),
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
        assert_eq!(decoder.subtype(), PnmSubtype::ArbitraryMap);

        let mut image = vec![0; decoder.total_bytes() as usize];
        decoder.read_image(&mut image).unwrap();
        assert_eq!(
            image,
            vec![
                0xde, 0xad, 0xbe, 0xef, 0xde, 0xad, 0xbe, 0xef, 0xde, 0xad, 0xbe, 0xef, 0xde, 0xad,
                0xbe, 0xef
            ]
        );
        match PnmDecoder::new(&pamdata[..]).unwrap().into_inner() {
            (
                _,
                PnmHeader {
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

    /// Tests reading of a valid grayscale_alpha pam
    #[test]
    fn pam_grayscale_alpha() {
        let pamdata = b"P7
HEIGHT 1
WIDTH 2
MAXVAL 65535
DEPTH 2
TUPLTYPE GRAYSCALE_ALPHA
# Comment line
ENDHDR
\xdc\xba\x32\x10\xdc\xba\x32\x10";
        let decoder = PnmDecoder::new(&pamdata[..]).unwrap();
        assert_eq!(decoder.color_type(), ColorType::La16);
        assert_eq!(decoder.original_color_type(), ExtendedColorType::La16);
        assert_eq!(decoder.dimensions(), (2, 1));
        assert_eq!(decoder.subtype(), PnmSubtype::ArbitraryMap);

        let mut image = vec![0; decoder.total_bytes() as usize];
        decoder.read_image(&mut image).unwrap();
        assert_eq!(
            image,
            [
                u16::to_ne_bytes(0xdcba),
                u16::to_ne_bytes(0x3210),
                u16::to_ne_bytes(0xdcba),
                u16::to_ne_bytes(0x3210)
            ]
            .concat()
        );
        match PnmDecoder::new(&pamdata[..]).unwrap().into_inner() {
            (
                _,
                PnmHeader {
                    decoded:
                        HeaderRecord::Arbitrary(ArbitraryHeader {
                            width: 2,
                            height: 1,
                            maxval: 65535,
                            depth: 2,
                            tupltype: Some(ArbitraryTuplType::GrayscaleAlpha),
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
        assert_eq!(decoder.subtype(), PnmSubtype::ArbitraryMap);

        let mut image = vec![0; decoder.total_bytes() as usize];
        decoder.read_image(&mut image).unwrap();
        assert_eq!(
            image,
            vec![0xde, 0xad, 0xbe, 0xef, 0xde, 0xad, 0xbe, 0xef, 0xde, 0xad, 0xbe, 0xef]
        );
        match PnmDecoder::new(&pamdata[..]).unwrap().into_inner() {
            (
                _,
                PnmHeader {
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

    /// Tests reading of a valid rgb_alpha pam
    #[test]
    fn pam_rgb_alpha() {
        let pamdata = b"P7
WIDTH 1
HEIGHT 3
DEPTH 4
MAXVAL 15
TUPLTYPE RGB_ALPHA
# Comment line
ENDHDR
\x00\x01\x02\x03\x0a\x0b\x0c\x0d\x05\x06\x07\x08";
        let decoder = PnmDecoder::new(&pamdata[..]).unwrap();
        assert_eq!(decoder.color_type(), ColorType::Rgba8);
        assert_eq!(decoder.original_color_type(), ExtendedColorType::Rgba8);
        assert_eq!(decoder.dimensions(), (1, 3));
        assert_eq!(decoder.subtype(), PnmSubtype::ArbitraryMap);

        let mut image = vec![0; decoder.total_bytes() as usize];
        decoder.read_image(&mut image).unwrap();
        assert_eq!(image, b"\x00\x11\x22\x33\xaa\xbb\xcc\xdd\x55\x66\x77\x88",);
        match PnmDecoder::new(&pamdata[..]).unwrap().into_inner() {
            (
                _,
                PnmHeader {
                    decoded:
                        HeaderRecord::Arbitrary(ArbitraryHeader {
                            width: 1,
                            height: 3,
                            maxval: 15,
                            depth: 4,
                            tupltype: Some(ArbitraryTuplType::RGBAlpha),
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
        let pbmbinary = [&b"P4 6 2\n"[..], &[0b0110_1100_u8, 0b1011_0111]].concat();
        let decoder = PnmDecoder::new(&pbmbinary[..]).unwrap();
        assert_eq!(decoder.color_type(), ColorType::L8);
        assert_eq!(decoder.original_color_type(), ExtendedColorType::L1);
        assert_eq!(decoder.dimensions(), (6, 2));
        assert_eq!(
            decoder.subtype(),
            PnmSubtype::Bitmap(SampleEncoding::Binary)
        );
        let mut image = vec![0; decoder.total_bytes() as usize];
        decoder.read_image(&mut image).unwrap();
        assert_eq!(image, vec![255, 0, 0, 255, 0, 0, 0, 255, 0, 0, 255, 0]);
        match PnmDecoder::new(&pbmbinary[..]).unwrap().into_inner() {
            (
                _,
                PnmHeader {
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

    /// A previous infinite loop.
    #[test]
    fn pbm_binary_ascii_termination() {
        use std::io::{BufReader, Cursor, Error, ErrorKind, Read, Result};
        struct FailRead(Cursor<&'static [u8]>);

        impl Read for FailRead {
            fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
                match self.0.read(buf) {
                    Ok(n) if n > 0 => Ok(n),
                    _ => Err(Error::new(
                        ErrorKind::BrokenPipe,
                        "Simulated broken pipe error",
                    )),
                }
            }
        }

        let pbmbinary = BufReader::new(FailRead(Cursor::new(b"P1 1 1\n")));

        let decoder = PnmDecoder::new(pbmbinary).unwrap();
        let mut image = vec![0; decoder.total_bytes() as usize];
        decoder
            .read_image(&mut image)
            .expect_err("Image is malformed");
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
        assert_eq!(decoder.subtype(), PnmSubtype::Bitmap(SampleEncoding::Ascii));

        let mut image = vec![0; decoder.total_bytes() as usize];
        decoder.read_image(&mut image).unwrap();
        assert_eq!(image, vec![255, 0, 0, 255, 0, 0, 0, 255, 0, 0, 255, 0]);
        match PnmDecoder::new(&pbmbinary[..]).unwrap().into_inner() {
            (
                _,
                PnmHeader {
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
        assert_eq!(decoder.subtype(), PnmSubtype::Bitmap(SampleEncoding::Ascii));

        let mut image = vec![0; decoder.total_bytes() as usize];
        decoder.read_image(&mut image).unwrap();
        assert_eq!(image, vec![255, 0, 0, 255, 0, 0, 0, 255, 0, 0, 255, 0]);
        match PnmDecoder::new(&pbmbinary[..]).unwrap().into_inner() {
            (
                _,
                PnmHeader {
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
            PnmSubtype::Graymap(SampleEncoding::Binary)
        );
        let mut image = vec![0; decoder.total_bytes() as usize];
        decoder.read_image(&mut image).unwrap();
        assert_eq!(image, elements);
        match PnmDecoder::new(&pbmbinary[..]).unwrap().into_inner() {
            (
                _,
                PnmHeader {
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
            PnmSubtype::Graymap(SampleEncoding::Ascii)
        );
        let mut image = vec![0; decoder.total_bytes() as usize];
        decoder.read_image(&mut image).unwrap();
        assert_eq!(image, (0..16).collect::<Vec<_>>());
        match PnmDecoder::new(&pbmbinary[..]).unwrap().into_inner() {
            (
                _,
                PnmHeader {
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

    #[test]
    fn ppm_ascii() {
        let ascii = b"P3 1 1 2000\n0 1000 2000";
        let decoder = PnmDecoder::new(&ascii[..]).unwrap();
        let mut image = vec![0; decoder.total_bytes() as usize];
        decoder.read_image(&mut image).unwrap();
        assert_eq!(
            image,
            [
                0_u16.to_ne_bytes(),
                (u16::MAX / 2 + 1).to_ne_bytes(),
                u16::MAX.to_ne_bytes()
            ]
            .into_iter()
            .flatten()
            .collect::<Vec<_>>()
        );
    }

    #[test]
    fn dimension_overflow() {
        let pamdata = b"P7
# Comment line
MAXVAL 255
TUPLTYPE RGB
DEPTH 3
WIDTH 4294967295
HEIGHT 4294967295
ENDHDR
\xde\xad\xbe\xef\xde\xad\xbe\xef\xde\xad\xbe\xef";

        assert!(PnmDecoder::new(&pamdata[..]).is_err());
    }

    #[test]
    fn issue_1508() {
        let _ = crate::load_from_memory(b"P391919 16999 1 1 9 919 16999 1 9999 999* 99999 N");
    }

    #[test]
    fn issue_1616_overflow() {
        let data = [
            80, 54, 10, 52, 50, 57, 52, 56, 50, 57, 52, 56, 35, 56, 10, 52, 10, 48, 10, 12, 12, 56,
        ];
        // Validate: we have a header. Note: we might already calculate that this will fail but
        // then we could not return information about the header to the caller.
        let decoder = PnmDecoder::new(&data[..]).unwrap();
        let mut image = vec![0; decoder.total_bytes() as usize];
        let _ = decoder.read_image(&mut image);
    }

    #[test]
    fn data_too_short() {
        let data = b"P3 16 16 1\n";
        let decoder = PnmDecoder::new(&data[..]).unwrap();
        let mut image = vec![0; decoder.total_bytes() as usize];

        let _ = decoder.read_image(&mut image).unwrap_err();
    }

    #[test]
    fn no_integers_with_plus() {
        let data = b"P3 +1 1 1\n";
        assert!(PnmDecoder::new(&data[..]).is_err());
    }

    #[test]
    fn incomplete_pnm_header() {
        let data = b"P5 2 3 \n";
        assert!(PnmDecoder::new(&data[..]).is_err());
    }

    #[test]
    fn leading_zeros() {
        let data = b"P2 03 00000000000002 00100\n011 22 033\n44 055 66\n";
        let decoder = PnmDecoder::new(&data[..]).unwrap();
        let mut image = vec![0; decoder.total_bytes() as usize];
        assert!(decoder.read_image(&mut image).is_ok());
    }

    #[test]
    fn header_overflow() {
        let data = b"P1 4294967295 4294967297\n";
        assert!(PnmDecoder::new(&data[..]).is_err());
    }

    #[test]
    fn header_large_dimension() {
        let data = b"P4 1 01234567890\n";
        let decoder = PnmDecoder::new(&data[..]).unwrap();
        assert!(decoder.dimensions() == (1, 1234567890));
    }

    #[test]
    fn pam_header_whitespace_variants() {
        let pam_header = concat!(
            /*P7*/ "\n",
            "\tWIDTH 2\n",
            "HEIGHT\r\r3\r\r\n",
            "\n",
            " \tTUPLTYPE  \t  CYAN\n",
            "  \n",
            "DEPTH 1\n",
            "\rMAXVAL  00100\n",
            "TUPLTYPE MAGENTA \n",
            "# \tComment\x0b\x0c\r line\n",
            "\rTUPLTYPE  \t  YELLOW\t KHAKI\n",
            "##\\\n",
            "\x0b\x0cENDHDR \t\n",
        );
        let mut reader: &[u8] = pam_header.as_bytes();
        let header = reader.read_arbitrary_header().unwrap();
        assert!(reader.is_empty());
        match header {
            ArbitraryHeader {
                width: 2,
                height: 3,
                maxval: 100,
                depth: 1,
                tupltype: Some(ArbitraryTuplType::Custom(x)),
            } if x == "CYAN MAGENTA YELLOW\t KHAKI" => (),
            _ => panic!("header misparse: {:?}", header),
        }
    }

    #[test]
    fn pam_header_no_tupltype_line() {
        let pam_header = b"
MAXVAL 255
DEPTH 3
WIDTH 1
HEIGHT 2
ENDHDR
";
        let mut reader: &[u8] = pam_header.as_slice();
        let header = reader.read_arbitrary_header().unwrap();
        assert!(reader.is_empty());
        match header {
            ArbitraryHeader {
                width: 1,
                height: 2,
                maxval: 255,
                depth: 3,
                tupltype: None,
            } => (),
            _ => panic!("header misparse: {:?}", header),
        }
    }

    #[test]
    fn pam_header_long_whitespace() {
        let template = "
MAXVAL 1\t
DEPTH\t1
WIDTH 1
HEIGHT 1
TUPLTYPE\tGRAYSCALE\t
ENDHDR
";
        let pam_header = template.replace('\t', &" ".repeat(256));

        let mut reader: &[u8] = pam_header.as_bytes();
        let header = reader.read_arbitrary_header().unwrap();
        assert!(reader.is_empty());
        match header {
            ArbitraryHeader {
                width: 1,
                height: 1,
                maxval: 1,
                depth: 1,
                tupltype: Some(ArbitraryTuplType::Grayscale),
            } => (),
            _ => panic!("header misparse: {:?}", header),
        }
    }

    /// Get the DecoderError produced on the input PAM header, or panic
    fn pam_header_decode_error(header: &[u8]) -> DecoderError {
        use std::error::Error;

        let mut reader = header;
        let error = reader.read_arbitrary_header().unwrap_err();
        let ImageError::Decoding(x) = error else {
            panic!("unexpected error type");
        };
        x.source()
            .expect("should have error source")
            .downcast_ref::<DecoderError>()
            .expect("expected DecoderError source")
            .clone()
    }

    #[test]
    fn pam_header_empty_tupltype() {
        let header = b"
MAXVAL 255
DEPTH 3
WIDTH 1
HEIGHT 2
TUPLTYPE\t
ENDHDR
";
        assert!(matches!(
            pam_header_decode_error(header),
            DecoderError::TupleTypeEmpty
        ));
    }

    #[test]
    fn pam_header_integer_overflow() {
        let header = b"
MAXVAL 255
DEPTH 3
WIDTH 1000000000000000
HEIGHT 1
TUPLTYPE RGB
ENDHDR
";
        assert!(matches!(
            pam_header_decode_error(header),
            DecoderError::Overflow(_)
        ));
    }

    #[test]
    fn pam_header_missing_value() {
        let header = b"
MAXVAL 255
DEPTH 3
WIDTH
HEIGHT 1
TUPLTYPE RGB
ENDHDR
";
        assert!(matches!(
            pam_header_decode_error(header),
            DecoderError::HeaderLineMissingValue
        ));
    }

    #[test]
    fn pam_header_bad_integer() {
        // "3 3" is not a valid integer
        let header = b"
MAXVAL 255
DEPTH 3
WIDTH 3 3
HEIGHT 1
TUPLTYPE RGB
ENDHDR
";
        assert!(matches!(
            pam_header_decode_error(header),
            DecoderError::InvalidDigit(_)
        ));
    }

    #[test]
    fn pam_header_long_tupltype_1() {
        let template = "
MAXVAL 255
DEPTH 3
WIDTH 3
HEIGHT 1
TUPLTYPE LONG
ENDHDR
";
        let header = template.replace("LONG", &"LONG".repeat(256));
        assert!(matches!(
            pam_header_decode_error(header.as_bytes()),
            DecoderError::TupleTypeTooLong
        ));
    }

    #[test]
    fn pam_header_long_tupltype_2() {
        let template = "
MAXVAL 255
DEPTH 3
WIDTH 3
HEIGHT 1
TUPLTYPE LONG
ENDHDR
";
        let header = template.replace("TUPLTYPE LONG\n", &"TUPLTYPE LONG\n".repeat(256));
        assert!(matches!(
            pam_header_decode_error(header.as_bytes()),
            DecoderError::TupleTypeTooLong
        ));
    }

    #[test]
    fn pam_header_unknown() {
        let header = b"
MAXVAL 255
DEPTH 3
WIDTH 3
HEIGHT 1
tupltype RGB
ENDHDR
";
        let DecoderError::HeaderLineUnknown { token, too_long } = pam_header_decode_error(header)
        else {
            panic!("unexpected error");
        };
        assert!(token == "tupltype" && !too_long);
    }

    #[test]
    fn pam_header_long_identifier() {
        let template = "
MAXVAL 255
DEPTH 3
WIDTH 3
HEIGHT 1
LONG RGB
ENDHDR
";
        let header = template.replace("LONG", &"LONG".repeat(256));
        let DecoderError::HeaderLineUnknown { token: _, too_long } =
            pam_header_decode_error(header.as_bytes())
        else {
            panic!("unexpected error");
        };
        assert!(too_long);
    }

    #[test]
    fn pam_header_bad_end() {
        let header = b"
MAXVAL 255
DEPTH 3
WIDTH 3
HEIGHT 1
TUPLTYPE RGB
ENDHDR OOPS
";
        assert!(matches!(
            pam_header_decode_error(header),
            DecoderError::InvalidEndHeader
        ));
    }
}
