use crate::utils::vec_try_with_capacity;
use std::cmp::{self, Ordering};
use std::io::{self, BufRead, Seek, SeekFrom};
use std::iter::{repeat, Rev};
use std::slice::ChunksExactMut;
use std::{error, fmt};

use byteorder_lite::{LittleEndian, ReadBytesExt};

use crate::color::ColorType;
use crate::error::{
    DecodingError, ImageError, ImageResult, InsufficientDataError, UnsupportedError,
    UnsupportedErrorKind,
};
use crate::io::ReadExt;
use crate::{ImageDecoder, ImageFormat};

const BITMAPCOREHEADER_SIZE: u32 = 12;
const BITMAPINFOHEADER_SIZE: u32 = 40;
const BITMAPV2HEADER_SIZE: u32 = 52;
const BITMAPV3HEADER_SIZE: u32 = 56;
const BITMAPV4HEADER_SIZE: u32 = 108;
const BITMAPV5HEADER_SIZE: u32 = 124;

// Compression method constants
const BI_RGB: u32 = 0;
const BI_RLE8: u32 = 1;
const BI_RLE4: u32 = 2;
const BI_BITFIELDS: u32 = 3;
const BI_JPEG: u32 = 4; // Used in legacy Windows pass-through printing path - not supported
const BI_PNG: u32 = 5; // Used in legacy Windows pass-through printing path - not supported
const BI_ALPHABITFIELDS: u32 = 6;
const BI_CMYK: u32 = 11;
const BI_CMYKRLE8: u32 = 12;
const BI_CMYKRLE4: u32 = 13;

static LOOKUP_TABLE_3_BIT_TO_8_BIT: [u8; 8] = [0, 36, 73, 109, 146, 182, 219, 255];
static LOOKUP_TABLE_4_BIT_TO_8_BIT: [u8; 16] = [
    0, 17, 34, 51, 68, 85, 102, 119, 136, 153, 170, 187, 204, 221, 238, 255,
];
static LOOKUP_TABLE_5_BIT_TO_8_BIT: [u8; 32] = [
    0, 8, 16, 25, 33, 41, 49, 58, 66, 74, 82, 90, 99, 107, 115, 123, 132, 140, 148, 156, 165, 173,
    181, 189, 197, 206, 214, 222, 230, 239, 247, 255,
];
static LOOKUP_TABLE_6_BIT_TO_8_BIT: [u8; 64] = [
    0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 45, 49, 53, 57, 61, 65, 69, 73, 77, 81, 85, 89, 93,
    97, 101, 105, 109, 113, 117, 121, 125, 130, 134, 138, 142, 146, 150, 154, 158, 162, 166, 170,
    174, 178, 182, 186, 190, 194, 198, 202, 206, 210, 215, 219, 223, 227, 231, 235, 239, 243, 247,
    251, 255,
];

static R5_G5_B5_COLOR_MASK: Bitfields = Bitfields {
    r: Bitfield { len: 5, shift: 10 },
    g: Bitfield { len: 5, shift: 5 },
    b: Bitfield { len: 5, shift: 0 },
    a: Bitfield { len: 0, shift: 0 },
};
const R8_G8_B8_COLOR_MASK: Bitfields = Bitfields {
    r: Bitfield { len: 8, shift: 24 },
    g: Bitfield { len: 8, shift: 16 },
    b: Bitfield { len: 8, shift: 8 },
    a: Bitfield { len: 0, shift: 0 },
};
const R8_G8_B8_A8_COLOR_MASK: Bitfields = Bitfields {
    r: Bitfield { len: 8, shift: 16 },
    g: Bitfield { len: 8, shift: 8 },
    b: Bitfield { len: 8, shift: 0 },
    a: Bitfield { len: 8, shift: 24 },
};

const RLE_ESCAPE: u8 = 0;
const RLE_ESCAPE_EOL: u8 = 0;
const RLE_ESCAPE_EOF: u8 = 1;
const RLE_ESCAPE_DELTA: u8 = 2;

const ALPHA_OPAQUE: u8 = 0xFF;

/// The maximum width/height the decoder will process.
const MAX_WIDTH_HEIGHT: i32 = 0xFFFF;

/// The value of the V5 header field indicating an embedded ICC profile ("MBED").
const PROFILE_EMBEDDED: u32 = 0x4D424544;

/// Parsed BITMAPCOREHEADER fields (excludes 4-byte size field).
struct ParsedCoreHeader {
    width: i32,
    height: i32,
    bit_count: u16,
    image_type: ImageType,
}

impl ParsedCoreHeader {
    /// Parse BITMAPCOREHEADER fields from an 8-byte buffer.
    fn parse(buffer: &[u8; 8]) -> ImageResult<Self> {
        let width = i32::from(u16::from_le_bytes(buffer[0..2].try_into().unwrap()));
        let height = i32::from(u16::from_le_bytes(buffer[2..4].try_into().unwrap()));

        let planes = u16::from_le_bytes(buffer[4..6].try_into().unwrap());
        if planes != 1 {
            return Err(DecoderError::MoreThanOnePlane.into());
        }

        let bit_count = u16::from_le_bytes(buffer[6..8].try_into().unwrap());
        let image_type = match bit_count {
            1 | 4 | 8 => ImageType::Palette,
            24 => ImageType::RGB24,
            _ => {
                return Err(
                    DecoderError::InvalidChannelWidth(ChannelWidthError::Rgb, bit_count).into(),
                )
            }
        };

        Ok(ParsedCoreHeader {
            width,
            height,
            bit_count,
            image_type,
        })
    }
}

/// Parsed BITMAPINFOHEADER fields (excludes 4-byte size field).
struct ParsedInfoHeader {
    width: i32,
    height: i32,
    top_down: bool,
    bit_count: u16,
    compression: u32,
    colors_used: u32,
}

impl ParsedInfoHeader {
    /// Parse BITMAPINFOHEADER fields from a 36-byte buffer.
    fn parse(buffer: &[u8; 36]) -> ImageResult<Self> {
        let width = i32::from_le_bytes(buffer[0..4].try_into().unwrap());
        let mut height = i32::from_le_bytes(buffer[4..8].try_into().unwrap());

        // Width cannot be negative
        if width < 0 {
            return Err(DecoderError::NegativeWidth(width).into());
        } else if width > MAX_WIDTH_HEIGHT || height > MAX_WIDTH_HEIGHT {
            return Err(DecoderError::ImageTooLarge(width, height).into());
        }

        if height == i32::MIN {
            return Err(DecoderError::InvalidHeight.into());
        }

        // A negative height indicates a top-down DIB
        let top_down = if height < 0 {
            height = -height;
            true
        } else {
            false
        };

        let planes = u16::from_le_bytes(buffer[8..10].try_into().unwrap());
        if planes != 1 {
            return Err(DecoderError::MoreThanOnePlane.into());
        }

        let bit_count = u16::from_le_bytes(buffer[10..12].try_into().unwrap());
        let compression = u32::from_le_bytes(buffer[12..16].try_into().unwrap());

        // Top-down DIBs cannot be compressed
        if top_down && compression != BI_RGB && compression != BI_BITFIELDS {
            return Err(DecoderError::ImageTypeInvalidForTopDown(compression).into());
        }

        // Skip size_image (16-19), x_pix_permeter (20-23), y_pix_permeter (24-27)
        let colors_used = u32::from_le_bytes(buffer[28..32].try_into().unwrap());
        // Skip important_colors (32-35)
        Ok(ParsedInfoHeader {
            width,
            height,
            top_down,
            bit_count,
            compression,
            colors_used,
        })
    }
}

/// Parsed bitfield masks from a 12 or 16 byte buffer.
struct ParsedBitfields {
    r_mask: u32,
    g_mask: u32,
    b_mask: u32,
    a_mask: u32,
}

impl ParsedBitfields {
    /// Parse bitfield masks from a buffer.
    /// For BITMAPINFOHEADER with BI_BITFIELDS: 12 bytes (R, G, B)
    /// For V3/V4/V5 headers: 16 bytes (R, G, B, A)
    fn parse(buffer: &[u8]) -> ImageResult<Self> {
        if buffer.len() < 12 {
            return Err(ImageError::Decoding(DecodingError::new(
                ImageFormat::Bmp.into(),
                "Bitfields buffer too small",
            )));
        }

        let r_mask = u32::from_le_bytes([buffer[0], buffer[1], buffer[2], buffer[3]]);
        let g_mask = u32::from_le_bytes([buffer[4], buffer[5], buffer[6], buffer[7]]);
        let b_mask = u32::from_le_bytes([buffer[8], buffer[9], buffer[10], buffer[11]]);
        let a_mask = if buffer.len() >= 16 {
            u32::from_le_bytes([buffer[12], buffer[13], buffer[14], buffer[15]])
        } else {
            0
        };

        Ok(ParsedBitfields {
            r_mask,
            g_mask,
            b_mask,
            a_mask,
        })
    }

    /// Apply parsed bitfields to the decoder state.
    fn apply_to_decoder<R: BufRead + Seek>(self, decoder: &mut BmpDecoder<R>) -> ImageResult<()> {
        decoder.bitfields = match decoder.image_type {
            ImageType::Bitfields16 => Some(Bitfields::from_mask(
                self.r_mask,
                self.g_mask,
                self.b_mask,
                self.a_mask,
                16,
            )?),
            ImageType::Bitfields32 => Some(Bitfields::from_mask(
                self.r_mask,
                self.g_mask,
                self.b_mask,
                self.a_mask,
                32,
            )?),
            _ => None,
        };

        if decoder.bitfields.is_some() && self.a_mask != 0 {
            decoder.add_alpha_channel = true;
        }

        Ok(())
    }
}

/// Parsed ICC profile metadata from V5 header buffer.
struct ParsedIccProfile {
    profile_offset: u32,
    profile_size: u32,
}

impl ParsedIccProfile {
    /// Parse ICC profile metadata from V5 header buffer (bytes 52-119 of the header).
    /// Expects a buffer containing at least bytes from offset 52 to 119.
    fn parse(buffer: &[u8]) -> ImageResult<Option<Self>> {
        // V5 header fields:
        // bV5CSType at offset 56 (byte 52 in buffer after 4-byte size field = index 52)
        // bV5ProfileData at offset 112 (byte 108 in buffer after 4-byte size field = index 108)
        // bV5ProfileSize at offset 116 (byte 112 in buffer after 4-byte size field = index 112)
        const V5_CS_TYPE_OFFSET: usize = 52;
        const V5_PROFILE_DATA_OFFSET: usize = 108;
        const V5_PROFILE_SIZE_OFFSET: usize = 112;

        if buffer.len() < V5_PROFILE_SIZE_OFFSET + 4 {
            return Ok(None);
        }

        let cs_type = u32::from_le_bytes([
            buffer[V5_CS_TYPE_OFFSET],
            buffer[V5_CS_TYPE_OFFSET + 1],
            buffer[V5_CS_TYPE_OFFSET + 2],
            buffer[V5_CS_TYPE_OFFSET + 3],
        ]);

        // Only embedded profiles are supported
        if cs_type != PROFILE_EMBEDDED {
            return Ok(None);
        }

        let profile_offset = u32::from_le_bytes([
            buffer[V5_PROFILE_DATA_OFFSET],
            buffer[V5_PROFILE_DATA_OFFSET + 1],
            buffer[V5_PROFILE_DATA_OFFSET + 2],
            buffer[V5_PROFILE_DATA_OFFSET + 3],
        ]);
        let profile_size = u32::from_le_bytes([
            buffer[V5_PROFILE_SIZE_OFFSET],
            buffer[V5_PROFILE_SIZE_OFFSET + 1],
            buffer[V5_PROFILE_SIZE_OFFSET + 2],
            buffer[V5_PROFILE_SIZE_OFFSET + 3],
        ]);

        if profile_size == 0 || profile_offset == 0 {
            return Ok(None);
        }

        Ok(Some(ParsedIccProfile {
            profile_offset,
            profile_size,
        }))
    }
}

#[derive(PartialEq, Copy, Clone, Debug)]
enum ImageType {
    Palette,
    RGB16,
    RGB24,
    RGB32,
    RGBA32,
    RLE8,
    RLE4,
    Bitfields16,
    Bitfields32,
}

#[derive(PartialEq)]
enum BMPHeaderType {
    Core,
    Info,
    V2,
    V3,
    V4,
    V5,
}

#[derive(PartialEq)]
enum FormatFullBytes {
    RGB24,
    RGB32,
    RGBA32,
    Format888,
}

enum Chunker<'a> {
    FromTop(ChunksExactMut<'a, u8>),
    FromBottom(Rev<ChunksExactMut<'a, u8>>),
}

pub(crate) struct RowIterator<'a> {
    chunks: Chunker<'a>,
}

impl<'a> Iterator for RowIterator<'a> {
    type Item = &'a mut [u8];

    #[inline(always)]
    fn next(&mut self) -> Option<&'a mut [u8]> {
        match self.chunks {
            Chunker::FromTop(ref mut chunks) => chunks.next(),
            Chunker::FromBottom(ref mut chunks) => chunks.next(),
        }
    }
}

/// All errors that can occur when attempting to parse a BMP
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum DecoderError {
    // Failed to decompress RLE data.
    CorruptRleData,

    /// The bitfield mask interleaves set and unset bits
    BitfieldMaskNonContiguous,
    /// Bitfield mask invalid (e.g. too long for specified type)
    BitfieldMaskInvalid,
    /// Bitfield (of the specified width – 16- or 32-bit) mask not present
    BitfieldMaskMissing(u32),
    /// Bitfield (of the specified width – 16- or 32-bit) masks not present
    BitfieldMasksMissing(u32),

    /// BMP's "BM" signature wrong or missing
    BmpSignatureInvalid,
    /// More than the exactly one allowed plane specified by the format
    MoreThanOnePlane,
    /// Invalid amount of bits per channel for the specified image type
    InvalidChannelWidth(ChannelWidthError, u16),

    /// The width is negative
    NegativeWidth(i32),
    /// One of the dimensions is larger than a soft limit
    ImageTooLarge(i32, i32),
    /// The height is `i32::min_value()`
    ///
    /// General negative heights specify top-down DIBs
    InvalidHeight,

    /// Specified image type is invalid for top-down BMPs (i.e. is compressed)
    ImageTypeInvalidForTopDown(u32),
    /// Image type not currently recognized by the decoder
    ImageTypeUnknown(u32),

    /// Bitmap header smaller than the core header
    HeaderTooSmall(u32),

    /// The palette is bigger than allowed by the bit count of the BMP
    PaletteSizeExceeded {
        colors_used: u32,
        bit_count: u16,
    },
}

impl fmt::Display for DecoderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DecoderError::CorruptRleData => f.write_str("Corrupt RLE data"),
            DecoderError::BitfieldMaskNonContiguous => f.write_str("Non-contiguous bitfield mask"),
            DecoderError::BitfieldMaskInvalid => f.write_str("Invalid bitfield mask"),
            DecoderError::BitfieldMaskMissing(bb) => {
                f.write_fmt(format_args!("Missing {bb}-bit bitfield mask"))
            }
            DecoderError::BitfieldMasksMissing(bb) => {
                f.write_fmt(format_args!("Missing {bb}-bit bitfield masks"))
            }
            DecoderError::BmpSignatureInvalid => f.write_str("BMP signature not found"),
            DecoderError::MoreThanOnePlane => f.write_str("More than one plane"),
            DecoderError::InvalidChannelWidth(tp, n) => {
                f.write_fmt(format_args!("Invalid channel bit count for {tp}: {n}"))
            }
            DecoderError::NegativeWidth(w) => f.write_fmt(format_args!("Negative width ({w})")),
            DecoderError::ImageTooLarge(w, h) => f.write_fmt(format_args!(
                "Image too large (one of ({w}, {h}) > soft limit of {MAX_WIDTH_HEIGHT})"
            )),
            DecoderError::InvalidHeight => f.write_str("Invalid height"),
            DecoderError::ImageTypeInvalidForTopDown(tp) => f.write_fmt(format_args!(
                "Invalid image type {tp} for top-down image."
            )),
            DecoderError::ImageTypeUnknown(tp) => {
                f.write_fmt(format_args!("Unknown image compression type {tp}"))
            }
            DecoderError::HeaderTooSmall(s) => {
                f.write_fmt(format_args!("Bitmap header too small ({s} bytes)"))
            }
            DecoderError::PaletteSizeExceeded {
                colors_used,
                bit_count,
            } => f.write_fmt(format_args!(
                "Palette size {colors_used} exceeds maximum size for BMP with bit count of {bit_count}"
            )),
        }
    }
}

impl From<DecoderError> for ImageError {
    fn from(e: DecoderError) -> ImageError {
        ImageError::Decoding(DecodingError::new(ImageFormat::Bmp.into(), e))
    }
}

impl error::Error for DecoderError {}

/// Distinct image types whose saved channel width can be invalid
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum ChannelWidthError {
    /// RGB
    Rgb,
    /// 8-bit run length encoding
    Rle8,
    /// 4-bit run length encoding
    Rle4,
    /// Bitfields (16- or 32-bit)
    Bitfields,
}

impl fmt::Display for ChannelWidthError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            ChannelWidthError::Rgb => "RGB",
            ChannelWidthError::Rle8 => "RLE8",
            ChannelWidthError::Rle4 => "RLE4",
            ChannelWidthError::Bitfields => "bitfields",
        })
    }
}

/// Calculate BMP row padding bytes needed to align to 4-byte boundary.
/// BMP rows must be padded to a multiple of 4 bytes.
#[inline]
fn calculate_row_padding(row_bytes: usize) -> usize {
    (4 - (row_bytes % 4)) % 4
}

/// Convenience function to check if the combination of width, length and number of
/// channels would result in a buffer that would overflow.
fn check_for_overflow(width: i32, length: i32, channels: usize) -> ImageResult<()> {
    num_bytes(width, length, channels)
        .map(|_| ())
        .ok_or_else(|| {
            ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                ImageFormat::Bmp.into(),
                UnsupportedErrorKind::GenericFeature(format!(
                    "Image dimensions ({width}x{length} w/{channels} channels) are too large"
                )),
            ))
        })
}

/// Calculate how many many bytes a buffer holding a decoded image with these properties would
/// require. Returns `None` if the buffer size would overflow or if one of the sizes are negative.
fn num_bytes(width: i32, length: i32, channels: usize) -> Option<usize> {
    if width <= 0 || length <= 0 {
        None
    } else {
        match channels.checked_mul(width as usize) {
            Some(n) => n.checked_mul(length as usize),
            None => None,
        }
    }
}

/// Call the provided function on each row of the provided buffer, returning Err if the provided
/// function returns an error, extends the buffer if it's not large enough.
fn with_rows<F>(
    buffer: &mut [u8],
    width: i32,
    height: i32,
    channels: usize,
    top_down: bool,
    mut func: F,
) -> io::Result<()>
where
    F: FnMut(&mut [u8]) -> io::Result<()>,
{
    // An overflow should already have been checked for when this is called,
    // though we check anyhow, as it somehow seems to increase performance slightly.
    let row_width = channels.checked_mul(width as usize).unwrap();
    let full_image_size = row_width.checked_mul(height as usize).unwrap();
    assert_eq!(buffer.len(), full_image_size);

    if !top_down {
        for row in buffer.chunks_mut(row_width).rev() {
            func(row)?;
        }
    } else {
        for row in buffer.chunks_mut(row_width) {
            func(row)?;
        }
    }
    Ok(())
}

fn set_8bit_pixel_run<'a, T: Iterator<Item = &'a u8>>(
    pixel_iter: &mut ChunksExactMut<u8>,
    palette: &[[u8; 3]],
    indices: T,
    n_pixels: usize,
) -> bool {
    for idx in indices.take(n_pixels) {
        if let Some(pixel) = pixel_iter.next() {
            let rgb = palette[*idx as usize];
            pixel[0] = rgb[0];
            pixel[1] = rgb[1];
            pixel[2] = rgb[2];
        } else {
            return false;
        }
    }
    true
}

fn set_4bit_pixel_run<'a, T: Iterator<Item = &'a u8>>(
    pixel_iter: &mut ChunksExactMut<u8>,
    palette: &[[u8; 3]],
    indices: T,
    mut n_pixels: usize,
) -> bool {
    for idx in indices {
        macro_rules! set_pixel {
            ($i:expr) => {
                if n_pixels == 0 {
                    break;
                }
                if let Some(pixel) = pixel_iter.next() {
                    let rgb = palette[$i as usize];
                    pixel[0] = rgb[0];
                    pixel[1] = rgb[1];
                    pixel[2] = rgb[2];
                } else {
                    return false;
                }
                n_pixels -= 1;
            };
        }
        set_pixel!(idx >> 4);
        set_pixel!(idx & 0xf);
    }
    true
}

#[rustfmt::skip]
fn set_2bit_pixel_run<'a, T: Iterator<Item = &'a u8>>(
    pixel_iter: &mut ChunksExactMut<u8>,
    palette: &[[u8; 3]],
    indices: T,
    mut n_pixels: usize,
) -> bool {
    for idx in indices {
        macro_rules! set_pixel {
            ($i:expr) => {
                if n_pixels == 0 {
                    break;
                }
                if let Some(pixel) = pixel_iter.next() {
                    let rgb = palette[$i as usize];
                    pixel[0] = rgb[0];
                    pixel[1] = rgb[1];
                    pixel[2] = rgb[2];
                } else {
                    return false;
                }
                n_pixels -= 1;
            };
        }
        set_pixel!((idx >> 6) & 0x3u8);
        set_pixel!((idx >> 4) & 0x3u8);
        set_pixel!((idx >> 2) & 0x3u8);
        set_pixel!( idx       & 0x3u8);
    }
    true
}

fn set_1bit_pixel_run<'a, T: Iterator<Item = &'a u8>>(
    pixel_iter: &mut ChunksExactMut<u8>,
    palette: &[[u8; 3]],
    indices: T,
) {
    for idx in indices {
        let mut bit = 0x80;
        loop {
            if let Some(pixel) = pixel_iter.next() {
                let rgb = palette[usize::from((idx & bit) != 0)];
                pixel[0] = rgb[0];
                pixel[1] = rgb[1];
                pixel[2] = rgb[2];
            } else {
                return;
            }

            bit >>= 1;
            if bit == 0 {
                break;
            }
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
struct Bitfield {
    shift: u32,
    len: u32,
}

impl Bitfield {
    fn from_mask(mask: u32, max_len: u32) -> ImageResult<Bitfield> {
        if mask == 0 {
            return Ok(Bitfield { shift: 0, len: 0 });
        }
        let mut shift = mask.trailing_zeros();
        let mut len = (!(mask >> shift)).trailing_zeros();
        if len != mask.count_ones() {
            return Err(DecoderError::BitfieldMaskNonContiguous.into());
        }
        if len + shift > max_len {
            return Err(DecoderError::BitfieldMaskInvalid.into());
        }
        if len > 8 {
            shift += len - 8;
            len = 8;
        }
        Ok(Bitfield { shift, len })
    }

    fn read(&self, data: u32) -> u8 {
        let data = data >> self.shift;
        match self.len {
            1 => ((data & 0b1) * 0xff) as u8,
            2 => ((data & 0b11) * 0x55) as u8,
            3 => LOOKUP_TABLE_3_BIT_TO_8_BIT[(data & 0b00_0111) as usize],
            4 => LOOKUP_TABLE_4_BIT_TO_8_BIT[(data & 0b00_1111) as usize],
            5 => LOOKUP_TABLE_5_BIT_TO_8_BIT[(data & 0b01_1111) as usize],
            6 => LOOKUP_TABLE_6_BIT_TO_8_BIT[(data & 0b11_1111) as usize],
            7 => (((data & 0x7f) << 1) | ((data & 0x7f) >> 6)) as u8,
            8 => (data & 0xff) as u8,
            _ => panic!(),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
struct Bitfields {
    r: Bitfield,
    g: Bitfield,
    b: Bitfield,
    a: Bitfield,
}

impl Bitfields {
    fn from_mask(
        r_mask: u32,
        g_mask: u32,
        b_mask: u32,
        a_mask: u32,
        max_len: u32,
    ) -> ImageResult<Bitfields> {
        let bitfields = Bitfields {
            r: Bitfield::from_mask(r_mask, max_len)?,
            g: Bitfield::from_mask(g_mask, max_len)?,
            b: Bitfield::from_mask(b_mask, max_len)?,
            a: Bitfield::from_mask(a_mask, max_len)?,
        };
        if bitfields.r.len == 0 || bitfields.g.len == 0 || bitfields.b.len == 0 {
            return Err(DecoderError::BitfieldMaskMissing(max_len).into());
        }
        Ok(bitfields)
    }
}

/// Internal state for resumable BMP decoding
#[derive(Clone, Debug)]
enum DecoderState {
    Initial,
    ReadingFileHeader {
        bytes_read: usize,
        buffer: Vec<u8>,
    },
    ReadingDibHeaderSize {
        bytes_read: usize,
        buffer: Vec<u8>,
    },
    ReadingDibHeader {
        header_size: u32,
        bytes_read: usize,
        buffer: Vec<u8>,
    },
    ReadingBitmasks {
        bytes_read: usize,
        buffer: Vec<u8>,
    },
    ReadingPalette {
        entries_read: u32,
        total_entries: u32,
        bytes_per_color: usize,
        buffer: Vec<u8>,
    },
    ReadyForPixelData {
        current_row: u32,
        row_byte_length: usize,
    },
    ReadingIccProfile {
        bytes_to_skip: u64,
        size: u32,
        bytes_read: usize,
        buffer: Vec<u8>,
    },
    ReadyForRleData {
        current_row: u32,
        x_position: usize,
        row_buffer: Vec<u8>,
        hit_eof: bool,
    },
    Complete,
}

/// A bmp decoder
pub struct BmpDecoder<R> {
    reader: R,

    bmp_header_type: BMPHeaderType,
    indexed_color: bool,

    width: i32,
    height: i32,
    data_offset: u64,
    top_down: bool,
    no_file_header: bool,
    add_alpha_channel: bool,
    has_loaded_metadata: bool,
    image_type: ImageType,

    bit_count: u16,
    colors_used: u32,
    palette: Option<Vec<[u8; 3]>>,
    bitfields: Option<Bitfields>,
    icc_profile: Option<Vec<u8>>,
    icc_profile_offset: Option<u64>,
    icc_profile_size: Option<u32>,

    /// State for resumable streaming decoding. None = traditional mode
    streaming_state: Option<DecoderState>,
}

enum RLEInsn {
    EndOfFile,
    EndOfRow,
    Delta(u8, u8),
    Absolute(u8, Vec<u8>),
    PixelRun(u8, u8),
}

impl<R: BufRead + Seek> BmpDecoder<R> {
    /// Helper: Try to read exact number of bytes into buffer incrementally
    /// Returns Ok(true) if all bytes read, Ok(false) if need more data
    fn try_read_exact_incremental(
        reader: &mut R,
        buffer: &mut Vec<u8>,
        bytes_read: &mut usize,
        total_bytes: usize,
    ) -> ImageResult<bool> {
        // Ensure buffer has capacity
        if buffer.len() < total_bytes {
            buffer.resize(total_bytes, 0);
        }

        // Try to read remaining bytes
        while *bytes_read < total_bytes {
            match reader.read(&mut buffer[*bytes_read..total_bytes]) {
                Ok(0) => {
                    // EOF - need more data
                    return Ok(false);
                }
                Ok(n) => {
                    *bytes_read += n;
                }
                Err(e) if e.kind() == io::ErrorKind::WouldBlock => {
                    // Would block - need more data
                    return Ok(false);
                }
                Err(e) if e.kind() == io::ErrorKind::Interrupted => {
                    // Interrupted, try again
                    continue;
                }
                Err(e) => return Err(ImageError::IoError(e)),
            }
        }

        Ok(true)
    }

    /// Create a new BMP decoder in streaming mode.
    ///
    /// Unlike `new()`, metadata is not automatically loaded. Call `try_read_metadata()`
    /// to incrementally load metadata, then use `read_row()` for pixel data.
    pub fn new_streaming(reader: R) -> ImageResult<BmpDecoder<R>> {
        let mut decoder = Self::new_decoder(reader);
        decoder.streaming_state = Some(DecoderState::Initial);
        Ok(decoder)
    }

    fn new_decoder(reader: R) -> BmpDecoder<R> {
        BmpDecoder {
            reader,

            bmp_header_type: BMPHeaderType::Info,
            indexed_color: false,

            width: 0,
            height: 0,
            data_offset: 0,
            top_down: false,
            no_file_header: false,
            add_alpha_channel: false,
            has_loaded_metadata: false,
            image_type: ImageType::Palette,

            bit_count: 0,
            colors_used: 0,
            palette: None,
            bitfields: None,
            icc_profile: None,
            icc_profile_offset: None,
            icc_profile_size: None,
            streaming_state: None,
        }
    }

    /// Create a new decoder that decodes from the stream ```r```
    pub fn new(reader: R) -> ImageResult<BmpDecoder<R>> {
        let mut decoder = Self::new_decoder(reader);
        decoder.read_metadata()?;
        Ok(decoder)
    }

    /// Create a new decoder that decodes from the stream ```r``` without first
    /// reading a BITMAPFILEHEADER. This is useful for decoding the `CF_DIB` format
    /// directly from the Windows clipboard.
    pub fn new_without_file_header(reader: R) -> ImageResult<BmpDecoder<R>> {
        let mut decoder = Self::new_decoder(reader);
        decoder.no_file_header = true;
        decoder.read_metadata()?;
        Ok(decoder)
    }

    #[cfg(feature = "ico")]
    pub(crate) fn new_with_ico_format(reader: R) -> ImageResult<BmpDecoder<R>> {
        let mut decoder = Self::new_decoder(reader);
        decoder.read_metadata_in_ico_format()?;
        Ok(decoder)
    }

    /// If true, the palette in BMP does not apply to the image even if it is found.
    /// In other words, the output image is the indexed color.
    pub fn set_indexed_color(&mut self, indexed_color: bool) {
        self.indexed_color = indexed_color;
    }

    #[cfg(feature = "ico")]
    pub(crate) fn reader(&mut self) -> &mut R {
        &mut self.reader
    }

    fn read_file_header(&mut self) -> ImageResult<()> {
        if self.no_file_header {
            return Ok(());
        }
        let mut signature = [0; 2];
        self.reader.read_exact(&mut signature)?;

        if signature != b"BM"[..] {
            return Err(DecoderError::BmpSignatureInvalid.into());
        }

        // The next 8 bytes represent file size, followed the 4 reserved bytes
        // We're not interesting these values
        self.reader.read_u32::<LittleEndian>()?;
        self.reader.read_u32::<LittleEndian>()?;

        self.data_offset = u64::from(self.reader.read_u32::<LittleEndian>()?);

        Ok(())
    }

    /// Determine the image type from the compression method and bit count.
    fn image_type_from_compression(
        compression: u32,
        bit_count: u16,
        add_alpha_channel: bool,
    ) -> ImageResult<ImageType> {
        match compression {
            BI_RGB => match bit_count {
                1 | 2 | 4 | 8 => Ok(ImageType::Palette),
                16 => Ok(ImageType::RGB16),
                24 => Ok(ImageType::RGB24),
                32 if add_alpha_channel => Ok(ImageType::RGBA32),
                32 => Ok(ImageType::RGB32),
                _ => {
                    Err(DecoderError::InvalidChannelWidth(ChannelWidthError::Rgb, bit_count).into())
                }
            },
            BI_RLE8 => match bit_count {
                8 => Ok(ImageType::RLE8),
                _ => Err(
                    DecoderError::InvalidChannelWidth(ChannelWidthError::Rle8, bit_count).into(),
                ),
            },
            BI_RLE4 => match bit_count {
                4 => Ok(ImageType::RLE4),
                _ => Err(
                    DecoderError::InvalidChannelWidth(ChannelWidthError::Rle4, bit_count).into(),
                ),
            },
            BI_BITFIELDS | BI_ALPHABITFIELDS => match bit_count {
                16 => Ok(ImageType::Bitfields16),
                32 => Ok(ImageType::Bitfields32),
                _ => Err(DecoderError::InvalidChannelWidth(
                    ChannelWidthError::Bitfields,
                    bit_count,
                )
                .into()),
            },
            BI_JPEG => Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Bmp.into(),
                    UnsupportedErrorKind::GenericFeature("JPEG compression".to_owned()),
                ),
            )),
            BI_PNG => Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Bmp.into(),
                    UnsupportedErrorKind::GenericFeature("PNG compression".to_owned()),
                ),
            )),
            BI_CMYK | BI_CMYKRLE4 | BI_CMYKRLE8 => Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Bmp.into(),
                    UnsupportedErrorKind::GenericFeature("CMYK format".to_owned()),
                ),
            )),
            _ => Err(DecoderError::ImageTypeUnknown(compression).into()),
        }
    }

    /// Incrementally read BMP metadata (streaming mode only).
    ///
    /// Returns `Ok(Some(()))` when complete, `Err(ImageError::InsufficientData(_))` when more data is needed,
    /// or `Err(_)` on other failures. Call repeatedly until metadata is complete.
    pub fn try_read_metadata(&mut self) -> ImageResult<Option<()>> {
        // Only works in streaming mode - check this FIRST
        if self.streaming_state.is_none() {
            return Err(ImageError::Decoding(DecodingError::new(
                ImageFormat::Bmp.into(),
                "try_read_metadata() requires streaming mode (use new_streaming())",
            )));
        }

        // If metadata already loaded, return immediately UNLESS we're reading ICC profile
        if self.has_loaded_metadata {
            match self.streaming_state.as_ref() {
                Some(DecoderState::ReadingIccProfile { .. }) => {}
                Some(DecoderState::Complete) => return Ok(Some(())),
                _ => return Ok(Some(())),
            }
        }

        loop {
            let state = self
                .streaming_state
                .take()
                .expect("streaming_state was None");

            match state {
                DecoderState::Initial => {
                    self.streaming_state = Some(DecoderState::ReadingFileHeader {
                        bytes_read: 0,
                        buffer: Vec::new(),
                    });
                }
                DecoderState::ReadingFileHeader {
                    mut bytes_read,
                    mut buffer,
                } => {
                    if self.no_file_header {
                        self.streaming_state = Some(DecoderState::ReadingDibHeaderSize {
                            bytes_read: 0,
                            buffer: Vec::new(),
                        });
                        continue;
                    }

                    const FILE_HEADER_SIZE: usize = 14;
                    if !Self::try_read_exact_incremental(
                        &mut self.reader,
                        &mut buffer,
                        &mut bytes_read,
                        FILE_HEADER_SIZE,
                    )? {
                        self.streaming_state =
                            Some(DecoderState::ReadingFileHeader { bytes_read, buffer });
                        return Err(ImageError::InsufficientData(InsufficientDataError::new()));
                    }

                    if &buffer[0..2] != b"BM" {
                        return Err(DecoderError::BmpSignatureInvalid.into());
                    }

                    self.data_offset =
                        u32::from_le_bytes([buffer[10], buffer[11], buffer[12], buffer[13]]) as u64;

                    self.streaming_state = Some(DecoderState::ReadingDibHeaderSize {
                        bytes_read: 0,
                        buffer: Vec::new(),
                    });
                }
                DecoderState::ReadingDibHeaderSize {
                    mut bytes_read,
                    mut buffer,
                } => {
                    const HEADER_SIZE_BYTES: usize = 4;
                    if !Self::try_read_exact_incremental(
                        &mut self.reader,
                        &mut buffer,
                        &mut bytes_read,
                        HEADER_SIZE_BYTES,
                    )? {
                        self.streaming_state =
                            Some(DecoderState::ReadingDibHeaderSize { bytes_read, buffer });
                        return Err(ImageError::InsufficientData(InsufficientDataError::new()));
                    }

                    let bmp_header_size =
                        u32::from_le_bytes([buffer[0], buffer[1], buffer[2], buffer[3]]);

                    self.bmp_header_type = match bmp_header_size {
                        BITMAPCOREHEADER_SIZE => BMPHeaderType::Core,
                        BITMAPINFOHEADER_SIZE => BMPHeaderType::Info,
                        BITMAPV2HEADER_SIZE => BMPHeaderType::V2,
                        BITMAPV3HEADER_SIZE => BMPHeaderType::V3,
                        BITMAPV4HEADER_SIZE => BMPHeaderType::V4,
                        BITMAPV5HEADER_SIZE => BMPHeaderType::V5,
                        _ if bmp_header_size < BITMAPCOREHEADER_SIZE => {
                            return Err(DecoderError::HeaderTooSmall(bmp_header_size).into());
                        }
                        _ => {
                            return Err(ImageError::Unsupported(
                                UnsupportedError::from_format_and_kind(
                                    ImageFormat::Bmp.into(),
                                    UnsupportedErrorKind::GenericFeature(format!(
                                        "Unknown bitmap header type (size={bmp_header_size})"
                                    )),
                                ),
                            ));
                        }
                    };

                    self.streaming_state = Some(DecoderState::ReadingDibHeader {
                        header_size: bmp_header_size,
                        bytes_read: 0, // Will read the rest of the header
                        buffer: Vec::new(),
                    });
                }
                DecoderState::ReadingDibHeader {
                    header_size,
                    mut bytes_read,
                    mut buffer,
                } => {
                    // Read remaining DIB header bytes (header_size - 4, since we already read the size field)
                    let remaining_header_bytes = (header_size - 4) as usize;

                    if !Self::try_read_exact_incremental(
                        &mut self.reader,
                        &mut buffer,
                        &mut bytes_read,
                        remaining_header_bytes,
                    )? {
                        self.streaming_state = Some(DecoderState::ReadingDibHeader {
                            header_size,
                            bytes_read,
                            buffer,
                        });
                        return Err(ImageError::InsufficientData(InsufficientDataError::new()));
                    }

                    // Parse the DIB header using shared parsing logic
                    match self.bmp_header_type {
                        BMPHeaderType::Core => {
                            let parsed = ParsedCoreHeader::parse(buffer[..8].try_into().unwrap())?;

                            self.width = parsed.width;
                            self.height = parsed.height;
                            self.bit_count = parsed.bit_count;
                            self.image_type = parsed.image_type;

                            check_for_overflow(self.width, self.height, self.num_channels())?;
                        }
                        _ => {
                            let parsed = ParsedInfoHeader::parse(buffer[..36].try_into().unwrap())?;

                            self.width = parsed.width;
                            self.height = parsed.height;
                            self.top_down = parsed.top_down;
                            self.bit_count = parsed.bit_count;
                            self.colors_used = parsed.colors_used;
                            self.image_type = Self::image_type_from_compression(
                                parsed.compression,
                                parsed.bit_count,
                                self.add_alpha_channel,
                            )?;

                            check_for_overflow(self.width, self.height, self.num_channels())?;
                        }
                    }

                    // Check if we need to store ICC profile metadata (V5 header)
                    if header_size >= BITMAPV5HEADER_SIZE {
                        if let Some(icc_meta) = ParsedIccProfile::parse(&buffer)? {
                            self.icc_profile_offset = Some(u64::from(icc_meta.profile_offset));
                            self.icc_profile_size = Some(icc_meta.profile_size);
                        }
                    }

                    // Check if we need to read bitmasks
                    if self.image_type == ImageType::Bitfields16
                        || self.image_type == ImageType::Bitfields32
                    {
                        // For V3/V4/V5 headers, bitmasks are embedded in the header at bytes 40-55
                        // For Info headers with BI_BITFIELDS, bitmasks come after the header
                        if matches!(
                            self.bmp_header_type,
                            BMPHeaderType::V3 | BMPHeaderType::V4 | BMPHeaderType::V5
                        ) {
                            // Extract bitmasks from header buffer
                            // Masks are at bytes 40-55 in the full header, but we've already read
                            // the 4-byte size field, so they're at bytes 36-51 in our buffer
                            let parsed = ParsedBitfields::parse(&buffer[36..52])?;
                            parsed.apply_to_decoder(self)?;

                            // No need to read bitmasks separately - go to next state
                            if matches!(
                                self.image_type,
                                ImageType::Palette | ImageType::RLE4 | ImageType::RLE8
                            ) {
                                let num_entries = self.get_palette_size()?;
                                let bytes_per_color = self.bytes_per_color();
                                self.streaming_state = Some(DecoderState::ReadingPalette {
                                    entries_read: 0,
                                    total_entries: num_entries as u32,
                                    bytes_per_color,
                                    buffer: Vec::new(),
                                });
                            } else {
                                // Ready for pixel data
                                let num_channels = self.num_channels();
                                let row_byte_length = (self.width as usize) * num_channels;

                                if matches!(self.image_type, ImageType::RLE4 | ImageType::RLE8) {
                                    self.streaming_state = Some(DecoderState::ReadyForRleData {
                                        current_row: 0,
                                        x_position: 0,
                                        row_buffer: vec![0u8; row_byte_length],
                                        hit_eof: false,
                                    });
                                } else {
                                    self.streaming_state = Some(DecoderState::ReadyForPixelData {
                                        current_row: 0,
                                        row_byte_length,
                                    });
                                }
                            }
                        } else {
                            // For Info headers or V3, need to read bitmasks from after the header
                            self.streaming_state = Some(DecoderState::ReadingBitmasks {
                                bytes_read: 0,
                                buffer: Vec::new(),
                            });
                        }
                    } else if matches!(
                        self.image_type,
                        ImageType::Palette | ImageType::RLE4 | ImageType::RLE8
                    ) {
                        let num_entries = self.get_palette_size()?;
                        let bytes_per_color = self.bytes_per_color();
                        self.streaming_state = Some(DecoderState::ReadingPalette {
                            entries_read: 0,
                            total_entries: num_entries as u32,
                            bytes_per_color,
                            buffer: Vec::new(),
                        });
                    } else {
                        // No palette or bitmasks needed, ready for pixels
                        let num_channels = self.num_channels();
                        let row_byte_length = (self.width as usize) * num_channels;

                        // Check if RLE format
                        if matches!(self.image_type, ImageType::RLE4 | ImageType::RLE8) {
                            self.streaming_state = Some(DecoderState::ReadyForRleData {
                                current_row: 0,
                                x_position: 0,
                                row_buffer: vec![0u8; row_byte_length],
                                hit_eof: false,
                            });
                        } else {
                            self.streaming_state = Some(DecoderState::ReadyForPixelData {
                                current_row: 0,
                                row_byte_length,
                            });
                        }
                    }
                }
                DecoderState::ReadingBitmasks {
                    mut bytes_read,
                    mut buffer,
                } => {
                    // Read bitmasks incrementally from after the header
                    // This state is only used for BITMAPINFOHEADER with BI_BITFIELDS
                    // (V3/V4/V5 bitmasks are extracted directly from the header buffer)
                    let bitmask_size = 12; // 3 masks (R, G, B) for Info

                    if !Self::try_read_exact_incremental(
                        &mut self.reader,
                        &mut buffer,
                        &mut bytes_read,
                        bitmask_size,
                    )? {
                        // Need more data
                        self.streaming_state =
                            Some(DecoderState::ReadingBitmasks { bytes_read, buffer });
                        return Err(ImageError::InsufficientData(InsufficientDataError::new()));
                    }

                    // Parse bitmasks from buffer
                    let parsed = ParsedBitfields::parse(&buffer)?;
                    parsed.apply_to_decoder(self)?;

                    // Check if we need palette after bitmasks
                    if matches!(
                        self.image_type,
                        ImageType::Palette | ImageType::RLE4 | ImageType::RLE8
                    ) {
                        let num_entries = self.get_palette_size()?;
                        let bytes_per_color = self.bytes_per_color();
                        self.streaming_state = Some(DecoderState::ReadingPalette {
                            entries_read: 0,
                            total_entries: num_entries as u32,
                            bytes_per_color,
                            buffer: Vec::new(),
                        });
                    } else {
                        let num_channels = self.num_channels();
                        let row_byte_length = (self.width as usize) * num_channels;

                        // Check if RLE format
                        if matches!(self.image_type, ImageType::RLE4 | ImageType::RLE8) {
                            self.streaming_state = Some(DecoderState::ReadyForRleData {
                                current_row: 0,
                                x_position: 0,
                                row_buffer: vec![0u8; row_byte_length],
                                hit_eof: false,
                            });
                        } else {
                            self.streaming_state = Some(DecoderState::ReadyForPixelData {
                                current_row: 0,
                                row_byte_length,
                            });
                        }
                    }
                }
                DecoderState::ReadingPalette {
                    mut entries_read,
                    total_entries,
                    bytes_per_color,
                    mut buffer,
                } => {
                    // Read palette incrementally
                    const MAX_PALETTE_SIZE: u32 = 256;
                    let actual_entries = total_entries.min(MAX_PALETTE_SIZE);
                    let total_bytes = (actual_entries * bytes_per_color as u32) as usize;

                    // Calculate how many bytes we've already read
                    let mut bytes_read = (entries_read * bytes_per_color as u32) as usize;

                    if !Self::try_read_exact_incremental(
                        &mut self.reader,
                        &mut buffer,
                        &mut bytes_read,
                        total_bytes,
                    )? {
                        // Update entries_read based on bytes_read
                        entries_read = (bytes_read / bytes_per_color) as u32;

                        // Need more data
                        self.streaming_state = Some(DecoderState::ReadingPalette {
                            entries_read,
                            total_entries,
                            bytes_per_color,
                            buffer,
                        });
                        return Err(ImageError::InsufficientData(InsufficientDataError::new()));
                    }

                    // Parse palette from buffer
                    let mut palette = vec_try_with_capacity(MAX_PALETTE_SIZE as usize)?;
                    for i in 0..(actual_entries as usize) {
                        let offset = i * bytes_per_color;
                        let b = buffer[offset];
                        let g = buffer[offset + 1];
                        let r = buffer[offset + 2];
                        palette.push([r, g, b]);
                    }

                    // Pad to 256 entries to prevent out-of-bounds access
                    palette.resize(MAX_PALETTE_SIZE as usize, [0, 0, 0]);
                    self.palette = Some(palette);

                    // If there are more entries than we read, skip them
                    if total_entries > MAX_PALETTE_SIZE {
                        let skip_bytes =
                            ((total_entries - MAX_PALETTE_SIZE) * bytes_per_color as u32) as i64;
                        self.reader.seek(SeekFrom::Current(skip_bytes))?;
                    }

                    let num_channels = self.num_channels();
                    let row_byte_length = (self.width as usize) * num_channels;

                    // Check if RLE format
                    if matches!(self.image_type, ImageType::RLE4 | ImageType::RLE8) {
                        let row_byte_length = (self.width as usize) * num_channels;
                        self.streaming_state = Some(DecoderState::ReadyForRleData {
                            current_row: 0,
                            x_position: 0,
                            row_buffer: vec![0u8; row_byte_length],
                            hit_eof: false,
                        });
                    } else {
                        self.streaming_state = Some(DecoderState::ReadyForPixelData {
                            current_row: 0,
                            row_byte_length,
                        });
                    }
                }
                DecoderState::ReadyForPixelData { .. } => {
                    // Metadata reading complete
                    self.streaming_state = Some(state); // Restore state
                    if self.no_file_header {
                        self.data_offset = self.reader.stream_position()?;
                    }
                    self.has_loaded_metadata = true;
                    return Ok(Some(()));
                }
                DecoderState::ReadyForRleData { .. } => {
                    // Metadata reading complete
                    self.streaming_state = Some(state); // Restore state
                    if self.no_file_header {
                        self.data_offset = self.reader.stream_position()?;
                    }
                    self.has_loaded_metadata = true;
                    return Ok(Some(()));
                }
                DecoderState::ReadingIccProfile {
                    bytes_to_skip,
                    size,
                    mut bytes_read,
                    mut buffer,
                } => {
                    // First, skip bytes from end of pixel data to ICC profile location
                    if bytes_to_skip > 0 {
                        let mut skip_buffer = vec![0u8; bytes_to_skip.min(8192) as usize];
                        let mut remaining = bytes_to_skip;

                        while remaining > 0 {
                            let to_read = remaining.min(skip_buffer.len() as u64) as usize;
                            match self.reader.read_exact(&mut skip_buffer[..to_read]) {
                                Ok(_) => remaining -= to_read as u64,
                                Err(e)
                                    if e.kind() == io::ErrorKind::WouldBlock
                                        || e.kind() == io::ErrorKind::Interrupted
                                        || e.kind() == io::ErrorKind::UnexpectedEof =>
                                {
                                    // Save state and return to try again later
                                    self.streaming_state = Some(DecoderState::ReadingIccProfile {
                                        bytes_to_skip: remaining,
                                        size,
                                        bytes_read,
                                        buffer,
                                    });
                                    return Err(ImageError::InsufficientData(
                                        InsufficientDataError::new(),
                                    ));
                                }
                                Err(e) => return Err(ImageError::IoError(e)),
                            }
                        }
                    }

                    // Now read ICC profile data incrementally
                    if !Self::try_read_exact_incremental(
                        &mut self.reader,
                        &mut buffer,
                        &mut bytes_read,
                        size as usize,
                    )? {
                        // Need more data
                        self.streaming_state = Some(DecoderState::ReadingIccProfile {
                            bytes_to_skip: 0, // Already skipped
                            size,
                            bytes_read,
                            buffer,
                        });
                        return Err(ImageError::InsufficientData(InsufficientDataError::new()));
                    }

                    // Profile fully read, store it
                    self.icc_profile = Some(buffer);

                    // Mark as complete
                    self.streaming_state = Some(DecoderState::Complete);
                    return Ok(Some(()));
                }
                DecoderState::Complete => {
                    // Already complete
                    return Ok(Some(()));
                }
            }
        }
    }

    /// Read a single row of pixel data (streaming mode only).
    ///
    /// The buffer must be exactly `width * num_channels` bytes.
    /// Returns `Ok(Some(&[u8]))` with row data, `Ok(None)` when all rows read,
    /// or `Err(_)` on failure. Call `try_read_metadata()` first.
    pub fn try_read_row<'a>(&mut self, buf: &'a mut [u8]) -> ImageResult<Option<&'a [u8]>> {
        // Check streaming mode
        if self.streaming_state.is_none() {
            return Err(ImageError::Decoding(DecodingError::new(
                ImageFormat::Bmp.into(),
                "try_read_row() requires streaming mode (use new_streaming())",
            )));
        }

        // Check if we're in RLE mode
        if matches!(
            self.streaming_state,
            Some(DecoderState::ReadyForRleData { .. })
        ) {
            return self.read_row_rle(buf);
        }

        // Uncompressed formats - extract state values
        let (current_row, row_byte_length) = if let Some(DecoderState::ReadyForPixelData {
            current_row,
            row_byte_length,
        }) = &self.streaming_state
        {
            (*current_row, *row_byte_length)
        } else {
            if !self.has_loaded_metadata {
                return Err(ImageError::Decoding(DecodingError::new(
                    ImageFormat::Bmp.into(),
                    "Must call try_read_metadata() before try_read_row()",
                )));
            }
            return Ok(None);
        };

        // Validate buffer size
        if buf.len() != row_byte_length {
            return Err(ImageError::Decoding(DecodingError::new(
                ImageFormat::Bmp.into(),
                format!(
                    "Buffer size mismatch: expected {} bytes, got {}",
                    row_byte_length,
                    buf.len()
                ),
            )));
        }

        // Check if we've read all rows
        if current_row >= self.height as u32 {
            // Check if we need to read ICC profile
            if self.check_icc_profile_after_pixels()? {
                // ICC profile will be read when user calls try_read_metadata() again
                return Ok(None);
            }
            // No ICC profile or already past it, mark complete
            self.streaming_state = Some(DecoderState::Complete);
            return Ok(None);
        }

        // In streaming mode, rows are returned in file order (as they appear on disk)
        let physical_row = current_row;

        // Calculate row stride based on format
        let (row_bytes_in_file, row_padding) = match self.image_type {
            ImageType::Palette => {
                let bits_per_row = i32::from(self.bit_count) * self.width;
                let bytes_per_row = ((bits_per_row + 31) / 32 * 4) as usize;
                (bytes_per_row, 0) // Already padded in calculation
            }
            ImageType::RGB16 | ImageType::Bitfields16 => {
                let bytes_per_row = self.width as usize * 2;
                let padding = calculate_row_padding(bytes_per_row);
                (bytes_per_row, padding)
            }
            ImageType::RGB24 => {
                let bytes_per_row = self.width as usize * 3;
                let padding = calculate_row_padding(bytes_per_row);
                (bytes_per_row, padding)
            }
            ImageType::RGB32 | ImageType::RGBA32 | ImageType::Bitfields32 => {
                let bytes_per_row = self.width as usize * 4;
                (bytes_per_row, 0) // 32-bit rows are always aligned
            }
            _ => unreachable!(), // RLE formats already rejected above
        };

        let row_offset =
            self.data_offset + (physical_row as u64 * (row_bytes_in_file + row_padding) as u64);

        self.seek_or_insufficient(SeekFrom::Start(row_offset))?;

        // Read and decode the row data based on format
        match self.image_type {
            ImageType::RGB24 => {
                let num_channels = self.num_channels();
                for pixel in buf.chunks_exact_mut(num_channels) {
                    self.read_exact_or_insufficient(&mut pixel[0..3])?;
                    pixel[0..3].reverse(); // BGR -> RGB
                    if num_channels == 4 {
                        pixel[3] = ALPHA_OPAQUE;
                    }
                }
                // Skip padding
                if row_padding > 0 {
                    let mut padding = vec![0u8; row_padding];
                    self.read_exact_or_insufficient(&mut padding)?;
                }
            }
            ImageType::RGB32 => {
                // Read all 4 bytes atomically to avoid partial read state issues
                let num_channels = self.num_channels();
                let mut temp = [0u8; 4];
                for pixel in buf.chunks_exact_mut(num_channels) {
                    self.read_exact_or_insufficient(&mut temp)?;
                    // Copy BGR and reverse to RGB
                    pixel[0] = temp[2];
                    pixel[1] = temp[1];
                    pixel[2] = temp[0];
                    // temp[3] is the X byte, ignored
                    if num_channels == 4 {
                        pixel[3] = ALPHA_OPAQUE;
                    }
                }
            }
            ImageType::RGBA32 => {
                let num_channels = self.num_channels();
                for pixel in buf.chunks_exact_mut(num_channels) {
                    self.read_exact_or_insufficient(pixel)?;
                    pixel[0..3].reverse(); // BGR -> RGB, keep A
                }
            }
            ImageType::Palette => {
                let mut indices = vec![0u8; row_bytes_in_file];
                self.read_exact_or_insufficient(&mut indices)?;

                let palette = self.palette.as_ref().unwrap();
                let num_channels = self.num_channels();
                let width = self.width as usize;

                if self.indexed_color {
                    // Return raw indices
                    buf[0..width].copy_from_slice(&indices[0..width]);
                } else {
                    // Decode palette indices to RGB(A)
                    let mut pixel_iter = buf.chunks_exact_mut(num_channels);
                    match self.bit_count {
                        1 => set_1bit_pixel_run(&mut pixel_iter, palette, indices.iter()),
                        2 => {
                            if !set_2bit_pixel_run(&mut pixel_iter, palette, indices.iter(), width)
                            {
                                return Err(ImageError::Decoding(DecodingError::new(
                                    ImageFormat::Bmp.into(),
                                    "Corrupt palette data",
                                )));
                            }
                        }
                        4 => {
                            if !set_4bit_pixel_run(&mut pixel_iter, palette, indices.iter(), width)
                            {
                                return Err(ImageError::Decoding(DecodingError::new(
                                    ImageFormat::Bmp.into(),
                                    "Corrupt palette data",
                                )));
                            }
                        }
                        8 => {
                            if !set_8bit_pixel_run(&mut pixel_iter, palette, indices.iter(), width)
                            {
                                return Err(ImageError::Decoding(DecodingError::new(
                                    ImageFormat::Bmp.into(),
                                    "Corrupt palette data",
                                )));
                            }
                        }
                        _ => unreachable!(),
                    }

                    // Set alpha channel if needed
                    if num_channels == 4 {
                        for chunk in buf.chunks_exact_mut(4) {
                            chunk[3] = ALPHA_OPAQUE;
                        }
                    }
                }
            }
            ImageType::RGB16 | ImageType::Bitfields16 => {
                let bitfields = match self.image_type {
                    ImageType::RGB16 => &R5_G5_B5_COLOR_MASK,
                    ImageType::Bitfields16 => self.bitfields.as_ref().unwrap(),
                    _ => unreachable!(),
                };

                let num_channels = self.num_channels();
                for pixel in buf.chunks_exact_mut(num_channels) {
                    let data = match self.reader.read_u16::<LittleEndian>() {
                        Ok(d) => u32::from(d),
                        Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => {
                            return Err(ImageError::InsufficientData(InsufficientDataError::new()))
                        }
                        Err(e) => return Err(ImageError::IoError(e)),
                    };

                    pixel[0] = bitfields.r.read(data);
                    pixel[1] = bitfields.g.read(data);
                    pixel[2] = bitfields.b.read(data);
                    if num_channels == 4 {
                        if bitfields.a.len != 0 {
                            pixel[3] = bitfields.a.read(data);
                        } else {
                            pixel[3] = ALPHA_OPAQUE;
                        }
                    }
                }

                // Skip padding
                if row_padding > 0 {
                    let mut padding = vec![0u8; row_padding];
                    self.read_exact_or_insufficient(&mut padding)?;
                }
            }
            ImageType::Bitfields32 => {
                let bitfields = self.bitfields.unwrap();
                let num_channels = self.num_channels();

                // Apply bitfield masks to extract R, G, B values
                // Note: Cannot use optimized direct read even for standard masks because
                // BMP files store pixels in little-endian BGR order, requiring extraction
                for pixel in buf.chunks_exact_mut(num_channels) {
                    let data = self.read_u32_or_insufficient()?;

                    pixel[0] = bitfields.r.read(data);
                    pixel[1] = bitfields.g.read(data);
                    pixel[2] = bitfields.b.read(data);
                    if num_channels == 4 {
                        if bitfields.a.len != 0 {
                            pixel[3] = bitfields.a.read(data);
                        } else {
                            pixel[3] = ALPHA_OPAQUE;
                        }
                    }
                }
            }
            _ => unreachable!(), // Already validated above
        }

        // Advance to next row
        let next_row = current_row + 1;
        self.streaming_state = Some(DecoderState::ReadyForPixelData {
            current_row: next_row,
            row_byte_length,
        });

        Ok(Some(buf))
    }

    /// Incrementally decode RLE data one row at a time (streaming decoder).
    /// Returns rows in file order (bottom-up for standard BMPs).
    fn read_row_rle<'a>(&mut self, buf: &'a mut [u8]) -> ImageResult<Option<&'a [u8]>> {
        let state = self.streaming_state.take().expect("state was None");

        let DecoderState::ReadyForRleData {
            current_row,
            mut x_position,
            mut row_buffer,
            hit_eof,
        } = state
        else {
            self.streaming_state = Some(state);
            return Ok(None);
        };

        // Seek to data_offset on first row
        if current_row == 0 && x_position == 0 && !hit_eof {
            if let Err(e) = self.seek_or_insufficient(SeekFrom::Start(self.data_offset)) {
                self.streaming_state = Some(DecoderState::ReadyForRleData {
                    current_row,
                    x_position,
                    row_buffer,
                    hit_eof: false,
                });
                return Err(e);
            }
        }

        // Validate buffer size
        let row_size = self.width as usize * self.num_channels();
        if buf.len() != row_size {
            self.streaming_state = Some(DecoderState::ReadyForRleData {
                current_row,
                x_position,
                row_buffer,
                hit_eof,
            });
            return Err(ImageError::Decoding(DecodingError::new(
                ImageFormat::Bmp.into(),
                format!(
                    "Buffer size mismatch: expected {} bytes, got {}",
                    row_size,
                    buf.len()
                ),
            )));
        }

        let total_rows = self.height as u32;

        // If we hit EOF, just pad remaining rows with zeros
        if hit_eof {
            buf.fill(0);
            let next_row = current_row + 1;

            if next_row >= total_rows {
                // All rows done, check for ICC profile
                if self.check_icc_profile_after_pixels()? {
                    return Ok(Some(buf));
                }
                self.streaming_state = Some(DecoderState::Complete);
            } else {
                self.streaming_state = Some(DecoderState::ReadyForRleData {
                    current_row: next_row,
                    x_position: 0,
                    row_buffer,
                    hit_eof: true,
                });
            }

            return Ok(Some(buf));
        }

        let num_channels = self.num_channels();
        let palette = self.palette.clone().unwrap();
        let width = self.width as usize;
        let total_rows = self.height as u32;
        let row_byte_length = width * num_channels;

        // Process RLE instructions until we have a complete row
        loop {
            // Read next instruction
            let control_byte = match self.read_u8_or_insufficient() {
                Ok(b) => b,
                Err(ImageError::InsufficientData(_)) => {
                    // Need more data - save state and return
                    self.streaming_state = Some(DecoderState::ReadyForRleData {
                        current_row,
                        x_position,
                        row_buffer,
                        hit_eof: false,
                    });
                    return Err(ImageError::InsufficientData(InsufficientDataError::new()));
                }
                Err(e) => {
                    self.streaming_state = Some(DecoderState::ReadyForRleData {
                        current_row,
                        x_position,
                        row_buffer,
                        hit_eof: false,
                    });
                    return Err(e);
                }
            };

            if control_byte == RLE_ESCAPE {
                let op = self.read_u8_or_insufficient()?;

                match op {
                    RLE_ESCAPE_EOL => {
                        // End of line - fill rest of row with black and return it
                        let start_offset = (x_position * num_channels).min(row_byte_length);
                        row_buffer[start_offset..row_byte_length].fill(0);
                        return self.complete_rle_row(buf, row_buffer, current_row, 0, false);
                    }
                    RLE_ESCAPE_EOF => {
                        // End of file - fill remaining pixels in current row
                        let start_offset = (x_position * num_channels).min(row_byte_length);
                        row_buffer[start_offset..row_byte_length].fill(0);
                        return self.complete_rle_row(buf, row_buffer, current_row, 0, true);
                    }
                    RLE_ESCAPE_DELTA => {
                        let xdelta = self.read_u8_or_insufficient()? as usize;
                        let ydelta = self.read_u8_or_insufficient()? as usize;

                        // Fill skipped pixels with black
                        if ydelta > 0 {
                            // Fill rest of current row
                            let start_offset = (x_position * num_channels).min(row_byte_length);
                            row_buffer[start_offset..row_byte_length].fill(0);

                            // If we're completing the current row, return it
                            if current_row < total_rows {
                                // Copy current row to output buffer
                                buf.copy_from_slice(&row_buffer);

                                // Skip additional rows (ydelta - 1)
                                // Note: these skipped rows are lost in streaming mode
                                // complete_rle_row will add 1, so we need current_row + ydelta - 1
                                let row_after_skip = current_row + (ydelta as u32) - 1;

                                // Prepare buffer for next row - clear it, then fill first xdelta pixels with black
                                row_buffer.fill(0);
                                // (filling with 0 is already done, just set position)

                                return self.complete_rle_row(
                                    buf,
                                    row_buffer,
                                    row_after_skip,
                                    xdelta,
                                    false,
                                );
                            }
                        } else {
                            // Just x delta - fill with black
                            for _ in 0..xdelta {
                                if x_position < width {
                                    for c in 0..num_channels {
                                        row_buffer[x_position * num_channels + c] = 0;
                                    }
                                    x_position += 1;
                                }
                            }
                        }
                    }
                    _ => {
                        // Absolute mode - read literal pixels
                        let length = op as usize;
                        let mut num_bytes = length;
                        if self.image_type == ImageType::RLE4 {
                            num_bytes = length.div_ceil(2);
                        }
                        // Pad to word boundary
                        num_bytes += num_bytes & 1;

                        let mut indices = vec![0u8; num_bytes];
                        self.reader
                            .read_exact(&mut indices)
                            .map_err(ImageError::IoError)?;

                        // Decode pixels
                        if self.image_type == ImageType::RLE8 {
                            for &idx in &indices[..length] {
                                if x_position >= width {
                                    break;
                                }
                                let pixel = palette[idx as usize];
                                Self::write_palette_pixel(
                                    &mut row_buffer,
                                    x_position,
                                    num_channels,
                                    pixel,
                                );
                                x_position += 1;
                            }
                        } else {
                            // RLE4
                            let mut pixels_decoded = 0;
                            for &byte in &indices {
                                if pixels_decoded >= length {
                                    break;
                                }
                                let idx1 = (byte >> 4) as usize;
                                if x_position < width {
                                    let pixel = palette[idx1];
                                    Self::write_palette_pixel(
                                        &mut row_buffer,
                                        x_position,
                                        num_channels,
                                        pixel,
                                    );
                                    x_position += 1;
                                    pixels_decoded += 1;
                                }
                                if pixels_decoded >= length {
                                    break;
                                }
                                let idx2 = (byte & 0x0F) as usize;
                                if x_position < width {
                                    let pixel = palette[idx2];
                                    Self::write_palette_pixel(
                                        &mut row_buffer,
                                        x_position,
                                        num_channels,
                                        pixel,
                                    );
                                    x_position += 1;
                                    pixels_decoded += 1;
                                }
                            }
                        }
                    }
                }
            } else {
                // Pixel run
                let palette_index = self.read_u8_or_insufficient()?;
                let n_pixels = control_byte as usize;

                if self.image_type == ImageType::RLE8 {
                    let pixel = palette[palette_index as usize];
                    for _ in 0..n_pixels {
                        if x_position >= width {
                            break;
                        }
                        Self::write_palette_pixel(&mut row_buffer, x_position, num_channels, pixel);
                        x_position += 1;
                    }
                } else {
                    // RLE4 - two pixels per run
                    let idx1 = (palette_index >> 4) as usize;
                    let idx2 = (palette_index & 0x0F) as usize;
                    let pixel1 = palette[idx1];
                    let pixel2 = palette[idx2];

                    for i in 0..n_pixels {
                        if x_position >= width {
                            break;
                        }
                        let pixel = if i % 2 == 0 { pixel1 } else { pixel2 };
                        Self::write_palette_pixel(&mut row_buffer, x_position, num_channels, pixel);
                        x_position += 1;
                    }
                }
            }
        }
    }

    /// Returns whether the BMP rows are stored in top-down order.
    ///
    /// Returns `true` for top-down (negative height), `false` for bottom-up (standard).
    pub fn is_top_down(&self) -> bool {
        // In traditional mode (no streaming_state), always return true
        // because images are normalized to top-down while decoded
        if self.streaming_state.is_none() {
            true
        } else {
            self.top_down
        }
    }

    fn read_metadata(&mut self) -> ImageResult<()> {
        if !self.has_loaded_metadata {
            self.read_file_header()?;
            let bmp_header_offset = self.reader.stream_position()?;
            let bmp_header_size = self.reader.read_u32::<LittleEndian>()?;
            let bmp_header_end = bmp_header_offset + u64::from(bmp_header_size);

            self.bmp_header_type = match bmp_header_size {
                BITMAPCOREHEADER_SIZE => BMPHeaderType::Core,
                BITMAPINFOHEADER_SIZE => BMPHeaderType::Info,
                BITMAPV2HEADER_SIZE => BMPHeaderType::V2,
                BITMAPV3HEADER_SIZE => BMPHeaderType::V3,
                BITMAPV4HEADER_SIZE => BMPHeaderType::V4,
                BITMAPV5HEADER_SIZE => BMPHeaderType::V5,
                _ if bmp_header_size < BITMAPCOREHEADER_SIZE => {
                    // Size of any valid header types won't be smaller than core header type.
                    return Err(DecoderError::HeaderTooSmall(bmp_header_size).into());
                }
                _ => {
                    return Err(ImageError::Unsupported(
                        UnsupportedError::from_format_and_kind(
                            ImageFormat::Bmp.into(),
                            UnsupportedErrorKind::GenericFeature(format!(
                                "Unknown bitmap header type (size={bmp_header_size})"
                            )),
                        ),
                    ))
                }
            };

            // Read the entire DIB header into a buffer (same approach as streaming decoder)
            let remaining_header_bytes = (bmp_header_size - 4) as usize;
            let mut buffer = vec![0u8; remaining_header_bytes];
            self.reader.read_exact(&mut buffer)?;

            // Parse header using shared parsing logic (identical to streaming decoder)
            match self.bmp_header_type {
                BMPHeaderType::Core => {
                    let parsed = ParsedCoreHeader::parse(buffer[..8].try_into().unwrap())?;

                    self.width = parsed.width;
                    self.height = parsed.height;
                    self.bit_count = parsed.bit_count;
                    self.image_type = parsed.image_type;

                    check_for_overflow(self.width, self.height, self.num_channels())?;
                }
                _ => {
                    let parsed = ParsedInfoHeader::parse(buffer[..36].try_into().unwrap())?;

                    self.width = parsed.width;
                    self.height = parsed.height;
                    self.top_down = parsed.top_down;
                    self.bit_count = parsed.bit_count;
                    self.colors_used = parsed.colors_used;
                    self.image_type = Self::image_type_from_compression(
                        parsed.compression,
                        parsed.bit_count,
                        self.add_alpha_channel,
                    )?;

                    check_for_overflow(self.width, self.height, self.num_channels())?;
                }
            }

            // Parse ICC profile metadata from V5 header buffer (identical to streaming decoder)
            if bmp_header_size >= BITMAPV5HEADER_SIZE {
                if let Some(icc_meta) = ParsedIccProfile::parse(&buffer)? {
                    // Read the actual ICC profile data
                    self.reader
                        .seek(SeekFrom::Start(u64::from(icc_meta.profile_offset)))?;
                    let mut profile_data = vec![0u8; icc_meta.profile_size as usize];
                    self.reader.read_exact(&mut profile_data)?;
                    self.icc_profile = Some(profile_data);

                    // Seek back to end of header
                    self.reader.seek(SeekFrom::Start(bmp_header_end))?;
                }
            }

            // Parse bitfields (identical to streaming decoder)
            let mut bitmask_bytes_offset = 0;
            if self.image_type == ImageType::Bitfields16
                || self.image_type == ImageType::Bitfields32
            {
                // For V3/V4/V5 headers, bitmasks are embedded in the header buffer
                // For Info headers with BI_BITFIELDS, bitmasks come after the header
                if matches!(
                    self.bmp_header_type,
                    BMPHeaderType::V3 | BMPHeaderType::V4 | BMPHeaderType::V5
                ) {
                    // Extract bitmasks from header buffer (bytes 36-51 after size field)
                    let parsed = ParsedBitfields::parse(&buffer[36..52])?;
                    parsed.apply_to_decoder(self)?;
                } else {
                    // For Info headers, bitmasks are after the header
                    let buffer_size = 12; // RGB masks only for Info header
                    let mut bitmask_buffer = vec![0u8; buffer_size];
                    self.reader.read_exact(&mut bitmask_buffer)?;

                    let parsed = ParsedBitfields::parse(&bitmask_buffer)?;
                    parsed.apply_to_decoder(self)?;

                    // Track that we read extra bytes after the header
                    bitmask_bytes_offset = 12;
                }
            }

            self.reader
                .seek(SeekFrom::Start(bmp_header_end + bitmask_bytes_offset))?;

            match self.image_type {
                ImageType::Palette | ImageType::RLE4 | ImageType::RLE8 => self.read_palette()?,
                _ => {}
            }

            if self.no_file_header {
                // Use the offset of the end of metadata instead of reading a BMP file header.
                self.data_offset = self.reader.stream_position()?;
            }

            self.has_loaded_metadata = true;
        }
        Ok(())
    }

    #[cfg(feature = "ico")]
    #[doc(hidden)]
    pub fn read_metadata_in_ico_format(&mut self) -> ImageResult<()> {
        self.no_file_header = true;
        self.add_alpha_channel = true;
        self.read_metadata()?;

        // The height field in an ICO file is doubled to account for the AND mask
        // (whether or not an AND mask is actually present).
        self.height /= 2;
        Ok(())
    }

    fn get_palette_size(&mut self) -> ImageResult<usize> {
        match self.colors_used {
            0 => Ok(1 << self.bit_count),
            _ => {
                if self.colors_used > 1 << self.bit_count {
                    return Err(DecoderError::PaletteSizeExceeded {
                        colors_used: self.colors_used,
                        bit_count: self.bit_count,
                    }
                    .into());
                }
                Ok(self.colors_used as usize)
            }
        }
    }

    fn bytes_per_color(&self) -> usize {
        match self.bmp_header_type {
            BMPHeaderType::Core => 3,
            _ => 4,
        }
    }

    fn read_palette(&mut self) -> ImageResult<()> {
        const MAX_PALETTE_SIZE: usize = 256; // Palette indices are u8.

        let bytes_per_color = self.bytes_per_color();
        let palette_size = self.get_palette_size()?;
        let max_length = MAX_PALETTE_SIZE * bytes_per_color;

        let length = palette_size * bytes_per_color;
        let mut buf = vec_try_with_capacity(max_length)?;

        // Resize and read the palette entries to the buffer.
        // We limit the buffer to at most 256 colours to avoid any oom issues as
        // 8-bit images can't reference more than 256 indexes anyhow.
        buf.resize(cmp::min(length, max_length), 0);
        self.reader.by_ref().read_exact(&mut buf)?;

        // Allocate 256 entries even if palette_size is smaller, to prevent corrupt files from
        // causing an out-of-bounds array access.
        match length.cmp(&max_length) {
            Ordering::Greater => {
                self.reader
                    .seek(SeekFrom::Current((length - max_length) as i64))?;
            }
            Ordering::Less => buf.resize(max_length, 0),
            Ordering::Equal => (),
        }

        let p: Vec<[u8; 3]> = (0..MAX_PALETTE_SIZE)
            .map(|i| {
                let b = buf[bytes_per_color * i];
                let g = buf[bytes_per_color * i + 1];
                let r = buf[bytes_per_color * i + 2];
                [r, g, b]
            })
            .collect();

        self.palette = Some(p);

        Ok(())
    }

    /// Get the palette that is embedded in the BMP image, if any.
    pub fn get_palette(&self) -> Option<&[[u8; 3]]> {
        self.palette.as_ref().map(|vec| &vec[..])
    }

    fn num_channels(&self) -> usize {
        if self.indexed_color {
            1
        } else if self.add_alpha_channel {
            4
        } else {
            3
        }
    }

    fn get_pixel_data_size(&self) -> u64 {
        // Calculate total size of pixel data including padding
        let bytes_per_pixel = self.num_channels();
        let row_width_bytes = self.width as usize * bytes_per_pixel;
        let row_padding = calculate_row_padding(row_width_bytes);
        let row_stride = row_width_bytes + row_padding;
        (row_stride * self.height as usize) as u64
    }

    /// Helper to check if ICC profile should be read after pixel data is complete.
    /// Returns true if transitioned to ICC reading state, false otherwise.
    fn check_icc_profile_after_pixels(&mut self) -> ImageResult<bool> {
        if let (Some(profile_offset), Some(profile_size)) =
            (self.icc_profile_offset, self.icc_profile_size)
        {
            let pixel_data_size = self.get_pixel_data_size();
            let end_of_pixel_data = self.data_offset + pixel_data_size;
            self.reader
                .seek(SeekFrom::Start(end_of_pixel_data))
                .map_err(ImageError::IoError)?;

            if profile_offset >= end_of_pixel_data {
                let bytes_to_skip = profile_offset - end_of_pixel_data;
                self.streaming_state = Some(DecoderState::ReadingIccProfile {
                    bytes_to_skip,
                    size: profile_size,
                    bytes_read: 0,
                    buffer: Vec::new(),
                });
                return Ok(true);
            }
        }
        Ok(false)
    }

    /// Helper to read exact number of bytes, converting UnexpectedEof/WouldBlock/Interrupted to InsufficientData.
    fn read_exact_or_insufficient(&mut self, buf: &mut [u8]) -> ImageResult<()> {
        match self.reader.read_exact(buf) {
            Ok(_) => Ok(()),
            Err(e)
                if e.kind() == io::ErrorKind::UnexpectedEof
                    || e.kind() == io::ErrorKind::WouldBlock
                    || e.kind() == io::ErrorKind::Interrupted =>
            {
                Err(ImageError::InsufficientData(InsufficientDataError::new()))
            }
            Err(e) => Err(ImageError::IoError(e)),
        }
    }

    /// Helper to seek, converting UnexpectedEof/WouldBlock/Interrupted to InsufficientData.
    fn seek_or_insufficient(&mut self, pos: SeekFrom) -> ImageResult<u64> {
        match self.reader.seek(pos) {
            Ok(n) => Ok(n),
            Err(e)
                if e.kind() == io::ErrorKind::UnexpectedEof
                    || e.kind() == io::ErrorKind::WouldBlock
                    || e.kind() == io::ErrorKind::Interrupted =>
            {
                Err(ImageError::InsufficientData(InsufficientDataError::new()))
            }
            Err(e) => Err(ImageError::IoError(e)),
        }
    }

    /// Helper to read a single byte, converting UnexpectedEof/WouldBlock/Interrupted to InsufficientData.
    fn read_u8_or_insufficient(&mut self) -> ImageResult<u8> {
        match self.reader.read_u8() {
            Ok(b) => Ok(b),
            Err(e)
                if e.kind() == io::ErrorKind::UnexpectedEof
                    || e.kind() == io::ErrorKind::WouldBlock
                    || e.kind() == io::ErrorKind::Interrupted =>
            {
                Err(ImageError::InsufficientData(InsufficientDataError::new()))
            }
            Err(e) => Err(ImageError::IoError(e)),
        }
    }

    /// Helper to read a u32, converting UnexpectedEof/WouldBlock/Interrupted to InsufficientData.
    fn read_u32_or_insufficient(&mut self) -> ImageResult<u32> {
        match self.reader.read_u32::<LittleEndian>() {
            Ok(n) => Ok(n),
            Err(e)
                if e.kind() == io::ErrorKind::UnexpectedEof
                    || e.kind() == io::ErrorKind::WouldBlock
                    || e.kind() == io::ErrorKind::Interrupted =>
            {
                Err(ImageError::InsufficientData(InsufficientDataError::new()))
            }
            Err(e) => Err(ImageError::IoError(e)),
        }
    }

    /// Helper to write a palette pixel to the row buffer.
    #[inline]
    fn write_palette_pixel(
        row_buffer: &mut [u8],
        x_position: usize,
        num_channels: usize,
        pixel: [u8; 3],
    ) {
        let offset = x_position * num_channels;
        row_buffer[offset] = pixel[0];
        row_buffer[offset + 1] = pixel[1];
        row_buffer[offset + 2] = pixel[2];
        if num_channels == 4 {
            row_buffer[offset + 3] = ALPHA_OPAQUE;
        }
    }

    /// Helper to complete an RLE row: copy row data to output buffer,
    /// increment row counter, and update streaming state.
    /// The caller must ensure row_buffer contains the complete row data before calling.
    /// Returns Ok(Some(buf)) if a row was returned, Ok(None) if all rows complete.
    fn complete_rle_row<'a>(
        &mut self,
        buf: &'a mut [u8],
        row_buffer: Vec<u8>,
        current_row: u32,
        next_x_position: usize,
        hit_eof: bool,
    ) -> ImageResult<Option<&'a [u8]>> {
        // Copy complete row data to output buffer
        buf.copy_from_slice(&row_buffer);

        let next_row = current_row + 1;

        // Check if we've decoded all rows
        if next_row >= self.height as u32 {
            // Check for ICC profile
            if self.check_icc_profile_after_pixels()? {
                return Ok(Some(buf));
            }
            self.streaming_state = Some(DecoderState::Complete);
        } else {
            // Setup state for next row - create fresh zeroed buffer
            let width = self.width as usize;
            let num_channels = self.num_channels();
            let new_row_buffer = vec![0u8; width * num_channels];
            self.streaming_state = Some(DecoderState::ReadyForRleData {
                current_row: next_row,
                x_position: next_x_position,
                row_buffer: new_row_buffer,
                hit_eof,
            });
        }
        Ok(Some(buf))
    }

    fn rows<'a>(&self, pixel_data: &'a mut [u8]) -> RowIterator<'a> {
        let stride = self.width as usize * self.num_channels();
        if self.top_down {
            RowIterator {
                chunks: Chunker::FromTop(pixel_data.chunks_exact_mut(stride)),
            }
        } else {
            RowIterator {
                chunks: Chunker::FromBottom(pixel_data.chunks_exact_mut(stride).rev()),
            }
        }
    }

    fn read_palettized_pixel_data(&mut self, buf: &mut [u8]) -> ImageResult<()> {
        let num_channels = self.num_channels();
        let row_byte_length = ((i32::from(self.bit_count) * self.width + 31) / 32 * 4) as usize;
        let mut indices = vec![0; row_byte_length];
        let palette = self.palette.as_ref().unwrap();
        let bit_count = self.bit_count;
        let reader = &mut self.reader;
        let width = self.width as usize;
        let skip_palette = self.indexed_color;

        reader.seek(SeekFrom::Start(self.data_offset))?;

        if num_channels == 4 {
            buf.chunks_exact_mut(4).for_each(|c| c[3] = ALPHA_OPAQUE);
        }

        with_rows(
            buf,
            self.width,
            self.height,
            num_channels,
            self.top_down,
            |row| {
                reader.read_exact(&mut indices)?;
                if skip_palette {
                    row.clone_from_slice(&indices[0..width]);
                } else {
                    let mut pixel_iter = row.chunks_exact_mut(num_channels);
                    match bit_count {
                        1 => {
                            set_1bit_pixel_run(&mut pixel_iter, palette, indices.iter());
                        }
                        2 => {
                            set_2bit_pixel_run(&mut pixel_iter, palette, indices.iter(), width);
                        }
                        4 => {
                            set_4bit_pixel_run(&mut pixel_iter, palette, indices.iter(), width);
                        }
                        8 => {
                            set_8bit_pixel_run(&mut pixel_iter, palette, indices.iter(), width);
                        }
                        _ => panic!(),
                    }
                }
                Ok(())
            },
        )?;

        Ok(())
    }

    fn read_16_bit_pixel_data(
        &mut self,
        buf: &mut [u8],
        bitfields: Option<&Bitfields>,
    ) -> ImageResult<()> {
        let num_channels = self.num_channels();
        let row_padding_len = self.width as usize % 2 * 2;
        let row_padding = &mut [0; 2][..row_padding_len];
        let bitfields = match bitfields {
            Some(b) => b,
            None => self.bitfields.as_ref().unwrap(),
        };
        let reader = &mut self.reader;

        reader.seek(SeekFrom::Start(self.data_offset))?;

        with_rows(
            buf,
            self.width,
            self.height,
            num_channels,
            self.top_down,
            |row| {
                for pixel in row.chunks_mut(num_channels) {
                    let data = u32::from(reader.read_u16::<LittleEndian>()?);

                    pixel[0] = bitfields.r.read(data);
                    pixel[1] = bitfields.g.read(data);
                    pixel[2] = bitfields.b.read(data);
                    if num_channels == 4 {
                        if bitfields.a.len != 0 {
                            pixel[3] = bitfields.a.read(data);
                        } else {
                            pixel[3] = ALPHA_OPAQUE;
                        }
                    }
                }
                reader.read_exact(row_padding)
            },
        )?;

        Ok(())
    }

    /// Read image data from a reader in 32-bit formats that use bitfields.
    fn read_32_bit_pixel_data(&mut self, buf: &mut [u8]) -> ImageResult<()> {
        let num_channels = self.num_channels();

        let bitfields = self.bitfields.as_ref().unwrap();

        let reader = &mut self.reader;
        reader.seek(SeekFrom::Start(self.data_offset))?;

        with_rows(
            buf,
            self.width,
            self.height,
            num_channels,
            self.top_down,
            |row| {
                for pixel in row.chunks_mut(num_channels) {
                    let data = reader.read_u32::<LittleEndian>()?;

                    pixel[0] = bitfields.r.read(data);
                    pixel[1] = bitfields.g.read(data);
                    pixel[2] = bitfields.b.read(data);
                    if num_channels == 4 {
                        if bitfields.a.len != 0 {
                            pixel[3] = bitfields.a.read(data);
                        } else {
                            pixel[3] = ALPHA_OPAQUE;
                        }
                    }
                }
                Ok(())
            },
        )?;

        Ok(())
    }

    /// Read image data from a reader where the colours are stored as 8-bit values (24 or 32-bit).
    fn read_full_byte_pixel_data(
        &mut self,
        buf: &mut [u8],
        format: &FormatFullBytes,
    ) -> ImageResult<()> {
        let num_channels = self.num_channels();
        let row_padding_len = match *format {
            FormatFullBytes::RGB24 => calculate_row_padding(self.width as usize * 3),
            _ => 0,
        };
        let row_padding = &mut [0; 4][..row_padding_len];

        self.reader.seek(SeekFrom::Start(self.data_offset))?;

        let reader = &mut self.reader;

        with_rows(
            buf,
            self.width,
            self.height,
            num_channels,
            self.top_down,
            |row| {
                for pixel in row.chunks_mut(num_channels) {
                    if *format == FormatFullBytes::Format888 {
                        reader.read_u8()?;
                    }

                    // Read the colour values (b, g, r).
                    // Reading 3 bytes and reversing them is significantly faster than reading one
                    // at a time.
                    reader.read_exact(&mut pixel[0..3])?;
                    pixel[0..3].reverse();

                    if *format == FormatFullBytes::RGB32 {
                        reader.read_u8()?;
                    }

                    // Read the alpha channel if present
                    if *format == FormatFullBytes::RGBA32 {
                        reader.read_exact(&mut pixel[3..4])?;
                    } else if num_channels == 4 {
                        pixel[3] = ALPHA_OPAQUE;
                    }
                }
                reader.read_exact(row_padding)
            },
        )?;

        Ok(())
    }

    fn read_rle_data(&mut self, buf: &mut [u8], image_type: ImageType) -> ImageResult<()> {
        // Seek to the start of the actual image data.
        self.reader.seek(SeekFrom::Start(self.data_offset))?;

        let num_channels = self.num_channels();
        let p = self.palette.as_ref().unwrap();

        // Handling deltas in the RLE scheme means that we need to manually
        // iterate through rows and pixels.  Even if we didn't have to handle
        // deltas, we have to ensure that a single runlength doesn't straddle
        // two rows.
        let mut row_iter = self.rows(buf);

        while let Some(row) = row_iter.next() {
            let mut pixel_iter = row.chunks_exact_mut(num_channels);

            let mut x = 0;
            loop {
                let instruction = {
                    let control_byte = self.reader.read_u8()?;
                    match control_byte {
                        RLE_ESCAPE => {
                            let op = self.reader.read_u8()?;

                            match op {
                                RLE_ESCAPE_EOL => RLEInsn::EndOfRow,
                                RLE_ESCAPE_EOF => RLEInsn::EndOfFile,
                                RLE_ESCAPE_DELTA => {
                                    let xdelta = self.reader.read_u8()?;
                                    let ydelta = self.reader.read_u8()?;
                                    RLEInsn::Delta(xdelta, ydelta)
                                }
                                _ => {
                                    let mut length = op as usize;
                                    if self.image_type == ImageType::RLE4 {
                                        length = length.div_ceil(2);
                                    }
                                    length += length & 1;
                                    let mut buffer = Vec::new();
                                    self.reader.read_exact_vec(&mut buffer, length)?;
                                    RLEInsn::Absolute(op, buffer)
                                }
                            }
                        }
                        _ => {
                            let palette_index = self.reader.read_u8()?;
                            RLEInsn::PixelRun(control_byte, palette_index)
                        }
                    }
                };

                match instruction {
                    RLEInsn::EndOfFile => {
                        pixel_iter.for_each(|p| p.fill(0));
                        row_iter.for_each(|r| r.fill(0));
                        return Ok(());
                    }
                    RLEInsn::EndOfRow => {
                        pixel_iter.for_each(|p| p.fill(0));
                        break;
                    }
                    RLEInsn::Delta(x_delta, y_delta) => {
                        // The msdn site on bitmap compression doesn't specify
                        // what happens to the values skipped when encountering
                        // a delta code, however IE and the windows image
                        // preview seems to replace them with black pixels,
                        // so we stick to that.

                        if y_delta > 0 {
                            // Zero out the remainder of the current row.
                            pixel_iter.for_each(|p| p.fill(0));

                            // If any full rows are skipped, zero them out.
                            for _ in 1..y_delta {
                                let row = row_iter.next().ok_or(DecoderError::CorruptRleData)?;
                                row.fill(0);
                            }

                            // Set the pixel iterator to the start of the next row.
                            pixel_iter = row_iter
                                .next()
                                .ok_or(DecoderError::CorruptRleData)?
                                .chunks_exact_mut(num_channels);

                            // Zero out the pixels up to the current point in the row.
                            for _ in 0..x {
                                pixel_iter
                                    .next()
                                    .ok_or(DecoderError::CorruptRleData)?
                                    .fill(0);
                            }
                        }

                        for _ in 0..x_delta {
                            let pixel = pixel_iter.next().ok_or(DecoderError::CorruptRleData)?;
                            pixel.fill(0);
                        }
                        x += x_delta as usize;
                    }
                    RLEInsn::Absolute(length, indices) => {
                        // Absolute mode cannot span rows, so if we run
                        // out of pixels to process, we should stop
                        // processing the image.
                        match image_type {
                            ImageType::RLE8 => {
                                if !set_8bit_pixel_run(
                                    &mut pixel_iter,
                                    p,
                                    indices.iter(),
                                    length as usize,
                                ) {
                                    return Err(DecoderError::CorruptRleData.into());
                                }
                            }
                            ImageType::RLE4 => {
                                if !set_4bit_pixel_run(
                                    &mut pixel_iter,
                                    p,
                                    indices.iter(),
                                    length as usize,
                                ) {
                                    return Err(DecoderError::CorruptRleData.into());
                                }
                            }
                            _ => unreachable!(),
                        }
                        x += length as usize;
                    }
                    RLEInsn::PixelRun(n_pixels, palette_index) => {
                        match image_type {
                            ImageType::RLE8 => {
                                // A pixel run isn't allowed to span rows.
                                // imagemagick produces invalid images where n_pixels exceeds row length,
                                // so we clamp n_pixels to the row length to display them properly:
                                // https://github.com/image-rs/image/issues/2321
                                //
                                // This is like set_8bit_pixel_run() but doesn't fail when `n_pixels` is too large
                                let repeat_pixel: [u8; 3] = p[palette_index as usize];
                                (&mut pixel_iter).take(n_pixels as usize).for_each(|p| {
                                    p[2] = repeat_pixel[2];
                                    p[1] = repeat_pixel[1];
                                    p[0] = repeat_pixel[0];
                                });
                            }
                            ImageType::RLE4 => {
                                if !set_4bit_pixel_run(
                                    &mut pixel_iter,
                                    p,
                                    repeat(&palette_index),
                                    n_pixels as usize,
                                ) {
                                    return Err(DecoderError::CorruptRleData.into());
                                }
                            }
                            _ => unreachable!(),
                        }
                        x += n_pixels as usize;
                    }
                }
            }
        }

        Ok(())
    }

    /// Read the actual data of the image. This function is deliberately not public because it
    /// cannot be called multiple times without seeking back the underlying reader in between.
    pub(crate) fn read_image_data(&mut self, buf: &mut [u8]) -> ImageResult<()> {
        match self.image_type {
            ImageType::Palette => self.read_palettized_pixel_data(buf),
            ImageType::RGB16 => self.read_16_bit_pixel_data(buf, Some(&R5_G5_B5_COLOR_MASK)),
            ImageType::RGB24 => self.read_full_byte_pixel_data(buf, &FormatFullBytes::RGB24),
            ImageType::RGB32 => self.read_full_byte_pixel_data(buf, &FormatFullBytes::RGB32),
            ImageType::RGBA32 => self.read_full_byte_pixel_data(buf, &FormatFullBytes::RGBA32),
            ImageType::RLE8 => self.read_rle_data(buf, ImageType::RLE8),
            ImageType::RLE4 => self.read_rle_data(buf, ImageType::RLE4),
            ImageType::Bitfields16 => match self.bitfields {
                Some(_) => self.read_16_bit_pixel_data(buf, None),
                None => Err(DecoderError::BitfieldMasksMissing(16).into()),
            },
            ImageType::Bitfields32 => match self.bitfields {
                Some(R8_G8_B8_COLOR_MASK) => {
                    self.read_full_byte_pixel_data(buf, &FormatFullBytes::Format888)
                }
                Some(R8_G8_B8_A8_COLOR_MASK) => {
                    self.read_full_byte_pixel_data(buf, &FormatFullBytes::RGBA32)
                }
                Some(_) => self.read_32_bit_pixel_data(buf),
                None => Err(DecoderError::BitfieldMasksMissing(32).into()),
            },
        }
    }
}

impl<R: BufRead + Seek> ImageDecoder for BmpDecoder<R> {
    fn dimensions(&self) -> (u32, u32) {
        (self.width as u32, self.height as u32)
    }

    fn color_type(&self) -> ColorType {
        if self.indexed_color {
            ColorType::L8
        } else if self.add_alpha_channel {
            ColorType::Rgba8
        } else {
            ColorType::Rgb8
        }
    }

    fn icc_profile(&mut self) -> ImageResult<Option<Vec<u8>>> {
        Ok(self.icc_profile.clone())
    }

    fn read_image(mut self, buf: &mut [u8]) -> ImageResult<()> {
        assert_eq!(u64::try_from(buf.len()), Ok(self.total_bytes()));
        self.read_image_data(buf)
    }

    fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()> {
        (*self).read_image(buf)
    }
}

#[cfg(test)]
mod test {
    use std::io::{BufReader, Cursor, Read, Seek, SeekFrom};

    use super::*;

    #[test]
    fn test_bitfield_len() {
        for len in 1..9 {
            let bitfield = Bitfield { shift: 0, len };
            for i in 0..(1 << len) {
                let read = bitfield.read(i);
                let calc = (f64::from(i) / f64::from((1 << len) - 1) * 255f64).round() as u8;
                if read != calc {
                    println!("len:{len} i:{i} read:{read} calc:{calc}");
                }
                assert_eq!(read, calc);
            }
        }
    }

    #[test]
    fn read_rle_too_short() {
        let data = vec![
            0x42, 0x4d, 0x04, 0xee, 0xfe, 0xff, 0xff, 0x10, 0xff, 0x00, 0x04, 0x00, 0x00, 0x00,
            0x7c, 0x00, 0x00, 0x00, 0x0c, 0x41, 0x00, 0x00, 0x07, 0x10, 0x00, 0x00, 0x01, 0x00,
            0x04, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0d, 0x00, 0x00, 0x00,
            0x00, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff, 0xff, 0xff, 0xfe, 0x21,
            0xff, 0x00, 0x66, 0x61, 0x72, 0x62, 0x66, 0x65, 0x6c, 0x64, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0xff, 0xd8, 0xff, 0x00, 0x00, 0x19, 0x51, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfa, 0xff, 0x00, 0x00, 0x00,
            0x00, 0x01, 0x00, 0x11, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0f, 0x00,
            0x00, 0x00, 0x00, 0x2d, 0x31, 0x31, 0x35, 0x36, 0x00, 0xff, 0x00, 0x00, 0x52, 0x3a,
            0x37, 0x30, 0x7e, 0x71, 0x63, 0x91, 0x5a, 0x04, 0x00, 0x10, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x2d, 0x35, 0x37, 0x00, 0xff, 0x00, 0x00, 0x52,
            0x3a, 0x37, 0x30, 0x7e, 0x71, 0x63, 0x91, 0x5a, 0x04, 0x05, 0x3c, 0x00, 0x00, 0x11,
            0x00, 0x5d, 0x7a, 0x82, 0xb7, 0xca, 0x2d, 0x31, 0xff, 0xff, 0xc7, 0x95, 0x33, 0x2e,
            0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x7c, 0x00,
            0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20, 0x66, 0x00, 0x4d,
            0x4d, 0x00, 0x2a, 0x00,
        ];

        let decoder = BmpDecoder::new(Cursor::new(&data)).unwrap();
        let mut buf = vec![0; usize::try_from(decoder.total_bytes()).unwrap()];
        assert!(decoder.read_image(&mut buf).is_ok());
    }

    #[test]
    fn test_no_header() {
        let tests = [
            "Info_R8_G8_B8.bmp",
            "Info_A8_R8_G8_B8.bmp",
            "Info_8_Bit.bmp",
            "Info_4_Bit.bmp",
            "Info_1_Bit.bmp",
        ];

        for name in &tests {
            let path = format!("tests/images/bmp/images/{name}");
            let ref_img = crate::open(&path).unwrap();
            let mut data = std::fs::read(&path).unwrap();
            // skip the BITMAPFILEHEADER
            let slice = &mut data[14..];
            let decoder = BmpDecoder::new_without_file_header(Cursor::new(slice)).unwrap();
            let no_hdr_img = crate::DynamicImage::from_decoder(decoder).unwrap();
            assert_eq!(ref_img, no_hdr_img);
        }
    }

    #[test]
    fn test_icc_profile() {
        // V5 header file without embedded ICC profile
        let f =
            BufReader::new(std::fs::File::open("tests/images/bmp/images/V5_24_Bit.bmp").unwrap());
        let mut decoder = BmpDecoder::new(f).unwrap();
        let profile = decoder.icc_profile().unwrap();
        assert!(profile.is_none());

        // Test files with embedded ICC profiles
        let f =
            BufReader::new(std::fs::File::open("tests/images/bmp/images/rgb24prof.bmp").unwrap());
        let mut decoder = BmpDecoder::new(f).unwrap();
        let profile = decoder.icc_profile().unwrap();
        assert!(profile.is_some());
        assert_eq!(profile.unwrap().len(), 3048);

        let f =
            BufReader::new(std::fs::File::open("tests/images/bmp/images/rgb24prof2.bmp").unwrap());
        let mut decoder = BmpDecoder::new(f).unwrap();
        let profile = decoder.icc_profile().unwrap();
        assert!(profile.is_some());
        assert_eq!(profile.unwrap().len(), 540);
    }

    #[test]
    fn test_streaming_vs_traditional() {
        let test_files = [
            ("Info_R8_G8_B8.bmp", "RGB24"),
            ("Info_R8_G8_B8_Top_Down.bmp", "RGB24 top-down"),
            ("Info_A8_R8_G8_B8.bmp", "RGBA32"),
            ("Info_A8_R8_G8_B8_Top_Down.bmp", "RGBA32 top-down"),
            ("Info_8_Bit.bmp", "Palette 8-bit"),
            ("Info_8_Bit_Top_Down.bmp", "Palette 8-bit top-down"),
            ("Info_4_Bit.bmp", "Palette 4-bit"),
            ("Info_4_Bit_Top_Down.bmp", "Palette 4-bit top-down"),
            ("Info_1_Bit.bmp", "Palette 1-bit"),
            ("Info_1_Bit_Top_Down.bmp", "Palette 1-bit top-down"),
            ("Core_1_Bit.bmp", "Core header 1-bit"),
            ("Core_4_Bit.bmp", "Core header 4-bit"),
            ("Core_8_Bit.bmp", "Core header 8-bit"),
            ("Info_X1_R5_G5_B5.bmp", "16-bit RGB"),
            ("Info_X1_R5_G5_B5_Top_Down.bmp", "16-bit RGB top-down"),
            ("pal8rle.bmp", "RLE8 compressed"),
            ("pal4rle.bmp", "RLE4 compressed"),
            ("pal4rlecut.bmp", "RLE4 early termination"),
            ("pal4rletrns.bmp", "RLE4 delta/transparency"),
            ("V3_R5_G6_B5.bmp", "V3 16-bit RGB565"),
            ("V3_R5_G6_B5_Top_Down.bmp", "V3 16-bit RGB565 top-down"),
            ("V3_A1_R5_G5_B5.bmp", "V3 16-bit ARGB1555"),
            ("V3_A4_R4_G4_B4.bmp", "V3 16-bit ARGB4444"),
            ("V3_X8_R8_G8_B8.bmp", "V3 32-bit XRGB"),
            ("V4_24_Bit.bmp", "V4 24-bit RGB"),
            ("V4_R5_G6_B5.bmp", "V4 16-bit RGB565 bitfields"),
            ("V4_A8_R8_G8_B8.bmp", "V4 32-bit ARGB bitfields"),
            ("V5_24_Bit.bmp", "V5 24-bit RGB"),
            ("V5_A1_R5_G5_B5.bmp", "V5 16-bit ARGB1555 bitfields"),
        ];

        for (file, format_desc) in &test_files {
            let path = format!("tests/images/bmp/images/{file}");

            // Traditional decode
            let f = BufReader::new(std::fs::File::open(&path).unwrap());
            let mut traditional = BmpDecoder::new(f).unwrap_or_else(|e| {
                panic!("Traditional decoder failed for {}: {:?}", file, e);
            });
            let trad_width = traditional.dimensions().0;
            let trad_height = traditional.dimensions().1;
            let trad_color_type = traditional.color_type();
            let trad_total_bytes = traditional.total_bytes();
            let mut traditional_bytes = vec![0u8; trad_total_bytes as usize];
            traditional.read_image_data(&mut traditional_bytes).unwrap();

            // Streaming decode
            let f = BufReader::new(std::fs::File::open(&path).unwrap());
            let mut streaming = BmpDecoder::new_streaming(f).unwrap();
            while streaming.try_read_metadata().unwrap().is_none() {}

            let stream_width = streaming.dimensions().0;
            let stream_height = streaming.dimensions().1;
            let stream_color_type = streaming.color_type();
            let stream_total_bytes = streaming.total_bytes();

            // Validate all metadata matches
            assert_eq!(
                trad_width, stream_width,
                "Width mismatch for {format_desc}: {file}"
            );
            assert_eq!(
                trad_height, stream_height,
                "Height mismatch for {format_desc}: {file}"
            );
            assert_eq!(
                trad_color_type, stream_color_type,
                "Color type mismatch for {format_desc}: {file}"
            );
            assert_eq!(
                trad_total_bytes, stream_total_bytes,
                "Total bytes mismatch for {format_desc}: {file}"
            );
            // Note: is_top_down() differs between decoders by design:
            // - Traditional decoder always returns true (normalizes to top-down)
            // - Streaming decoder returns actual file orientation

            // Decode streaming rows
            let row_size = stream_width as usize * streaming.num_channels();
            let mut streaming_rows = Vec::new();
            let mut row_buffer = vec![0u8; row_size];
            while let Some(row_data) = streaming.try_read_row(&mut row_buffer).unwrap() {
                streaming_rows.push(row_data.to_vec());
            }

            assert_eq!(
                streaming_rows.len(),
                stream_height as usize,
                "Row count mismatch for {format_desc}: {file}"
            );

            // Compare pixel data row by row
            let stream_is_top_down = streaming.is_top_down();
            for row_idx in 0..stream_height as usize {
                let trad_row_start = row_idx * row_size;
                let trad_row_end = trad_row_start + row_size;
                let trad_row = &traditional_bytes[trad_row_start..trad_row_end];

                // Map streaming file-order row to traditional top-down row
                let stream_row_idx = if stream_is_top_down {
                    row_idx // Top-down file: file order = top-down
                } else {
                    (stream_height as usize - 1) - row_idx // Bottom-up file: flip to compare with traditional's top-down
                };
                let stream_row = &streaming_rows[stream_row_idx];

                assert_eq!(
                    trad_row,
                    &stream_row[..],
                    "Row {row_idx} pixel data mismatch for {format_desc}: {file}"
                );
            }
        }
    }

    #[test]
    fn test_read_row_requires_metadata() {
        let f = BufReader::new(
            std::fs::File::open("tests/images/bmp/images/Info_R8_G8_B8.bmp").unwrap(),
        );
        let mut decoder = BmpDecoder::new_streaming(f).unwrap();

        // Try to read row before loading metadata
        let mut buf = vec![0u8; 100];
        let result = decoder.try_read_row(&mut buf);
        assert!(result.is_err());
    }

    #[test]
    fn test_read_row_buffer_size_validation() {
        let f = BufReader::new(
            std::fs::File::open("tests/images/bmp/images/Info_R8_G8_B8.bmp").unwrap(),
        );
        let mut decoder = BmpDecoder::new_streaming(f).unwrap();
        decoder.try_read_metadata().unwrap();

        // Wrong buffer size
        let mut wrong_buf = vec![0u8; 10]; // Too small
        let result = decoder.try_read_row(&mut wrong_buf);
        assert!(result.is_err());
    }

    #[test]
    fn test_streaming_mode_required() {
        // Create decoder in traditional mode
        let f = BufReader::new(
            std::fs::File::open("tests/images/bmp/images/Info_R8_G8_B8.bmp").unwrap(),
        );
        let mut decoder = BmpDecoder::new(f).unwrap();

        // try_read_metadata should fail
        let result = decoder.try_read_metadata();
        assert!(result.is_err());
    }

    #[test]
    fn test_is_top_down() {
        // Standard bottom-up BMP file
        let f = BufReader::new(
            std::fs::File::open("tests/images/bmp/images/Info_R8_G8_B8.bmp").unwrap(),
        );

        // Decoding with streaming decoder should return file value.
        let mut decoder = BmpDecoder::new_streaming(f).unwrap();
        decoder.try_read_metadata().unwrap();

        let is_top_down = decoder.is_top_down();
        assert!(!is_top_down);

        // Standard decoder should return always top down.
        let mut traditional_decoder = BmpDecoder::new(BufReader::new(
            std::fs::File::open("tests/images/bmp/images/Info_R8_G8_B8.bmp").unwrap(),
        ))
        .unwrap();
        traditional_decoder.read_metadata().unwrap();
        let is_top_down_traditional = traditional_decoder.is_top_down();
        assert!(is_top_down_traditional);
    }

    #[test]
    fn test_streaming_icc_profile() {
        use std::io::Cursor;

        // Test file with embedded ICC profile
        let data = std::fs::read("tests/images/bmp/images/rgb24prof.bmp").unwrap();
        let mut decoder = BmpDecoder::new_streaming(Cursor::new(data)).unwrap();

        // Load metadata
        while decoder.try_read_metadata().unwrap().is_none() {}

        // Read all pixel rows
        let (width, height) = decoder.dimensions();
        let num_channels = 3;
        let mut row_buf = vec![0u8; width as usize * num_channels];

        for i in 0..height {
            let result = decoder.try_read_row(&mut row_buf).unwrap();
            assert!(result.is_some(), "Expected row data for row {}", i);
        }

        // After all rows are read, should transition to ReadingIccProfile state
        let result = decoder.try_read_row(&mut row_buf).unwrap();
        assert!(result.is_none(), "No more rows expected");

        // Now read the ICC profile by calling try_read_metadata again
        while decoder.try_read_metadata().unwrap().is_none() {}

        // Check that ICC profile is available
        let profile = decoder.icc_profile().unwrap();
        assert!(profile.is_some(), "Expected ICC profile");
        assert_eq!(
            profile.unwrap().len(),
            3048,
            "Expected 3048 byte ICC profile"
        );
    }

    /// Mock reader supporting both incomplete data (EOF) and async WouldBlock scenarios
    struct MockStreamingReader {
        data: Vec<u8>,
        position: usize,
        // Controls incomplete data simulation
        available_bytes: Option<usize>,
        // Controls WouldBlock simulation
        block_after_operations: Option<usize>,
        operations_count: usize,
    }

    impl MockStreamingReader {
        fn new_incomplete(data: Vec<u8>, available_bytes: usize) -> Self {
            Self {
                data,
                position: 0,
                available_bytes: Some(available_bytes),
                block_after_operations: None,
                operations_count: 0,
            }
        }

        fn new_would_block(data: Vec<u8>, block_after: usize) -> Self {
            Self {
                data,
                position: 0,
                available_bytes: None,
                block_after_operations: Some(block_after),
                operations_count: 0,
            }
        }

        fn make_bytes_available(&mut self, total_bytes: usize) {
            self.available_bytes = Some(total_bytes);
        }

        fn set_block_after(&mut self, operations: usize) {
            self.block_after_operations = Some(operations);
        }
    }

    impl Read for MockStreamingReader {
        fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
            // Check WouldBlock condition
            if let Some(block_after) = self.block_after_operations {
                self.operations_count += 1;
                if self.operations_count > block_after {
                    return Err(io::Error::new(io::ErrorKind::WouldBlock, "would block"));
                }
            }

            // Check incomplete data condition
            let max_available = if let Some(available) = self.available_bytes {
                available.saturating_sub(self.position)
            } else {
                self.data.len() - self.position
            };

            if max_available == 0 {
                return Ok(0); // Simulate EOF/need more data
            }

            let to_read = buf
                .len()
                .min(max_available)
                .min(self.data.len() - self.position);
            if to_read == 0 {
                return Ok(0);
            }

            buf[..to_read].copy_from_slice(&self.data[self.position..self.position + to_read]);
            self.position += to_read;
            Ok(to_read)
        }
    }

    impl BufRead for MockStreamingReader {
        fn fill_buf(&mut self) -> io::Result<&[u8]> {
            let max_available = if let Some(available) = self.available_bytes {
                available.saturating_sub(self.position)
            } else {
                self.data.len() - self.position
            };
            let end = (self.position + max_available).min(self.data.len());
            Ok(&self.data[self.position..end])
        }

        fn consume(&mut self, amt: usize) {
            self.position += amt;
        }
    }

    impl Seek for MockStreamingReader {
        fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
            // Check WouldBlock condition for seek operations
            if let Some(block_after) = self.block_after_operations {
                self.operations_count += 1;
                if self.operations_count > block_after {
                    return Err(io::Error::new(io::ErrorKind::WouldBlock, "would block"));
                }
            }

            let new_pos = match pos {
                SeekFrom::Start(n) => n as i64,
                SeekFrom::Current(n) => self.position as i64 + n,
                SeekFrom::End(n) => self.data.len() as i64 + n,
            };

            if new_pos < 0 {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "Invalid seek position",
                ));
            }

            self.position = new_pos as usize;
            Ok(self.position as u64)
        }
    }

    #[test]
    fn test_streaming_data_incrementally_received() {
        // Test that decoder handles incomplete data streams correctly (e.g., network streaming)
        let full_data = std::fs::read("tests/images/bmp/images/Info_R8_G8_B8.bmp").unwrap();

        // Start with only first 50 bytes available
        let reader = MockStreamingReader::new_incomplete(full_data.clone(), 50);
        let mut decoder = BmpDecoder::new_streaming(reader).unwrap();

        // Should get InsufficientData with incomplete stream
        match decoder.try_read_metadata() {
            Err(ImageError::InsufficientData(_)) => {} // Expected
            other => panic!(
                "Expected InsufficientData with incomplete data, got: {:?}",
                other
            ),
        }

        // Make more data available (150 bytes total)
        decoder.reader.make_bytes_available(150);
        let _ = decoder.try_read_metadata(); // May still be incomplete

        // Make all data available
        decoder.reader.make_bytes_available(full_data.len());

        // Should complete now
        loop {
            match decoder.try_read_metadata() {
                Ok(Some(())) => break,
                Err(ImageError::InsufficientData(_)) => continue,
                other => panic!("Unexpected result: {:?}", other),
            }
        }

        // Verify metadata and pixel reading
        let (width, height) = decoder.dimensions();
        assert!(width > 0 && height > 0, "Should have valid dimensions");

        let mut row_buf = vec![0u8; width as usize * 3];
        assert!(
            decoder.try_read_row(&mut row_buf).is_ok(),
            "Should read first row"
        );
    }

    #[test]
    fn test_async_error_handling() {
        // Test that WouldBlock/Interrupted errors are properly converted to InsufficientData
        // This ensures compatibility with async/coroutine contexts per maintainer feedback
        let data = std::fs::read("tests/images/bmp/images/Info_R8_G8_B8.bmp").unwrap();

        // Test 1: WouldBlock during metadata reading
        let reader = MockStreamingReader::new_would_block(data.clone(), 2);
        let mut decoder = BmpDecoder::new_streaming(reader).unwrap();

        match decoder.try_read_metadata() {
            Err(ImageError::InsufficientData(_)) => {} // Expected - WouldBlock → InsufficientData
            other => panic!("Expected InsufficientData for WouldBlock, got: {:?}", other),
        }

        // Test 2: WouldBlock during pixel reading (seek operation)
        let reader = MockStreamingReader::new_would_block(data.clone(), 1000);
        let mut decoder = BmpDecoder::new_streaming(reader).unwrap();

        // Read metadata successfully
        loop {
            match decoder.try_read_metadata() {
                Ok(Some(())) => break,
                Ok(None) => continue,
                Err(ImageError::InsufficientData(_)) => {
                    decoder
                        .reader
                        .set_block_after(decoder.reader.operations_count + 10);
                    continue;
                }
                Err(e) => panic!("Unexpected error: {:?}", e),
            }
        }

        // Trigger WouldBlock on next seek
        decoder
            .reader
            .set_block_after(decoder.reader.operations_count + 1);
        let (width, _) = decoder.dimensions();
        let mut row_buf = vec![0u8; width as usize * 3];

        match decoder.try_read_row(&mut row_buf) {
            Err(ImageError::InsufficientData(_)) => {} // Expected - seek WouldBlock → InsufficientData
            other => panic!(
                "Expected InsufficientData for seek WouldBlock, got: {:?}",
                other
            ),
        }

        // Test 3: Verify resumption after WouldBlock
        decoder.reader.set_block_after(usize::MAX);
        match decoder.try_read_row(&mut row_buf) {
            Ok(Some(_)) => {} // Success - decoder resumed correctly
            other => panic!("Expected successful resumption, got: {:?}", other),
        }
    }
}
