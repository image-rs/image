use crate::utils::vec_try_with_capacity;
use std::cmp::{self, Ordering};
use std::io::{self, BufRead, Seek, SeekFrom};
use std::iter::{repeat, Rev};
use std::slice::ChunksExactMut;
use std::{error, fmt};

use crate::color::ColorType;
use crate::error::{
    DecodingError, ImageError, ImageResult, UnsupportedError, UnsupportedErrorKind,
};
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

/// Opaque alpha channel value (fully opaque)
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

/// Parsed bitfield masks from DIB header.
struct ParsedBitfields {
    r_mask: u32,
    g_mask: u32,
    b_mask: u32,
    a_mask: u32,
}

impl ParsedBitfields {
    /// Parse bitfield masks from buffer.
    /// Caller must ensure buffer has sufficient length; this method does not validate.
    /// Note: Caller must ensure buffer has 12 (V2/Core) or 16 (V3/V4/V5) bytes length; this method does not validate.
    #[track_caller]
    fn parse(buffer: &[u8], has_alpha: bool) -> Self {
        let r_mask = u32::from_le_bytes(buffer[0..4].try_into().unwrap());
        let g_mask = u32::from_le_bytes(buffer[4..8].try_into().unwrap());
        let b_mask = u32::from_le_bytes(buffer[8..12].try_into().unwrap());
        let a_mask = if has_alpha {
            u32::from_le_bytes(buffer[12..16].try_into().unwrap())
        } else {
            0
        };

        ParsedBitfields {
            r_mask,
            g_mask,
            b_mask,
            a_mask,
        }
    }
}

/// Parsed ICC profile metadata from V5 header.
struct ParsedIccProfile {
    profile_offset: u32,
    profile_size: u32,
}

impl ParsedIccProfile {
    /// Parse ICC profile metadata from V5 header buffer.
    /// Returns None if no embedded ICC profile is present.
    /// Note: Caller must ensure buffer has 116 bytes length; this method does not validate.
    #[track_caller]
    fn parse(buffer: &[u8]) -> Option<Self> {
        // bV5CSType is at offset 56 from header start, which is offset 52 from after the size field
        let cs_type = u32::from_le_bytes(buffer[52..56].try_into().unwrap());

        // Only embedded profiles are supported
        if cs_type != PROFILE_EMBEDDED {
            return None;
        }

        // bV5ProfileData is at offset 112 from header start, which is offset 108 from after size field
        let profile_offset = u32::from_le_bytes(buffer[108..112].try_into().unwrap());

        // bV5ProfileSize is at offset 116 from header start, which is offset 112 from after size field
        let profile_size = u32::from_le_bytes(buffer[112..116].try_into().unwrap());

        if profile_size == 0 || profile_offset == 0 {
            return None;
        }

        Some(ParsedIccProfile {
            profile_offset,
            profile_size,
        })
    }
}

#[derive(PartialEq, Copy, Clone)]
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

/// Progress within the metadata reading phase.
///
/// The metadata is split into phases:
/// 1. Headers: File header, DIB header, and bitmasks (~30-150 bytes total).
///    These are always re-read together on retry since they're small.
/// 2. Optional data: Palette (up to 1KB) and ICC profile (variable, can be several KB).
///    These are tracked separately since they can be larger.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
enum MetadataProgress {
    /// Initial state, nothing read yet.
    #[default]
    NotStarted,
    /// Headers have been read; now reading palette (if needed).
    /// Stores the ICC profile metadata (offset, size) for use after palette is read.
    ReadingPalette { icc_meta: Option<(u32, u32)> },
    /// Headers and palette (if any) have been read; now reading ICC profile (if needed).
    /// Stores the ICC profile size for the read operation.
    ReadingIccProfile { size: u32 },
    /// All metadata has been read successfully.
    Complete,
}

/// Decoder state for resumable decoding.
///
/// This allows the decoder to recover from `UnexpectedEof` errors.
/// For row-based formats, decoding can resume from the last successfully
/// decoded row. For RLE formats, decoding restarts from the beginning.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DecoderState {
    /// Currently reading metadata (headers, palette, ICC profile).
    ReadingMetadata { progress: MetadataProgress },
    /// Currently reading row-based (non-RLE) image data.
    /// Stores the number of rows successfully decoded.
    ReadingRowData { rows_decoded: u32 },
    /// Currently reading RLE-compressed data.
    /// RLE cannot be resumed at row boundaries due to its sequential nature,
    /// so we restart from the beginning on UnexpectedEof.
    ReadingRleData,
    /// Image data has been fully decoded.
    ImageDecoded,
}

impl Default for DecoderState {
    fn default() -> Self {
        DecoderState::ReadingMetadata {
            progress: MetadataProgress::default(),
        }
    }
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

/// BMP rows must be padded to a multiple of 4 bytes.
#[inline]
fn calculate_row_padding(bytes_per_row: usize) -> usize {
    (4 - (bytes_per_row % 4)) % 4
}

/// Allocate a row buffer with OOM protection.
fn allocate_row_buffer(size: usize) -> ImageResult<Vec<u8>> {
    let mut buffer = vec_try_with_capacity(size).map_err(|_| {
        ImageError::Unsupported(UnsupportedError::from_format_and_kind(
            ImageFormat::Bmp.into(),
            UnsupportedErrorKind::GenericFeature(format!(
                "Row buffer allocation ({} bytes) too large",
                size
            )),
        ))
    })?;
    buffer.resize(size, 0);
    Ok(buffer)
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

/// Process rows with resumability support.
///
/// Calls `func` for each row from `start_row` to `height`, passing the output row slice.
/// On success, returns the total number of rows (height).
/// On error, returns the number of rows successfully completed before the error.
///
/// The caller is responsible for seeking to the correct file position before calling.
fn with_rows_resumable<F>(
    buffer: &mut [u8],
    width: i32,
    height: i32,
    channels: usize,
    top_down: bool,
    start_row: u32,
    mut func: F,
) -> Result<u32, (u32, io::Error)>
where
    F: FnMut(&mut [u8]) -> io::Result<()>,
{
    // An overflow should already have been checked for when this is called,
    // though we check anyhow, as it somehow seems to increase performance slightly.
    let row_width = channels.checked_mul(width as usize).unwrap();
    let height = height as u32;

    /// Get the index of a row in the output buffer given the file row index.
    /// For top-down images, row 0 in the file is row 0 in the buffer.
    /// For bottom-up images, row 0 in the file is the last row in the buffer.
    #[inline]
    fn output_row_index(file_row: u32, height: u32, top_down: bool) -> usize {
        if top_down {
            file_row as usize
        } else {
            (height - 1 - file_row) as usize
        }
    }

    /// Get a mutable reference to a specific row in the output buffer.
    #[inline]
    fn get_row_mut(buf: &mut [u8], row_index: usize, row_stride: usize) -> &mut [u8] {
        let start = row_index * row_stride;
        &mut buf[start..][..row_stride]
    }

    for file_row in start_row..height {
        let out_row_idx = output_row_index(file_row, height, top_down);
        let row = get_row_mut(buffer, out_row_idx, row_width);

        if let Err(e) = func(row) {
            return Err((file_row, e));
        }
    }
    Ok(height)
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

#[derive(PartialEq, Eq)]
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

#[derive(PartialEq, Eq)]
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

/// Helper to read RLE data using the already-buffered reader.
/// Avoids double-buffering since BmpDecoder already requires BufRead.
struct RleReader<'a, R> {
    reader: &'a mut R,
}

impl<'a, R: BufRead> RleReader<'a, R> {
    fn new(reader: &'a mut R) -> Self {
        Self { reader }
    }

    fn read_byte(&mut self) -> io::Result<u8> {
        let buf = self.reader.fill_buf()?;
        if buf.is_empty() {
            return Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "unexpected end of RLE data",
            ));
        }
        let byte = buf[0];
        self.reader.consume(1);
        Ok(byte)
    }

    fn read_exact(&mut self, buf: &mut [u8]) -> io::Result<()> {
        let mut remaining = buf.len();
        let mut offset = 0;

        while remaining > 0 {
            let available = self.reader.fill_buf()?;
            if available.is_empty() {
                return Err(io::Error::new(
                    io::ErrorKind::UnexpectedEof,
                    "unexpected end of RLE data",
                ));
            }

            let to_read = remaining.min(available.len());
            buf[offset..offset + to_read].copy_from_slice(&available[..to_read]);
            self.reader.consume(to_read);
            offset += to_read;
            remaining -= to_read;
        }

        Ok(())
    }
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
    image_type: ImageType,

    bit_count: u16,
    colors_used: u32,
    palette: Option<Vec<[u8; 3]>>,
    bitfields: Option<Bitfields>,
    icc_profile: Option<Vec<u8>>,

    /// Current decoder state for resumable decoding.
    state: DecoderState,

    /// Stream position to seek to on retry.
    seek_position: Option<u64>,
}

enum RLEInsn<'a> {
    EndOfFile,
    EndOfRow,
    Delta(u8, u8),
    Absolute(u8, &'a [u8]),
    PixelRun(u8, u8),
}

impl<R: BufRead + Seek> BmpDecoder<R> {
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
            image_type: ImageType::Palette,

            bit_count: 0,
            colors_used: 0,
            palette: None,
            bitfields: None,
            icc_profile: None,

            state: DecoderState::ReadingMetadata {
                progress: MetadataProgress::NotStarted,
            },
            seek_position: None,
        }
    }

    /// Create a new decoder that decodes from the stream ```r```
    pub fn new(reader: R) -> ImageResult<BmpDecoder<R>> {
        let mut decoder = Self::new_decoder(reader);
        decoder.read_metadata()?;
        Ok(decoder)
    }

    /// Create a new decoder that decodes from the stream `r` without reading
    /// metadata immediately. This allows for resumable decoding when the
    /// underlying reader may return `UnexpectedEof`.
    ///
    /// After creating the decoder, call `read_metadata()` to read the BMP
    /// headers. If it returns an `UnexpectedEof` error, you can retry on the
    /// same decoder instance after more data becomes available.
    ///
    /// Once metadata is read, call `read_image_data()` to read the pixel data.
    /// This also supports retrying on `UnexpectedEof`.
    ///
    /// # Example
    ///
    /// ```ignore
    /// use image::codecs::bmp::BmpDecoder;
    /// use image::error::ImageError;
    /// use image::ImageDecoder;
    /// use std::io;
    ///
    /// fn is_unexpected_eof(err: &ImageError) -> bool {
    ///     matches!(err, ImageError::IoError(e) if e.kind() == io::ErrorKind::UnexpectedEof)
    /// }
    ///
    /// let mut decoder = BmpDecoder::new_resumable(reader);
    ///
    /// // Phase 1: Read metadata (with retry on UnexpectedEof)
    /// loop {
    ///     match decoder.read_metadata() {
    ///         Ok(()) => break,
    ///         Err(ref e) if is_unexpected_eof(e) => {
    ///             // Wait for more data and retry on same decoder
    ///             continue;
    ///         }
    ///         Err(e) => return Err(e),
    ///     }
    /// }
    ///
    /// // Phase 2: Read image data (with retry on UnexpectedEof)
    /// let mut buf = vec![0u8; decoder.total_bytes() as usize];
    /// loop {
    ///     match decoder.read_image_data(&mut buf) {
    ///         Ok(()) => break,
    ///         Err(ref e) if is_unexpected_eof(e) => {
    ///             // Wait for more data and retry on same decoder
    ///             continue;
    ///         }
    ///         Err(e) => return Err(e),
    ///     }
    /// }
    /// ```
    pub fn new_resumable(reader: R) -> BmpDecoder<R> {
        Self::new_decoder(reader)
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

        // Read entire 14-byte file header
        const FILE_HEADER_SIZE: usize = 14;
        let mut buffer = [0u8; FILE_HEADER_SIZE];
        self.reader.read_exact(&mut buffer)?;

        // Check signature
        if &buffer[0..2] != b"BM" {
            return Err(DecoderError::BmpSignatureInvalid.into());
        }

        // Skip file size (4 bytes) and reserved (4 bytes) at offsets 2-9
        // Extract data_offset from bytes 10-13
        let data_offset = u32::from_le_bytes([buffer[10], buffer[11], buffer[12], buffer[13]]);
        self.data_offset = u64::from(data_offset);

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

    /// Read BITMAPCOREHEADER <https://msdn.microsoft.com/en-us/library/vs/alm/dd183372(v=vs.85).aspx>
    ///
    /// returns Err if any of the values are invalid.
    fn read_bitmap_core_header(&mut self) -> ImageResult<()> {
        // Core header (after size field): width(2), height(2), planes(2), bitcount(2) = 8 bytes
        let mut buffer = [0u8; 8];
        self.reader.read_exact(&mut buffer)?;

        let parsed = ParsedCoreHeader::parse(&buffer)?;

        self.width = parsed.width;
        self.height = parsed.height;
        self.bit_count = parsed.bit_count;
        self.image_type = parsed.image_type;

        check_for_overflow(self.width, self.height, self.num_channels())?;

        Ok(())
    }

    /// Read BITMAPINFOHEADER <https://msdn.microsoft.com/en-us/library/vs/alm/dd183376(v=vs.85).aspx>
    /// or BITMAPV{2|3|4|5}HEADER.
    ///
    /// returns Err if any of the values are invalid.
    fn read_bitmap_info_header(&mut self) -> ImageResult<()> {
        // Info header (after size field): 36 bytes minimum
        let mut buffer = [0u8; 36];
        self.reader.read_exact(&mut buffer)?;

        let parsed = ParsedInfoHeader::parse(&buffer)?;

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

        Ok(())
    }

    fn read_bitmasks(&mut self) -> ImageResult<()> {
        // Determine if we need to read alpha mask
        let has_alpha = matches!(
            self.bmp_header_type,
            BMPHeaderType::V3 | BMPHeaderType::V4 | BMPHeaderType::V5
        );

        // Read bitfield masks into buffer
        let buffer_size = if has_alpha { 16 } else { 12 };
        let mut buffer = vec![0u8; buffer_size];
        self.reader.read_exact(&mut buffer)?;

        // Parse masks using shared logic
        let parsed = ParsedBitfields::parse(&buffer, has_alpha);

        // Create Bitfields from parsed masks
        self.bitfields = match self.image_type {
            ImageType::Bitfields16 | ImageType::Bitfields32 => {
                let max_len = match self.image_type {
                    ImageType::Bitfields16 => 16,
                    ImageType::Bitfields32 => 32,
                    _ => unreachable!(),
                };
                Some(Bitfields::from_mask(
                    parsed.r_mask,
                    parsed.g_mask,
                    parsed.b_mask,
                    parsed.a_mask,
                    max_len,
                )?)
            }
            _ => None,
        };

        if self.bitfields.is_some() && parsed.a_mask != 0 {
            self.add_alpha_channel = true;
        }

        Ok(())
    }

    /// Read ICC profile data from the file.
    /// `offset` is the file offset where the profile data starts.
    /// `size` is the size of the profile data in bytes.
    fn read_icc_profile(&mut self, offset: u64, size: u32) -> ImageResult<()> {
        self.reader.seek(SeekFrom::Start(offset))?;
        let mut profile_data = vec![0u8; size as usize];
        self.reader.read_exact(&mut profile_data)?;
        self.icc_profile = Some(profile_data);
        Ok(())
    }

    /// Read BMP metadata (headers, palette, etc.).
    ///
    /// On `UnexpectedEof`, the decoder can be retried - the implementation tracks
    /// progress and resumes from where it left off. Once successful, subsequent
    /// calls are no-ops.
    ///
    /// Metadata reading is divided into phases:
    /// 1. Headers: File header, DIB header, and bitmasks (~30-150 bytes).
    ///    These are re-read together on retry since they're small.
    /// 2. Palette: Up to 1KB for indexed color images.
    /// 3. ICC profile: Variable size, can be several KB (V5 headers only).
    pub fn read_metadata(&mut self) -> ImageResult<()> {
        // Check if we're in a metadata reading state
        let progress = match self.state {
            DecoderState::ReadingMetadata { progress } => progress,
            _ => return Ok(()), // Already past metadata phase
        };

        if progress == MetadataProgress::Complete {
            // Metadata complete, transition to image reading state
            self.state = if self.is_rle() {
                DecoderState::ReadingRleData
            } else {
                DecoderState::ReadingRowData { rows_decoded: 0 }
            };
            return Ok(());
        }

        match self.read_metadata_impl(progress) {
            Ok(()) => {
                // Transition directly to the appropriate image reading state
                self.state = if self.is_rle() {
                    DecoderState::ReadingRleData
                } else {
                    DecoderState::ReadingRowData { rows_decoded: 0 }
                };
                Ok(())
            }
            Err(e) => Err(e),
        }
    }

    /// Internal implementation of metadata reading with phased resumability.
    ///
    /// Uses recursive calls to progress through phases. Each phase either:
    /// - Succeeds and calls the next phase
    /// - Fails with an error (which may be retryable like UnexpectedEof)
    ///
    /// Recursion depth is bounded (max 3): NotStarted → ReadingPalette → ReadingIccProfile → Complete
    fn read_metadata_impl(&mut self, progress: MetadataProgress) -> ImageResult<()> {
        match progress {
            MetadataProgress::NotStarted => {
                let icc_meta = self.read_headers()?;
                // Headers complete, determine next phase based on what's needed
                let needs_palette = matches!(
                    self.image_type,
                    ImageType::Palette | ImageType::RLE4 | ImageType::RLE8
                );
                let next = if needs_palette {
                    self.seek_position = Some(self.reader.stream_position()?);
                    MetadataProgress::ReadingPalette { icc_meta }
                } else if let Some((offset, size)) = icc_meta {
                    self.seek_position = Some(u64::from(offset));
                    MetadataProgress::ReadingIccProfile { size }
                } else {
                    self.seek_position = None;
                    MetadataProgress::Complete
                };
                self.state = DecoderState::ReadingMetadata { progress: next };
                self.read_metadata_impl(next)
            }
            MetadataProgress::ReadingPalette { icc_meta } => {
                // Seek to palette position (stored in seek_position after headers phase)
                if let Some(pos) = self.seek_position {
                    self.reader.seek(SeekFrom::Start(pos))?;
                }
                self.read_palette()?;
                // Palette complete, prepare for ICC profile phase
                let next = if let Some((offset, size)) = icc_meta {
                    self.seek_position = Some(u64::from(offset));
                    MetadataProgress::ReadingIccProfile { size }
                } else {
                    self.seek_position = None;
                    MetadataProgress::Complete
                };
                self.state = DecoderState::ReadingMetadata { progress: next };
                self.read_metadata_impl(next)
            }
            MetadataProgress::ReadingIccProfile { size } => {
                if let Some(offset) = self.seek_position {
                    self.read_icc_profile(offset, size)?;
                }
                self.state = DecoderState::ReadingMetadata {
                    progress: MetadataProgress::Complete,
                };
                self.read_metadata_impl(MetadataProgress::Complete)
            }
            MetadataProgress::Complete => {
                if self.no_file_header {
                    // Use the offset of the end of metadata instead of reading a BMP file header.
                    self.data_offset = self.reader.stream_position()?;
                }
                Ok(())
            }
        }
    }

    /// Read headers phase: file header, DIB header, and bitmasks.
    /// On retry, this re-reads everything since headers are small (~30-150 bytes).
    /// Returns ICC profile metadata (offset, size) if present in V5 header.
    fn read_headers(&mut self) -> ImageResult<Option<(u32, u32)>> {
        // On first call, record the current position.
        // On retry, seek back to the recorded position.
        // This is needed for no_file_header mode where the BMP data may start mid-file.
        match self.seek_position {
            Some(pos) => {
                self.reader.seek(SeekFrom::Start(pos))?;
            }
            None => {
                self.seek_position = Some(self.reader.stream_position()?);
            }
        }

        self.read_file_header()?;
        let bmp_header_offset = self.reader.stream_position()?;

        // Read header size into buffer for consistency with buffer-based pattern
        let mut size_buffer = [0u8; 4];
        self.reader.read_exact(&mut size_buffer)?;
        let bmp_header_size = u32::from_le_bytes(size_buffer);

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

        match self.bmp_header_type {
            BMPHeaderType::Core => {
                self.read_bitmap_core_header()?;
            }
            BMPHeaderType::Info
            | BMPHeaderType::V2
            | BMPHeaderType::V3
            | BMPHeaderType::V4
            | BMPHeaderType::V5 => {
                self.read_bitmap_info_header()?;
            }
        }

        let mut bitmask_bytes_offset = 0;
        if self.image_type == ImageType::Bitfields16 || self.image_type == ImageType::Bitfields32 {
            self.read_bitmasks()?;

            // Per https://learn.microsoft.com/en-us/windows/win32/gdi/bitmap-header-types, bitmaps
            // using the `BITMAPINFOHEADER`, `BITMAPV4HEADER`, or `BITMAPV5HEADER` structures with
            // an image type of `BI_BITFIELD` contain RGB bitfield masks immediately after the header.
            //
            // `read_bitmasks` correctly reads these from earlier in the header itself but we must
            // ensure the reader starts on the image data itself, not these extra mask bytes.
            if matches!(
                self.bmp_header_type,
                BMPHeaderType::Info | BMPHeaderType::V4 | BMPHeaderType::V5
            ) {
                // This is `size_of::<u32>() * 3` (a red, green, and blue mask), but with less noise.
                bitmask_bytes_offset = 12;
            }
        };

        // Parse ICC profile metadata from V5 header (but don't read the profile data yet)
        let mut icc_meta = None;
        if bmp_header_size >= BITMAPV5HEADER_SIZE {
            // Read the full V5 header into a buffer for ICC profile metadata parsing
            // V5 header is 124 bytes total, minus 4-byte size field = 120 bytes
            let mut header_buffer = vec![0u8; (bmp_header_size - 4) as usize];
            let current_pos = self.reader.stream_position()?;
            self.reader.seek(SeekFrom::Start(bmp_header_offset + 4))?;
            self.reader.read_exact(&mut header_buffer)?;

            // Extract ICC profile metadata for later reading (in phase 3)
            if let Some(parsed) = ParsedIccProfile::parse(&header_buffer) {
                icc_meta = Some((parsed.profile_offset, parsed.profile_size));
            }

            // Seek back to where we were
            self.reader.seek(SeekFrom::Start(current_pos))?;
        }

        // Seek to position after headers (where palette or image data starts)
        self.reader
            .seek(SeekFrom::Start(bmp_header_end + bitmask_bytes_offset))?;

        Ok(icc_meta)
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
        let width = self.width as usize;
        let skip_palette = self.indexed_color;

        let start_row = self.rows_decoded();

        let file_offset = self.data_offset + (start_row as u64 * row_byte_length as u64);
        self.reader.seek(SeekFrom::Start(file_offset))?;

        // Set alpha to opaque for all pixels if needed (only on first call)
        if start_row == 0 && num_channels == 4 {
            buf.chunks_exact_mut(4).for_each(|c| c[3] = ALPHA_OPAQUE);
        }

        let reader = &mut self.reader;
        let result = with_rows_resumable(
            buf,
            self.width,
            self.height,
            num_channels,
            self.top_down,
            start_row,
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
        );

        self.finish_row_decode(result)
    }

    fn read_16_bit_pixel_data(
        &mut self,
        buf: &mut [u8],
        bitfields: Option<&Bitfields>,
    ) -> ImageResult<()> {
        let num_channels = self.num_channels();
        let bitfields = match bitfields {
            Some(b) => b,
            None => self.bitfields.as_ref().unwrap(),
        };

        let row_data_len = self.width as usize * 2;
        let row_padding_len = calculate_row_padding(row_data_len);
        let total_row_len = row_data_len + row_padding_len;

        let start_row = self.rows_decoded();
        let width = self.width;
        let height = self.height;
        let top_down = self.top_down;

        let file_offset = self.data_offset + (start_row as u64 * total_row_len as u64);
        self.reader.seek(SeekFrom::Start(file_offset))?;

        let mut row_buffer = allocate_row_buffer(total_row_len)?;

        let reader = &mut self.reader;
        let result = with_rows_resumable(
            buf,
            width,
            height,
            num_channels,
            top_down,
            start_row,
            |row| {
                reader.read_exact(&mut row_buffer)?;
                for (row_data, pixel) in row_buffer
                    .chunks_exact(2)
                    .zip(row.chunks_exact_mut(num_channels))
                {
                    let data = u32::from(u16::from_le_bytes(row_data.try_into().unwrap()));
                    pixel[0] = bitfields.r.read(data);
                    pixel[1] = bitfields.g.read(data);
                    pixel[2] = bitfields.b.read(data);
                    if num_channels == 4 {
                        pixel[3] = if bitfields.a.len != 0 {
                            bitfields.a.read(data)
                        } else {
                            ALPHA_OPAQUE
                        };
                    }
                }
                Ok(())
            },
        );

        self.finish_row_decode(result)
    }

    /// Read image data from a reader in 32-bit formats that use bitfields.
    fn read_32_bit_pixel_data(&mut self, buf: &mut [u8]) -> ImageResult<()> {
        let num_channels = self.num_channels();
        let bitfields = self.bitfields.as_ref().unwrap();

        let row_data_len = self.width as usize * 4;

        let start_row = self.rows_decoded();
        let width = self.width;
        let height = self.height;
        let top_down = self.top_down;

        let file_offset = self.data_offset + (start_row as u64 * row_data_len as u64);
        self.reader.seek(SeekFrom::Start(file_offset))?;

        let mut row_buffer = allocate_row_buffer(row_data_len)?;

        let reader = &mut self.reader;
        let result = with_rows_resumable(
            buf,
            width,
            height,
            num_channels,
            top_down,
            start_row,
            |row| {
                reader.read_exact(&mut row_buffer)?;
                for (row_data, pixel) in row_buffer
                    .chunks_exact(4)
                    .zip(row.chunks_exact_mut(num_channels))
                {
                    let data = u32::from_le_bytes(row_data.try_into().unwrap());
                    pixel[0] = bitfields.r.read(data);
                    pixel[1] = bitfields.g.read(data);
                    pixel[2] = bitfields.b.read(data);
                    if num_channels == 4 {
                        pixel[3] = if bitfields.a.len != 0 {
                            bitfields.a.read(data)
                        } else {
                            ALPHA_OPAQUE
                        };
                    }
                }
                Ok(())
            },
        );

        self.finish_row_decode(result)
    }

    /// Read image data from a reader where the colours are stored as 8-bit values (24 or 32-bit).
    fn read_full_byte_pixel_data(
        &mut self,
        buf: &mut [u8],
        format: &FormatFullBytes,
    ) -> ImageResult<()> {
        let num_channels = self.num_channels();
        let row_data_len = match *format {
            FormatFullBytes::RGB24 => self.width as usize * 3,
            FormatFullBytes::Format888 => self.width as usize * 4,
            FormatFullBytes::RGB32 | FormatFullBytes::RGBA32 => self.width as usize * 4,
        };
        let row_padding_len = match *format {
            FormatFullBytes::RGB24 => calculate_row_padding(row_data_len),
            _ => 0,
        };
        let total_row_len = row_data_len + row_padding_len;

        let start_row = self.rows_decoded();
        let width = self.width;
        let height = self.height;
        let top_down = self.top_down;

        let file_offset = self.data_offset + (start_row as u64 * total_row_len as u64);
        self.reader.seek(SeekFrom::Start(file_offset))?;

        let mut row_buffer = allocate_row_buffer(total_row_len)?;

        let reader = &mut self.reader;
        let result = with_rows_resumable(
            buf,
            width,
            height,
            num_channels,
            top_down,
            start_row,
            |row| {
                reader.read_exact(&mut row_buffer)?;

                for (i, pixel) in row.chunks_mut(num_channels).enumerate() {
                    let offset = match *format {
                        FormatFullBytes::Format888 => i * 4 + 1, // Skip first byte
                        _ => {
                            i * match *format {
                                FormatFullBytes::RGB24 => 3,
                                _ => 4,
                            }
                        }
                    };

                    // Read the colour values (b, g, r) and reverse to (r, g, b)
                    pixel[0..3].copy_from_slice(&row_buffer[offset..offset + 3]);
                    pixel[0..3].reverse();

                    // Read the alpha channel if present
                    if *format == FormatFullBytes::RGBA32 {
                        pixel[3] = row_buffer[offset + 3];
                    } else if num_channels == 4 {
                        pixel[3] = ALPHA_OPAQUE;
                    }
                }
                Ok(())
            },
        );

        self.finish_row_decode(result)
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

        // Wrap reader in buffered RLE reader for efficient byte-by-byte access
        let mut rle_reader = RleReader::new(&mut self.reader);

        // Pre-allocate buffer for RLE absolute mode (max 256 bytes)
        let mut rle_indices_buffer = [0u8; 256];

        while let Some(row) = row_iter.next() {
            let mut pixel_iter = row.chunks_exact_mut(num_channels);

            let mut x = 0;
            loop {
                let instruction = {
                    let control_byte = rle_reader.read_byte()?;

                    match control_byte {
                        RLE_ESCAPE => {
                            let op = rle_reader.read_byte()?;

                            match op {
                                RLE_ESCAPE_EOL => RLEInsn::EndOfRow,
                                RLE_ESCAPE_EOF => RLEInsn::EndOfFile,
                                RLE_ESCAPE_DELTA => {
                                    let xdelta = rle_reader.read_byte()?;
                                    let ydelta = rle_reader.read_byte()?;
                                    RLEInsn::Delta(xdelta, ydelta)
                                }
                                _ => {
                                    let mut length = op as usize;
                                    if self.image_type == ImageType::RLE4 {
                                        length = length.div_ceil(2);
                                    }
                                    length += length & 1;
                                    rle_reader.read_exact(&mut rle_indices_buffer[..length])?;
                                    RLEInsn::Absolute(op, &rle_indices_buffer[..length])
                                }
                            }
                        }
                        _ => {
                            let palette_index = rle_reader.read_byte()?;
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

    /// Determine if the current image type is RLE-compressed.
    fn is_rle(&self) -> bool {
        matches!(self.image_type, ImageType::RLE4 | ImageType::RLE8)
    }

    /// Get the current number of rows decoded from the state.
    /// Returns 0 for non-row-based states.
    fn rows_decoded(&self) -> u32 {
        match self.state {
            DecoderState::ReadingRowData { rows_decoded } => rows_decoded,
            _ => 0,
        }
    }

    /// Handle the result of a row-based decode operation, updating state accordingly.
    fn finish_row_decode(&mut self, result: Result<u32, (u32, io::Error)>) -> ImageResult<()> {
        let (Ok(rows) | Err((rows, _))) = result;
        self.state = DecoderState::ReadingRowData { rows_decoded: rows };
        match result {
            Ok(_) => Ok(()),
            Err((_, e)) => Err(e)?,
        }
    }

    /// Read the actual pixel data of the image.
    ///
    /// Must be called after `read_metadata()` succeeds. On `UnexpectedEof`, the decoder
    /// can be retried - each format reader seeks to the correct position at the start.
    ///
    /// For row-based formats (non-RLE), already-decoded rows are preserved in the buffer
    /// and decoding resumes from the next row. For RLE formats, decoding restarts from
    /// the beginning and buffer contents are undefined after an error.
    pub fn read_image_data(&mut self, buf: &mut [u8]) -> ImageResult<()> {
        if self.state == DecoderState::ImageDecoded {
            // Already decoded, nothing to do
            return Ok(());
        }

        match self.read_image_data_impl(buf) {
            Ok(()) => {
                self.state = DecoderState::ImageDecoded;
                Ok(())
            }
            Err(e) => Err(e),
        }
    }

    /// Internal implementation of image data reading.
    fn read_image_data_impl(&mut self, buf: &mut [u8]) -> ImageResult<()> {
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
    use std::io::{BufRead, BufReader, Cursor, Seek};

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

    /// A reader that simulates partial data availability for testing resumable decoding.
    /// It wraps a byte slice and limits how many bytes can be read before returning UnexpectedEof.
    struct PartialReader {
        data: Vec<u8>,
        position: u64,
        available_bytes: usize,
    }

    impl PartialReader {
        fn new(data: Vec<u8>) -> Self {
            Self {
                data,
                position: 0,
                available_bytes: 0,
            }
        }

        /// Set the number of bytes available for reading (absolute, not additive).
        fn set_available(&mut self, bytes: usize) {
            self.available_bytes = bytes.min(self.data.len());
        }
    }

    impl io::Read for PartialReader {
        fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
            if self.position as usize >= self.available_bytes {
                return Err(io::Error::new(
                    io::ErrorKind::UnexpectedEof,
                    "simulated partial data",
                ));
            }

            let available = self.available_bytes - self.position as usize;
            let to_read = buf.len().min(available);
            let start = self.position as usize;
            buf[..to_read].copy_from_slice(&self.data[start..start + to_read]);
            self.position += to_read as u64;
            Ok(to_read)
        }
    }

    impl BufRead for PartialReader {
        fn fill_buf(&mut self) -> io::Result<&[u8]> {
            if self.position as usize >= self.available_bytes {
                return Err(io::Error::new(
                    io::ErrorKind::UnexpectedEof,
                    "simulated partial data",
                ));
            }

            let start = self.position as usize;
            Ok(&self.data[start..self.available_bytes])
        }

        fn consume(&mut self, amt: usize) {
            self.position += amt as u64;
        }
    }

    impl Seek for PartialReader {
        fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
            let new_pos = match pos {
                SeekFrom::Start(offset) => offset as i64,
                SeekFrom::End(offset) => self.data.len() as i64 + offset,
                SeekFrom::Current(offset) => self.position as i64 + offset,
            };

            if new_pos < 0 {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "seek to negative position",
                ));
            }

            self.position = new_pos as u64;
            Ok(self.position)
        }
    }

    /// Helper to check if an error is UnexpectedEof
    fn is_unexpected_eof(err: &ImageError) -> bool {
        matches!(err, ImageError::IoError(e) if e.kind() == io::ErrorKind::UnexpectedEof)
    }

    /// Test resumable decoding with various BMP formats.
    /// Verifies that read_metadata() and read_image_data() can be retried after
    /// UnexpectedEof and produce identical results to normal decoding.
    /// Also verifies metadata phase progress and row-level progress for non-RLE formats.
    #[test]
    fn test_resumable_decoding() {
        use crate::ImageDecoder;

        // Test multiple BMP formats to ensure resumable decoding works across variants
        // (path, is_rle, has_palette, has_icc_profile)
        let test_files = [
            (
                "tests/images/bmp/images/Info_R8_G8_B8.bmp",
                false,
                false,
                false,
            ), // 24-bit RGB
            (
                "tests/images/bmp/images/Info_A8_R8_G8_B8.bmp",
                false,
                false,
                false,
            ), // 32-bit RGBA
            ("tests/images/bmp/images/Info_8_Bit.bmp", false, true, false), // 8-bit palette
            ("tests/images/bmp/images/Core_8_Bit.bmp", false, true, false), // Core header + palette
            ("tests/images/bmp/images/pal8rle.bmp", true, true, false),     // 8-bit RLE
            ("tests/images/bmp/images/pal4rle.bmp", true, true, false),     // 4-bit RLE
            ("tests/images/bmp/images/rgb24prof.bmp", false, false, true), // 24-bit with ICC profile
        ];

        for (path, is_rle, has_palette, has_icc_profile) in test_files {
            let data = std::fs::read(path).unwrap();
            let file_size = data.len();

            // Get reference result from normal decoding
            let mut ref_decoder = BmpDecoder::new(Cursor::new(data.clone())).unwrap();
            let expected_bytes = ref_decoder.total_bytes() as usize;
            let mut ref_buf = vec![0u8; expected_bytes];
            let ref_icc_len = ref_decoder.icc_profile().unwrap().map(|p| p.len());
            ref_decoder.read_image(&mut ref_buf).unwrap();

            // Test resumable decoding with simulated streaming
            let reader = PartialReader::new(data);
            let mut decoder = BmpDecoder::new_resumable(reader);

            // Track metadata phase transitions
            let mut saw_reading_palette = false;
            let mut saw_reading_icc = false;

            // Phase 1: Stream bytes until metadata succeeds
            let mut bytes_available = 0;
            loop {
                decoder.reader.set_available(bytes_available);
                match decoder.read_metadata() {
                    Ok(()) => break,
                    Err(ref e) if is_unexpected_eof(e) => {
                        // Track metadata phase transitions
                        if let DecoderState::ReadingMetadata { progress } = decoder.state {
                            match progress {
                                MetadataProgress::ReadingPalette { .. } => {
                                    saw_reading_palette = true
                                }
                                MetadataProgress::ReadingIccProfile { .. } => {
                                    saw_reading_icc = true
                                }
                                _ => {}
                            }
                        }

                        // Simulate more data arriving (add 10 bytes at a time)
                        bytes_available += 10;
                        assert!(
                            bytes_available <= file_size,
                            "{}: metadata should succeed before EOF",
                            path
                        );
                    }
                    Err(e) => {
                        panic!("{}: unexpected error during metadata: {:?}", path, e);
                    }
                }
            }

            // Verify metadata phase transitions occurred as expected
            if has_palette {
                assert!(
                    saw_reading_palette,
                    "{}: should have seen ReadingPalette phase for palette image",
                    path
                );
            }
            if has_icc_profile {
                assert!(
                    saw_reading_icc,
                    "{}: should have seen ReadingIccProfile phase for ICC profile image",
                    path
                );
                // Verify ICC profile was read correctly
                let icc = decoder.icc_profile().unwrap();
                assert_eq!(
                    icc.map(|p| p.len()),
                    ref_icc_len,
                    "{}: ICC profile length mismatch",
                    path
                );
            }

            // Verify dimensions are available after metadata
            let (width, height) = decoder.dimensions();
            assert!(width > 0 && height > 0, "{}: invalid dimensions", path);
            assert_eq!(
                decoder.total_bytes() as usize,
                expected_bytes,
                "{}: total_bytes mismatch",
                path
            );

            // Phase 2: Stream bytes until image data succeeds
            let mut buf = vec![0u8; expected_bytes];
            loop {
                decoder.reader.set_available(bytes_available);
                match decoder.read_image_data(&mut buf) {
                    Ok(()) => break,
                    Err(ref e) if is_unexpected_eof(e) => {
                        // For non-RLE formats, verify state tracks row progress
                        if !is_rle {
                            match decoder.state {
                                DecoderState::ReadingRowData { rows_decoded } => {
                                    // rows_decoded should be less than height (not complete)
                                    assert!(
                                        rows_decoded < height,
                                        "{}: completed {}/{} rows but expected incomplete on partial data",
                                        path, rows_decoded, height
                                    );
                                }
                                _ => panic!(
                                    "{}: expected ReadingRowData state for non-RLE format, got {:?}",
                                    path, decoder.state
                                ),
                            }
                        }

                        bytes_available += 100;
                        assert!(
                            bytes_available <= file_size + 100, // Allow small overshoot due to chunk size
                            "{}: image data should succeed before EOF",
                            path
                        );
                    }
                    Err(e) => {
                        panic!("{}: unexpected error during image data: {:?}", path, e);
                    }
                }
            }

            // Verify decoded data matches reference
            assert_eq!(buf, ref_buf, "{}: decoded data mismatch", path);
        }
    }
}
