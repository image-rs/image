//!  Decoding of DDS images
//!
//!  DDS (DirectDraw Surface) is a container format for storing DXT (S3TC) compressed images.
//!
//!  # Related Links
//!  * <https://docs.microsoft.com/en-us/windows/win32/direct3ddds/dx-graphics-dds-pguide> - Description of the DDS format.

use std::{error, fmt};
use std::io::Read;

use byteorder::{LittleEndian, ReadBytesExt};

use crate::color::ColorType;
use crate::dxt::{DxtDecoder, DXTReader, DXTVariant};
use crate::error::{
    DecodingError, ImageError, ImageFormatHint, ImageResult, UnsupportedError, UnsupportedErrorKind,
};
use crate::image::{ImageDecoder, ImageFormat};

/// Errors that can occur during decoding and parsing a DDS image
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum DecoderError {
    /// Wrong DDS channel width
    PixelFormatSizeInvalid(u32),
    /// Wrong DDS header size
    HeaderSizeInvalid(u32),
    /// Wrong DDS header flags
    HeaderFlagsInvalid(u32),

    /// DDS "DDS " signature invalid or missing
    DdsSignatureInvalid,
}

impl fmt::Display for DecoderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DecoderError::PixelFormatSizeInvalid(s) =>
                f.write_fmt(format_args!("Invalid DDS PixelFormat size: {}", s)),
            DecoderError::HeaderSizeInvalid(s) =>
                f.write_fmt(format_args!("Invalid DDS header size: {}", s)),
            DecoderError::HeaderFlagsInvalid(fs) =>
                f.write_fmt(format_args!("Invalid DDS header flags: {:#010X}", fs)),
            DecoderError::DdsSignatureInvalid =>
                f.write_str("DDS signature not found"),
        }
    }
}

impl From<DecoderError> for ImageError {
    fn from(e: DecoderError) -> ImageError {
        ImageError::Decoding(DecodingError::new(ImageFormat::Dds.into(), e))
    }
}

impl error::Error for DecoderError {}

/// Header used by DDS image files
#[derive(Debug)]
struct Header {
    flags: u32,
    height: u32,
    width: u32,
    pitch_or_linear_size: u32,
    depth: u32,
    mipmap_count: u32,
    pixel_format: PixelFormat,
    caps: u32,
    caps2: u32,
}

/// DDS pixel format
#[derive(Debug)]
struct PixelFormat {
    flags: u32,
    fourcc: [u8; 4],
    rgb_bit_count: u32,
    r_bit_mask: u32,
    g_bit_mask: u32,
    b_bit_mask: u32,
    a_bit_mask: u32,
}

impl PixelFormat {
    fn from_reader(r: &mut dyn Read) -> ImageResult<Self> {
        let size = r.read_u32::<LittleEndian>()?;
        if size != 32 {
            return Err(DecoderError::PixelFormatSizeInvalid(size).into());
        }

        Ok(Self {
            flags: r.read_u32::<LittleEndian>()?,
            fourcc: {
                let mut v = [0; 4];
                r.read_exact(&mut v)?;
                v
            },
            rgb_bit_count: r.read_u32::<LittleEndian>()?,
            r_bit_mask: r.read_u32::<LittleEndian>()?,
            g_bit_mask: r.read_u32::<LittleEndian>()?,
            b_bit_mask: r.read_u32::<LittleEndian>()?,
            a_bit_mask: r.read_u32::<LittleEndian>()?,
        })
    }
}

impl Header {
    fn from_reader(r: &mut dyn Read) -> ImageResult<Self> {
        let size = r.read_u32::<LittleEndian>()?;
        if size != 124 {
            return Err(DecoderError::HeaderSizeInvalid(size).into());
        }

        const REQUIRED_FLAGS: u32 = 0x1 | 0x2 | 0x4 | 0x1000;
        const VALID_FLAGS: u32 = 0x1 | 0x2 | 0x4 | 0x8 | 0x1000 | 0x20000 | 0x80000 | 0x800000;
        let flags = r.read_u32::<LittleEndian>()?;
        if flags & (REQUIRED_FLAGS | !VALID_FLAGS) != REQUIRED_FLAGS {
            return Err(DecoderError::HeaderFlagsInvalid(flags).into());
        }

        let height = r.read_u32::<LittleEndian>()?;
        let width = r.read_u32::<LittleEndian>()?;
        let pitch_or_linear_size = r.read_u32::<LittleEndian>()?;
        let depth = r.read_u32::<LittleEndian>()?;
        let mipmap_count = r.read_u32::<LittleEndian>()?;
        // Skip `dwReserved1`
        {
            let mut skipped = [0; 4 * 11];
            r.read_exact(&mut skipped)?;
        }
        let pixel_format = PixelFormat::from_reader(r)?;
        let caps = r.read_u32::<LittleEndian>()?;
        let caps2 = r.read_u32::<LittleEndian>()?;
        // Skip `dwCaps3`, `dwCaps4`, `dwReserved2` (unused)
        {
            let mut skipped = [0; 4 + 4 + 4];
            r.read_exact(&mut skipped)?;
        }

        Ok(Self {
            flags,
            height,
            width,
            pitch_or_linear_size,
            depth,
            mipmap_count,
            pixel_format,
            caps,
            caps2,
        })
    }
}


/// The representation of a DDS decoder
pub struct DdsDecoder<R: Read> {
    inner: DxtDecoder<R>,
}

impl<R: Read> DdsDecoder<R> {
    /// Create a new decoder that decodes from the stream `r`
    pub fn new(mut r: R) -> ImageResult<Self> {
        let mut magic = [0; 4];
        r.read_exact(&mut magic)?;
        if magic != b"DDS "[..] {
            return Err(DecoderError::DdsSignatureInvalid.into());
        }

        let header = Header::from_reader(&mut r)?;

        if header.pixel_format.flags & 0x4 != 0 {
            let variant = match &header.pixel_format.fourcc {
                b"DXT1" => DXTVariant::DXT1,
                b"DXT3" => DXTVariant::DXT3,
                b"DXT5" => DXTVariant::DXT5,
                fourcc => {
                    return Err(ImageError::Unsupported(
                        UnsupportedError::from_format_and_kind(
                            ImageFormat::Dds.into(),
                            UnsupportedErrorKind::GenericFeature(format!("DDS FourCC {:?}", fourcc)),
                        ),
                    ))
                }
            };
            let inner = DxtDecoder::new(r, header.width, header.height, variant)?;
            Ok(Self { inner })
        } else {
            // For now, supports only DXT variants
            Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Dds.into(),
                    UnsupportedErrorKind::Format(ImageFormatHint::Name("DDS".to_string())),
                ),
            ))
        }
    }
}

impl<'a, R: 'a + Read> ImageDecoder<'a> for DdsDecoder<R> {
    type Reader = DXTReader<R>;

    fn dimensions(&self) -> (u32, u32) {
        self.inner.dimensions()
    }

    fn color_type(&self) -> ColorType {
        self.inner.color_type()
    }

    fn scanline_bytes(&self) -> u64 {
        self.inner.scanline_bytes()
    }

    fn into_reader(self) -> ImageResult<Self::Reader> {
        self.inner.into_reader()
    }

    fn read_image(self, buf: &mut [u8]) -> ImageResult<()> {
        self.inner.read_image(buf)
    }
}

