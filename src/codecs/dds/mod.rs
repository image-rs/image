//!  Decoding of DDS images
//!
//!  DDS (DirectDraw Surface) is a container format for storing DXT (S3TC) compressed images.
//!
//!  # Related Links
//!  * <https://docs.microsoft.com/en-us/windows/win32/direct3ddds/dx-graphics-dds-pguide> - Description of the DDS format.

mod bc;
mod convert;
mod decoder;
mod error;
mod header;

use std::io::Read;

use crate::color::ColorType;
use crate::error::{ImageError, ImageResult, UnsupportedError, UnsupportedErrorKind};
use crate::image::{ImageDecoder, ImageFormat};

use decoder::{DX10Decoder, SupportedFormat};
use error::DecoderError;
use header::{BitFlags, Caps2, DxgiFormat, Flags, Header, PixelFormat, PixelFormatFlags};

/// The representation of a DDS decoder
pub struct DdsDecoder<R: Read> {
    inner: DX10Decoder<R>,
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

        // Don't allow empty images. Decoders should be allowed to assume that
        // the image has at least one pixel.
        if header.width == 0 || header.height == 0 {
            return Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Dds.into(),
                    UnsupportedErrorKind::GenericFeature("Empty images are not supported".into()),
                ),
            ));
        }

        // Don't allow images that are too large.
        // Doing this check here prevents overflows later on.
        const MAX_SIZE: u32 = 1 << 24;
        if header.width > MAX_SIZE || header.height > MAX_SIZE {
            return Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Dds.into(),
                    UnsupportedErrorKind::GenericFeature(format!(
                        "Image dimensions ({}x{}) are larger than the maximum allowed size",
                        header.width, header.height
                    )),
                ),
            ));
        }

        if header.caps2.has_bits(Caps2::VOLUME) {
            return Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormat::Dds.into(),
                    UnsupportedErrorKind::GenericFeature(
                        "Volume textures are not supported".into(),
                    ),
                ),
            ));
        }

        let format = if let Some(dx10_header) = header.dx10 {
            // decide based on DXGI format
            dxgi_format_to_supported(dx10_header.dxgi_format)
        } else {
            // decide based on FourCC or PixelFormat
            pixel_format_to_supported(&header.pixel_format)
        };

        if let Some(format) = format {
            let cube = header.caps2.has_bits(Caps2::CUBEMAP)
                && header.caps2.has_bits(Caps2::CUBEMAP_ALL_FACES);
            let mip_count = if header.flags.has_bits(Flags::MIPMAP_COUNT) && header.mipmap_count > 1
            {
                (header.mipmap_count - 1) as u8
            } else {
                0
            };

            let decoder = DX10Decoder {
                width: header.width,
                height: header.height,
                format,
                mip_count,
                cube,
                inner: r,
            };
            let (width, height) = decoder.dimensions();

            if crate::utils::check_dimension_overflow(
                width,
                height,
                format.color_type().bytes_per_pixel(),
            ) {
                return Err(ImageError::Unsupported(
                    UnsupportedError::from_format_and_kind(
                        ImageFormat::Dds.into(),
                        UnsupportedErrorKind::GenericFeature(format!(
                            "Image dimensions ({}x{}) are too large",
                            width, height
                        )),
                    ),
                ));
            }

            return Ok(Self { inner: decoder });
        }

        // The DDS file is not supported.
        // Try to give a good error message
        let details = if let Some(dx10_header) = header.dx10 {
            format!("DDS DXGI Format {:?}", dx10_header.dxgi_format)
        } else if let Some(four_cc) = header.four_cc() {
            format!("DDS FourCC {:?}", four_cc)
        } else {
            format!("DDS PixelFormat {:?}", header.pixel_format)
        };

        Err(ImageError::Unsupported(
            UnsupportedError::from_format_and_kind(
                ImageFormat::Dds.into(),
                UnsupportedErrorKind::GenericFeature(details),
            ),
        ))
    }
}

fn dxgi_format_to_supported(dxgi_format: DxgiFormat) -> Option<SupportedFormat> {
    match dxgi_format {
        // uncompressed formats
        DxgiFormat::R8G8B8A8_TYPELESS
        | DxgiFormat::R8G8B8A8_UNORM
        | DxgiFormat::R8G8B8A8_UNORM_SRGB => Some(SupportedFormat::R8G8B8A8_UNORM),
        DxgiFormat::R8G8B8A8_SNORM => Some(SupportedFormat::R8G8B8A8_SNORM),
        DxgiFormat::B8G8R8A8_TYPELESS
        | DxgiFormat::B8G8R8A8_UNORM
        | DxgiFormat::B8G8R8A8_UNORM_SRGB => Some(SupportedFormat::B8G8R8A8_UNORM),
        DxgiFormat::B8G8R8X8_TYPELESS
        | DxgiFormat::B8G8R8X8_UNORM
        | DxgiFormat::B8G8R8X8_UNORM_SRGB => Some(SupportedFormat::B8G8R8X8_UNORM),
        DxgiFormat::B5G6R5_UNORM => Some(SupportedFormat::B5G6R5_UNORM),
        DxgiFormat::B5G5R5A1_UNORM => Some(SupportedFormat::B5G5R5A1_UNORM),
        DxgiFormat::B4G4R4A4_UNORM => Some(SupportedFormat::B4G4R4A4_UNORM),
        DxgiFormat::R8_TYPELESS | DxgiFormat::R8_UNORM => Some(SupportedFormat::R8_UNORM),
        DxgiFormat::R8_SNORM => Some(SupportedFormat::R8_SNORM),
        DxgiFormat::R8G8_UNORM => Some(SupportedFormat::R8G8_UNORM),
        DxgiFormat::R8G8_SNORM => Some(SupportedFormat::R8G8_SNORM),
        DxgiFormat::A8_UNORM => Some(SupportedFormat::A8_UNORM),
        DxgiFormat::R16_TYPELESS | DxgiFormat::R16_UNORM => Some(SupportedFormat::R16_UNORM),
        DxgiFormat::R16_SNORM => Some(SupportedFormat::R16_SNORM),
        DxgiFormat::R16_FLOAT => Some(SupportedFormat::R16_FLOAT),
        DxgiFormat::R16G16_TYPELESS | DxgiFormat::R16G16_UNORM => {
            Some(SupportedFormat::R16G16_UNORM)
        }
        DxgiFormat::R16G16_SNORM => Some(SupportedFormat::R16G16_SNORM),
        DxgiFormat::R16G16_FLOAT => Some(SupportedFormat::R16G16_FLOAT),
        DxgiFormat::R16G16B16A16_TYPELESS | DxgiFormat::R16G16B16A16_UNORM => {
            Some(SupportedFormat::R16G16B16A16_UNORM)
        }
        DxgiFormat::R16G16B16A16_SNORM => Some(SupportedFormat::R16G16B16A16_SNORM),
        DxgiFormat::R16G16B16A16_FLOAT => Some(SupportedFormat::R16G16B16A16_FLOAT),
        DxgiFormat::R10G10B10A2_TYPELESS | DxgiFormat::R10G10B10A2_UNORM => {
            Some(SupportedFormat::R10G10B10A2_UNORM)
        }
        DxgiFormat::R11G11B10_FLOAT => Some(SupportedFormat::R11G11B10_FLOAT),
        DxgiFormat::R9G9B9E5_SHAREDEXP => Some(SupportedFormat::R9G9B9E5_SHAREDEXP),
        DxgiFormat::R32_TYPELESS | DxgiFormat::R32_FLOAT => Some(SupportedFormat::R32_FLOAT),
        DxgiFormat::R32G32_TYPELESS | DxgiFormat::R32G32_FLOAT => {
            Some(SupportedFormat::R32G32_FLOAT)
        }
        DxgiFormat::R32G32B32_TYPELESS | DxgiFormat::R32G32B32_FLOAT => {
            Some(SupportedFormat::R32G32B32_FLOAT)
        }
        DxgiFormat::R32G32B32A32_TYPELESS | DxgiFormat::R32G32B32A32_FLOAT => {
            Some(SupportedFormat::R32G32B32A32_FLOAT)
        }
        DxgiFormat::R10G10B10_XR_BIAS_A2_UNORM => Some(SupportedFormat::R10G10B10_XR_BIAS_A2_UNORM),

        // sub-sampled formats
        DxgiFormat::R8G8_B8G8_UNORM => Some(SupportedFormat::R8G8_B8G8_UNORM),
        DxgiFormat::G8R8_G8B8_UNORM => Some(SupportedFormat::G8R8_G8B8_UNORM),

        // block compression formats
        DxgiFormat::BC1_TYPELESS | DxgiFormat::BC1_UNORM | DxgiFormat::BC1_UNORM_SRGB => {
            Some(SupportedFormat::BC1_ALPHA_UNORM)
        }
        DxgiFormat::BC2_TYPELESS | DxgiFormat::BC2_UNORM | DxgiFormat::BC2_UNORM_SRGB => {
            Some(SupportedFormat::BC2_UNORM)
        }
        DxgiFormat::BC3_TYPELESS | DxgiFormat::BC3_UNORM | DxgiFormat::BC3_UNORM_SRGB => {
            Some(SupportedFormat::BC3_UNORM)
        }
        DxgiFormat::BC4_TYPELESS | DxgiFormat::BC4_UNORM => Some(SupportedFormat::BC4_UNORM),
        DxgiFormat::BC4_SNORM => Some(SupportedFormat::BC4_SNORM),
        DxgiFormat::BC5_TYPELESS | DxgiFormat::BC5_UNORM => Some(SupportedFormat::BC5_UNORM),
        DxgiFormat::BC5_SNORM => Some(SupportedFormat::BC5_SNORM),
        DxgiFormat::BC6H_TYPELESS | DxgiFormat::BC6H_UF16 => Some(SupportedFormat::BC6H_UF16),
        DxgiFormat::BC6H_SF16 => Some(SupportedFormat::BC6H_SF16),
        DxgiFormat::BC7_TYPELESS | DxgiFormat::BC7_UNORM | DxgiFormat::BC7_UNORM_SRGB => {
            Some(SupportedFormat::BC7_UNORM)
        }
        _ => None,
    }
}
fn four_cc_to_dxgi(four_cc: [u8; 4]) -> Option<DxgiFormat> {
    match &four_cc {
        b"DXT1" => Some(DxgiFormat::BC1_UNORM),
        b"DXT2" => Some(DxgiFormat::BC2_UNORM),
        b"DXT3" => Some(DxgiFormat::BC2_UNORM),
        b"DXT4" => Some(DxgiFormat::BC3_UNORM),
        b"DXT5" => Some(DxgiFormat::BC3_UNORM),

        b"ATI1" => Some(DxgiFormat::BC4_UNORM),
        b"BC4U" => Some(DxgiFormat::BC4_UNORM),
        b"BC4S" => Some(DxgiFormat::BC4_SNORM),

        b"ATI2" => Some(DxgiFormat::BC5_UNORM),
        b"BC5U" => Some(DxgiFormat::BC5_UNORM),
        b"BC5S" => Some(DxgiFormat::BC5_SNORM),

        b"RGBG" => Some(DxgiFormat::R8G8_B8G8_UNORM),
        b"GRGB" => Some(DxgiFormat::G8R8_G8B8_UNORM),

        b"YUY2" => Some(DxgiFormat::YUY2),

        _ => {
            // Some old encoders use the FOURCC field to store D3DFORMAT constants:
            // https://learn.microsoft.com/en-us/windows/win32/direct3d9/d3dformat
            //
            // We can theoretically support most of them. However, testing them
            // is hard because there aren't many programs that produce them
            // (AFAIK). Texconv from the DirectX SDK is one of them, but it only
            // produces the following formats.
            match u32::from_le_bytes(four_cc) {
                36 => Some(DxgiFormat::R16G16B16A16_UNORM),

                110 => Some(DxgiFormat::R16G16B16A16_SNORM),
                111 => Some(DxgiFormat::R16_FLOAT),
                112 => Some(DxgiFormat::R16G16_FLOAT),
                113 => Some(DxgiFormat::R16G16B16A16_FLOAT),
                114 => Some(DxgiFormat::R32_FLOAT),
                115 => Some(DxgiFormat::R32G32_FLOAT),
                116 => Some(DxgiFormat::R32G32B32A32_FLOAT),
                _ => None,
            }
        }
    }
}
fn pixel_format_to_supported(pf: &PixelFormat) -> Option<SupportedFormat> {
    if pf.flags.has(PixelFormatFlags::FOURCC) {
        return four_cc_to_dxgi(pf.fourcc).and_then(dxgi_format_to_supported);
    }

    // known patterns
    for (pattern, format) in KNOWN_PIXEL_FORMATS {
        if pattern.matches(pf) {
            return Some(*format);
        }
    }

    None
}

struct PFPattern {
    flags: PixelFormatFlags,
    rgb_bit_count: u32,
    r_bit_mask: u32,
    g_bit_mask: u32,
    b_bit_mask: u32,
    a_bit_mask: u32,
}
impl PFPattern {
    fn matches(&self, pf: &PixelFormat) -> bool {
        pf.flags == self.flags
            && pf.rgb_bit_count == self.rgb_bit_count
            && pf.r_bit_mask == self.r_bit_mask
            && pf.g_bit_mask == self.g_bit_mask
            && pf.b_bit_mask == self.b_bit_mask
            && pf.a_bit_mask == self.a_bit_mask
    }
}
const KNOWN_PIXEL_FORMATS: &[(PFPattern, SupportedFormat)] = {
    const fn alpha_only(bit_count: u32, a_mask: u32) -> PFPattern {
        PFPattern {
            flags: PixelFormatFlags::ALPHA,
            rgb_bit_count: bit_count,
            r_bit_mask: 0,
            g_bit_mask: 0,
            b_bit_mask: 0,
            a_bit_mask: a_mask,
        }
    }
    const fn grayscale(bit_count: u32, r_mask: u32) -> PFPattern {
        PFPattern {
            flags: PixelFormatFlags::LUMINANCE,
            rgb_bit_count: bit_count,
            r_bit_mask: r_mask,
            g_bit_mask: 0,
            b_bit_mask: 0,
            a_bit_mask: 0,
        }
    }
    const fn rgb(bit_count: u32, r_mask: u32, g_mask: u32, b_mask: u32) -> PFPattern {
        PFPattern {
            flags: PixelFormatFlags::RGB,
            rgb_bit_count: bit_count,
            r_bit_mask: r_mask,
            g_bit_mask: g_mask,
            b_bit_mask: b_mask,
            a_bit_mask: 0,
        }
    }
    const fn rgba(bit_count: u32, r_mask: u32, g_mask: u32, b_mask: u32, a_mask: u32) -> PFPattern {
        PFPattern {
            flags: PixelFormatFlags::RGBA,
            rgb_bit_count: bit_count,
            r_bit_mask: r_mask,
            g_bit_mask: g_mask,
            b_bit_mask: b_mask,
            a_bit_mask: a_mask,
        }
    }
    const fn snorm(
        bit_count: u32,
        r_mask: u32,
        g_mask: u32,
        b_mask: u32,
        a_mask: u32,
    ) -> PFPattern {
        PFPattern {
            flags: PixelFormatFlags::SNORM,
            rgb_bit_count: bit_count,
            r_bit_mask: r_mask,
            g_bit_mask: g_mask,
            b_bit_mask: b_mask,
            a_bit_mask: a_mask,
        }
    }

    &[
        // alpha
        (alpha_only(8, 0xFF), SupportedFormat::A8_UNORM),
        // grayscale
        (grayscale(8, 0xFF), SupportedFormat::R8_UNORM),
        (grayscale(16, 0xFFFF), SupportedFormat::R16_UNORM),
        // rgb
        (
            rgb(16, 0xF800, 0x07E0, 0x001F),
            SupportedFormat::B5G6R5_UNORM,
        ),
        (
            rgb(32, 0xFF0000, 0xFF00, 0xFF),
            SupportedFormat::B8G8R8X8_UNORM,
        ),
        (
            rgb(32, 0xFFFF, 0xFFFF0000, 0),
            SupportedFormat::R16G16_UNORM,
        ),
        (rgb(16, 0xFF, 0xFF00, 0), SupportedFormat::R8G8_UNORM),
        (
            rgb(24, 0xFF0000, 0xFF00, 0xFF),
            SupportedFormat::B8G8R8_UNORM,
        ),
        (
            rgb(24, 0xFF, 0xFF00, 0xFF0000),
            SupportedFormat::R8G8B8_UNORM,
        ),
        // rgba
        (
            rgba(16, 0xF00, 0xF0, 0xF, 0xF000),
            SupportedFormat::B4G4R4A4_UNORM,
        ),
        (
            rgba(16, 0x7C00, 0x3E0, 0x1F, 0x8000),
            SupportedFormat::B5G5R5A1_UNORM,
        ),
        (
            rgba(32, 0xFF0000, 0xFF00, 0xFF, 0xFF000000),
            SupportedFormat::B8G8R8A8_UNORM,
        ),
        (
            rgba(32, 0xFF, 0xFF00, 0xFF0000, 0xFF000000),
            SupportedFormat::R8G8B8A8_UNORM,
        ),
        // snorm
        (
            snorm(32, 0xFF, 0xFF00, 0xFF0000, 0xFF000000),
            SupportedFormat::R8G8B8A8_SNORM,
        ),
        (snorm(16, 0xFF, 0xFF00, 0, 0), SupportedFormat::R8G8_SNORM),
        (
            snorm(32, 0xFFFF, 0xFFFF0000, 0, 0),
            SupportedFormat::R16G16_SNORM,
        ),
        // special
        (
            // I have no idea why, but LUMINANCE + ALPHAPIXELS is used for R8G8_UNORM
            PFPattern {
                flags: PixelFormatFlags::LUMINANCE_ALPHA,
                rgb_bit_count: 16,
                r_bit_mask: 0xFF,
                g_bit_mask: 0,
                b_bit_mask: 0,
                a_bit_mask: 0xFF00,
            },
            SupportedFormat::R8G8_UNORM,
        ),
    ]
};

impl<R: Read> ImageDecoder for DdsDecoder<R> {
    fn dimensions(&self) -> (u32, u32) {
        self.inner.dimensions()
    }

    fn color_type(&self) -> ColorType {
        self.inner.color_type()
    }

    fn read_image(self, buf: &mut [u8]) -> ImageResult<()> {
        self.inner.read_image(buf)
    }

    fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()> {
        (*self).read_image(buf)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn dimension_overflow() {
        // A DXT1 header set to 0xFFFF_FFFC width and height (the highest u32%4 == 0)
        let header = [
            0x44, 0x44, 0x53, 0x20, 0x7C, 0x0, 0x0, 0x0, 0x7, 0x10, 0x8, 0x0, 0xFC, 0xFF, 0xFF,
            0xFF, 0xFC, 0xFF, 0xFF, 0xFF, 0x0, 0xC0, 0x12, 0x0, 0x0, 0x0, 0x0, 0x0, 0x1, 0x0, 0x0,
            0x0, 0x49, 0x4D, 0x41, 0x47, 0x45, 0x4D, 0x41, 0x47, 0x49, 0x43, 0x4B, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x20, 0x0, 0x0, 0x0,
            0x4, 0x0, 0x0, 0x0, 0x44, 0x58, 0x54, 0x31, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x10, 0x0, 0x0, 0x0,
            0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
        ];

        assert!(DdsDecoder::new(&header[..]).is_err());
    }
}
