use std::io::Read;

use byteorder_lite::{LittleEndian, ReadBytesExt};

use crate::error::ImageResult;

use super::error::DecoderError;

/// The DDS_HEADER structure contains information about the dimensions, format, and mipmap count of a texture.
///
/// https://learn.microsoft.com/en-us/windows/win32/direct3ddds/dds-header
#[derive(Debug)]
pub(crate) struct Header {
    pub(crate) flags: Flags,
    pub(crate) height: u32,
    pub(crate) width: u32,
    pub(crate) _pitch_or_linear_size: u32,
    pub(crate) _depth: u32,
    pub(crate) mipmap_count: u32,
    pub(crate) pixel_format: PixelFormat,
    pub(crate) caps: Caps,
    pub(crate) caps2: Caps2,
    pub(crate) dx10: Option<DX10Header>,
}

impl Header {
    pub(crate) fn from_reader(r: &mut dyn Read) -> ImageResult<Self> {
        let size = r.read_u32::<LittleEndian>()?;
        if size != 124 {
            return Err(DecoderError::HeaderSizeInvalid(size).into());
        }

        let flags = r.read_u32::<LittleEndian>()?;
        let flags = Flags::from_u32(flags).ok_or(DecoderError::HeaderFlagsInvalid(flags))?;

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
        let caps = Caps::from_u32(caps).ok_or(DecoderError::HeaderCapsInvalid(caps))?;
        let caps2 = r.read_u32::<LittleEndian>()?;
        let caps2 = Caps2::from_u32(caps2).ok_or(DecoderError::HeaderCaps2Invalid(caps2))?;
        // Skip `dwCaps3`, `dwCaps4`, `dwReserved2` (unused)
        {
            let mut skipped = [0; 4 + 4 + 4];
            r.read_exact(&mut skipped)?;
        }

        let dx10 = if pixel_format.flags.has(PixelFormatFlags::FOURCC)
            && pixel_format.fourcc == *b"DX10"
        {
            Some(DX10Header::from_reader(r)?)
        } else {
            None
        };

        Ok(Self {
            flags,
            height,
            width,
            _pitch_or_linear_size: pitch_or_linear_size,
            _depth: depth,
            mipmap_count,
            pixel_format,
            caps,
            caps2,
            dx10,
        })
    }

    pub(crate) fn four_cc(&self) -> Option<[u8; 4]> {
        if self.pixel_format.flags.has(PixelFormatFlags::FOURCC) {
            Some(self.pixel_format.fourcc)
        } else {
            None
        }
    }
}

pub(crate) trait BitFlags
where
    Self: Sized + Copy,
{
    fn bits(self) -> u32;

    /// Returns true if all flags are set.
    fn has_bits(self, flag: u32) -> bool {
        self.bits() & flag == flag
    }
    /// Returns true if all flags are set.
    fn has(self, flag: Self) -> bool {
        self.bits() & flag.bits() == flag.bits()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct Flags(u32);

impl Flags {
    fn from_u32(flags: u32) -> Option<Self> {
        if flags & (Self::REQUIRED | !Self::ALL) != Self::REQUIRED {
            None
        } else {
            Some(Self(flags))
        }
    }

    const ALL: u32 = Self::CAPS
        | Self::HEIGHT
        | Self::WIDTH
        | Self::PITCH
        | Self::PIXEL_FORMAT
        | Self::MIPMAP_COUNT
        | Self::LINEAR_SIZE
        | Self::DEPTH;
    const REQUIRED: u32 = Self::CAPS | Self::HEIGHT | Self::WIDTH | Self::PIXEL_FORMAT;

    /// Required in every .dds file.
    pub(crate) const CAPS: u32 = 0x1;
    /// Required in every .dds file.
    pub(crate) const HEIGHT: u32 = 0x2;
    /// Required in every .dds file.
    pub(crate) const WIDTH: u32 = 0x4;
    /// Required when pitch is provided for an uncompressed texture.
    pub(crate) const PITCH: u32 = 0x8;
    /// Required in every .dds file.
    pub(crate) const PIXEL_FORMAT: u32 = 0x1000;
    /// Required in a mipmapped texture.
    pub(crate) const MIPMAP_COUNT: u32 = 0x20000;
    /// Required when pitch is provided for a compressed texture.
    pub(crate) const LINEAR_SIZE: u32 = 0x80000;
    /// Required in a depth texture.
    pub(crate) const DEPTH: u32 = 0x800000;
}
impl BitFlags for Flags {
    fn bits(self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct Caps(u32);

impl Caps {
    fn from_u32(flags: u32) -> Option<Self> {
        if flags & Self::REQUIRED != Self::REQUIRED {
            None
        } else {
            // Same reasoning as for PixelFormatFlags: So encoders store
            // non-standard flags in the reserved bits. Most of these
            // non-standard flags can safely be ignored.
            Some(Self(flags & Self::ALL))
        }
    }

    const ALL: u32 = Self::COMPLEX | Self::MIPMAP | Self::TEXTURE;
    const REQUIRED: u32 = Self::TEXTURE;

    /// Optional; must be used on any file that contains more than one surface (a mipmap, a cubic environment map, or mipmapped volume texture).
    pub(crate) const COMPLEX: u32 = 0x8;
    /// Optional; should be used for a mipmap.
    pub(crate) const MIPMAP: u32 = 0x400000;
    /// Required
    pub(crate) const TEXTURE: u32 = 0x1000;
}
impl BitFlags for Caps {
    fn bits(self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct Caps2(u32);

impl Caps2 {
    fn from_u32(flags: u32) -> Option<Self> {
        if flags & !Self::ALL != 0 {
            None
        } else {
            Some(Self(flags))
        }
    }

    const ALL: u32 = Self::CUBEMAP | Self::CUBEMAP_ALL_FACES | Self::VOLUME;

    /// Required for a cube map.
    pub(crate) const CUBEMAP: u32 = 0x200;
    /// Required when these surfaces are stored in a cube map.
    pub(crate) const CUBEMAP_POSITIVEX: u32 = 0x400;
    /// Required when these surfaces are stored in a cube map.
    pub(crate) const CUBEMAP_NEGATIVEX: u32 = 0x800;
    /// Required when these surfaces are stored in a cube map.
    pub(crate) const CUBEMAP_POSITIVEY: u32 = 0x1000;
    /// Required when these surfaces are stored in a cube map.
    pub(crate) const CUBEMAP_NEGATIVEY: u32 = 0x2000;
    /// Required when these surfaces are stored in a cube map.
    pub(crate) const CUBEMAP_POSITIVEZ: u32 = 0x4000;
    /// Required when these surfaces are stored in a cube map.
    pub(crate) const CUBEMAP_NEGATIVEZ: u32 = 0x8000;
    /// Required for a volume texture.
    pub(crate) const VOLUME: u32 = 0x200000;

    /// Although Direct3D 9 supports partial cube-maps, Direct3D 10, 10.1, and 11 require that you define all six cube-map faces (that is, you must set DDS_CUBEMAP_ALLFACES).
    pub(crate) const CUBEMAP_ALL_FACES: u32 = Self::CUBEMAP_POSITIVEX
        | Self::CUBEMAP_NEGATIVEX
        | Self::CUBEMAP_POSITIVEY
        | Self::CUBEMAP_NEGATIVEY
        | Self::CUBEMAP_POSITIVEZ
        | Self::CUBEMAP_NEGATIVEZ;
}
impl BitFlags for Caps2 {
    fn bits(self) -> u32 {
        self.0
    }
}

/// DDS pixel format
#[derive(Debug)]
pub(crate) struct PixelFormat {
    pub(crate) flags: PixelFormatFlags,
    pub(crate) fourcc: [u8; 4],
    pub(crate) rgb_bit_count: u32,
    pub(crate) r_bit_mask: u32,
    pub(crate) g_bit_mask: u32,
    pub(crate) b_bit_mask: u32,
    pub(crate) a_bit_mask: u32,
}

impl PixelFormat {
    fn from_reader(r: &mut dyn Read) -> ImageResult<Self> {
        let size = r.read_u32::<LittleEndian>()?;
        if size != 32 {
            return Err(DecoderError::PixelFormatSizeInvalid(size).into());
        }

        let flags = r.read_u32::<LittleEndian>()?;
        let flags = PixelFormatFlags::from_u32(flags);

        Ok(Self {
            flags,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct PixelFormatFlags(u32);

impl PixelFormatFlags {
    fn from_u32(flags: u32) -> Self {
        // There are some encoders that store additional information in
        // reserved flags. While this is non-standard, these DDS files are
        // still compatible with DirectX, because it just ignores unknown
        // flags. So we do the same.
        Self(flags & Self::ALL.bits())
    }

    const ALL: Self = Self(
        Self::ALPHAPIXELS.0
            | Self::ALPHA.0
            | Self::FOURCC.0
            | Self::RGB.0
            | Self::YUV.0
            | Self::LUMINANCE.0
            | Self::PAL8.0
            | Self::PAL8A.0
            | Self::SNORM.0,
    );

    // Official docs are outdated. The following constants are from (1) the
    // official docs and (2) the source code of DirectXTex:
    // https://github.com/microsoft/DirectXTex/blob/af1c8b3cb4cae9354a7aade2f999ebf97d46e4fb/DirectXTex/DDS.h#L42

    /// Texture contains alpha data; dwRGBAlphaBitMask contains valid data.
    pub(crate) const ALPHAPIXELS: Self = Self(0x1);
    /// Used in some older DDS files for alpha channel only uncompressed data (dwRGBBitCount contains the alpha channel bitcount; dwABitMask contains valid data)
    pub(crate) const ALPHA: Self = Self(0x2);
    /// Texture contains compressed RGB data; dwFourCC contains valid data.
    pub(crate) const FOURCC: Self = Self(0x4);
    /// Texture contains uncompressed RGB data; dwRGBBitCount and the RGB masks (dwRBitMask, dwGBitMask, dwBBitMask) contain valid data.
    pub(crate) const RGB: Self = Self(0x40);
    pub(crate) const RGBA: Self = Self(Self::RGB.0 | Self::ALPHAPIXELS.0);
    /// Used in some older DDS files for YUV uncompressed data (dwRGBBitCount contains the YUV bit count; dwRBitMask contains the Y mask, dwGBitMask contains the U mask, dwBBitMask contains the V mask)
    pub(crate) const YUV: Self = Self(0x200);
    /// Used in some older DDS files for single channel color uncompressed data (dwRGBBitCount contains the luminance channel bit count; dwRBitMask contains the channel mask). Can be combined with DDPF_ALPHAPIXELS for a two channel DDS file.
    pub(crate) const LUMINANCE: Self = Self(0x20000);
    pub(crate) const LUMINANCE_ALPHA: Self = Self(Self::LUMINANCE.0 | Self::ALPHAPIXELS.0);
    pub(crate) const PAL8: Self = Self(0x20);
    pub(crate) const PAL8A: Self = Self(Self::PAL8.0 | Self::ALPHAPIXELS.0);
    /// While DirectXTex calls this flag `BUMPDUDV` (bumpmap dUdV), this just says that the texture contains SNORM data. Which channels the texture contains depends on which bit masks are non-zero. All dw*BitMask fields contain valid data.
    pub(crate) const SNORM: Self = Self(0x80000);
}
impl BitFlags for PixelFormatFlags {
    fn bits(self) -> u32 {
        self.0
    }
}
impl std::ops::BitOr for PixelFormatFlags {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self {
        Self(self.0 | rhs.0)
    }
}

/// Extended DX10 header used by some DDS image files
#[derive(Debug, Copy, Clone)]
pub(crate) struct DX10Header {
    pub(crate) dxgi_format: DxgiFormat,
    pub(crate) _resource_dimension: ResourceDimension,
    pub(crate) _misc_flag: u32,
    pub(crate) _array_size: u32,
    pub(crate) _misc_flags_2: u32,
}

impl DX10Header {
    fn from_reader(r: &mut dyn Read) -> ImageResult<Self> {
        let dxgi_format = r.read_u32::<LittleEndian>()?;
        let resource_dimension = r.read_u32::<LittleEndian>()?;
        let misc_flag = r.read_u32::<LittleEndian>()?;
        let array_size = r.read_u32::<LittleEndian>()?;
        let misc_flags_2 = r.read_u32::<LittleEndian>()?;

        let dxgi_format = DxgiFormat::from_u32(dxgi_format)
            .ok_or(DecoderError::DxgiFormatInvalid(dxgi_format))?;
        let resource_dimension = ResourceDimension::from_u32(resource_dimension)
            .ok_or(DecoderError::ResourceDimensionInvalid(resource_dimension))?;

        if !matches!(
            resource_dimension,
            ResourceDimension::Texture1D
                | ResourceDimension::Texture2D
                | ResourceDimension::Texture3D,
        ) {
            // Only 1D (2), 2D (3) and 3D (4) resource dimensions are allowed
            return Err(DecoderError::ResourceDimensionUnsupported(resource_dimension).into());
        }
        if misc_flag != 0x0 && misc_flag != 0x4 {
            // Invalid flag
            // Only no (0x0) and DDS_RESOURCE_MISC_TEXTURECUBE (0x4) flags are allowed
            return Err(DecoderError::Dx10FlagsInvalid(misc_flag).into());
        }
        if resource_dimension == ResourceDimension::Texture3D && array_size != 1 {
            // Invalid array size
            // 3D textures must have an array size of 1
            return Err(DecoderError::Dx10ArraySizeInvalid(array_size).into());
        }
        if misc_flags_2 > 0x4 {
            // Invalid alpha flags
            return Err(DecoderError::Dx10FlagsInvalid(misc_flags_2).into());
        }

        let dx10_header = Self {
            dxgi_format,
            _resource_dimension: resource_dimension,
            _misc_flag: misc_flag,
            _array_size: array_size,
            _misc_flags_2: misc_flags_2,
        };

        Ok(dx10_header)
    }
}

/// Resource data formats, including fully-typed and typeless formats. A list of modifiers at the bottom of the page more fully describes each format type.
///
/// https://learn.microsoft.com/en-us/windows/win32/api/dxgiformat/ne-dxgiformat-dxgi_format
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub(crate) struct DxgiFormat(u8);
impl DxgiFormat {
    #![allow(dead_code)]

    fn from_u32(format: u32) -> Option<Self> {
        if format <= 132 {
            Some(Self(format as u8))
        } else {
            None
        }
    }

    pub(crate) const UNKNOWN: DxgiFormat = DxgiFormat(0);
    pub(crate) const R32G32B32A32_TYPELESS: DxgiFormat = DxgiFormat(1);
    pub(crate) const R32G32B32A32_FLOAT: DxgiFormat = DxgiFormat(2);
    pub(crate) const R32G32B32A32_UINT: DxgiFormat = DxgiFormat(3);
    pub(crate) const R32G32B32A32_SINT: DxgiFormat = DxgiFormat(4);
    pub(crate) const R32G32B32_TYPELESS: DxgiFormat = DxgiFormat(5);
    pub(crate) const R32G32B32_FLOAT: DxgiFormat = DxgiFormat(6);
    pub(crate) const R32G32B32_UINT: DxgiFormat = DxgiFormat(7);
    pub(crate) const R32G32B32_SINT: DxgiFormat = DxgiFormat(8);
    pub(crate) const R16G16B16A16_TYPELESS: DxgiFormat = DxgiFormat(9);
    pub(crate) const R16G16B16A16_FLOAT: DxgiFormat = DxgiFormat(10);
    pub(crate) const R16G16B16A16_UNORM: DxgiFormat = DxgiFormat(11);
    pub(crate) const R16G16B16A16_UINT: DxgiFormat = DxgiFormat(12);
    pub(crate) const R16G16B16A16_SNORM: DxgiFormat = DxgiFormat(13);
    pub(crate) const R16G16B16A16_SINT: DxgiFormat = DxgiFormat(14);
    pub(crate) const R32G32_TYPELESS: DxgiFormat = DxgiFormat(15);
    pub(crate) const R32G32_FLOAT: DxgiFormat = DxgiFormat(16);
    pub(crate) const R32G32_UINT: DxgiFormat = DxgiFormat(17);
    pub(crate) const R32G32_SINT: DxgiFormat = DxgiFormat(18);
    pub(crate) const R32G8X24_TYPELESS: DxgiFormat = DxgiFormat(19);
    pub(crate) const D32_FLOAT_S8X24_UINT: DxgiFormat = DxgiFormat(20);
    pub(crate) const R32_FLOAT_X8X24_TYPELESS: DxgiFormat = DxgiFormat(21);
    pub(crate) const X32_TYPELESS_G8X24_UINT: DxgiFormat = DxgiFormat(22);
    pub(crate) const R10G10B10A2_TYPELESS: DxgiFormat = DxgiFormat(23);
    pub(crate) const R10G10B10A2_UNORM: DxgiFormat = DxgiFormat(24);
    pub(crate) const R10G10B10A2_UINT: DxgiFormat = DxgiFormat(25);
    pub(crate) const R11G11B10_FLOAT: DxgiFormat = DxgiFormat(26);
    pub(crate) const R8G8B8A8_TYPELESS: DxgiFormat = DxgiFormat(27);
    pub(crate) const R8G8B8A8_UNORM: DxgiFormat = DxgiFormat(28);
    pub(crate) const R8G8B8A8_UNORM_SRGB: DxgiFormat = DxgiFormat(29);
    pub(crate) const R8G8B8A8_UINT: DxgiFormat = DxgiFormat(30);
    pub(crate) const R8G8B8A8_SNORM: DxgiFormat = DxgiFormat(31);
    pub(crate) const R8G8B8A8_SINT: DxgiFormat = DxgiFormat(32);
    pub(crate) const R16G16_TYPELESS: DxgiFormat = DxgiFormat(33);
    pub(crate) const R16G16_FLOAT: DxgiFormat = DxgiFormat(34);
    pub(crate) const R16G16_UNORM: DxgiFormat = DxgiFormat(35);
    pub(crate) const R16G16_UINT: DxgiFormat = DxgiFormat(36);
    pub(crate) const R16G16_SNORM: DxgiFormat = DxgiFormat(37);
    pub(crate) const R16G16_SINT: DxgiFormat = DxgiFormat(38);
    pub(crate) const R32_TYPELESS: DxgiFormat = DxgiFormat(39);
    pub(crate) const D32_FLOAT: DxgiFormat = DxgiFormat(40);
    pub(crate) const R32_FLOAT: DxgiFormat = DxgiFormat(41);
    pub(crate) const R32_UINT: DxgiFormat = DxgiFormat(42);
    pub(crate) const R32_SINT: DxgiFormat = DxgiFormat(43);
    pub(crate) const R24G8_TYPELESS: DxgiFormat = DxgiFormat(44);
    pub(crate) const D24_UNORM_S8_UINT: DxgiFormat = DxgiFormat(45);
    pub(crate) const R24_UNORM_X8_TYPELESS: DxgiFormat = DxgiFormat(46);
    pub(crate) const X24_TYPELESS_G8_UINT: DxgiFormat = DxgiFormat(47);
    pub(crate) const R8G8_TYPELESS: DxgiFormat = DxgiFormat(48);
    pub(crate) const R8G8_UNORM: DxgiFormat = DxgiFormat(49);
    pub(crate) const R8G8_UINT: DxgiFormat = DxgiFormat(50);
    pub(crate) const R8G8_SNORM: DxgiFormat = DxgiFormat(51);
    pub(crate) const R8G8_SINT: DxgiFormat = DxgiFormat(52);
    pub(crate) const R16_TYPELESS: DxgiFormat = DxgiFormat(53);
    pub(crate) const R16_FLOAT: DxgiFormat = DxgiFormat(54);
    pub(crate) const D16_UNORM: DxgiFormat = DxgiFormat(55);
    pub(crate) const R16_UNORM: DxgiFormat = DxgiFormat(56);
    pub(crate) const R16_UINT: DxgiFormat = DxgiFormat(57);
    pub(crate) const R16_SNORM: DxgiFormat = DxgiFormat(58);
    pub(crate) const R16_SINT: DxgiFormat = DxgiFormat(59);
    pub(crate) const R8_TYPELESS: DxgiFormat = DxgiFormat(60);
    pub(crate) const R8_UNORM: DxgiFormat = DxgiFormat(61);
    pub(crate) const R8_UINT: DxgiFormat = DxgiFormat(62);
    pub(crate) const R8_SNORM: DxgiFormat = DxgiFormat(63);
    pub(crate) const R8_SINT: DxgiFormat = DxgiFormat(64);
    pub(crate) const A8_UNORM: DxgiFormat = DxgiFormat(65);
    pub(crate) const R1_UNORM: DxgiFormat = DxgiFormat(66);
    pub(crate) const R9G9B9E5_SHAREDEXP: DxgiFormat = DxgiFormat(67);
    pub(crate) const R8G8_B8G8_UNORM: DxgiFormat = DxgiFormat(68);
    pub(crate) const G8R8_G8B8_UNORM: DxgiFormat = DxgiFormat(69);
    pub(crate) const BC1_TYPELESS: DxgiFormat = DxgiFormat(70);
    pub(crate) const BC1_UNORM: DxgiFormat = DxgiFormat(71);
    pub(crate) const BC1_UNORM_SRGB: DxgiFormat = DxgiFormat(72);
    pub(crate) const BC2_TYPELESS: DxgiFormat = DxgiFormat(73);
    pub(crate) const BC2_UNORM: DxgiFormat = DxgiFormat(74);
    pub(crate) const BC2_UNORM_SRGB: DxgiFormat = DxgiFormat(75);
    pub(crate) const BC3_TYPELESS: DxgiFormat = DxgiFormat(76);
    pub(crate) const BC3_UNORM: DxgiFormat = DxgiFormat(77);
    pub(crate) const BC3_UNORM_SRGB: DxgiFormat = DxgiFormat(78);
    pub(crate) const BC4_TYPELESS: DxgiFormat = DxgiFormat(79);
    pub(crate) const BC4_UNORM: DxgiFormat = DxgiFormat(80);
    pub(crate) const BC4_SNORM: DxgiFormat = DxgiFormat(81);
    pub(crate) const BC5_TYPELESS: DxgiFormat = DxgiFormat(82);
    pub(crate) const BC5_UNORM: DxgiFormat = DxgiFormat(83);
    pub(crate) const BC5_SNORM: DxgiFormat = DxgiFormat(84);
    pub(crate) const B5G6R5_UNORM: DxgiFormat = DxgiFormat(85);
    pub(crate) const B5G5R5A1_UNORM: DxgiFormat = DxgiFormat(86);
    pub(crate) const B8G8R8A8_UNORM: DxgiFormat = DxgiFormat(87);
    pub(crate) const B8G8R8X8_UNORM: DxgiFormat = DxgiFormat(88);
    pub(crate) const R10G10B10_XR_BIAS_A2_UNORM: DxgiFormat = DxgiFormat(89);
    pub(crate) const B8G8R8A8_TYPELESS: DxgiFormat = DxgiFormat(90);
    pub(crate) const B8G8R8A8_UNORM_SRGB: DxgiFormat = DxgiFormat(91);
    pub(crate) const B8G8R8X8_TYPELESS: DxgiFormat = DxgiFormat(92);
    pub(crate) const B8G8R8X8_UNORM_SRGB: DxgiFormat = DxgiFormat(93);
    pub(crate) const BC6H_TYPELESS: DxgiFormat = DxgiFormat(94);
    pub(crate) const BC6H_UF16: DxgiFormat = DxgiFormat(95);
    pub(crate) const BC6H_SF16: DxgiFormat = DxgiFormat(96);
    pub(crate) const BC7_TYPELESS: DxgiFormat = DxgiFormat(97);
    pub(crate) const BC7_UNORM: DxgiFormat = DxgiFormat(98);
    pub(crate) const BC7_UNORM_SRGB: DxgiFormat = DxgiFormat(99);
    pub(crate) const AYUV: DxgiFormat = DxgiFormat(100);
    pub(crate) const Y410: DxgiFormat = DxgiFormat(101);
    pub(crate) const Y416: DxgiFormat = DxgiFormat(102);
    pub(crate) const NV12: DxgiFormat = DxgiFormat(103);
    pub(crate) const P010: DxgiFormat = DxgiFormat(104);
    pub(crate) const P016: DxgiFormat = DxgiFormat(105);
    pub(crate) const OPAQUE_420: DxgiFormat = DxgiFormat(106);
    pub(crate) const YUY2: DxgiFormat = DxgiFormat(107);
    pub(crate) const Y210: DxgiFormat = DxgiFormat(108);
    pub(crate) const Y216: DxgiFormat = DxgiFormat(109);
    pub(crate) const NV11: DxgiFormat = DxgiFormat(110);
    pub(crate) const AI44: DxgiFormat = DxgiFormat(111);
    pub(crate) const IA44: DxgiFormat = DxgiFormat(112);
    pub(crate) const P8: DxgiFormat = DxgiFormat(113);
    pub(crate) const A8P8: DxgiFormat = DxgiFormat(114);
    pub(crate) const B4G4R4A4_UNORM: DxgiFormat = DxgiFormat(115);
    pub(crate) const P208: DxgiFormat = DxgiFormat(130);
    pub(crate) const V208: DxgiFormat = DxgiFormat(131);
    pub(crate) const V408: DxgiFormat = DxgiFormat(132);
}

/// Identifies the type of resource being used.
///
/// https://learn.microsoft.com/en-us/windows/win32/api/d3d10/ne-d3d10-d3d10_resource_dimension
/// https://learn.microsoft.com/en-us/windows/win32/api/d3d11/ne-d3d11-d3d11_resource_dimension
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub(crate) enum ResourceDimension {
    Unknown,
    Buffer,
    Texture1D,
    Texture2D,
    Texture3D,
}

impl ResourceDimension {
    fn from_u32(dimension: u32) -> Option<Self> {
        match dimension {
            0 => Some(Self::Unknown),
            1 => Some(Self::Buffer),
            2 => Some(Self::Texture1D),
            3 => Some(Self::Texture2D),
            4 => Some(Self::Texture3D),
            _ => None,
        }
    }
}
