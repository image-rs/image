use std::io::{Read, Seek, SeekFrom};

use crate::codecs::dds::convert::div_ceil;
use crate::color::ColorType;
use crate::error::{UnsupportedError, UnsupportedErrorKind};
use crate::{ImageDecoder, ImageError, ImageFormat, ImageResult};

use super::bc::{
    decode_bc1_block, decode_bc2_block, decode_bc3_block, decode_bc4_signed_block,
    decode_bc4_unsigned_block, decode_bc5_signed_block, decode_bc5_unsigned_block,
    decode_bc6_signed_block, decode_bc6_unsigned_block, decode_bc7_block,
};
use super::convert::{
    f10_to_f32, f11_to_f32, f16_to_f32, float3_to_bytes, float4_to_bytes, snorm16_to_unorm16,
    snorm8_to_unorm8, x10_to_x16, x1_to_x8, x2_to_x16, x4_to_x8, x5_to_x8, B5G6R5,
};

#[derive(Debug, Clone, Copy)]
#[allow(non_camel_case_types)]
pub(crate) enum SupportedFormat {
    // uncompressed formats
    R8G8B8_UNORM,
    B8G8R8_UNORM,
    R8G8B8A8_UNORM,
    R8G8B8A8_SNORM,
    B8G8R8A8_UNORM,
    B8G8R8X8_UNORM,
    B5G6R5_UNORM,
    B5G5R5A1_UNORM,
    B4G4R4A4_UNORM,
    R8_SNORM,
    R8_UNORM,
    R8G8_UNORM,
    R8G8_SNORM,
    A8_UNORM,
    R16_UNORM,
    R16_SNORM,
    R16G16_UNORM,
    R16G16_SNORM,
    R16G16B16A16_UNORM,
    R16G16B16A16_SNORM,
    R10G10B10A2_UNORM,
    R11G11B10_FLOAT,
    R9G9B9E5_SHAREDEXP,
    R16_FLOAT,
    R16G16_FLOAT,
    R16G16B16A16_FLOAT,
    R32_FLOAT,
    R32G32_FLOAT,
    R32G32B32_FLOAT,
    R32G32B32A32_FLOAT,
    R10G10B10_XR_BIAS_A2_UNORM,

    // sub-sampled formats
    R8G8_B8G8_UNORM,
    G8R8_G8B8_UNORM,

    // block compression formats
    BC1_ALPHA_UNORM,
    BC2_UNORM,
    BC3_UNORM,
    BC4_UNORM,
    BC4_SNORM,
    BC5_UNORM,
    BC5_SNORM,
    BC6H_UF16,
    BC6H_SF16,
    BC7_UNORM,
}

impl SupportedFormat {
    pub(crate) fn color_type(&self) -> ColorType {
        match self {
            Self::R8G8B8A8_UNORM
            | Self::R8G8B8A8_SNORM
            | Self::B8G8R8A8_UNORM
            | Self::B5G5R5A1_UNORM
            | Self::B4G4R4A4_UNORM
            | Self::BC1_ALPHA_UNORM
            | Self::BC2_UNORM
            | Self::BC3_UNORM
            | Self::BC7_UNORM => ColorType::Rgba8,
            Self::R8G8B8_UNORM
            | Self::B8G8R8_UNORM
            | Self::B8G8R8X8_UNORM
            | Self::B5G6R5_UNORM
            | Self::R8G8_UNORM
            | Self::R8G8_SNORM
            | Self::R8G8_B8G8_UNORM
            | Self::G8R8_G8B8_UNORM
            | Self::BC5_UNORM
            | Self::BC5_SNORM => ColorType::Rgb8,
            Self::R8_UNORM | Self::R8_SNORM | Self::BC4_UNORM | Self::BC4_SNORM => ColorType::L8,
            Self::A8_UNORM => ColorType::La8,
            Self::R16_UNORM | Self::R16_SNORM => ColorType::L16,
            Self::R16G16_UNORM | Self::R16G16_SNORM => ColorType::Rgb16,
            Self::R16G16B16A16_UNORM | Self::R16G16B16A16_SNORM | Self::R10G10B10A2_UNORM => {
                ColorType::Rgba16
            }
            Self::R11G11B10_FLOAT
            | Self::R9G9B9E5_SHAREDEXP
            | Self::R16_FLOAT
            | Self::R16G16_FLOAT
            | Self::R32_FLOAT
            | Self::R32G32_FLOAT
            | Self::R32G32B32_FLOAT
            | Self::BC6H_SF16
            | Self::BC6H_UF16 => ColorType::Rgb32F,
            Self::R16G16B16A16_FLOAT
            | Self::R32G32B32A32_FLOAT
            | Self::R10G10B10_XR_BIAS_A2_UNORM => ColorType::Rgba32F,
        }
    }

    fn get_surface_bytes(&self, size: Size) -> usize {
        let pixels = size.width * size.height;

        match self {
            // 1 bytes per pixel
            Self::R8_UNORM | Self::R8_SNORM | Self::A8_UNORM => pixels,
            // 2 bytes per pixel
            Self::B5G6R5_UNORM
            | Self::B5G5R5A1_UNORM
            | Self::B4G4R4A4_UNORM
            | Self::R8G8_UNORM
            | Self::R8G8_SNORM
            | Self::R16_UNORM
            | Self::R16_SNORM
            | Self::R16_FLOAT => 2 * pixels,
            // 3 bytes per pixel
            Self::R8G8B8_UNORM | Self::B8G8R8_UNORM => 3 * pixels,
            // 4 bytes per pixel
            Self::R8G8B8A8_UNORM
            | Self::R8G8B8A8_SNORM
            | Self::B8G8R8A8_UNORM
            | Self::B8G8R8X8_UNORM
            | Self::R16G16_UNORM
            | Self::R16G16_SNORM
            | Self::R10G10B10A2_UNORM
            | Self::R11G11B10_FLOAT
            | Self::R9G9B9E5_SHAREDEXP
            | Self::R16G16_FLOAT
            | Self::R32_FLOAT
            | Self::R10G10B10_XR_BIAS_A2_UNORM => 4 * pixels,
            // 8 bytes per pixel
            Self::R16G16B16A16_UNORM
            | Self::R16G16B16A16_SNORM
            | Self::R16G16B16A16_FLOAT
            | Self::R32G32_FLOAT => 8 * pixels,
            // 12 bytes per pixel
            Self::R32G32B32_FLOAT => 8 * pixels,
            // 16 bytes per pixel
            Self::R32G32B32A32_FLOAT => 8 * pixels,

            // sub-sampled formats
            Self::R8G8_B8G8_UNORM | Self::G8R8_G8B8_UNORM => {
                // 4 bytes per one 2x1 block
                let blocks_x = div_ceil(size.width, 2);
                let blocks_y = size.height;
                4 * blocks_x * blocks_y
            }

            // block compression formats
            Self::BC1_ALPHA_UNORM | Self::BC4_UNORM | Self::BC4_SNORM => {
                // 8 bytes per one 4x4 block
                let blocks_x = (size.width + 3) / 4;
                let blocks_y = (size.height + 3) / 4;
                8 * blocks_x * blocks_y
            }
            Self::BC2_UNORM
            | Self::BC3_UNORM
            | Self::BC5_UNORM
            | Self::BC5_SNORM
            | Self::BC6H_SF16
            | Self::BC6H_UF16
            | Self::BC7_UNORM => {
                // 16 bytes per one 4x4 block
                let blocks_x = div_ceil(size.width, 4);
                let blocks_y = div_ceil(size.height, 4);
                16 * blocks_x * blocks_y
            }
        }
    }

    /// Reads a single surface from `r` and writes it to `buf`.
    ///
    /// `buf` is assumed to be aligned to the color type.
    fn read_surface(&self, r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
        if size.is_empty() {
            // ensure that decoders don't have to deal with empty images
            return Ok(());
        }

        match self {
            // uncompressed formats
            Self::R8G8B8_UNORM => decode_R8G8B8_UNORM(r, size, buf),
            Self::B8G8R8_UNORM => decode_B8G8R8_UNORM(r, size, buf),
            Self::R8G8B8A8_UNORM => decode_R8G8B8A8_UNORM(r, size, buf),
            Self::R8G8B8A8_SNORM => decode_R8G8B8A8_SNORM(r, size, buf),
            Self::B8G8R8A8_UNORM => decode_B8G8R8A8_UNORM(r, size, buf),
            Self::B8G8R8X8_UNORM => decode_B8G8R8X8_UNORM(r, size, buf),
            Self::B5G6R5_UNORM => decode_B5G6R5_UNORM(r, size, buf),
            Self::B5G5R5A1_UNORM => decode_B5G5R5A1_UNORM(r, size, buf),
            Self::B4G4R4A4_UNORM => decode_B4G4R4A4_UNORM(r, size, buf),
            Self::R8_UNORM => decode_R8_UNORM(r, size, buf),
            Self::R8_SNORM => decode_R8_SNORM(r, size, buf),
            Self::R8G8_UNORM => decode_R8G8_UNORM(r, size, buf),
            Self::R8G8_SNORM => decode_R8G8_SNORM(r, size, buf),
            Self::A8_UNORM => decode_A8_UNORM(r, size, buf),
            Self::R16_UNORM => decode_R16_UNORM(r, size, buf),
            Self::R16_SNORM => decode_R16_SNORM(r, size, buf),
            Self::R16G16_UNORM => decode_R16G16_UNORM(r, size, buf),
            Self::R16G16_SNORM => decode_R16G16_SNORM(r, size, buf),
            Self::R16G16B16A16_UNORM => decode_R16G16B16A16_UNORM(r, size, buf),
            Self::R16G16B16A16_SNORM => decode_R16G16B16A16_SNORM(r, size, buf),
            Self::R10G10B10A2_UNORM => decode_R10G10B10A2_UNORM(r, size, buf),
            Self::R11G11B10_FLOAT => decode_R11G11B10_FLOAT(r, size, buf),
            Self::R9G9B9E5_SHAREDEXP => decode_R9G9B9E5_SHAREDEXP(r, size, buf),
            Self::R16_FLOAT => decode_R16_FLOAT(r, size, buf),
            Self::R16G16_FLOAT => decode_R16G16_FLOAT(r, size, buf),
            Self::R16G16B16A16_FLOAT => decode_R16G16B16A16_FLOAT(r, size, buf),
            Self::R32_FLOAT => decode_R32_FLOAT(r, size, buf),
            Self::R32G32_FLOAT => decode_R32G32_FLOAT(r, size, buf),
            Self::R32G32B32_FLOAT => decode_R32G32B32_FLOAT(r, size, buf),
            Self::R32G32B32A32_FLOAT => decode_R32G32B32A32_FLOAT(r, size, buf),
            Self::R10G10B10_XR_BIAS_A2_UNORM => decode_R10G10B10_XR_BIAS_A2_UNORM(r, size, buf),

            // sub-sampled formats
            Self::R8G8_B8G8_UNORM => decode_R8G8_B8G8_UNORM(r, size, buf),
            Self::G8R8_G8B8_UNORM => decode_G8R8_G8B8_UNORM(r, size, buf),

            // block compression formats
            Self::BC1_ALPHA_UNORM => decode_BC1_ALPHA_UNORM(r, size, buf),
            Self::BC2_UNORM => decode_BC2_UNORM(r, size, buf),
            Self::BC3_UNORM => decode_BC3_UNORM(r, size, buf),
            Self::BC4_UNORM => decode_BC4_UNORM(r, size, buf),
            Self::BC4_SNORM => decode_BC4_SNORM(r, size, buf),
            Self::BC5_UNORM => decode_BC5_UNORM(r, size, buf),
            Self::BC5_SNORM => decode_BC5_SNORM(r, size, buf),
            Self::BC6H_UF16 => decode_BC6H_UF16(r, size, buf),
            Self::BC6H_SF16 => decode_BC6H_SF16(r, size, buf),
            Self::BC7_UNORM => decode_BC7_UNORM(r, size, buf),
        }
    }
}

pub(crate) struct DX10Decoder<R> {
    pub(crate) width: u32,
    pub(crate) height: u32,
    pub(crate) format: SupportedFormat,
    pub(crate) cube: bool,
    /// The number of mipmap levels including the top level.
    pub(crate) mip_count: u32,
    pub(crate) inner: R,
}

impl<R: Read + Seek> DX10Decoder<R> {
    pub(crate) fn get_cube_size(width: u32, height: u32) -> Option<(u32, u32)> {
        Some((width.checked_mul(4)?, height.checked_mul(3)?))
    }

    fn read_surface(&mut self, buf: &mut [u8]) -> ImageResult<()> {
        let r: &mut dyn Read = &mut self.inner;
        let size = Size {
            width: self.width as usize,
            height: self.height as usize,
        };

        self.format.read_surface(r, size, buf)
    }

    fn read_cube(&mut self, cube_buf: &mut [u8]) -> ImageResult<()> {
        // zero-out the buffer to make it transparent
        cube_buf.fill(0);

        let w = self.width as usize;
        let h = self.height as usize;

        let surface_color = self.format.color_type();

        let surface_channels = surface_color.channel_count() as usize;

        let face_offsets =
            [(2, 1), (0, 1), (1, 0), (1, 2), (1, 1), (3, 1)].map(|(x, y)| (x * w, y * h));

        match surface_color {
            ColorType::L8 | ColorType::La8 | ColorType::Rgb8 | ColorType::Rgba8 => {
                let mut temp_buf = vec![0u8; w * h * surface_channels];

                #[allow(clippy::needless_range_loop)]
                for face_index in 0..6 {
                    self.read_surface(bytemuck::cast_slice_mut(temp_buf.as_mut()))?;
                    self.skip_mips()?;

                    let offset = face_offsets[face_index];
                    self.write_cube_face(cube_buf, &temp_buf, offset, 0xFF)?;
                }
            }
            ColorType::L16 | ColorType::La16 | ColorType::Rgb16 | ColorType::Rgba16 => {
                let buf: &mut [[u8; 2]] = bytemuck::cast_slice_mut(cube_buf);
                let mut temp_buf = vec![0u16; w * h * surface_channels];

                #[allow(clippy::needless_range_loop)]
                for face_index in 0..6 {
                    self.read_surface(bytemuck::cast_slice_mut(temp_buf.as_mut()))?;
                    self.skip_mips()?;

                    let offset = face_offsets[face_index];
                    self.write_cube_face(
                        buf,
                        bytemuck::cast_slice(&temp_buf),
                        offset,
                        [0xFF, 0xFF],
                    )?;
                }
            }
            ColorType::Rgb32F | ColorType::Rgba32F => {
                let buf: &mut [[u8; 4]] = bytemuck::cast_slice_mut(cube_buf);
                let mut temp_buf = vec![0.0_f32; w * h * surface_channels];

                #[allow(clippy::needless_range_loop)]
                for face_index in 0..6 {
                    self.read_surface(bytemuck::cast_slice_mut(temp_buf.as_mut()))?;
                    self.skip_mips()?;

                    let offset = face_offsets[face_index];
                    self.write_cube_face(
                        buf,
                        bytemuck::cast_slice(&temp_buf),
                        offset,
                        1.0_f32.to_ne_bytes(),
                    )?;
                }
            }
            #[allow(unreachable_patterns)]
            _ => {
                return Err(ImageError::Unsupported(
                    UnsupportedError::from_format_and_kind(
                        ImageFormat::Dds.into(),
                        UnsupportedErrorKind::GenericFeature(format!(
                            "Cubemaps are not supported for {:?} formats",
                            surface_color
                        )),
                    ),
                ));
            }
        }

        Ok(())
    }
    fn write_cube_face<T: Copy>(
        &mut self,
        cube_buf: &mut [T],
        surface_buf: &[T],
        face_offset: (usize, usize),
        alpha: T,
    ) -> ImageResult<()> {
        let w = self.width as usize;
        let h = self.height as usize;

        let surface_color = self.format.color_type();
        let cube_color = Self::get_cube_color(surface_color);

        let surface_channels = surface_color.channel_count() as usize;
        let cube_channels = cube_color.channel_count() as usize;

        let (x_offset, y_offset) = face_offset;

        for y in 0..h {
            let src = y * w;
            let dst = (y + y_offset) * w * 4 + x_offset;
            let src = &surface_buf[(src * surface_channels)..((src + w) * surface_channels)];
            let dst = &mut cube_buf[(dst * cube_channels)..((dst + w) * cube_channels)];

            if surface_channels == cube_channels {
                // just copy the line
                dst.copy_from_slice(src);
            } else {
                match (surface_channels, cube_channels) {
                    (1, 2) => {
                        for (&src, dst) in src.iter().zip(dst.chunks_exact_mut(2)) {
                            dst[0] = src;
                            dst[1] = alpha;
                        }
                    }
                    (3, 4) => {
                        for (src, dst) in src.chunks_exact(3).zip(dst.chunks_exact_mut(4)) {
                            dst[0] = src[0];
                            dst[1] = src[1];
                            dst[2] = src[2];
                            dst[3] = alpha;
                        }
                    }
                    _ => {
                        return Err(ImageError::Unsupported(
                            UnsupportedError::from_format_and_kind(
                                ImageFormat::Dds.into(),
                                UnsupportedErrorKind::GenericFeature(format!(
                                    "Cubemap's surface and final color type are not compatible. {:?} and {:?} as not compatible",
                                    surface_color, cube_color
                                )),
                            ),
                        ));
                    }
                }
            }
        }

        Ok(())
    }

    fn skip_mips(&mut self) -> ImageResult<()> {
        if self.mip_count < 2 {
            // no mips
            return Ok(());
        }

        let size = Size {
            width: self.width as usize,
            height: self.height as usize,
        };
        let mut total_bytes = 0;
        for level in 1..self.mip_count {
            total_bytes += self.format.get_surface_bytes(size.get_mip(level));
        }

        self.inner.seek(SeekFrom::Current(total_bytes as i64))?;

        Ok(())
    }

    fn get_cube_color(color: ColorType) -> ColorType {
        // add alpha
        match color {
            ColorType::L8 => ColorType::La8,
            ColorType::L16 => ColorType::La16,
            ColorType::Rgb8 => ColorType::Rgba8,
            ColorType::Rgb16 => ColorType::Rgba16,
            ColorType::Rgb32F => ColorType::Rgba32F,
            _ => color,
        }
    }
}

impl<R: Read + Seek> ImageDecoder for DX10Decoder<R> {
    fn dimensions(&self) -> (u32, u32) {
        if self.cube {
            Self::get_cube_size(self.width, self.height).expect("cube size should fit within u32")
        } else {
            (self.width, self.height)
        }
    }

    fn color_type(&self) -> ColorType {
        let color = self.format.color_type();
        if self.cube {
            Self::get_cube_color(color)
        } else {
            color
        }
    }

    fn read_image(mut self, buf: &mut [u8]) -> ImageResult<()> {
        if self.cube {
            self.read_cube(buf)
        } else {
            self.read_surface(buf)
        }
    }

    fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()> {
        (*self).read_image(buf)
    }
}

#[derive(Debug, Clone, Copy)]
struct Size {
    width: usize,
    height: usize,
}
impl Size {
    fn is_empty(&self) -> bool {
        self.width == 0 || self.height == 0
    }

    fn check(self, buf: &[u8], color: ColorType) {
        let total_bytes = self
            .width
            .saturating_mul(self.height)
            .saturating_mul(color.bytes_per_pixel() as usize);

        assert_eq!(buf.len(), total_bytes);
    }

    fn get_mip(self, level: u32) -> Self {
        Self {
            width: (self.width >> level).max(1),
            height: (self.height >> level).max(1),
        }
    }
}

#[inline(always)]
fn by_row_8<const N: usize, const M: usize>(
    r: &mut dyn Read,
    size: Size,
    buf: &mut [u8],
    color: ColorType,
    process_pixel: impl Fn([u8; N]) -> [u8; M],
) -> ImageResult<()>
where
    [u8; N]: bytemuck::Pod,
    [u8; M]: bytemuck::Pod,
{
    assert_eq!(color.bytes_per_pixel() as usize, M);
    size.check(buf, color);

    let buf: &mut [[u8; M]] = bytemuck::cast_slice_mut(buf);

    let mut row = vec![[0u8; N]; size.width];
    for buf in buf.chunks_exact_mut(size.width) {
        r.read_exact(bytemuck::cast_slice_mut(row.as_mut()))?;
        for (pixel, out) in row.iter().zip(buf) {
            *out = process_pixel(*pixel);
        }
    }
    Ok(())
}
#[inline(always)]
fn by_row_16<const N: usize, const M: usize>(
    r: &mut dyn Read,
    size: Size,
    buf: &mut [u8],
    color: ColorType,
    process_pixel: impl Fn([u16; N]) -> [u16; M],
) -> ImageResult<()>
where
    [[u8; 2]; N]: bytemuck::Pod,
    [u16; M]: bytemuck::Pod + Default,
{
    size.check(buf, color);
    let bytes_per_pixel = color.bytes_per_pixel() as usize;
    assert_eq!(bytes_per_pixel, M * 2);

    let mut row = vec![[[0_u8; 2]; N]; size.width];
    let mut aligned_buf: Option<Vec<[u16; M]>> = None;
    for buf in buf.chunks_exact_mut(size.width * bytes_per_pixel) {
        r.read_exact(bytemuck::cast_slice_mut(row.as_mut()))?;

        write_to_aligned_buffer(buf, &mut aligned_buf, |buf| {
            for (pixel, out) in row.iter().zip(buf) {
                *out = process_pixel(pixel.map(u16::from_le_bytes));
            }
        });
    }
    Ok(())
}
#[inline(always)]
fn by_row_32<const N: usize, const M: usize>(
    r: &mut dyn Read,
    size: Size,
    buf: &mut [u8],
    color: ColorType,
    process_pixel: impl Fn([u32; N]) -> [u32; M],
) -> ImageResult<()>
where
    [[u8; 4]; N]: bytemuck::Pod,
    [u32; M]: bytemuck::Pod + Default,
{
    size.check(buf, color);
    let bytes_per_pixel = color.bytes_per_pixel() as usize;
    assert_eq!(bytes_per_pixel, M * 4);

    let mut row = vec![[[0u8; 4]; N]; size.width];
    let mut aligned_buf: Option<Vec<[u32; M]>> = None;
    for buf in buf.chunks_exact_mut(size.width * bytes_per_pixel) {
        r.read_exact(bytemuck::cast_slice_mut(row.as_mut()))?;

        write_to_aligned_buffer(buf, &mut aligned_buf, |buf| {
            for (pixel, out) in row.iter().zip(buf) {
                *out = process_pixel(pixel.map(u32::from_le_bytes));
            }
        });
    }
    Ok(())
}
fn write_to_aligned_buffer<T>(
    buf: &mut [u8],
    temp_buf: &mut Option<Vec<T>>,
    write: impl FnOnce(&mut [T]),
) where
    T: bytemuck::Pod + Default + Copy,
{
    if let Ok(buf) = bytemuck::try_cast_slice_mut(buf) {
        // the buffer is aligned already
        write(buf);
    } else {
        // use a temporary buffer and copy over
        let size = std::mem::size_of::<T>();
        assert_eq!(buf.len() % size, 0);
        let len = buf.len() / size;

        let temp = if let Some(temp) = temp_buf {
            assert_eq!(temp.len(), len);
            temp
        } else {
            let temp = vec![T::default(); len];
            *temp_buf = Some(temp);
            // we just assigned a value, so unwrap is okay
            temp_buf.as_mut().unwrap()
        };

        write(temp);

        buf.copy_from_slice(bytemuck::cast_slice(temp));
    }
}

#[allow(non_snake_case)]
fn decode_R8G8B8_UNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    size.check(buf, ColorType::Rgb8);

    r.read_exact(buf)?;
    Ok(())
}
#[allow(non_snake_case)]
fn decode_B8G8R8_UNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_row_8(r, size, buf, ColorType::Rgb8, |[b, g, r]| [r, g, b])
}
#[allow(non_snake_case)]
fn decode_R8G8B8A8_UNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    size.check(buf, ColorType::Rgba8);

    r.read_exact(buf)?;
    Ok(())
}
#[allow(non_snake_case)]
fn decode_R8G8B8A8_SNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    size.check(buf, ColorType::Rgba8);

    r.read_exact(buf)?;
    buf.iter_mut().for_each(|p| *p = snorm8_to_unorm8(*p));
    Ok(())
}
#[allow(non_snake_case)]
fn decode_B8G8R8A8_UNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    size.check(buf, ColorType::Rgba8);

    // read everything in BGRA order
    r.read_exact(buf)?;
    // swap R and B
    for i in (0..buf.len()).step_by(4) {
        buf.swap(i, i + 2);
    }
    Ok(())
}
#[allow(non_snake_case)]
fn decode_B8G8R8X8_UNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_row_8(r, size, buf, ColorType::Rgb8, |[b, g, r, _x]| [r, g, b])
}
#[allow(non_snake_case)]
fn decode_B5G6R5_UNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_row_8(r, size, buf, ColorType::Rgb8, |bytes| {
        B5G6R5::from_le_bytes(bytes).to_rgb8()
    })
}
#[allow(non_snake_case)]
fn decode_B5G5R5A1_UNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_row_8(r, size, buf, ColorType::Rgba8, |bytes| {
        let bgra = u16::from_le_bytes(bytes);
        let b5 = bgra & 0x1F;
        let g5 = (bgra >> 5) & 0x1F;
        let r5 = (bgra >> 10) & 0x1F;
        let a1 = (bgra >> 15) & 0x1;

        [x5_to_x8(r5), x5_to_x8(g5), x5_to_x8(b5), x1_to_x8(a1)]
    })
}
#[allow(non_snake_case)]
fn decode_B4G4R4A4_UNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_row_8(r, size, buf, ColorType::Rgba8, |[low, high]| {
        let b4 = low & 0xF;
        let g4 = (low >> 4) & 0xF;
        let r4 = high & 0xF;
        let a4 = (high >> 4) & 0xF;

        [x4_to_x8(r4), x4_to_x8(g4), x4_to_x8(b4), x4_to_x8(a4)]
    })
}
#[allow(non_snake_case)]
fn decode_R8_UNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    size.check(buf, ColorType::L8);

    r.read_exact(buf)?;
    Ok(())
}
#[allow(non_snake_case)]
fn decode_R8_SNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    size.check(buf, ColorType::L8);

    r.read_exact(buf)?;
    buf.iter_mut().for_each(|p| *p = snorm8_to_unorm8(*p));
    Ok(())
}
#[allow(non_snake_case)]
fn decode_R8G8_UNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_row_8(r, size, buf, ColorType::Rgb8, |[r, g]| [r, g, 0])
}
#[allow(non_snake_case)]
fn decode_R8G8_SNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_row_8(r, size, buf, ColorType::Rgb8, |[r, g]| {
        [snorm8_to_unorm8(r), snorm8_to_unorm8(g), 128]
    })
}
#[allow(non_snake_case)]
fn decode_A8_UNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_row_8(r, size, buf, ColorType::La8, |[a]| [0, a])
}

#[allow(non_snake_case)]
fn decode_R16_UNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    size.check(buf, ColorType::L16);

    // read everything in LE order
    r.read_exact(buf)?;
    fix_endian_u16(buf);

    Ok(())
}
#[allow(non_snake_case)]
fn decode_R16G16B16A16_UNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    size.check(buf, ColorType::Rgba16);

    // read everything in LE order
    r.read_exact(buf)?;
    fix_endian_u16(buf);

    Ok(())
}
#[allow(non_snake_case)]
fn decode_R16G16_UNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_row_16(r, size, buf, ColorType::Rgb16, |[r, g]| [r, g, 0])
}
#[allow(non_snake_case)]
fn decode_R10G10B10A2_UNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_row_16(r, size, buf, ColorType::Rgba16, |[low, high]| {
        let rgba: u32 = (high as u32) << 16 | low as u32;
        let r10 = rgba & 0x3FF;
        let g10 = (rgba >> 10) & 0x3FF;
        let b10 = (rgba >> 20) & 0x3FF;
        let a2 = (rgba >> 30) & 0x3;

        [
            x10_to_x16(r10),
            x10_to_x16(g10),
            x10_to_x16(b10),
            x2_to_x16(a2),
        ]
    })
}
#[allow(non_snake_case)]
fn decode_R16_SNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_row_16(r, size, buf, ColorType::L16, |[r]| [snorm16_to_unorm16(r)])
}
#[allow(non_snake_case)]
fn decode_R16G16B16A16_SNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_row_16(r, size, buf, ColorType::Rgba16, |[r, g, b, a]| {
        [
            snorm16_to_unorm16(r),
            snorm16_to_unorm16(g),
            snorm16_to_unorm16(b),
            snorm16_to_unorm16(a),
        ]
    })
}
#[allow(non_snake_case)]
fn decode_R16G16_SNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_row_16(r, size, buf, ColorType::Rgb16, |[r, g]| {
        [snorm16_to_unorm16(r), snorm16_to_unorm16(g), 32768]
    })
}

#[allow(non_snake_case)]
fn decode_R11G11B10_FLOAT(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_row_32(r, size, buf, ColorType::Rgb32F, |[rgb]| {
        let r = f11_to_f32((rgb & 0x7FF) as u16).to_bits();
        let g = f11_to_f32(((rgb >> 11) & 0x7FF) as u16).to_bits();
        let b = f10_to_f32(((rgb >> 22) & 0x3FF) as u16).to_bits();
        [r, g, b]
    })
}
#[allow(non_snake_case)]
fn decode_R9G9B9E5_SHAREDEXP(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_row_32(r, size, buf, ColorType::Rgb32F, |[rgb]| {
        let r_mant = rgb & 0x1FF;
        let g_mant = (rgb >> 9) & 0x1FF;
        let b_mant = (rgb >> 18) & 0x1FF;
        let exp = (rgb >> 27) & 0x1F;

        fn to_f32(e5: u32, m9: u32) -> f32 {
            // based on f16_to_f32
            if e5 == 0 {
                // denorm
                m9 as f32 * 2.0_f32.powi(-23)
            } else if e5 != 31 {
                m9 as f32 * 2.0_f32.powi(e5 as i32 - 24)
            } else if m9 == 0 {
                f32::INFINITY
            } else {
                f32::NAN
            }
        }

        let r = to_f32(exp, r_mant).to_bits();
        let g = to_f32(exp, g_mant).to_bits();
        let b = to_f32(exp, b_mant).to_bits();

        [r, g, b]
    })
}
#[allow(non_snake_case)]
fn decode_R10G10B10_XR_BIAS_A2_UNORM(
    r: &mut dyn Read,
    size: Size,
    buf: &mut [u8],
) -> ImageResult<()> {
    by_row_32(r, size, buf, ColorType::Rgba32F, |[rgba]| {
        // Do not ask me why, but this format is really weird. This is what
        // the docs say about it:
        //   A four-component, 32-bit 2.8-biased fixed-point format that supports
        //   10 bits for each color channel and 2-bit alpha.
        //
        // 2.8 fixed-point means that the value is stored as a 10-bit integer,
        // with 8 bits of fraction. So we have values between 0.0 and 4.0
        // (exclusive). But that would be too easy. For a given stored value x,
        // the actual value is (x-1.5)/2.0. So the actual range is -0.75 to 1.25.
        //
        // I have no idea why, but that's how it works. Also, you won't really
        // find more information about this online. I had to reverse-engineer
        // a known image to figure this out.

        let r_fixed = rgba & 0x3FF;
        let g_fixed = (rgba >> 10) & 0x3FF;
        let b_fixed = (rgba >> 20) & 0x3FF;
        let a_2 = (rgba >> 30) & 0x3;

        fn to_f32(fixed: u32) -> f32 {
            // 0b01_1000_0000 == 1.5
            (fixed as i32 - 0b01_1000_0000) as f32 / 255.0 / 2.0
        }

        let r = to_f32(r_fixed).to_bits();
        let g = to_f32(g_fixed).to_bits();
        let b = to_f32(b_fixed).to_bits();

        [r, g, b, (a_2 as f32 / 3.0).to_bits()]
    })
}

#[allow(non_snake_case)]
fn decode_R16_FLOAT(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_row_8(r, size, buf, ColorType::Rgb32F, |bytes: [u8; 2]| {
        let r = f16_to_f32(u16::from_le_bytes(bytes));
        float3_to_bytes([r, r, r])
    })
}
#[allow(non_snake_case)]
fn decode_R16G16_FLOAT(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_row_8(r, size, buf, ColorType::Rgb32F, |bytes: [u8; 4]| {
        let shorts: [[u8; 2]; 2] = bytemuck::cast(bytes);
        let [r, g] = shorts.map(u16::from_le_bytes).map(f16_to_f32);
        float3_to_bytes([r, g, 0.0])
    })
}
#[allow(non_snake_case)]
fn decode_R16G16B16A16_FLOAT(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_row_8(r, size, buf, ColorType::Rgba32F, |bytes: [u8; 8]| {
        let shorts: [[u8; 2]; 4] = bytemuck::cast(bytes);
        let floats = shorts.map(u16::from_le_bytes).map(f16_to_f32);
        float4_to_bytes(floats)
    })
}

#[allow(non_snake_case)]
fn decode_R32_FLOAT(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_row_32(r, size, buf, ColorType::Rgb32F, |[r]| [r, r, r])
}
#[allow(non_snake_case)]
fn decode_R32G32_FLOAT(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_row_32(r, size, buf, ColorType::Rgb32F, |[r, g]| [r, g, 0])
}
#[allow(non_snake_case)]
fn decode_R32G32B32_FLOAT(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    size.check(buf, ColorType::Rgb32F);

    // read everything in LE order
    r.read_exact(buf)?;
    fix_endian_u32(buf);

    Ok(())
}
#[allow(non_snake_case)]
fn decode_R32G32B32A32_FLOAT(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    size.check(buf, ColorType::Rgba32F);

    // read everything in LE order
    r.read_exact(buf)?;
    fix_endian_u32(buf);

    Ok(())
}

fn by_row_8_sub_sample(
    r: &mut dyn Read,
    size: Size,
    mut buf: &mut [u8],
    process_pixel1: impl Fn(&mut [u8], [u8; 4]),
    process_pixel2: impl Fn(&mut [u8], [u8; 4]),
) -> ImageResult<()> {
    size.check(buf, ColorType::Rgb8);
    const BYTES_PER_PIXEL: usize = 3;

    let row_pixels = div_ceil(size.width, 2);
    assert_ne!(row_pixels, 0);

    let mut row = vec![[0u8; 4]; row_pixels];
    for _ in 0..size.height {
        r.read_exact(bytemuck::cast_slice_mut(row.as_mut()))?;

        // the following loop is split to avoid a branch in the inner loop

        // everything but the last pixel(s)
        for (i, pixel) in row[0..(row.len() - 1)].iter().enumerate() {
            let buf_i1 = i * BYTES_PER_PIXEL * 2;
            let buf_i2 = buf_i1 + BYTES_PER_PIXEL;
            process_pixel1(&mut buf[buf_i1..(buf_i1 + BYTES_PER_PIXEL)], *pixel);
            process_pixel2(&mut buf[buf_i2..(buf_i2 + BYTES_PER_PIXEL)], *pixel);
        }

        // last pixel(s)
        {
            let i = row.len() - 1;
            let pixel = row[i];
            let buf_i1 = i * BYTES_PER_PIXEL * 2;
            let buf_i2 = buf_i1 + BYTES_PER_PIXEL;
            process_pixel1(&mut buf[buf_i1..(buf_i1 + BYTES_PER_PIXEL)], pixel);
            if i * 2 + 1 < size.width {
                process_pixel2(&mut buf[buf_i2..(buf_i2 + BYTES_PER_PIXEL)], pixel);
            }
        }

        buf = &mut buf[(size.width * BYTES_PER_PIXEL)..];
    }
    Ok(())
}
#[allow(non_snake_case)]
fn decode_R8G8_B8G8_UNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_row_8_sub_sample(
        r,
        size,
        buf,
        |buf, [r, g1, b, _g2]| {
            buf[0] = r;
            buf[1] = g1;
            buf[2] = b;
        },
        |buf, [r, _g1, b, g2]| {
            buf[0] = r;
            buf[1] = g2;
            buf[2] = b;
        },
    )
}
#[allow(non_snake_case)]
fn decode_G8R8_G8B8_UNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_row_8_sub_sample(
        r,
        size,
        buf,
        |buf, [g1, r, _g2, b]| {
            buf[0] = r;
            buf[1] = g1;
            buf[2] = b;
        },
        |buf, [_g1, r, g2, b]| {
            buf[0] = r;
            buf[1] = g2;
            buf[2] = b;
        },
    )
}

fn by_4x4_block<const BYTES_PER_BLOCK: usize, const BYTES_PER_PIXEL: usize>(
    r: &mut dyn Read,
    size: Size,
    mut buf: &mut [u8],
    color: ColorType,
    process_block: impl Fn([u8; BYTES_PER_BLOCK]) -> [[u8; BYTES_PER_PIXEL]; 16],
) -> ImageResult<()>
where
    [u8; BYTES_PER_BLOCK]: bytemuck::Pod,
    [u8; BYTES_PER_PIXEL]: bytemuck::Pod,
{
    size.check(buf, color);
    assert_eq!(BYTES_PER_PIXEL, color.bytes_per_pixel() as usize);

    const BLOCK_SIZE: usize = 4;
    fn get_blocks_size(coord: usize, image_size: usize) -> usize {
        if coord + BLOCK_SIZE <= image_size {
            BLOCK_SIZE
        } else {
            image_size - coord
        }
    }

    let row_blocks = div_ceil(size.width, BLOCK_SIZE);

    let mut row = vec![[0u8; BYTES_PER_BLOCK]; row_blocks];
    for y in (0..size.height).step_by(BLOCK_SIZE) {
        r.read_exact(bytemuck::cast_slice_mut(row.as_mut()))?;

        let block_h = get_blocks_size(y, size.height);
        for (block_i, block) in row.iter().enumerate() {
            let x = block_i * BLOCK_SIZE;
            let block = process_block(*block);
            let block_u8: &[u8] = bytemuck::cast_slice(&block);

            let block_w = get_blocks_size(x, size.width);
            for y in 0..block_h {
                for x in x..(x + block_w) {
                    let buf_i = (y * size.width + x) * BYTES_PER_PIXEL;
                    let block_i = (y * BLOCK_SIZE + (x % BLOCK_SIZE)) * BYTES_PER_PIXEL;
                    buf[buf_i..(buf_i + BYTES_PER_PIXEL)]
                        .copy_from_slice(&block_u8[block_i..(block_i + BYTES_PER_PIXEL)]);
                }
            }
        }

        if block_h == 4 {
            buf = &mut buf[(size.width * BYTES_PER_PIXEL * 4)..];
        }
    }
    Ok(())
}
#[allow(non_snake_case)]
fn decode_BC1_ALPHA_UNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_4x4_block(r, size, buf, ColorType::Rgba8, decode_bc1_block)
}
#[allow(non_snake_case)]
fn decode_BC2_UNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_4x4_block(r, size, buf, ColorType::Rgba8, decode_bc2_block)
}
#[allow(non_snake_case)]
fn decode_BC3_UNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_4x4_block(r, size, buf, ColorType::Rgba8, decode_bc3_block)
}
#[allow(non_snake_case)]
fn decode_BC4_UNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_4x4_block(r, size, buf, ColorType::L8, decode_bc4_unsigned_block)
}
#[allow(non_snake_case)]
fn decode_BC4_SNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_4x4_block(r, size, buf, ColorType::L8, decode_bc4_signed_block)
}
#[allow(non_snake_case)]
fn decode_BC5_UNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_4x4_block(r, size, buf, ColorType::Rgb8, decode_bc5_unsigned_block)
}
#[allow(non_snake_case)]
fn decode_BC5_SNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_4x4_block(r, size, buf, ColorType::Rgb8, decode_bc5_signed_block)
}
#[allow(non_snake_case)]
fn decode_BC6H_UF16(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_4x4_block(r, size, buf, ColorType::Rgb32F, |block_bytes| {
        decode_bc6_unsigned_block(block_bytes).map(float3_to_bytes)
    })
}
#[allow(non_snake_case)]
fn decode_BC6H_SF16(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_4x4_block(r, size, buf, ColorType::Rgb32F, |block_bytes| {
        decode_bc6_signed_block(block_bytes).map(float3_to_bytes)
    })
}
#[allow(non_snake_case)]
fn decode_BC7_UNORM(r: &mut dyn Read, size: Size, buf: &mut [u8]) -> ImageResult<()> {
    by_4x4_block(r, size, buf, ColorType::Rgba8, decode_bc7_block)
}

fn fix_endian_u16(buf: &mut [u8]) {
    assert!(buf.len() % 2 == 0);

    if cfg!(target_endian = "big") {
        for i in (0..buf.len()).step_by(2) {
            buf.swap(i, i + 1);
        }
    }
}
fn fix_endian_u32(buf: &mut [u8]) {
    assert!(buf.len() % 4 == 0);

    if cfg!(target_endian = "big") {
        for i in (0..buf.len()).step_by(4) {
            buf.swap(i, i + 3);
            buf.swap(i + 1, i + 2);
        }
    }
}
