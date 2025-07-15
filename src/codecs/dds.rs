//! Decoding and encoding DDS images
//!
//! DDS (DirectDraw Surface) is a container format for storing DXT (S3TC) compressed images.
//!
//! # Related Links
//!
//! * <https://docs.microsoft.com/en-us/windows/win32/direct3ddds/dx-graphics-dds-pguide> - Description of the DDS format.

use std::io::{Read, Seek, Write};

use dds::{Channels, Precision};

use crate::color::{ColorType, ExtendedColorType};
use crate::error::{
    DecodingError, EncodingError, ImageError, ImageResult, LimitError, LimitErrorKind,
    UnsupportedError, UnsupportedErrorKind,
};
use crate::{ImageDecoder, ImageDecoderRect, ImageEncoder, ImageFormat};

/// DDS decoder.
///
/// This decoder supports decoding DDS files with a single texture, including
/// cube maps. Texture arrays and volumes are not supported.
///
/// It's possible to set the color type the image is decoded as using
/// [`DdsDecoder::set_color_type`].
pub struct DdsDecoder<R: Read> {
    inner: dds::Decoder<R>,
    is_cubemap: bool,
    size: dds::Size,
    color: dds::ColorFormat,
}

impl<R: Read> DdsDecoder<R> {
    /// Create a new decoder that decodes from the stream `r`
    pub fn new(r: R) -> ImageResult<Self> {
        let options = dds::header::ParseOptions::new_permissive(None);
        let decoder =
            dds::Decoder::new_with_options(r, &options).map_err(ImageError::from_dds_decode)?;
        let layout = decoder.layout();

        // We only support DDS files with:
        // - A single main image with any number of mipmaps
        // - A texture array of length 1 representing a cube map
        match &layout {
            dds::DataLayout::Volume(_) => {
                return Err(ImageError::Decoding(DecodingError::new(
                    ImageFormat::Dds.into(),
                    "DDS volume textures are not supported for decoding",
                )))
            }
            dds::DataLayout::TextureArray(texture_array) => {
                let supported_length = match texture_array.kind() {
                    dds::TextureArrayKind::Textures => 1,
                    dds::TextureArrayKind::CubeMaps => 6,
                    dds::TextureArrayKind::PartialCubeMap(cube_map_faces) => cube_map_faces.count(),
                };
                if texture_array.len() != supported_length as usize {
                    return Err(ImageError::Decoding(DecodingError::new(
                        ImageFormat::Dds.into(),
                        "DDS texture arrays are not supported for decoding",
                    )));
                }
            }
            _ => {}
        }

        let mut size = decoder.main_size();
        let mut color = decoder.native_color();
        let is_cubemap = layout.is_cube_map();

        // all cube map faces are read as one RGBA image
        if is_cubemap {
            if let (Some(width), Some(height)) =
                (size.width.checked_mul(4), size.height.checked_mul(3))
            {
                size.width = width;
                size.height = height;
                color.channels = Channels::Rgba;
            } else {
                return Err(ImageError::Decoding(DecodingError::new(
                    ImageFormat::Dds.into(),
                    "DDS cube map faces are too large to decode",
                )));
            }
        }

        Ok(DdsDecoder {
            inner: decoder,
            is_cubemap,
            size,
            color,
        })
    }

    /// Set the color type for the decoder.
    ///
    /// The DDS decoder supports decoding images not just in their native color
    /// format, but any user-defined color format. This is useful for decoding
    /// images that do not cleanly fit into the native formats. E.g. the DDS
    /// format `B5G6R5_UNORM` is decoded as [`ColorType::Rgb8`] by default, but
    /// you may want to decode it as [`ColorType::Rgb32F`] instead to avoid the
    /// rounding error when converting to `u8`. Similarly, your application may
    /// only support 8-bit images, while the DDS file is in a 16/32-bit format.
    /// Decoding directly into the final color type is more efficient than
    /// decoding into the native format and then converting.
    ///
    /// # Panics
    ///
    /// [`ColorType::La8`] and [`ColorType::La16`] are not supported for decoding
    /// DDS files. This function will panic if you try to set the color type to
    /// these formats.
    #[track_caller]
    pub fn set_color_type(&mut self, color: ColorType) {
        self.color = match color {
            ColorType::L8 => dds::ColorFormat::GRAYSCALE_U8,
            ColorType::Rgb8 => dds::ColorFormat::RGB_U8,
            ColorType::Rgba8 => dds::ColorFormat::RGBA_U8,
            ColorType::L16 => dds::ColorFormat::GRAYSCALE_U16,
            ColorType::Rgb16 => dds::ColorFormat::RGB_U16,
            ColorType::Rgba16 => dds::ColorFormat::RGBA_U16,
            ColorType::Rgb32F => dds::ColorFormat::RGB_F32,
            ColorType::Rgba32F => dds::ColorFormat::RGBA_F32,
            ColorType::La8 | ColorType::La16 => {
                panic!("La8 and La16 are not supported for decoding DDS files")
            }
        };
    }
}

impl<R: Read + Seek> ImageDecoder for DdsDecoder<R> {
    fn dimensions(&self) -> (u32, u32) {
        (self.size.width, self.size.height)
    }

    fn color_type(&self) -> ColorType {
        to_color_type(self.color)
    }

    fn original_color_type(&self) -> ExtendedColorType {
        use dds::Format;

        match self.inner.format() {
            Format::R1_UNORM => ExtendedColorType::L1,
            Format::B4G4R4A4_UNORM | Format::A4B4G4R4_UNORM => ExtendedColorType::Rgba4,
            Format::A8_UNORM => ExtendedColorType::A8,
            _ => to_color_type(self.inner.native_color()).into(),
        }
    }

    fn set_limits(&mut self, limits: crate::Limits) -> ImageResult<()> {
        limits.check_dimensions(self.size.width, self.size.height)?;

        if let Some(max_alloc) = limits.max_alloc {
            self.inner.options.memory_limit = max_alloc.try_into().unwrap_or(usize::MAX);
        }

        Ok(())
    }

    #[track_caller]
    fn read_image(mut self, buf: &mut [u8]) -> ImageResult<()> {
        let color = self.color;
        let size = self.size;

        let image = dds::ImageViewMut::new(buf, size, color).expect("Invalid buffer length");

        if self.is_cubemap {
            self.inner
                .read_cube_map(image)
                .map_err(ImageError::from_dds_decode)?;
        } else {
            self.inner
                .read_surface(image)
                .map_err(ImageError::from_dds_decode)?;
        }

        Ok(())
    }

    fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()> {
        (*self).read_image(buf)
    }
}

impl<R: Read + Seek> ImageDecoderRect for DdsDecoder<R> {
    fn read_rect(
        &mut self,
        x: u32,
        y: u32,
        width: u32,
        height: u32,
        buf: &mut [u8],
        row_pitch: usize,
    ) -> ImageResult<()> {
        // reading rectangles is not supported for cube maps
        if self.is_cubemap {
            return Err(ImageError::Decoding(DecodingError::new(
                ImageFormat::Dds.into(),
                "Reading rects from cubemaps is not supported",
            )));
        }

        self.inner
            .read_surface_rect(
                buf,
                row_pitch,
                dds::Rect::new(x, y, width, height),
                self.color,
            )
            .map_err(ImageError::from_dds_decode)?;
        self.inner
            .rewind_to_previous_surface()
            .map_err(ImageError::from_dds_decode)?;

        Ok(())
    }
}

/// The format of a DDS header.
///
/// DDS supports 2 header formats:
///
/// - DX9: This is the legacy header format that was used before DirectX 10.
/// - DX10: The modern header format that was introduced with DirectX 10.
///
/// Both formats are widely supported nowadays, but the DX10 format is the
/// preferred format for a few reasons:
///
/// 1. It supports more features (such as texture arrays).
/// 2. It uses the DXGI format, which is more consistent, easier to work with,
///    and supports more and more varied formats.
///
///    DX9 has 2 ways to specify the pixel format of a file: FourCC and channel
///    bit masks. Neither are fully standardized, so formats are supported on
///    a best-effort basis.
/// 3. It is better-specified in general, meaning that the problem of two DDS
///    decoders interpreting the same file differently is virtually non-existent.
///
/// However, if compatibility with older software and hardware is a concern, DX9
/// may be the only choice.
///
/// Note that modern software and hardware will generally support both formats.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum HeaderFormat {
    /// The legacy header format used in DirectX 9 and earlier.
    Dx9,
    /// The modern header format used in DirectX 10 and later.
    #[default]
    Dx10,
}
impl HeaderFormat {
    fn from_header(header: &dds::header::Header) -> Self {
        match header {
            dds::header::Header::Dx9(_) => HeaderFormat::Dx9,
            dds::header::Header::Dx10(_) => HeaderFormat::Dx10,
        }
    }
}

/// The speed-quality tradeoff for compression.
///
/// This will generally only affect compressed formats, such as the BCn formats.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum CompressionQuality {
    /// Fastest compression, lowest quality.
    Fastest,
    /// A good balance between speed and quality.
    #[default]
    Default,
    /// Slowest compression, highest quality.
    High,
}

/// Format used for encoding.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum DdsFormat {
    /// The encoder will automatically pick a linear uncompressed format.
    #[default]
    AutoUncompressed,

    /// 8-bit uncompressed alpha.
    A8Unorm,
    /// 8-bit uncompressed Grayscale.
    R8Unorm,
    /// 8-bit uncompressed RG.
    R8G8Unorm,
    /// 8-bit uncompressed RGBA.
    R8G8B8A8Unorm,
    /// 8-bit uncompressed RGBA.
    ///
    /// This format requires a DX10 header.
    R8G8B8A8UnormSrgb,

    /// 16-bit uncompressed Grayscale.
    R16Unorm,
    /// 16-bit uncompressed RG.
    R16G16Unorm,
    /// 16-bit uncompressed RGBA.
    R16G16B16A16Unorm,

    /// 16-bit floating-point uncompressed Grayscale.
    R16Float,
    /// 16-bit floating-point uncompressed RG.
    R16G16Float,
    /// 16-bit floating-point uncompressed RGBA.
    R16G16B16A16Float,

    /// 32-bit floating-point uncompressed Grayscale.
    R32Float,
    /// 32-bit floating-point uncompressed RG.
    R32G32Float,
    /// 32-bit floating-point uncompressed RGB.
    ///
    /// This format requires a DX10 header.
    R32G32B32Float,
    /// 32-bit floating-point uncompressed RGBA.
    R32G32B32A32Float,

    /// 8-bit uncompressed BGR.
    ///
    /// This format requires a DX9 header.
    B8G8R8Unorm,
    /// 4-bit uncompressed BGRA.
    B4G4R4A4Unorm,
    /// 5-bit uncompressed BGR with 1-bit alpha.
    B5G5R5A1Unorm,
    /// Uncompressed BGRA with 5 bits for blue, 6 bits for green, and 5 bits for red.
    B5G6R5Unorm,
    /// 10-bit uncompressed RGB with 2-bit alpha.
    R10G10B10A2Unorm,

    /// An HDR format that can store floating-point numbers between 0.0 and 65408.0.
    ///
    /// This format requires a DX10 header.
    R9G9B9E5Float,
    /// An HDR format that stores components as unsigned 11/10-bit floating-point numbers.
    ///
    /// This format requires a DX10 header.
    R11G11B10Float,

    /// 8-bit YUV, 4:2:2 sub-sampled.
    YUY2,

    /// Linear BC1. This is also called `DXT1` in DX9.
    BC1Unorm,
    /// sRGB BC1.
    ///
    /// This format requires a DX10 header.
    BC1UnormSrgb,
    /// Linear BC2. This is also called `DXT3` in DX9.
    BC2Unorm,
    /// sRGB BC2.
    ///
    /// This format requires a DX10 header.
    BC2UnormSrgb,
    /// Linear BC3. This is also called `DXT5` in DX9.
    BC3Unorm,
    /// sRGB BC3.
    ///
    /// This format requires a DX10 header.
    BC3UnormSrgb,
    /// Unsigned BC4. This is also called `ATI1` or `BC4U` in DX9.
    BC4Unorm,
    /// Signed BC4. This is also called `BC4S` in DX9.
    BC4Snorm,
    /// Unsigned BC5. This is also called `ATI2` or `BC5U` in DX9.
    BC5Unorm,
    /// Signed BC5. This is also called `BC5S` in DX9.
    BC5Snorm,
}
impl DdsFormat {
    /// Returns whether the format is an sRGB format.
    ///
    /// Note that sRGB formats are only supported by DX10 headers.
    pub fn is_srgb(self) -> bool {
        matches!(
            self,
            DdsFormat::R8G8B8A8UnormSrgb
                | DdsFormat::BC1UnormSrgb
                | DdsFormat::BC2UnormSrgb
                | DdsFormat::BC3UnormSrgb
        )
    }

    /// Returns the corresponding DDS format. Note that the format will
    /// ALWAYS be linear, even for sRGB formats.
    fn to_format(self, color: ExtendedColorType) -> dds::Format {
        use dds::Format;

        match self {
            DdsFormat::AutoUncompressed => {
                match color {
                    // formats that can be represented exactly
                    ExtendedColorType::A8 => Format::A8_UNORM,
                    ExtendedColorType::L1 => Format::R1_UNORM,
                    ExtendedColorType::L8 => Format::R8_UNORM,
                    ExtendedColorType::Rgb8 | ExtendedColorType::Bgr8 => Format::B8G8R8_UNORM,
                    ExtendedColorType::Rgba8 | ExtendedColorType::Bgra8 => Format::R8G8B8A8_UNORM,
                    ExtendedColorType::L16 => Format::R16_UNORM,
                    ExtendedColorType::Rgba16 => Format::R16G16B16A16_UNORM,
                    ExtendedColorType::Rgb32F => Format::R32G32B32_FLOAT,
                    ExtendedColorType::Rgba32F => Format::R32G32B32A32_FLOAT,

                    // pick a format that can represent all values
                    ExtendedColorType::La8 => Format::R8G8B8A8_UNORM,
                    ExtendedColorType::La16 => Format::R16G16B16A16_UNORM,
                    ExtendedColorType::Rgb16 => Format::R16G16B16A16_UNORM,
                    ExtendedColorType::L2 | ExtendedColorType::L4 => Format::R8_UNORM,
                    ExtendedColorType::La1
                    | ExtendedColorType::Rgb1
                    | ExtendedColorType::Rgba1
                    | ExtendedColorType::La2
                    | ExtendedColorType::Rgb2
                    | ExtendedColorType::Rgba2
                    | ExtendedColorType::La4
                    | ExtendedColorType::Rgb4
                    | ExtendedColorType::Rgba4 => Format::B4G4R4A4_UNORM,

                    _ => unreachable!(),
                }
            }

            DdsFormat::A8Unorm => Format::A8_UNORM,
            DdsFormat::R8Unorm => Format::R8_UNORM,
            DdsFormat::R8G8Unorm => Format::R8G8_UNORM,
            DdsFormat::R8G8B8A8Unorm => Format::R8G8B8A8_UNORM,
            DdsFormat::R8G8B8A8UnormSrgb => Format::R8G8B8A8_UNORM,
            DdsFormat::R16Unorm => Format::R16_UNORM,
            DdsFormat::R16G16Unorm => Format::R16G16_UNORM,
            DdsFormat::R16G16B16A16Unorm => Format::R16G16B16A16_UNORM,
            DdsFormat::R16Float => Format::R16_FLOAT,
            DdsFormat::R16G16Float => Format::R16G16_FLOAT,
            DdsFormat::R16G16B16A16Float => Format::R16G16B16A16_FLOAT,
            DdsFormat::R32Float => Format::R32_FLOAT,
            DdsFormat::R32G32Float => Format::R32G32_FLOAT,
            DdsFormat::R32G32B32Float => Format::R32G32B32_FLOAT,
            DdsFormat::R32G32B32A32Float => Format::R32G32B32A32_FLOAT,
            DdsFormat::B8G8R8Unorm => Format::B8G8R8_UNORM,
            DdsFormat::B4G4R4A4Unorm => Format::B4G4R4A4_UNORM,
            DdsFormat::B5G5R5A1Unorm => Format::B5G5R5A1_UNORM,
            DdsFormat::B5G6R5Unorm => Format::B5G6R5_UNORM,
            DdsFormat::R10G10B10A2Unorm => Format::R10G10B10A2_UNORM,
            DdsFormat::R9G9B9E5Float => Format::R9G9B9E5_SHAREDEXP,
            DdsFormat::R11G11B10Float => Format::R11G11B10_FLOAT,
            DdsFormat::YUY2 => Format::YUY2,

            DdsFormat::BC1Unorm => Format::BC1_UNORM,
            DdsFormat::BC1UnormSrgb => Format::BC1_UNORM,
            DdsFormat::BC2Unorm => Format::BC2_UNORM,
            DdsFormat::BC2UnormSrgb => Format::BC2_UNORM,
            DdsFormat::BC3Unorm => Format::BC3_UNORM,
            DdsFormat::BC3UnormSrgb => Format::BC3_UNORM,
            DdsFormat::BC4Unorm => Format::BC4_UNORM,
            DdsFormat::BC4Snorm => Format::BC4_SNORM,
            DdsFormat::BC5Unorm => Format::BC5_UNORM,
            DdsFormat::BC5Snorm => Format::BC5_SNORM,
        }
    }
}

/// Whether and how many mipmaps to generate.
enum Mipmaps {
    None,
    Full,
    Fixed(u8),
}

/// DDS encoder.
///
/// Creates DDS files containing a single texture with optional mipmaps.
///
/// ## Usage
///
/// ```no_run
/// use image::codecs::dds::*;
///
/// let mut buffer = Vec::new();
/// let encoder = DdsEncoder::new(&mut buffer)
///     .with_format(DdsFormat::BC1Unorm)
///     .with_preferred_header(HeaderFormat::Dx9)
///     .with_compression_quality(CompressionQuality::High)
///     .with_mipmaps();
/// ```
///
/// An encoder is created from a writer and configured using the builder
/// methods. To encode an image, use the [`ImageEncoder::write_image`] method.
///
/// ## Options
///
/// ### Format
///
/// The DDS format to encode the image with can be set using the
/// [`DdsEncoder::with_format`] method. See [`DdsFormat`] for the list of all
/// supported formats. By default, [`DdsFormat::AutoUncompressed`] is used,
/// which will automatically pick an uncompressed format based on the color type
/// of the image.
///
/// The header format of the DDS file can be set using the
/// [`DdsEncoder::with_preferred_header`] method. See [`HeaderFormat`] for more
/// information about DDS headers. The default header format is DX10.
///
/// Note that the specified DDS format takes precedence over the preferred
/// header format. If a DDS format does not support the preferred header format,
/// the encoder will use the header format required by the DDS format. Use
/// [`DdsEncoder::header_format`] to get the actual header format that will be used
/// for encoding.
///
/// ### Mipmaps
///
/// The generate mipmaps, you [`DdsEncoder::with_mipmaps`] to generate a full
/// mipmap chain, or [`DdsEncoder::with_mipmap_count`] to generate a specific
/// number of mipmaps. By default, no mipmaps are generated.
///
/// If mipmaps are generated and the image contains an alpha channel, the alpha
/// channel will be treated as straight alpha/transparency by default.
/// If the alpha channel is something else (e.g. premultiplied alpha or a
/// different channel packed into alpha), use [`DdsEncoder::with_separate_alpha`]
/// to specify that the alpha channel should be treated as a separate channel
/// when resizing the image to generate mipmaps.
///
/// ### Quality
///
/// [`DdsEncoder::with_compression_quality`] can be used to set the compression
/// quality for BCn formats. This option trades off compression speed and
/// quality.
///
/// [`DdsEncoder::with_perceptual_error_metric`] can further improve quality for
/// albedo, diffuse, and photographic textures by using a perceptual error
/// metric when compressing BCn formats.
///
/// [`DdsEncoder::with_dithering`] enables dithering to reduce quantization and
/// banding artifacts when encoding images with a higher bit depth.
pub struct DdsEncoder<W: Write> {
    writer: W,
    format: DdsFormat,
    preferred_header: HeaderFormat,
    quality: dds::CompressionQuality,
    error_metric: dds::ErrorMetric,
    dithering: dds::Dithering,
    straight_alpha: bool,
    mipmaps: Mipmaps,
}

impl<W: Write> DdsEncoder<W> {
    /// Create a new encoder that encodes to the stream `w`
    pub fn new(w: W) -> Self {
        DdsEncoder {
            writer: w,
            format: DdsFormat::AutoUncompressed,
            preferred_header: HeaderFormat::Dx10,
            quality: dds::CompressionQuality::Normal,
            error_metric: dds::ErrorMetric::Uniform,
            dithering: dds::Dithering::None,
            straight_alpha: true,
            mipmaps: Mipmaps::None,
        }
    }

    /// Create a DDS file with the given format.
    pub fn with_format(mut self, format: DdsFormat) -> Self {
        self.format = format;
        self
    }

    /// Create a DDS file with the given header format.
    ///
    /// Note that the specified DDS format takes precedence over the preferred
    /// header format. So if a DDS format does not support the preferred header
    /// format, the encoder will use the header format specified by the DDS
    /// format.
    ///
    /// By default, the encoder will use the DX10 header format.
    pub fn with_preferred_header(mut self, header: HeaderFormat) -> Self {
        self.preferred_header = header;
        self
    }

    /// Set the compression quality for the encoder.
    ///
    /// By default, the compression quality is set to [`CompressionQuality::Default`].
    ///
    /// Currently, this option only affects BC1-BC5 formats.
    pub fn with_compression_quality(mut self, quality: CompressionQuality) -> Self {
        self.quality = match quality {
            CompressionQuality::Fastest => dds::CompressionQuality::Fast,
            CompressionQuality::Default => dds::CompressionQuality::Normal,
            CompressionQuality::High => dds::CompressionQuality::High,
        };
        self
    }

    /// Use a perceptual error metric for compression.
    ///
    /// By default, the encoder uses a uniform error metric for compression,
    /// meaning that it tries to minimize the mean squared error (MSE) between the
    /// original and compressed image.
    ///
    /// Better visual results can be achieved by using a perceptual error
    /// metric for albedo, diffuse, and photographic textures. This metric
    /// accounts for how the human eye perceives color, and will produce better
    /// results for these types of images.
    ///
    /// However, a perceptual error metric is not recommended for normal maps,
    /// specular maps, and height maps, as it will produce worse artifacts.
    ///
    /// Note: The perceptual error metric assumes that image data is in sRGB,
    /// irrespective of whether the DDS format is linear or sRGB. Do not use the
    /// perceptual error metric if the image data is in linear RGB or any other
    /// color space.
    ///
    /// Currently, this option only affects BC1-BC3 formats.
    pub fn with_perceptual_error_metric(mut self) -> Self {
        self.error_metric = dds::ErrorMetric::Perceptual;
        self
    }

    /// Enables dithering when encoding images with a higher bit depth to a
    /// lower bit depth.
    ///
    /// Note: Not all formats support dithering. The encoder will ignore this option
    /// if the format does not support dithering.
    pub fn with_dithering(mut self) -> Self {
        self.dithering = dds::Dithering::ColorAndAlpha;
        self
    }

    /// Create a DDS file with a full mip chain.
    ///
    /// Mipmaps will be automatically generated from the main image.
    ///
    /// To set a specific number of mipmaps, use [`DdsEncoder::with_mipmap_count`].
    ///
    /// Note: If the image contains an alpha channel that is **not** straight
    /// alpha, you should use also use [`DdsEncoder::with_separate_alpha`] to
    /// ensure that the other channels are not affected by the alpha channel.
    pub fn with_mipmaps(mut self) -> Self {
        self.mipmaps = Mipmaps::Full;
        self
    }

    /// Create a DDS file with the given number of mipmaps.
    ///
    /// Unless necessary, it is recommended to use [`DdsEncoder::with_mipmaps`]
    /// instead, to automatically generate the full mipmap chain.
    ///
    /// Note: Setting 0 or 1 mipmaps is equivalent to not generating any mipmaps
    /// at all.
    pub fn with_mipmap_count(mut self, mipmap_count: u8) -> Self {
        self.mipmaps = Mipmaps::Fixed(mipmap_count);
        self
    }

    /// Separate alpha from the other channels when resizing to generate mipmaps.
    ///
    /// By default, the encoder assumes straight alpha. This is necessary to
    /// prevent color bleeding when resizing the image to generate mipmaps.
    /// However, this also means that pixels for which the alpha channel is 0
    /// will lose any and all color information. This is not a problem if the
    /// alpha channels contains transparency information, but it is a problem
    /// if the alpha channel is used for other purposes (e.g. a mask or height
    /// map). In this case, use this function to specify that the alpha channel
    /// should be treated as a separate channel and not as transparency.
    pub fn with_separate_alpha(mut self) -> Self {
        self.straight_alpha = false;
        self
    }

    /// Returns the header format that will be used for encoding.
    ///
    /// This is useful, because the preferred header format may not be possible
    /// for the given DDS format.
    pub fn header_format(&self, color_type: ExtendedColorType) -> HeaderFormat {
        let (header, _) = self.get_dds_header_and_format(16, 16, color_type);
        HeaderFormat::from_header(&header)
    }

    fn get_dds_header_and_format(
        &self,
        width: u32,
        height: u32,
        color_type: ExtendedColorType,
    ) -> (dds::header::Header, dds::Format) {
        use dds::header::Header;

        let is_srgb = self.format.is_srgb();
        let format = self.format.to_format(color_type);

        let mut header: Header = if is_srgb {
            // force DX10 header
            let dxgi = dds::header::DxgiFormat::try_from(format).unwrap();
            dds::header::Dx10Header::new_image(width, height, dxgi.to_srgb()).into()
        } else {
            let header = Header::new_image(width, height, format);
            match self.preferred_header {
                HeaderFormat::Dx9 => header.to_dx9().map(Header::from).unwrap_or(header),
                HeaderFormat::Dx10 => header.to_dx10().map(Header::from).unwrap_or(header),
            }
        };

        match self.mipmaps {
            Mipmaps::None => {}
            Mipmaps::Full => header = header.with_mipmaps(),
            Mipmaps::Fixed(n) => {
                if n > 1 {
                    header = header.with_mipmap_count(n as u32);
                }
            }
        }

        (header, format)
    }
}

impl<W: Write> ImageEncoder for DdsEncoder<W> {
    fn write_image(
        self,
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ExtendedColorType,
    ) -> ImageResult<()> {
        let color = to_dds_color(color_type).ok_or_else(|| {
            ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                ImageFormat::Dds.into(),
                UnsupportedErrorKind::Color(color_type),
            ))
        })?;
        let image = dds::ImageView::new(buf, dds::Size::new(width, height), color)
            .expect("Invalid buffer length");

        // create the header
        let (header, format) = self.get_dds_header_and_format(width, height, color_type);

        // encode the image
        let mut encoder =
            dds::Encoder::new(self.writer, format, &header).map_err(ImageError::from_dds_encode)?;
        encoder.options.dithering = self.dithering;
        encoder.options.error_metric = self.error_metric;
        encoder.options.quality = self.quality;

        let options = dds::WriteOptions {
            generate_mipmaps: true,
            resize_straight_alpha: self.straight_alpha,
            ..Default::default()
        };

        encoder
            .write_surface_with(image, None, &options)
            .map_err(ImageError::from_dds_encode)?;
        encoder.finish().map_err(ImageError::from_dds_encode)?;

        Ok(())
    }
}

fn to_color_type(color: dds::ColorFormat) -> ColorType {
    match (color.channels, color.precision) {
        (Channels::Alpha | Channels::Rgba, Precision::U8) => ColorType::Rgba8,
        (Channels::Alpha | Channels::Rgba, Precision::U16) => ColorType::Rgba16,
        (Channels::Alpha | Channels::Rgba, Precision::F32) => ColorType::Rgba32F,
        (Channels::Rgb, Precision::U8) => ColorType::Rgb8,
        (Channels::Rgb, Precision::U16) => ColorType::Rgb16,
        (Channels::Rgb, Precision::F32) => ColorType::Rgb32F,
        (Channels::Grayscale, Precision::U8) => ColorType::L8,
        (Channels::Grayscale, Precision::U16) => ColorType::L16,
        (Channels::Grayscale, Precision::F32) => ColorType::Rgb32F,
    }
}

fn to_dds_color(color: ExtendedColorType) -> Option<dds::ColorFormat> {
    match color {
        ExtendedColorType::A8 => Some(dds::ColorFormat::ALPHA_U8),

        ExtendedColorType::L8 => Some(dds::ColorFormat::GRAYSCALE_U8),
        ExtendedColorType::L16 => Some(dds::ColorFormat::GRAYSCALE_U16),

        ExtendedColorType::Rgb8 => Some(dds::ColorFormat::RGB_U8),
        ExtendedColorType::Rgb16 => Some(dds::ColorFormat::RGB_U16),
        ExtendedColorType::Rgb32F => Some(dds::ColorFormat::RGB_F32),

        ExtendedColorType::Rgba8 => Some(dds::ColorFormat::RGBA_U8),
        ExtendedColorType::Rgba16 => Some(dds::ColorFormat::RGBA_U16),
        ExtendedColorType::Rgba32F => Some(dds::ColorFormat::RGBA_F32),

        _ => None,
    }
}

impl ImageError {
    fn from_dds_decode(e: dds::DecodingError) -> Self {
        match e {
            dds::DecodingError::Io(e) => ImageError::IoError(e),
            dds::DecodingError::MemoryLimitExceeded => {
                ImageError::Limits(LimitError::from_kind(LimitErrorKind::DimensionError))
            }
            _ => ImageError::Decoding(DecodingError::new(ImageFormat::Dds.into(), e)),
        }
    }
    fn from_dds_encode(e: dds::EncodingError) -> ImageError {
        match e {
            dds::EncodingError::Io(e) => ImageError::IoError(e),
            _ => ImageError::Encoding(EncodingError::new(ImageFormat::Dds.into(), e)),
        }
    }
}
