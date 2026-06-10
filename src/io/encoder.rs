use std::borrow::Cow;

use crate::color::FromPrimitive;
use crate::error::{ImageFormatHint, ImageResult, UnsupportedError, UnsupportedErrorKind};
use crate::{
    ColorType, DynamicImage, ExtendedColorType, GenericImageView, ImageBuffer, Luma, LumaA, Pixel,
    Rgb, Rgba,
};

/// The trait all encoders implement
pub trait ImageEncoder {
    /// Writes all the bytes in an image to the encoder.
    ///
    /// This function takes a slice of bytes of the pixel data of the image and encodes them. Just
    /// like for [`ImageDecoder::read_image`](crate::ImageDecoder), no particular alignment is
    /// required and data is expected to be in native endian. The implementation will reorder the
    /// endianness as necessary for the target encoding format.
    ///
    /// # Panics
    ///
    /// Panics if `buf.len() as u64 != color_type.buffer_size(width, height)`.
    fn write_image(
        self,
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ExtendedColorType,
    ) -> ImageResult<()>;

    /// Set the ICC profile to use for the image.
    ///
    /// This function is a no-op for formats that don't support ICC profiles.
    /// For formats that do support ICC profiles, the profile will be embedded
    /// in the image when it is saved.
    ///
    /// # Errors
    ///
    /// This function returns an error if the format does not support ICC profiles.
    fn set_icc_profile(&mut self, icc_profile: Vec<u8>) -> Result<(), UnsupportedError> {
        let _ = icc_profile;
        Err(UnsupportedError::from_format_and_kind(
            ImageFormatHint::Unknown,
            UnsupportedErrorKind::GenericFeature(
                "ICC profiles are not supported for this format".into(),
            ),
        ))
    }

    /// Set the EXIF metadata to use for the image.
    ///
    /// This function is a no-op for formats that don't support EXIF metadata.
    /// For formats that do support EXIF metadata, the metadata will be embedded
    /// in the image when it is saved.
    ///
    /// # Errors
    ///
    /// This function returns an error if the format does not support EXIF metadata or if the
    /// encoder doesn't implement saving EXIF metadata yet.
    fn set_exif_metadata(&mut self, exif: Vec<u8>) -> Result<(), UnsupportedError> {
        let _ = exif;
        Err(UnsupportedError::from_format_and_kind(
            ImageFormatHint::Unknown,
            UnsupportedErrorKind::GenericFeature(
                "EXIF metadata is not supported for this format".into(),
            ),
        ))
    }

    /// Set the XMP metadata to use for the image.
    ///
    /// This function is a no-op for formats that don't support XMP metadata.
    /// For formats that do support XMP metadata, the metadata will be embedded
    /// in the image when it is saved.
    ///
    /// # Errors
    ///
    /// This function returns an error if the format does not support XMP metadata or if the
    /// encoder doesn't implement saving XMP metadata yet.
    fn set_xmp_metadata(&mut self, xmp: Vec<u8>) -> Result<(), UnsupportedError> {
        let _ = xmp;
        Err(UnsupportedError::from_format_and_kind(
            ImageFormatHint::Unknown,
            UnsupportedErrorKind::GenericFeature(
                "XMP metadata is not supported for this format".into(),
            ),
        ))
    }

    /// All color types supported by this encoder. If `None`, supported colors aren't known.
    ///
    /// Encoders typically only support a select few color types for writing, and supported ones
    /// vary from encoder to encoder. This method allows encoders to specify which color types
    /// their [`write_image`](Self::write_image) method supports.
    ///
    /// This information is currently used for automatic color conversion by the `save*` and `write*`
    /// methods on [`DynamicImage`]. For more information, see [`DynamicImage::save`].
    fn supported_colors(&self) -> Option<&[ExtendedColorType]> {
        None
    }
}

pub(crate) trait ImageEncoderBoxed: ImageEncoder {
    fn write_image(
        self: Box<Self>,
        buf: &'_ [u8],
        width: u32,
        height: u32,
        color: ExtendedColorType,
    ) -> ImageResult<()>;
}
impl<T: ImageEncoder> ImageEncoderBoxed for T {
    fn write_image(
        self: Box<Self>,
        buf: &'_ [u8],
        width: u32,
        height: u32,
        color: ExtendedColorType,
    ) -> ImageResult<()> {
        (*self).write_image(buf, width, height, color)
    }
}

pub(crate) fn make_compatible_img(
    img: &DynamicImage,
    supported: Option<&[ExtendedColorType]>,
) -> Option<DynamicImage> {
    let color = img.color();
    let to = to_supported_color(color, supported?)?;
    if to == color {
        // no conversion necessary
        return None;
    }

    if color.has_color() != to.has_color() {
        // We don't want to convert RGB <-> Luma, because it's not clear how
        // this conversion should treat the color space information.
        return None;
    }

    // add or remove alpha as necessary
    let img = if to.has_alpha() != color.has_alpha() {
        Cow::Owned(toggle_alpha(img))
    } else {
        Cow::Borrowed(img)
    };

    // adjust precision as necessary
    let img = if decompose_color_type(to).precision != decompose_color_type(color).precision {
        Cow::Owned(to_precision(&img, decompose_color_type(to).precision))
    } else {
        img
    };

    match img {
        Cow::Borrowed(_) => None,
        Cow::Owned(img) => Some(img),
    }
}
fn to_supported_color(from: ColorType, supported: &[ExtendedColorType]) -> Option<ColorType> {
    let from = decompose_color_type(from);

    supported
        .iter()
        .filter_map(|c| c.color_type())
        .min_by_key(|&to| {
            let to = decompose_color_type(to);
            let mut loss = 0;

            // channel losses are heavily penalized, since a lot of information is lost
            // channel gains are penalized, since they are inefficient and don't add any information
            const ALPHA_LOST: u16 = 100;
            const ALPHA_GAIN: u16 = 3;
            const COLOR_LOST: u16 = 200;
            const COLOR_GAIN: u16 = 6;

            match (from.has_alpha, to.has_alpha) {
                (true, false) => loss += ALPHA_LOST,
                (false, true) => loss += ALPHA_GAIN,
                _ => {}
            }
            match (from.has_color, to.has_color) {
                (true, false) => loss += COLOR_LOST,
                (false, true) => loss += COLOR_GAIN,
                _ => {}
            }

            const PRECISION_LOST: u16 = 10;
            const PRECISION_GAIN: u16 = 1;
            match (to.precision as i16) - (from.precision as i16) {
                m @ 1.. => loss += PRECISION_LOST * m as u16,
                m @ ..=-1 => loss += PRECISION_GAIN * m.unsigned_abs(),
                0 => {}
            }

            loss
        })
}

/// If the image has an alpha channel, remove it. Otherwise, add an alpha channel with full opacity.
fn toggle_alpha(image: &DynamicImage) -> DynamicImage {
    match image {
        // no alpha => add it
        DynamicImage::ImageLuma8(buffer) => DynamicImage::ImageLumaA8(buffer.convert()),
        DynamicImage::ImageRgb8(buffer) => DynamicImage::ImageRgba8(buffer.convert()),
        DynamicImage::ImageLuma16(buffer) => DynamicImage::ImageLumaA16(buffer.convert()),
        DynamicImage::ImageRgb16(buffer) => DynamicImage::ImageRgba16(buffer.convert()),
        DynamicImage::ImageLuma32F(buffer) => DynamicImage::ImageLumaA32F(buffer.convert()),
        DynamicImage::ImageRgb32F(buffer) => DynamicImage::ImageRgba32F(buffer.convert()),
        // alpha => remove it
        DynamicImage::ImageLumaA8(buffer) => DynamicImage::ImageLuma8(buffer.convert()),
        DynamicImage::ImageRgba8(buffer) => DynamicImage::ImageRgb8(buffer.convert()),
        DynamicImage::ImageLumaA16(buffer) => DynamicImage::ImageLuma16(buffer.convert()),
        DynamicImage::ImageRgba16(buffer) => DynamicImage::ImageRgb16(buffer.convert()),
        DynamicImage::ImageLumaA32F(buffer) => DynamicImage::ImageLuma32F(buffer.convert()),
        DynamicImage::ImageRgba32F(buffer) => DynamicImage::ImageRgb32F(buffer.convert()),
    }
}

fn to_precision(image: &DynamicImage, target_precision: Precision) -> DynamicImage {
    let image_color = decompose_color_type(image.color());

    fn convert_precision<To: FromPrimitive<u8> + FromPrimitive<u16> + FromPrimitive<f32>>(
        buffer: &[u8],
        buffer_precision: Precision,
    ) -> Vec<To> {
        match buffer_precision {
            Precision::U8 => buffer
                .iter()
                .copied()
                .map(FromPrimitive::from_primitive)
                .collect(),
            // casts are valid, because the slice comes from a Vec<Precision>
            Precision::U16 => bytemuck::cast_slice::<_, u16>(buffer)
                .iter()
                .copied()
                .map(FromPrimitive::from_primitive)
                .collect(),
            Precision::F32 => bytemuck::cast_slice::<_, f32>(buffer)
                .iter()
                .copied()
                .map(FromPrimitive::from_primitive)
                .collect(),
        }
    }
    fn create_dyn_image<P: Pixel>(width: u32, height: u32, data: Vec<P::Subpixel>) -> DynamicImage
    where
        DynamicImage: From<ImageBuffer<P, Vec<P::Subpixel>>>,
    {
        let buffer: ImageBuffer<P, _> = ImageBuffer::from_vec(width, height, data).unwrap();
        DynamicImage::from(buffer)
    }

    let bytes = image.as_bytes();
    let (w, h) = image.dimensions();

    let mut out: DynamicImage = match target_precision {
        Precision::U8 => {
            let data: Vec<u8> = convert_precision(bytes, image_color.precision);

            match (image_color.has_color, image_color.has_alpha) {
                (false, false) => create_dyn_image::<Luma<u8>>(w, h, data),
                (false, true) => create_dyn_image::<LumaA<u8>>(w, h, data),
                (true, false) => create_dyn_image::<Rgb<u8>>(w, h, data),
                (true, true) => create_dyn_image::<Rgba<u8>>(w, h, data),
            }
        }
        Precision::U16 => {
            let data: Vec<u16> = convert_precision(bytes, image_color.precision);

            match (image_color.has_color, image_color.has_alpha) {
                (false, false) => create_dyn_image::<Luma<u16>>(w, h, data),
                (false, true) => create_dyn_image::<LumaA<u16>>(w, h, data),
                (true, false) => create_dyn_image::<Rgb<u16>>(w, h, data),
                (true, true) => create_dyn_image::<Rgba<u16>>(w, h, data),
            }
        }
        Precision::F32 => {
            let data: Vec<f32> = convert_precision(bytes, image_color.precision);

            match (image_color.has_color, image_color.has_alpha) {
                (false, false) => create_dyn_image::<Luma<f32>>(w, h, data),
                (false, true) => create_dyn_image::<LumaA<f32>>(w, h, data),
                (true, false) => create_dyn_image::<Rgb<f32>>(w, h, data),
                (true, true) => create_dyn_image::<Rgba<f32>>(w, h, data),
            }
        }
    };

    out.set_rgb_color_space(image.rgb_color_space());

    out
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum Precision {
    U8 = 0,
    U16 = 1,
    F32 = 2,
}
struct ColorDesc {
    /// RGB if true, Luma if false
    has_color: bool,
    /// Alpha if true, no alpha if false
    has_alpha: bool,
    precision: Precision,
}
fn decompose_color_type(color: ColorType) -> ColorDesc {
    use ColorType::*;
    ColorDesc {
        has_color: color.has_color(),
        has_alpha: color.has_alpha(),
        precision: match color {
            L8 | La8 | Rgb8 | Rgba8 => Precision::U8,
            L16 | La16 | Rgb16 | Rgba16 => Precision::U16,
            L32F | La32F | Rgb32F | Rgba32F => Precision::F32,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_conversion_png() {
        let png_supported_colors = &[
            ExtendedColorType::Rgb8,
            ExtendedColorType::Rgba8,
            ExtendedColorType::L8,
            ExtendedColorType::La8,
            ExtendedColorType::Rgb16,
            ExtendedColorType::Rgba16,
            ExtendedColorType::L16,
            ExtendedColorType::La16,
        ];
        let to = |from| to_supported_color(from, png_supported_colors).unwrap_or(from);

        assert_eq!(to(ColorType::L8), ColorType::L8);
        assert_eq!(to(ColorType::La8), ColorType::La8);
        assert_eq!(to(ColorType::Rgb8), ColorType::Rgb8);
        assert_eq!(to(ColorType::Rgba8), ColorType::Rgba8);
        assert_eq!(to(ColorType::L16), ColorType::L16);
        assert_eq!(to(ColorType::La16), ColorType::La16);
        assert_eq!(to(ColorType::Rgb16), ColorType::Rgb16);
        assert_eq!(to(ColorType::Rgba16), ColorType::Rgba16);
        assert_eq!(to(ColorType::Rgb32F), ColorType::Rgb16);
        assert_eq!(to(ColorType::Rgba32F), ColorType::Rgba16);
    }

    #[test]
    fn test_conversion_jpeg() {
        let jpeg_supported_colors = &[ExtendedColorType::Rgb8, ExtendedColorType::L8];
        let to = |from| to_supported_color(from, jpeg_supported_colors).unwrap_or(from);

        assert_eq!(to(ColorType::L8), ColorType::L8);
        assert_eq!(to(ColorType::La8), ColorType::L8);
        assert_eq!(to(ColorType::Rgb8), ColorType::Rgb8);
        assert_eq!(to(ColorType::Rgba8), ColorType::Rgb8);
        assert_eq!(to(ColorType::L16), ColorType::L8);
        assert_eq!(to(ColorType::La16), ColorType::L8);
        assert_eq!(to(ColorType::Rgb16), ColorType::Rgb8);
        assert_eq!(to(ColorType::Rgba16), ColorType::Rgb8);
        assert_eq!(to(ColorType::Rgb32F), ColorType::Rgb8);
        assert_eq!(to(ColorType::Rgba32F), ColorType::Rgb8);
    }

    #[test]
    fn test_conversion_bmp() {
        let bmp_supported_colors = &[
            ExtendedColorType::Rgb8,
            ExtendedColorType::Rgba8,
            ExtendedColorType::L1,
            ExtendedColorType::L8,
            ExtendedColorType::La8,
        ];
        let to = |from| to_supported_color(from, bmp_supported_colors).unwrap_or(from);

        assert_eq!(to(ColorType::L8), ColorType::L8);
        assert_eq!(to(ColorType::La8), ColorType::La8);
        assert_eq!(to(ColorType::Rgb8), ColorType::Rgb8);
        assert_eq!(to(ColorType::Rgba8), ColorType::Rgba8);
        assert_eq!(to(ColorType::L16), ColorType::L8);
        assert_eq!(to(ColorType::La16), ColorType::La8);
        assert_eq!(to(ColorType::Rgb16), ColorType::Rgb8);
        assert_eq!(to(ColorType::Rgba16), ColorType::Rgba8);
        assert_eq!(to(ColorType::Rgb32F), ColorType::Rgb8);
        assert_eq!(to(ColorType::Rgba32F), ColorType::Rgba8);
    }

    #[test]
    fn test_conversion_hdr() {
        let hdr_supported_colors = &[ExtendedColorType::Rgb32F];
        let to = |from| to_supported_color(from, hdr_supported_colors).unwrap_or(from);

        assert_eq!(to(ColorType::L8), ColorType::Rgb32F);
        assert_eq!(to(ColorType::La8), ColorType::Rgb32F);
        assert_eq!(to(ColorType::Rgb8), ColorType::Rgb32F);
        assert_eq!(to(ColorType::Rgba8), ColorType::Rgb32F);
        assert_eq!(to(ColorType::L16), ColorType::Rgb32F);
        assert_eq!(to(ColorType::La16), ColorType::Rgb32F);
        assert_eq!(to(ColorType::Rgb16), ColorType::Rgb32F);
        assert_eq!(to(ColorType::Rgba16), ColorType::Rgb32F);
        assert_eq!(to(ColorType::Rgb32F), ColorType::Rgb32F);
        assert_eq!(to(ColorType::Rgba32F), ColorType::Rgb32F);
    }
}
