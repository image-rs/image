use crate::error::{ImageFormatHint, ImageResult, UnsupportedError, UnsupportedErrorKind};
use crate::{ColorType, DynamicImage, ExtendedColorType};

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
    /// This information is currently used by the save and write method on [`DynamicImage`] to
    /// perform necessary conversions before encoding.
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

    Some(match to {
        ColorType::L8 => img.to_luma8().into(),
        ColorType::La8 => img.to_luma_alpha8().into(),
        ColorType::Rgb8 => img.to_rgb8().into(),
        ColorType::Rgba8 => img.to_rgba8().into(),
        ColorType::L16 => img.to_luma16().into(),
        ColorType::La16 => img.to_luma_alpha16().into(),
        ColorType::Rgb16 => img.to_rgb16().into(),
        ColorType::Rgba16 => img.to_rgba16().into(),
        ColorType::Rgb32F => img.to_rgb32f().into(),
        ColorType::Rgba32F => img.to_rgba32f().into(),
    })
}
fn to_supported_color(from: ColorType, supported: &[ExtendedColorType]) -> Option<ColorType> {
    supported
        .iter()
        .filter_map(|c| c.color_type())
        .max_by_key(|&to| {
            let mut loss = 0;

            // channel losses are heavily penalized, since a lot of information is lost
            // channel gains are penalized, since they are inefficient and don't add any information
            const ALPHA_LOST: u16 = 100;
            const ALPHA_GAIN: u16 = 3;
            const COLOR_LOST: u16 = 200;
            const COLOR_GAIN: u16 = 6;

            match (from.has_alpha(), to.has_alpha()) {
                (true, false) => loss += ALPHA_LOST,
                (false, true) => loss += ALPHA_GAIN,
                _ => {}
            }
            match (from.has_color(), to.has_color()) {
                (true, false) => loss += COLOR_LOST,
                (false, true) => loss += COLOR_GAIN,
                _ => {}
            }

            fn get_precision(c: ColorType) -> i16 {
                match c {
                    ColorType::L8 | ColorType::La8 | ColorType::Rgb8 | ColorType::Rgba8 => 0,
                    ColorType::L16 | ColorType::La16 | ColorType::Rgb16 | ColorType::Rgba16 => 1,
                    ColorType::Rgb32F | ColorType::Rgba32F => 2,
                }
            }

            const PRECISION_LOST: u16 = 10;
            const PRECISION_GAIN: u16 = 1;
            match get_precision(to) - get_precision(from) {
                m @ 1.. => loss += PRECISION_LOST * m as u16,
                m @ ..=-1 => loss += PRECISION_GAIN * m.unsigned_abs(),
                0 => {}
            }

            loss
        })
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
