use crate::codecs::png::CompressionType;
use crate::error::{ImageFormatHint, ImageResult, UnsupportedError, UnsupportedErrorKind};
use crate::{ColorType, DynamicImage};

/// Nominally public but DO NOT expose this type.
///
/// To be somewhat sure here's a compile fail test:
///
/// ```compile_fail
/// use image::MethodSealedToImage;
/// ```
///
/// ```compile_fail
/// use image::io::MethodSealedToImage;
/// ```
///
/// The same implementation strategy for a partially public trait is used in the standard library,
/// for the different effect of forbidding `Error::type_id` overrides thus making them reliable for
/// their calls through the `dyn` version of the trait.
///
/// Read more: <https://predr.ag/blog/definitive-guide-to-sealed-traits-in-rust/>
#[derive(Clone, Copy)]
pub struct MethodSealedToImage;

/// Encoder options that *must* be supported if set.
///
/// Each encoder is supports a specific option level, it is advisable that it is the newest
/// available one in the version of `image` it is written for. The options is non-exhaustive and
/// later versions may introduce new options. Since support must comprehensively check these
/// mandatory settings, [`EncodeOptions::check_support_level`] may be used to check that no options
/// beyond the supported level are active, so that only a well-defined list of attributes needs to
/// be considered. There's a clear in-active state for each option.
#[non_exhaustive]
pub struct EncodeOptions {
    /// If set, the encoder must refuse to encode the image if it can not guarantee a roundtrip.
    ///
    /// Part of [`EncodeOptionsLevel::V1_0`].
    pub lossless_image_data: bool,
}

pub enum EncodeOptionsLevel {
    None = 0,
    V1_0 = 1,
}

impl EncodeOptions {
    /// Check that no options beyond the indicate support level are used.
    pub fn check_support_level(&self, level: EncodeOptionsLevel) -> ImageResult<()> {
        match level {
            EncodeOptionsLevel::None => Ok(()),
            EncodeOptionsLevel::V1_0 => self.lossless_image_data.then_some(()).or_else(|| {
                Err(ImageResult::Err(UnsupportedError::from_format_and_kind(
                    ImageFormatHint::Unknown,
                    UnsupportedErrorKind::GenericFeature(
                        "lossless_image_data option is not supported".into(),
                    ),
                )))
            }),
        }
    }
}

impl Default for EncodeOptions {
    fn default() -> Self {
        EncodeOptions {
            lossless_image_data: false,
        }
    }
}

#[non_exhaustive]
pub struct EncodeHints {
    /// The requested level of compression, i.e. how much compute to spend on smaller files.
    pub compression: CompressionType,
}

/// The trait all encoders implement
pub trait ImageEncoder {
    /// Configure the encoder before writing any image data.
    fn configure(&mut self, options: &EncodeOptions) -> ImageResult<()> {
        options.check_support_level(EncodeOptionsLevel::None)
    }

    /// Configure the encoder with non-binding hints about the encoding.
    ///
    /// By hints we mean all options that do not affect the interface contract of the encoder such
    /// as the compression strength. A counter example would be sample encodings, which would
    /// affect buffer size.
    fn configure_hints(&mut self, format_options: &EncodeHints) {}

    /// Writes all the bytes in an image to the encoder.
    ///
    /// This function takes a slice of bytes of the pixel data of the image
    /// and encodes them. Just like for [`ImageDecoder::read_image`], no particular
    /// alignment is required and data is expected to be in native endian.
    /// The implementation will reorder the endianness as necessary for the target encoding format.
    ///
    /// # Panics
    ///
    /// Panics if `width * height * color_type.bytes_per_pixel() != buf.len()`.
    fn write_image(&mut self, buf: &[u8], layout: crate::ImageLayout) -> ImageResult<()>;

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

    /// Convert the image to a compatible format for the encoder. This is used by the encoding
    /// methods on `DynamicImage`.
    ///
    /// Note that this is method is sealed to the crate and effectively pub(crate) due to the
    /// argument type not being nameable.
    #[doc(hidden)]
    fn make_compatible_img(
        &self,
        _: MethodSealedToImage,
        _input: &DynamicImage,
    ) -> Option<DynamicImage> {
        None
    }
}

/// Implement `dynimage_conversion_sequence` for the common case of supporting only 8-bit colors
/// (with and without alpha).
#[allow(unused)]
pub(crate) fn dynimage_conversion_8bit(img: &DynamicImage) -> Option<DynamicImage> {
    use ColorType::*;

    match img.color() {
        Rgb8 | Rgba8 | L8 | La8 => None,
        L16 => Some(img.to_luma8().into()),
        La16 => Some(img.to_luma_alpha8().into()),
        Rgb16 | Rgb32F => Some(img.to_rgb8().into()),
        Rgba16 | Rgba32F => Some(img.to_rgba8().into()),
    }
}
