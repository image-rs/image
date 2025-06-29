use crate::error::{ImageFormatHint, ImageResult, UnsupportedError, UnsupportedErrorKind};
use crate::{ColorType, ExtendedColorType};

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

/// The trait all encoders implement
pub trait ImageEncoder {
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

    /// Check if a color buffer should be converted to another color type before encoding. The
    /// default implementation does not support the check, the encoder may return error results
    /// when attempting to `write_image` with such an unsupported color.
    ///
    /// Note that this is method is sealed to the crate and effectively pub(crate) due to the
    /// argument type not being nameable.
    ///
    /// We use this specifically for [`DynamicImage`](crate::DynamicImage) and
    /// [`ImageBuffer`](crate::ImageBuffer), hence the argument type being a supported color
    /// representation and not an [`ExtendedColorType`].
    #[doc(hidden)]
    fn dynimage_conversion_sequence(
        &mut self,
        _: MethodSealedToImage,
        _: ColorType,
    ) -> Option<ColorType> {
        None
    }
}
