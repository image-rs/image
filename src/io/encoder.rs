use crate::error::{ImageFormatHint, ImageResult, UnsupportedError, UnsupportedErrorKind};
use crate::ExtendedColorType;

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
}
