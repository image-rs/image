use std::path::{Path, PathBuf};

use fitsio::errors::Error as FitsError;
use crate::FitsCompression;

use crate::DynamicImage;

impl DynamicImage {
    /// Save the image data to a FITS file. The file name
    /// will be of the form `{file_prefix}_{yyyymmdd}_{hhmmss}.fits`.
    ///
    /// ### Note
    /// If compression is enabled, the compressed image data is stored
    /// in HDU 1 (IMAGE), while the uncompressed data is stored in the
    /// primary HDU. HDU 1 is created only if compression is enabled.
    /// The HDU containing the image also contains all the necessary
    /// metadata. In case compression is enabled, the primary HDU contains
    /// a key `COMPRESSED_IMAGE` with value `T` to indicate that the compressed
    /// image data is present in HDU 1.
    ///
    /// # Arguments
    ///  * `dir_prefix` - The directory where the file will be saved.
    ///  * `file_prefix` - The prefix of the file name. The file name will be of the form `{file_prefix}_{yyyymmdd}_{hhmmss}.fits`.
    ///  * `progname` - The name of the program that generated the image.
    ///  * `compress` - Whether to compress the FITS file. Compression uses the GZIP algorithm.
    ///  * `overwrite` - Whether to overwrite the file if it already exists.
    ///
    /// # Errors
    ///  * [`fitsio::errors::Error`] with the error description.
    pub fn savefits(
        &self,
        dir_prefix: &Path,
        file_prefix: &str,
        progname: Option<&str>,
        compress: FitsCompression,
        overwrite: bool,
    ) -> Result<PathBuf, FitsError> {
        match self {
            DynamicImage::ImageLuma8(p) => p.savefits(
                dir_prefix,
                file_prefix,
                progname,
                compress,
                overwrite,
            ),
            DynamicImage::ImageLumaA8(p) => p.savefits(
                dir_prefix,
                file_prefix,
                progname,
                compress,
                overwrite,
            ),
            DynamicImage::ImageRgb8(p) => p.savefits(
                dir_prefix,
                file_prefix,
                progname,
                compress,
                overwrite,
            ),
            DynamicImage::ImageRgba8(p) => p.savefits(
                dir_prefix,
                file_prefix,
                progname,
                compress,
                overwrite,
            ),
            DynamicImage::ImageLuma16(p) => p.savefits(
                dir_prefix,
                file_prefix,
                progname,
                compress,
                overwrite,
            ),
            DynamicImage::ImageLumaA16(p) => p.savefits(
                dir_prefix,
                file_prefix,
                progname,
                compress,
                overwrite,
            ),
            DynamicImage::ImageRgb16(p) => p.savefits(
                dir_prefix,
                file_prefix,
                progname,
                compress,
                overwrite,
            ),
            DynamicImage::ImageRgba16(p) => p.savefits(
                dir_prefix,
                file_prefix,
                progname,
                compress,
                overwrite,
            ),
            DynamicImage::ImageRgb32F(p) => p.savefits(
                dir_prefix,
                file_prefix,
                progname,
                compress,
                overwrite,
            ),
            DynamicImage::ImageRgba32F(p) => p.savefits(
                dir_prefix,
                file_prefix,
                progname,
                compress,
                overwrite,
            ),

        }
    }
}