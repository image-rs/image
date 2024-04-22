use chrono::{DateTime, Utc};
use fitsio::{errors::Error as FitsError, images::ImageDescription, FitsFile};
use std::path::PathBuf;

use std::{
    ops::Deref,
    path::Path,
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use crate::{ImageBuffer, Pixel, Primitive};

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
/// Compression algorithms used in FITS files.
pub enum FitsCompression {
    /// No compression.
    None,
    /// GZIP compression.
    Gzip,
    /// Rice compression.
    Rice,
    /// HCOMPRESS compression.
    Hcompress,
    /// HCOMPRESS with smoothing.
    Hsmooth,
    /// BZIP2 compression.
    Bzip2,
    /// PLIO compression.
    Plio,
}

impl From<Option<FitsCompression>> for FitsCompression {
    fn from(opt: Option<FitsCompression>) -> Self {
        opt.unwrap_or(FitsCompression::None)
    }
}

impl ToString for FitsCompression {
    fn to_string(&self) -> String {
        match self {
            FitsCompression::None => "uncomp",
            FitsCompression::Gzip => "gzip",
            FitsCompression::Rice => "rice",
            FitsCompression::Hcompress => "hcompress",
            FitsCompression::Hsmooth => "hscompress",
            FitsCompression::Bzip2 => "bzip2",
            FitsCompression::Plio => "plio",
        }
        .to_owned()
    }
}

#[cfg_attr(docsrs, doc(cfg(feature = "fitsio")))]
impl<P: Pixel, Container: Deref<Target = [P::Subpixel]>> ImageBuffer<P, Container> {
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
    ///  * `path` - The path to the FITS file.
    ///  * `compress` - Whether to compress the FITS file. Compression uses the GZIP algorithm.
    ///  * `overwrite` - Whether to overwrite the file if it already exists.
    ///
    /// # Errors
    ///  * [`fitsio::errors::Error`] with the error description.
    pub(crate) fn savefits(
        &self,
        path: &Path,
        compress: FitsCompression,
        overwrite: bool,
    ) -> Result<PathBuf, FitsError> {
        let cameraname;
        let timestamp = if let Some(metadata) = self.metadata() {
            metadata.timestamp()
        } else {
            SystemTime::now()
        };
        let timestamp: DateTime<Utc> = timestamp.into();
        let timestamp = timestamp.format("%Y-%m-%dT%H:%M:%S%.6f").to_string();
        let ts = if let Some(metadata) = self.metadata() {
            cameraname = metadata.camera_name();
            metadata
                .timestamp()
                .duration_since(UNIX_EPOCH)
                .map_err(|e| FitsError::Message(e.to_string()))?
                .as_millis() as u64
        } else {
            cameraname = "unknown";
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap_or(Duration::from_secs(0))
                .as_millis() as u64
        };

        let (width, height) = (self.width(), self.height());
        let numpix = P::CHANNEL_COUNT as usize;
        let imgsize = if numpix == 1 {
            vec![height as usize, width as usize, numpix]
        } else {
            vec![height as usize, width as usize, numpix]
        };

        let img_desc = ImageDescription {
            data_type: <P::Subpixel as Primitive>::image_type(),
            dimensions: &imgsize,
        };

        let fmt = match compress {
            FitsCompression::None => "fits",
            FitsCompression::Gzip => "fits[compress G]",
            FitsCompression::Rice => "fits[compress R]",
            FitsCompression::Hcompress => "fits[compress H]",
            FitsCompression::Hsmooth => "fits[compress HS]",
            FitsCompression::Bzip2 => "fits[compress B]",
            FitsCompression::Plio => "fits[compress P]",
        };

        let mut path = PathBuf::from(path);
        path.set_extension(fmt);

        let mut fptr = FitsFile::create(path.clone());
        if overwrite {
            fptr = fptr.overwrite();
        }
        if compress == FitsCompression::None {
            fptr = fptr.with_custom_primary(&img_desc);
        }
        let mut fptr = fptr.open()?;

        let hdu = if compress == FitsCompression::None {
            fptr.primary_hdu()?
        } else {
            let hdu = fptr.primary_hdu()?;
            hdu.write_key(&mut fptr, "COMPRESSED_IMAGE", "T")?;
            hdu.write_key(&mut fptr, "COMPRESSION_ALGO", compress.to_string())?;
            fptr.create_image("IMAGE", &img_desc)?
        };

        hdu.write_image(&mut fptr, self.inner_pixels())?;
        hdu.write_key(&mut fptr, "CAMERA", cameraname)?;
        hdu.write_key(&mut fptr, "DATE-OBS", timestamp.as_str())?;
        hdu.write_key(&mut fptr, "TIMESTAMP", ts)?;
        if let Some(meta) = self.metadata() {
            let (bin_x, bin_y) = meta.binning();
            hdu.write_key(&mut fptr, "XBINNING", bin_x)?;
            hdu.write_key(&mut fptr, "YBINNING", bin_y)?;
            let (xpixsz, ypixsz) = meta.pixel_size();
            hdu.write_key(&mut fptr, "XPIXSZ", xpixsz)?;
            hdu.write_key(&mut fptr, "YPIXSZ", ypixsz)?;
            hdu.write_key(&mut fptr, "EXPTIME", meta.exposure().as_secs_f64())?;
            hdu.write_key(&mut fptr, "CCD-TEMP", meta.temperature())?;
            let (org_x, org_y) = meta.origin();
            hdu.write_key(&mut fptr, "XORIGIN", org_x)?;
            hdu.write_key(&mut fptr, "YORIGIN", org_y)?;
            hdu.write_key(&mut fptr, "OFFSET", meta.offset())?;
            let (gain, min_gain, max_gain) = meta.gain();
            hdu.write_key(&mut fptr, "GAIN", gain)?;
            hdu.write_key(&mut fptr, "GAIN_MIN", min_gain)?;
            hdu.write_key(&mut fptr, "GAIN_MAX", max_gain)?;
            for obj in meta.extended_metadata().iter() {
                hdu.write_key(&mut fptr, &obj.name, obj.value.as_str())?;
            }
        }

        Ok(path)
    }
}
