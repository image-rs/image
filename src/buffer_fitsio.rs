use fitsio::{errors::Error as FitsError, images::ImageDescription, FitsFile};
use std::path::PathBuf;

use chrono::DateTime;
use std::{
    io,
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
    /// Save the image to a FITS file.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn savefits(
        &self,
        dir_prefix: &Path,
        file_prefix: &str,
        progname: Option<&str>,
        compress: FitsCompression,
        overwrite: bool,
    ) -> Result<PathBuf, FitsError> {
        if !dir_prefix.exists() {
            return Err(FitsError::Io(io::Error::new(
                io::ErrorKind::NotFound,
                format!("Directory {:?} does not exist", dir_prefix),
            )));
        }
        let timestamp;
        let cameraname;
        if let Some(metadata) = self.metadata() {
            timestamp = metadata
                .timestamp()
                .duration_since(UNIX_EPOCH)
                .map_err(|e| FitsError::Message(e.to_string()))?
                .as_millis();
            cameraname = metadata.camera_name();
        } else {
            timestamp = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap_or(Duration::from_secs(0))
                .as_millis();
            cameraname = "unknown";
        }
        let ts = timestamp as u64;
        // Create a NaiveDateTime from the timestamp
        let timestamp = DateTime::from_timestamp_millis(timestamp as i64).ok_or(
            FitsError::Message("Could not convert timestamp to NaiveDateTime".to_owned()),
        )?;

        let timestamp = timestamp.format("%Y%m%d_%H%M%S");

        let file_prefix = if file_prefix.trim().is_empty() {
            cameraname
        } else {
            file_prefix
        };

        let fpath = dir_prefix.join(Path::new(&format!("{}_{}.fits", file_prefix, timestamp)));

        let (width, height) = (self.width(), self.height());
        let numpix = P::CHANNEL_COUNT as usize;
        let imgsize = if numpix == 1 {
            vec![height as usize, width as usize]
        } else {
            vec![height as usize, width as usize, numpix]
        };

        let img_desc = ImageDescription {
            data_type: <P::Subpixel as Primitive>::image_type(),
            dimensions: &imgsize,
        };

        let fmt = match compress {
            FitsCompression::None => "",
            FitsCompression::Gzip => "[compress G]",
            FitsCompression::Rice => "[compress R]",
            FitsCompression::Hcompress => "[compress H]",
            FitsCompression::Hsmooth => "[compress HS]",
            FitsCompression::Bzip2 => "[compress B]",
            FitsCompression::Plio => "[compress P]",
        };

        let path = Path::new(dir_prefix).join(Path::new(&format!(
            "{}_{}.fits{}",
            file_prefix, timestamp, fmt,
        )));

        let mut fptr = FitsFile::create(path);
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
        hdu.write_key(&mut fptr, "PROGRAM", progname.unwrap_or("unknown"))?;
        hdu.write_key(&mut fptr, "CAMERA", cameraname)?;
        hdu.write_key(&mut fptr, "TIMESTAMP", ts)?;
        if let Some(meta) = self.metadata() {
            hdu.write_key(&mut fptr, "TEMPERATURE", meta.temperature())?;
            hdu.write_key(&mut fptr, "EXPOSURE_US", meta.exposure().as_micros() as u64)?;
            let (org_x, org_y) = meta.origin();
            hdu.write_key(&mut fptr, "ORIGIN_X", org_x)?;
            hdu.write_key(&mut fptr, "ORIGIN_Y", org_y)?;
            let (bin_x, bin_y) = meta.binning();
            hdu.write_key(&mut fptr, "BIN_X", bin_x)?;
            hdu.write_key(&mut fptr, "BIN_Y", bin_y)?;
            hdu.write_key(&mut fptr, "OFFSET", meta.offset())?;
            let (gain, min_gain, max_gain) = meta.gain();
            hdu.write_key(&mut fptr, "GAIN", gain)?;
            hdu.write_key(&mut fptr, "GAIN_MIN", min_gain)?;
            hdu.write_key(&mut fptr, "GAIN_MAX", max_gain)?;
            for obj in meta.extended_metadata().iter() {
                hdu.write_key(&mut fptr, &obj.name, obj.value.as_str())?;
            }
        }

        Ok(fpath)
    }
}
