use chrono::DateTime;
use std::{
    fs::remove_file,
    io,
    ops::Deref,
    path::{Path, PathBuf},
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use fitsio::{
    errors::Error as FitsError,
    images::{ImageDescription, ImageType},
    FitsFile,
};

use crate::{Pixel, SerialImageBuffer};

#[cfg_attr(docsrs, doc(cfg(feature = "fitsio")))]
impl<P: Pixel, Container: Deref<Target = [P::Subpixel]>> SerialImageBuffer<P, Container> {
    // #[cfg(feature = "fitsio")]
    /// Save the image data to a FITS file.
    ///
    /// # Arguments
    ///  * `dir_prefix` - The directory where the file will be saved.
    ///  * `file_prefix` - The prefix of the file name. The file name will be of the form `{file_prefix}_{timestamp}.fits`.
    ///  * `progname` - The name of the program that generated the image.
    ///  * `compress` - Whether to compress the FITS file.
    ///  * `overwrite` - Whether to overwrite the file if it already exists.
    ///  * `image_type` - The type of the image data (e.g. [`ImageType::UnsignedByte`])
    ///
    /// # Errors
    ///  * [`fitsio::errors::Error`] with the error description.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn savefits_generic(
        &self,
        dir_prefix: &Path,
        file_prefix: &str,
        progname: Option<&str>,
        compress: bool,
        overwrite: bool,
        data_type: ImageType,
        numpix: usize,
    ) -> Result<PathBuf, FitsError> {
        // let data_type = imagetype_from_pixel::<P>()?;
        if !dir_prefix.exists() {
            return Err(FitsError::Io(io::Error::new(
                io::ErrorKind::NotFound,
                format!("Directory {:?} does not exist", dir_prefix),
            )));
        }
        let timestamp;
        let cameraname;
        if let Some(metadata) = self.get_metadata() {
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

        if fpath.exists() {
            if !overwrite {
                return Err(FitsError::Io(io::Error::new(
                    io::ErrorKind::AlreadyExists,
                    format!("File {:?} already exists", fpath),
                )));
            } else {
                let res = remove_file(fpath.clone());
                if let Err(msg) = res {
                    return Err(FitsError::Io(io::Error::new(
                        io::ErrorKind::Other,
                        format!("Could not remove file {:?}: {}", fpath, msg),
                    )));
                }
            }
        }
        let (width, height) = (self.width(), self.height());
        let imgsize = [height as usize, width as usize, numpix];

        // let data_type =
        // let data_type = match TypeId::of::<P>() {
        //     u8 => ImageType::Short,
        //     u16 => fitsio::DataType::Long,
        //     f32 => fitsio::DataType::Float,
        //     f64 => fitsio::DataType::Double,
        //     _ => return Err(FitsError::Message("Unsupported pixel type".to_owned())),
        // };

        let img_desc = ImageDescription {
            data_type,
            dimensions: &imgsize,
        };

        let path = Path::new(dir_prefix).join(Path::new(&format!(
            "{}_{}.fits{}",
            file_prefix,
            timestamp,
            if compress { "[compress]" } else { "" }
        )));

        let mut fptr = FitsFile::create(path)
            .with_custom_primary(&img_desc)
            .open()?;
        let hdu = fptr.create_image("test", &img_desc)?;
        // let hdu = fptr.primary_hdu()?;
        hdu.write_image(&mut fptr, self.inner_pixels())?;
        hdu.write_key(&mut fptr, "PROGRAM", progname.unwrap_or("unknown"))?;
        hdu.write_key(&mut fptr, "CAMERA", cameraname)?;
        hdu.write_key(&mut fptr, "TIMESTAMP", ts)?;
        if let Some(meta) = self.get_metadata() {
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
