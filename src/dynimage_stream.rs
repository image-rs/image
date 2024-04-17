use base64::{engine::general_purpose::STANDARD_NO_PAD, Engine};
use std::io::Cursor;

use crate::{DynamicImage, ImageFormat, ImageResult};

impl DynamicImage {
    /// Encode the image as a PNG base64 string.
    pub fn png_base64(&self) -> ImageResult<String> {
        let mut buf = Cursor::new(Vec::new());
        self.write_to(&mut buf, ImageFormat::Png)?;
        Ok(STANDARD_NO_PAD.encode(buf.into_inner()))
    }

    /// Encode the image as a JPEG base64 string.
    pub fn jpeg_base64(&self) -> ImageResult<String> {
        let mut buf = Cursor::new(Vec::new());
        self.write_to(&mut buf, ImageFormat::Jpeg)?;
        Ok(STANDARD_NO_PAD.encode(buf.into_inner()))
    }

    /// Encode the image as a WEBP base64 string.
    pub fn webp_base64(&self) -> ImageResult<String> {
        let mut buf = Cursor::new(Vec::new());
        self.write_to(&mut buf, ImageFormat::WebP)?;
        Ok(STANDARD_NO_PAD.encode(buf.into_inner()))
    }
}
