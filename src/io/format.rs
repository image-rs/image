use std::fmt::{self};
use std::io::{Seek, Write};
use std::path::Path;
use std::{ffi::OsStr, io::BufRead};

use crate::error::{UnsupportedError, UnsupportedErrorKind};
use crate::hooks::GenericReader;
use crate::io::encoder::ImageEncoderBoxed;
use crate::ImageDecoder;
use crate::{
    error::{ImageError, ImageFormatHint, ImageResult},
    io::registry::{self, read_registry, RegistryId},
};

/// A supported image format.
///
/// Not all formats support both encoding and decoding.
// TODO: serde Serialize/Deserialize
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct ImageFormat(RegistryId);

#[allow(non_upper_case_globals)]
impl ImageFormat {
    /// An Image in PNG Format
    pub const Png: Self = Self(registry::PNG_ID);
    /// An Image in JPEG Format
    pub const Jpeg: Self = Self(registry::JPEG_ID);
    /// An Image in GIF Format
    pub const Gif: Self = Self(registry::GIF_ID);
    /// An Image in WEBP Format
    pub const WebP: Self = Self(registry::WEBP_ID);
    /// An Image in PNM Format
    pub const Pnm: Self = Self(registry::PNM_ID);
    /// An Image in TIFF Format
    pub const Tiff: Self = Self(registry::TIFF_ID);
    /// An Image in TGA Format
    pub const Tga: Self = Self(registry::TGA_ID);
    /// An Image in DDS Format
    pub const Dds: Self = Self(registry::DDS_ID);
    /// An Image in BMP Format
    pub const Bmp: Self = Self(registry::BMP_ID);
    /// An Image in ICO Format
    pub const Ico: Self = Self(registry::ICO_ID);
    /// An Image in HDR Format
    pub const Hdr: Self = Self(registry::HDR_ID);
    /// An Image in EXR Format
    pub const OpenExr: Self = Self(registry::EXR_ID);
    /// An Image in Farbfeld Format
    pub const Farbfeld: Self = Self(registry::FARBFELD_ID);
    /// An Image in AVIF Format
    pub const Avif: Self = Self(registry::AVIF_ID);
    /// An Image in QOI Format
    pub const Qoi: Self = Self(registry::QOI_ID);

    pub(crate) fn from_id(id: RegistryId) -> Self {
        Self(id)
    }
    pub(crate) fn id(&self) -> RegistryId {
        self.0
    }

    /// Return the image format specified by a path's file extension.
    ///
    /// # Example
    ///
    /// ```
    /// use image::ImageFormat;
    ///
    /// let format = ImageFormat::from_extension("jpg");
    /// assert_eq!(format, Some(ImageFormat::Jpeg));
    /// ```
    #[inline]
    pub fn from_extension<S>(ext: S) -> Option<Self>
    where
        S: AsRef<OsStr>,
    {
        // thin wrapper function to strip generics
        fn inner(ext: &OsStr) -> Option<ImageFormat> {
            let ext = ext.to_str()?;
            let id = read_registry(|reg| reg.get_by_extension(ext))?;
            Some(ImageFormat(id))
        }

        inner(ext.as_ref())
    }

    /// Return the image format specified by the path's file extension.
    ///
    /// # Example
    ///
    /// ```
    /// use image::ImageFormat;
    ///
    /// let format = ImageFormat::from_path("images/ferris.png")?;
    /// assert_eq!(format, ImageFormat::Png);
    ///
    /// # Ok::<(), image::error::ImageError>(())
    /// ```
    #[inline]
    pub fn from_path<P>(path: P) -> ImageResult<Self>
    where
        P: AsRef<Path>,
    {
        // thin wrapper function to strip generics
        fn inner(path: &Path) -> ImageResult<ImageFormat> {
            let exact_ext = path.extension();
            exact_ext
                .and_then(ImageFormat::from_extension)
                .ok_or_else(|| {
                    let format_hint = match exact_ext {
                        None => ImageFormatHint::Unknown,
                        Some(os) => ImageFormatHint::PathExtension(os.into()),
                    };
                    ImageError::Unsupported(format_hint.into())
                })
        }

        inner(path.as_ref())
    }

    /// Return the image format specified by a MIME type.
    ///
    /// # Example
    ///
    /// ```
    /// use image::ImageFormat;
    ///
    /// let format = ImageFormat::from_mime_type("image/png").unwrap();
    /// assert_eq!(format, ImageFormat::Png);
    /// ```
    pub fn from_mime_type<M>(mime_type: M) -> Option<Self>
    where
        M: AsRef<str>,
    {
        // thin wrapper function to strip generics
        fn inner(mime_type: &str) -> Option<ImageFormat> {
            let id = read_registry(|reg| reg.get_by_mime_type(mime_type))?;
            Some(ImageFormat(id))
        }

        inner(mime_type.as_ref())
    }

    /// Return the MIME type for this image format or "application/octet-stream" if no MIME type
    /// exists for the format.
    ///
    /// Some notes on a few of the MIME types:
    ///
    /// - The portable anymap format has a separate MIME type for the pixmap, graymap and bitmap
    ///   formats, but this method returns the general "image/x-portable-anymap" MIME type.
    /// - The Targa format has two common MIME types, "image/x-targa"  and "image/x-tga"; this
    ///   method returns "image/x-targa" for that format.
    /// - The QOI MIME type is still a work in progress. This method returns "image/x-qoi" for
    ///   that format.
    ///
    /// # Example
    ///
    /// ```
    /// use image::ImageFormat;
    ///
    /// let mime_type = ImageFormat::Png.to_mime_type();
    /// assert_eq!(mime_type, "image/png");
    /// ```
    #[must_use]
    pub fn to_mime_type(&self) -> &'static str {
        read_registry(|reg| reg.get(self.0).main_mime_type()).unwrap_or("application/octet-stream")
    }

    /// Return if the `ImageFormat` can be decoded by the lib.
    // TODO: rethink this API
    #[must_use]
    pub fn can_read(&self) -> bool {
        // Needs to be updated once a new variant's decoder is added to free_functions.rs::load
        read_registry(|reg| reg.get(self.0).can_read)
    }

    /// Return if the `ImageFormat` can be encoded by the lib.
    // TODO: rethink this API
    #[must_use]
    pub fn can_write(&self) -> bool {
        // Needs to be updated once a new variant's encoder is added to free_functions.rs::save_buffer_with_format_impl
        read_registry(|reg| reg.get(self.0).can_write)
    }

    /// Return a list of applicable extensions for this format.
    ///
    /// All currently recognized image formats specify at least on extension but for future
    /// compatibility you should not rely on this fact. The list may be empty if the format has no
    /// recognized file representation, for example in case it is used as a purely transient memory
    /// format.
    ///
    /// The method name `extensions` remains reserved for introducing another method in the future
    /// that yields a slice of `OsStr` which is blocked by several features of const evaluation.
    #[must_use]
    pub fn extensions_str(self) -> &'static [&'static str] {
        read_registry(|reg| reg.get(self.0).all_extensions())
    }

    /// Return the `ImageFormat`s which are enabled for reading.
    // TODO: rethink this API
    #[must_use]
    pub fn reading_enabled(&self) -> bool {
        read_registry(|reg| reg.get(self.0).feature_enabled)
    }

    /// Return the `ImageFormat`s which are enabled for writing.
    // TODO: rethink this API
    #[must_use]
    pub fn writing_enabled(&self) -> bool {
        read_registry(|reg| reg.get(self.0).feature_enabled)
    }

    /// Return all `ImageFormat`s.
    ///
    /// Format hooks added after calling this function will not be included in the returned
    /// iterator.
    pub fn all() -> impl Iterator<Item = Self> {
        read_registry(|reg| reg.all()).map(ImageFormat)
    }

    pub(crate) fn create_decoder<'a>(
        self,
        reader: impl BufRead + Seek + 'a,
        mut limits: Option<crate::Limits>,
    ) -> ImageResult<Box<dyn ImageDecoder + 'a>> {
        use crate::codecs::*;

        // Since hooks can add decoding functions for builtin formats, we always need to check the
        // registry to implement the override behavior hooks guarantee.
        let decoding_fn = read_registry(|reg| reg.get(self.id()).decoding_fn.clone());

        // use static dispatch for builtin-formats to avoid any overhead added by GenericReader
        let mut decoder: Box<dyn ImageDecoder + 'a> = if let Some(decoding_fn) = decoding_fn {
            (decoding_fn)(GenericReader::new(reader))?
        } else {
            match self {
                #[cfg(feature = "avif-native")]
                Self::Avif => Box::new(avif::AvifDecoder::new(reader)?),
                #[cfg(feature = "png")]
                Self::Png => Box::new(png::PngDecoder::with_limits(
                    reader,
                    limits.take().unwrap_or_default(),
                )?),
                #[cfg(feature = "gif")]
                Self::Gif => Box::new(gif::GifDecoder::new(reader)?),
                #[cfg(feature = "jpeg")]
                Self::Jpeg => Box::new(jpeg::JpegDecoder::new(reader)?),
                #[cfg(feature = "webp")]
                Self::WebP => Box::new(webp::WebPDecoder::new(reader)?),
                #[cfg(feature = "tiff")]
                Self::Tiff => Box::new(tiff::TiffDecoder::new(reader)?),
                #[cfg(feature = "tga")]
                Self::Tga => Box::new(tga::TgaDecoder::new(reader)?),
                #[cfg(feature = "dds")]
                Self::Dds => Box::new(dds::DdsDecoder::new(reader)?),
                #[cfg(feature = "bmp")]
                Self::Bmp => Box::new(bmp::BmpDecoder::new(reader)?),
                #[cfg(feature = "ico")]
                Self::Ico => Box::new(ico::IcoDecoder::new(reader)?),
                #[cfg(feature = "hdr")]
                Self::Hdr => Box::new(hdr::HdrDecoder::new(reader)?),
                #[cfg(feature = "exr")]
                Self::OpenExr => Box::new(openexr::OpenExrDecoder::new(reader)?),
                #[cfg(feature = "pnm")]
                Self::Pnm => Box::new(pnm::PnmDecoder::new(reader)?),
                #[cfg(feature = "ff")]
                Self::Farbfeld => Box::new(farbfeld::FarbfeldDecoder::new(reader)?),
                #[cfg(feature = "qoi")]
                Self::Qoi => Box::new(qoi::QoiDecoder::new(reader)?),
                _ => return Err(ImageError::Unsupported(ImageFormatHint::Exact(self).into())),
            }
        };

        // set limits (if any (left))
        if let Some(limits) = limits {
            decoder.set_limits(limits)?;
        }

        Ok(decoder)
    }
    pub(crate) fn create_encoder<'a, W: Write + Seek>(
        self,
        buffered_write: &'a mut W,
    ) -> ImageResult<Box<dyn ImageEncoderBoxed + 'a>> {
        use crate::codecs::*;

        Ok(match self {
            #[cfg(feature = "png")]
            Self::Png => Box::new(png::PngEncoder::new(buffered_write)),
            #[cfg(feature = "jpeg")]
            Self::Jpeg => Box::new(jpeg::JpegEncoder::new(buffered_write)),
            #[cfg(feature = "pnm")]
            Self::Pnm => Box::new(pnm::PnmEncoder::new(buffered_write)),
            #[cfg(feature = "gif")]
            Self::Gif => Box::new(gif::GifEncoder::new(buffered_write)),
            #[cfg(feature = "ico")]
            Self::Ico => Box::new(ico::IcoEncoder::new(buffered_write)),
            #[cfg(feature = "bmp")]
            Self::Bmp => Box::new(bmp::BmpEncoder::new(buffered_write)),
            #[cfg(feature = "ff")]
            Self::Farbfeld => Box::new(farbfeld::FarbfeldEncoder::new(buffered_write)),
            #[cfg(feature = "tga")]
            Self::Tga => Box::new(tga::TgaEncoder::new(buffered_write)),
            #[cfg(feature = "exr")]
            Self::OpenExr => Box::new(openexr::OpenExrEncoder::new(buffered_write)),
            #[cfg(feature = "tiff")]
            Self::Tiff => Box::new(tiff::TiffEncoder::new(buffered_write)),
            #[cfg(feature = "avif")]
            Self::Avif => Box::new(avif::AvifEncoder::new(buffered_write)),
            #[cfg(feature = "qoi")]
            Self::Qoi => Box::new(qoi::QoiEncoder::new(buffered_write)),
            #[cfg(feature = "webp")]
            Self::WebP => Box::new(webp::WebPEncoder::new_lossless(buffered_write)),
            #[cfg(feature = "hdr")]
            Self::Hdr => Box::new(hdr::HdrEncoder::new(buffered_write)),
            _ => {
                return Err(ImageError::Unsupported(
                    // TODO: probably change this error to be consistent with create_decoder
                    UnsupportedError::from_format_and_kind(
                        ImageFormatHint::Unknown,
                        UnsupportedErrorKind::Format(ImageFormatHint::Name(format!("{self:?}"))),
                    ),
                ));
            }
        })
    }
}
impl fmt::Debug for ImageFormat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            registry::PNG_ID => f.write_str("PNG"),
            registry::JPEG_ID => f.write_str("JPEG"),
            registry::GIF_ID => f.write_str("GIF"),
            registry::WEBP_ID => f.write_str("WEBP"),
            registry::PNM_ID => f.write_str("PNM"),
            registry::TIFF_ID => f.write_str("TIFF"),
            registry::TGA_ID => f.write_str("TGA"),
            registry::DDS_ID => f.write_str("DDS"),
            registry::BMP_ID => f.write_str("BMP"),
            registry::ICO_ID => f.write_str("ICO"),
            registry::HDR_ID => f.write_str("HDR"),
            registry::EXR_ID => f.write_str("EXR"),
            registry::FARBFELD_ID => f.write_str("Farbfeld"),
            registry::AVIF_ID => f.write_str("AVIF"),
            registry::QOI_ID => f.write_str("QOI"),
            _ => {
                let ext = read_registry(|reg| reg.get(self.0).main_extension());
                f.write_str(&ext.to_ascii_uppercase())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use std::path::Path;

    use super::{ImageFormat, ImageResult};

    #[test]
    fn test_image_format_from_path() {
        fn from_path(s: &str) -> ImageResult<ImageFormat> {
            ImageFormat::from_path(Path::new(s))
        }
        assert_eq!(from_path("./a.jpg").unwrap(), ImageFormat::Jpeg);
        assert_eq!(from_path("./a.jpeg").unwrap(), ImageFormat::Jpeg);
        assert_eq!(from_path("./a.JPEG").unwrap(), ImageFormat::Jpeg);
        assert_eq!(from_path("./a.pNg").unwrap(), ImageFormat::Png);
        assert_eq!(from_path("./a.gif").unwrap(), ImageFormat::Gif);
        assert_eq!(from_path("./a.webp").unwrap(), ImageFormat::WebP);
        assert_eq!(from_path("./a.tiFF").unwrap(), ImageFormat::Tiff);
        assert_eq!(from_path("./a.tif").unwrap(), ImageFormat::Tiff);
        assert_eq!(from_path("./a.tga").unwrap(), ImageFormat::Tga);
        assert_eq!(from_path("./a.dds").unwrap(), ImageFormat::Dds);
        assert_eq!(from_path("./a.bmp").unwrap(), ImageFormat::Bmp);
        assert_eq!(from_path("./a.Ico").unwrap(), ImageFormat::Ico);
        assert_eq!(from_path("./a.hdr").unwrap(), ImageFormat::Hdr);
        assert_eq!(from_path("./a.exr").unwrap(), ImageFormat::OpenExr);
        assert_eq!(from_path("./a.pbm").unwrap(), ImageFormat::Pnm);
        assert_eq!(from_path("./a.pAM").unwrap(), ImageFormat::Pnm);
        assert_eq!(from_path("./a.Ppm").unwrap(), ImageFormat::Pnm);
        assert_eq!(from_path("./a.pgm").unwrap(), ImageFormat::Pnm);
        assert_eq!(from_path("./a.AViF").unwrap(), ImageFormat::Avif);
        assert!(from_path("./a.txt").is_err());
        assert!(from_path("./a").is_err());
    }

    #[test]
    fn image_formats_are_recognized() {
        for format in ImageFormat::all() {
            let mut file = Path::new("file.nothing").to_owned();
            for ext in format.extensions_str() {
                assert!(file.set_extension(ext));
                match ImageFormat::from_path(&file) {
                    Err(_) => panic!("Path {} not recognized as {:?}", file.display(), format),
                    Ok(result) => assert_eq!(format, result),
                }
            }
        }
    }

    #[test]
    fn all() {
        let all_formats: HashSet<ImageFormat> = ImageFormat::all().collect();
        assert!(all_formats.contains(&ImageFormat::Avif));
        assert!(all_formats.contains(&ImageFormat::Gif));
        assert!(all_formats.contains(&ImageFormat::Bmp));
        assert!(all_formats.contains(&ImageFormat::Farbfeld));
        assert!(all_formats.contains(&ImageFormat::Jpeg));
    }

    #[test]
    fn reading_enabled() {
        assert_eq!(cfg!(feature = "jpeg"), ImageFormat::Jpeg.reading_enabled());
        assert_eq!(
            cfg!(feature = "ff"),
            ImageFormat::Farbfeld.reading_enabled()
        );
        assert!(!ImageFormat::Dds.reading_enabled());
    }

    #[test]
    fn writing_enabled() {
        assert_eq!(cfg!(feature = "jpeg"), ImageFormat::Jpeg.writing_enabled());
        assert_eq!(
            cfg!(feature = "ff"),
            ImageFormat::Farbfeld.writing_enabled()
        );
        assert!(!ImageFormat::Dds.writing_enabled());
    }
}
