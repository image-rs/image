use std::fs::File;
use std::io::{BufRead, BufWriter, Seek};
use std::path::Path;

use crate::{codecs::*, ExtendedColorType, ImageReader};

use crate::dynimage::DynamicImage;
use crate::error::{ImageError, ImageFormatHint, ImageResult};
use crate::error::{UnsupportedError, UnsupportedErrorKind};
use crate::image::ImageFormat;
#[allow(unused_imports)] // When no features are supported
use crate::image::{ImageDecoder, ImageEncoder};

/// Create a new image from a Reader.
///
/// Assumes the reader is already buffered. For optimal performance,
/// consider wrapping the reader with a `BufReader::new()`.
///
/// Try [`ImageReader`] for more advanced uses.
pub fn load<R: BufRead + Seek>(r: R, format: ImageFormat) -> ImageResult<DynamicImage> {
    let mut reader = ImageReader::new(r);
    reader.set_format(format);
    reader.decode()
}

#[allow(unused_variables)]
// Most variables when no features are supported
pub(crate) fn save_buffer_impl(
    path: &Path,
    buf: &[u8],
    width: u32,
    height: u32,
    color: ExtendedColorType,
) -> ImageResult<()> {
    let format = ImageFormat::from_path(path)?;
    save_buffer_with_format_impl(path, buf, width, height, color, format)
}

#[allow(unused_variables)]
// Most variables when no features are supported
pub(crate) fn save_buffer_with_format_impl(
    path: &Path,
    buf: &[u8],
    width: u32,
    height: u32,
    color: ExtendedColorType,
    format: ImageFormat,
) -> ImageResult<()> {
    let buffered_file_write = &mut BufWriter::new(File::create(path)?); // always seekable
    write_buffer_impl(buffered_file_write, buf, width, height, color, format)
}

#[allow(unused_variables)]
// Most variables when no features are supported
pub(crate) fn write_buffer_impl<W: std::io::Write + Seek>(
    buffered_write: &mut W,
    buf: &[u8],
    width: u32,
    height: u32,
    color: ExtendedColorType,
    format: ImageFormat,
) -> ImageResult<()> {
    match format {
        #[cfg(feature = "png")]
        ImageFormat::Png => {
            png::PngEncoder::new(buffered_write).write_image(buf, width, height, color)
        }
        #[cfg(feature = "jpeg")]
        ImageFormat::Jpeg => {
            jpeg::JpegEncoder::new(buffered_write).write_image(buf, width, height, color)
        }
        #[cfg(feature = "pnm")]
        ImageFormat::Pnm => {
            pnm::PnmEncoder::new(buffered_write).write_image(buf, width, height, color)
        }
        #[cfg(feature = "gif")]
        ImageFormat::Gif => gif::GifEncoder::new(buffered_write).encode(buf, width, height, color),
        #[cfg(feature = "ico")]
        ImageFormat::Ico => {
            ico::IcoEncoder::new(buffered_write).write_image(buf, width, height, color)
        }
        #[cfg(feature = "bmp")]
        ImageFormat::Bmp => {
            bmp::BmpEncoder::new(buffered_write).write_image(buf, width, height, color)
        }
        #[cfg(feature = "ff")]
        ImageFormat::Farbfeld => {
            farbfeld::FarbfeldEncoder::new(buffered_write).write_image(buf, width, height, color)
        }
        #[cfg(feature = "tga")]
        ImageFormat::Tga => {
            tga::TgaEncoder::new(buffered_write).write_image(buf, width, height, color)
        }
        #[cfg(feature = "exr")]
        ImageFormat::OpenExr => {
            openexr::OpenExrEncoder::new(buffered_write).write_image(buf, width, height, color)
        }
        #[cfg(feature = "tiff")]
        ImageFormat::Tiff => {
            tiff::TiffEncoder::new(buffered_write).write_image(buf, width, height, color)
        }
        #[cfg(feature = "avif")]
        ImageFormat::Avif => {
            avif::AvifEncoder::new(buffered_write).write_image(buf, width, height, color)
        }
        #[cfg(feature = "qoi")]
        ImageFormat::Qoi => {
            qoi::QoiEncoder::new(buffered_write).write_image(buf, width, height, color)
        }
        #[cfg(feature = "webp")]
        ImageFormat::WebP => {
            webp::WebPEncoder::new_lossless(buffered_write).write_image(buf, width, height, color)
        }
        #[cfg(feature = "hdr")]
        ImageFormat::Hdr => {
            hdr::HdrEncoder::new(buffered_write).write_image(buf, width, height, color)
        }
        _ => Err(ImageError::Unsupported(
            UnsupportedError::from_format_and_kind(
                ImageFormatHint::Unknown,
                UnsupportedErrorKind::Format(ImageFormatHint::Name(format!("{format:?}"))),
            ),
        )),
    }
}

static MAGIC_BYTES: [(&[u8], ImageFormat); 25] = [
    (b"\x89PNG\r\n\x1a\n", ImageFormat::Png),
    (&[0xff, 0xd8, 0xff], ImageFormat::Jpeg),
    (b"GIF89a", ImageFormat::Gif),
    (b"GIF87a", ImageFormat::Gif),
    (b"RIFF", ImageFormat::WebP), // TODO: better magic byte detection, see https://github.com/image-rs/image/issues/660
    (b"MM\x00*", ImageFormat::Tiff),
    (b"II*\x00", ImageFormat::Tiff),
    (b"DDS ", ImageFormat::Dds),
    (b"BM", ImageFormat::Bmp),
    (&[0, 0, 1, 0], ImageFormat::Ico),
    (b"#?RADIANCE", ImageFormat::Hdr),
    (b"P1", ImageFormat::Pnm),
    (b"P2", ImageFormat::Pnm),
    (b"P3", ImageFormat::Pnm),
    (b"P4", ImageFormat::Pnm),
    (b"P5", ImageFormat::Pnm),
    (b"P6", ImageFormat::Pnm),
    (b"P7", ImageFormat::Pnm),
    (b"farbfeld", ImageFormat::Farbfeld),
    (b"\0\0\0 ftypavif", ImageFormat::Avif),
    (b"\0\0\0\x1cftypavif", ImageFormat::Avif),
    (&[0x76, 0x2f, 0x31, 0x01], ImageFormat::OpenExr), // = &exr::meta::magic_number::BYTES
    (b"qoif", ImageFormat::Qoi),
    (&[0x0a, 0x02], ImageFormat::Pcx),
    (&[0x0a, 0x05], ImageFormat::Pcx),
];

/// Guess image format from memory block
///
/// Makes an educated guess about the image format based on the Magic Bytes at the beginning.
/// TGA is not supported by this function.
/// This is not to be trusted on the validity of the whole memory block
pub fn guess_format(buffer: &[u8]) -> ImageResult<ImageFormat> {
    match guess_format_impl(buffer) {
        Some(format) => Ok(format),
        None => Err(ImageError::Unsupported(ImageFormatHint::Unknown.into())),
    }
}

pub(crate) fn guess_format_impl(buffer: &[u8]) -> Option<ImageFormat> {
    for &(signature, format) in &MAGIC_BYTES {
        if buffer.starts_with(signature) {
            return Some(format);
        }
    }

    None
}
