use std::fs::File;
use std::io::{BufReader, BufWriter, Seek, BufRead};
use std::path::Path;
use std::u32;

use crate::codecs::*;

use crate::{ImageOutputFormat, color, error::{UnsupportedError, UnsupportedErrorKind}};
use crate::image;
use crate::dynimage::DynamicImage;
use crate::error::{ImageError, ImageFormatHint, ImageResult};
use crate::image::ImageFormat;
#[allow(unused_imports)]  // When no features are supported
use crate::image::{ImageDecoder, ImageEncoder};

pub(crate) fn open_impl(path: &Path) -> ImageResult<DynamicImage> {
    let buffered_read = BufReader::new(
        File::open(path).map_err(ImageError::IoError)?
    );

    load(buffered_read, ImageFormat::from_path(path)?)
}

/// Create a new image from a Reader.
///
/// Assumes the reader is already buffered. For optimal performance,
/// consider wrapping the reader with a `BufReader::new()`.
///
/// Try [`io::Reader`] for more advanced uses.
///
/// [`io::Reader`]: io/struct.Reader.html
#[allow(unused_variables)]
// r is unused if no features are supported.
pub fn load<R: BufRead + Seek>(buffered_reader: R, format: ImageFormat) -> ImageResult<DynamicImage> {
    #[allow(unreachable_patterns)]
    // Default is unreachable if all features are supported.
    match format {
        #[cfg(feature = "avif-decoder")]
        image::ImageFormat::Avif => DynamicImage::from_decoder(avif::AvifDecoder::new(buffered_reader)?),
        #[cfg(feature = "png")]
        image::ImageFormat::Png => DynamicImage::from_decoder(png::PngDecoder::new(buffered_reader)?),
        #[cfg(feature = "gif")]
        image::ImageFormat::Gif => DynamicImage::from_decoder(gif::GifDecoder::new(buffered_reader)?),
        #[cfg(feature = "jpeg")]
        image::ImageFormat::Jpeg => DynamicImage::from_decoder(jpeg::JpegDecoder::new(buffered_reader)?),
        #[cfg(feature = "webp")]
        image::ImageFormat::WebP => DynamicImage::from_decoder(webp::WebPDecoder::new(buffered_reader)?),
        #[cfg(feature = "tiff")]
        image::ImageFormat::Tiff => DynamicImage::from_decoder(tiff::TiffDecoder::new(buffered_reader)?),
        #[cfg(feature = "tga")]
        image::ImageFormat::Tga => DynamicImage::from_decoder(tga::TgaDecoder::new(buffered_reader)?),
        #[cfg(feature = "dds")]
        image::ImageFormat::Dds => DynamicImage::from_decoder(dds::DdsDecoder::new(buffered_reader)?),
        #[cfg(feature = "bmp")]
        image::ImageFormat::Bmp => DynamicImage::from_decoder(bmp::BmpDecoder::new(buffered_reader)?),
        #[cfg(feature = "ico")]
        image::ImageFormat::Ico => DynamicImage::from_decoder(ico::IcoDecoder::new(buffered_reader)?),
        #[cfg(feature = "hdr")]
        image::ImageFormat::Hdr => DynamicImage::from_decoder(hdr::HdrAdapter::new(buffered_reader)?),
        #[cfg(feature = "openexr")]
        image::ImageFormat::OpenExr => DynamicImage::from_decoder(openexr::OpenExrDecoder::new(buffered_reader)?),
        #[cfg(feature = "pnm")]
        image::ImageFormat::Pnm => DynamicImage::from_decoder(pnm::PnmDecoder::new(buffered_reader)?),
        #[cfg(feature = "farbfeld")]
        image::ImageFormat::Farbfeld => DynamicImage::from_decoder(farbfeld::FarbfeldDecoder::new(buffered_reader)?),
        _ => Err(ImageError::Unsupported(ImageFormatHint::Exact(format).into())),
    }
}

pub(crate) fn image_dimensions_impl(path: &Path) -> ImageResult<(u32, u32)> {
    let format = image::ImageFormat::from_path(path)?;
    let reader = BufReader::new(File::open(path)?);
    image_dimensions_with_format_impl(reader, format)
}

#[allow(unused_variables)]
// fin is unused if no features are supported.
pub(crate) fn image_dimensions_with_format_impl<R: BufRead + Seek>(buffered_read: R, format: ImageFormat)
                                                                   -> ImageResult<(u32, u32)>
{
    #[allow(unreachable_patterns,unreachable_code)]
    // Default is unreachable if all features are supported.
    // Code after the match is unreachable if none are.
    Ok(match format {
        #[cfg(feature = "avif-decoder")]
        image::ImageFormat::Avif => avif::AvifDecoder::new(buffered_read)?.dimensions(),
        #[cfg(feature = "jpeg")]
        image::ImageFormat::Jpeg => jpeg::JpegDecoder::new(buffered_read)?.dimensions(),
        #[cfg(feature = "png")]
        image::ImageFormat::Png => png::PngDecoder::new(buffered_read)?.dimensions(),
        #[cfg(feature = "gif")]
        image::ImageFormat::Gif => gif::GifDecoder::new(buffered_read)?.dimensions(),
        #[cfg(feature = "webp")]
        image::ImageFormat::WebP => webp::WebPDecoder::new(buffered_read)?.dimensions(),
        #[cfg(feature = "tiff")]
        image::ImageFormat::Tiff => tiff::TiffDecoder::new(buffered_read)?.dimensions(),
        #[cfg(feature = "tga")]
        image::ImageFormat::Tga => tga::TgaDecoder::new(buffered_read)?.dimensions(),
        #[cfg(feature = "dds")]
        image::ImageFormat::Dds => dds::DdsDecoder::new(buffered_read)?.dimensions(),
        #[cfg(feature = "bmp")]
        image::ImageFormat::Bmp => bmp::BmpDecoder::new(buffered_read)?.dimensions(),
        #[cfg(feature = "ico")]
        image::ImageFormat::Ico => ico::IcoDecoder::new(buffered_read)?.dimensions(),
        #[cfg(feature = "hdr")]
        image::ImageFormat::Hdr => hdr::HdrAdapter::new(buffered_read)?.dimensions(),
        #[cfg(feature = "openexr")]
        image::ImageFormat::OpenExr => openexr::OpenExrDecoder::new(buffered_read)?.dimensions(),
        #[cfg(feature = "pnm")]
        image::ImageFormat::Pnm => pnm::PnmDecoder::new(buffered_read)?.dimensions(),
        format => return Err(ImageError::Unsupported(ImageFormatHint::Exact(format).into())),
    })
}

#[allow(unused_variables)]
// Most variables when no features are supported
pub(crate) fn save_buffer_impl(
    path: &Path,
    buf: &[u8],
    width: u32,
    height: u32,
    color: color::ColorType,
) -> ImageResult<()> {
    let format =  ImageFormat::from_path(path)?;
    save_buffer_with_format_impl(path, buf, width, height, color, format)
}

#[allow(unused_variables)]
// Most variables when no features are supported
pub(crate) fn save_buffer_with_format_impl(
    path: &Path,
    buf: &[u8],
    width: u32,
    height: u32,
    color: color::ColorType,
    format: ImageFormat,
) -> ImageResult<()> {
    let buffered_file_write = &mut BufWriter::new(File::create(path)?); // always seekable

    let format = match format {
        #[cfg(feature = "pnm")]
        image::ImageFormat::Pnm => {
            let ext = path.extension()
            .and_then(|s| s.to_str())
            .map_or("".to_string(), |s| s.to_ascii_lowercase());
            ImageOutputFormat::Pnm(match &*ext {
                "pbm" => pnm::PnmSubtype::Bitmap(pnm::SampleEncoding::Binary),
                "pgm" => pnm::PnmSubtype::Graymap(pnm::SampleEncoding::Binary),
                "ppm" => pnm::PnmSubtype::Pixmap(pnm::SampleEncoding::Binary),
                "pam" => pnm::PnmSubtype::ArbitraryMap,
                _ => { return Err(ImageError::Unsupported(ImageFormatHint::Exact(format).into())) }, // Unsupported Pnm subtype.
            })
        },
        // #[cfg(feature = "hdr")]
        // image::ImageFormat::Hdr => hdr::HdrEncoder::new(fout).encode(&[Rgb<f32>], width, height), // usize
        format => format.into(),
    };

    write_buffer_impl(buffered_file_write, buf, width, height, color, format)
}

#[allow(unused_variables)]
// Most variables when no features are supported
pub(crate) fn write_buffer_impl<W: std::io::Write + Seek>(
    buffered_write: &mut W,
    buf: &[u8],
    width: u32,
    height: u32,
    color: color::ColorType,
    format: ImageOutputFormat,
) -> ImageResult<()> {
    match format {
        #[cfg(feature = "png")]
        ImageOutputFormat::Png => png::PngEncoder::new(buffered_write).write_image(buf, width, height, color),
        #[cfg(feature = "jpeg")]
        ImageOutputFormat::Jpeg(quality) => jpeg::JpegEncoder::new_with_quality(buffered_write, quality)
            .write_image(buf, width, height, color),
        #[cfg(feature = "pnm")]
        ImageOutputFormat::Pnm(subtype) => pnm::PnmEncoder::new(buffered_write)
            .with_subtype(subtype)
            .write_image(buf, width, height, color),
        #[cfg(feature = "gif")]
        ImageOutputFormat::Gif => gif::GifEncoder::new(buffered_write).encode(buf, width, height, color),
        #[cfg(feature = "ico")]
        ImageOutputFormat::Ico => ico::IcoEncoder::new(buffered_write).write_image(buf, width, height, color),
        #[cfg(feature = "bmp")]
        ImageOutputFormat::Bmp => bmp::BmpEncoder::new(buffered_write).write_image(buf, width, height, color),
        #[cfg(feature = "farbfeld")]
        ImageOutputFormat::Farbfeld => farbfeld::FarbfeldEncoder::new(buffered_write).write_image(buf, width, height, color),
        #[cfg(feature = "tga")]
        ImageOutputFormat::Tga => tga::TgaEncoder::new(buffered_write).write_image(buf, width, height, color),
        #[cfg(feature = "openexr")]
        ImageOutputFormat::OpenExr => openexr::OpenExrEncoder::new(buffered_write).write_image(buf, width, height, color),
        #[cfg(feature = "tiff")]
        ImageOutputFormat::Tiff => tiff::TiffEncoder::new(buffered_write).write_image(buf, width, height, color),
        #[cfg(feature = "avif-encoder")]
        ImageOutputFormat::Avif => avif::AvifEncoder::new(buffered_write).write_image(buf, width, height, color),

        image::ImageOutputFormat::Unsupported(msg) => {
            Err(ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                ImageFormatHint::Unknown,
                UnsupportedErrorKind::Format(ImageFormatHint::Name(msg)))))
        }

        image::ImageOutputFormat::__NonExhaustive(marker) => match marker._private {},

    }
}

static MAGIC_BYTES: [(&[u8], ImageFormat); 21] = [
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
    (&[0x76, 0x2f, 0x31, 0x01], ImageFormat::OpenExr), // = &exr::meta::magic_number::BYTES
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
