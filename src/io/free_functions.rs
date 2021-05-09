use std::fs::File;
use std::io::{BufRead, BufReader, BufWriter, Seek};
use std::path::Path;
use std::u32;

#[cfg(feature = "bmp")]
use crate::codecs::bmp;
#[cfg(feature = "gif")]
use crate::codecs::gif;
#[cfg(feature = "hdr")]
use crate::codecs::hdr;
#[cfg(feature = "openexr")]
use crate::codecs::openexr;
#[cfg(feature = "ico")]
use crate::codecs::ico;
#[cfg(feature = "jpeg")]
use crate::codecs::jpeg;
#[cfg(feature = "png")]
use crate::codecs::png;
#[cfg(feature = "pnm")]
use crate::codecs::pnm;
#[cfg(feature = "tga")]
use crate::codecs::tga;
#[cfg(feature = "dds")]
use crate::codecs::dds;
#[cfg(feature = "tiff")]
use crate::codecs::tiff;
#[cfg(feature = "webp")]
use crate::codecs::webp;
#[cfg(feature = "farbfeld")]
use crate::codecs::farbfeld;
#[cfg(any(feature = "avif-encoder", feature = "avif-decoder"))]
use crate::codecs::avif;

use crate::{ImageOutputFormat, color, error::{UnsupportedError, UnsupportedErrorKind}};
use crate::image;
use crate::dynimage::DynamicImage;
use crate::error::{ImageError, ImageFormatHint, ImageResult};
use crate::image::ImageFormat;
#[allow(unused_imports)]  // When no features are supported
use crate::image::{ImageDecoder, ImageEncoder};

pub(crate) fn open_impl(path: &Path) -> ImageResult<DynamicImage> {
    let fin = match File::open(path) {
        Ok(f) => f,
        Err(err) => return Err(ImageError::IoError(err)),
    };
    let fin = BufReader::new(fin);

    load(fin, ImageFormat::from_path(path)?)
}

/// Create a new image from a Reader
///
/// Try [`io::Reader`] for more advanced uses.
///
/// [`io::Reader`]: io/struct.Reader.html
#[allow(unused_variables)]
// r is unused if no features are supported.
pub fn load<R: BufRead + Seek>(r: R, format: ImageFormat) -> ImageResult<DynamicImage> {
    #[allow(unreachable_patterns)]
    // Default is unreachable if all features are supported.
    match format {
        #[cfg(feature = "avif-decoder")]
        image::ImageFormat::Avif => DynamicImage::from_decoder(avif::AvifDecoder::new(r)?),
        #[cfg(feature = "png")]
        image::ImageFormat::Png => DynamicImage::from_decoder(png::PngDecoder::new(r)?),
        #[cfg(feature = "gif")]
        image::ImageFormat::Gif => DynamicImage::from_decoder(gif::GifDecoder::new(r)?),
        #[cfg(feature = "jpeg")]
        image::ImageFormat::Jpeg => DynamicImage::from_decoder(jpeg::JpegDecoder::new(r)?),
        #[cfg(feature = "webp")]
        image::ImageFormat::WebP => DynamicImage::from_decoder(webp::WebPDecoder::new(r)?),
        #[cfg(feature = "tiff")]
        image::ImageFormat::Tiff => DynamicImage::from_decoder(tiff::TiffDecoder::new(r)?),
        #[cfg(feature = "tga")]
        image::ImageFormat::Tga => DynamicImage::from_decoder(tga::TgaDecoder::new(r)?),
        #[cfg(feature = "dds")]
        image::ImageFormat::Dds => DynamicImage::from_decoder(dds::DdsDecoder::new(r)?),
        #[cfg(feature = "bmp")]
        image::ImageFormat::Bmp => DynamicImage::from_decoder(bmp::BmpDecoder::new(r)?),
        #[cfg(feature = "ico")]
        image::ImageFormat::Ico => DynamicImage::from_decoder(ico::IcoDecoder::new(r)?),
        #[cfg(feature = "hdr")]
        image::ImageFormat::Hdr => DynamicImage::from_decoder(hdr::HdrAdapter::new(BufReader::new(r))?),
        #[cfg(feature = "openexr")]
        image::ImageFormat::Exr => DynamicImage::from_decoder(openexr::ExrDecoder::read(r)?),
        #[cfg(feature = "pnm")]
        image::ImageFormat::Pnm => DynamicImage::from_decoder(pnm::PnmDecoder::new(BufReader::new(r))?),
        #[cfg(feature = "farbfeld")]
        image::ImageFormat::Farbfeld => DynamicImage::from_decoder(farbfeld::FarbfeldDecoder::new(r)?),
        _ => Err(ImageError::Unsupported(ImageFormatHint::Exact(format).into())),
    }
}

pub(crate) fn image_dimensions_impl(path: &Path) -> ImageResult<(u32, u32)> {
    let format = image::ImageFormat::from_path(path)?;

    let fin = File::open(path)?;
    let fin = BufReader::new(fin);

    image_dimensions_with_format_impl(fin, format)
}

#[allow(unused_variables)]
// fin is unused if no features are supported.
pub(crate) fn image_dimensions_with_format_impl<R: BufRead + Seek>(fin: R, format: ImageFormat)
    -> ImageResult<(u32, u32)>
{
    #[allow(unreachable_patterns,unreachable_code)]
    // Default is unreachable if all features are supported.
    // Code after the match is unreachable if none are.
    Ok(match format {
        #[cfg(feature = "avif-decoder")]
        image::ImageFormat::Avif => avif::AvifDecoder::new(fin)?.dimensions(),
        #[cfg(feature = "jpeg")]
        image::ImageFormat::Jpeg => jpeg::JpegDecoder::new(fin)?.dimensions(),
        #[cfg(feature = "png")]
        image::ImageFormat::Png => png::PngDecoder::new(fin)?.dimensions(),
        #[cfg(feature = "gif")]
        image::ImageFormat::Gif => gif::GifDecoder::new(fin)?.dimensions(),
        #[cfg(feature = "webp")]
        image::ImageFormat::WebP => webp::WebPDecoder::new(fin)?.dimensions(),
        #[cfg(feature = "tiff")]
        image::ImageFormat::Tiff => tiff::TiffDecoder::new(fin)?.dimensions(),
        #[cfg(feature = "tga")]
        image::ImageFormat::Tga => tga::TgaDecoder::new(fin)?.dimensions(),
        #[cfg(feature = "dds")]
        image::ImageFormat::Dds => dds::DdsDecoder::new(fin)?.dimensions(),
        #[cfg(feature = "bmp")]
        image::ImageFormat::Bmp => bmp::BmpDecoder::new(fin)?.dimensions(),
        #[cfg(feature = "ico")]
        image::ImageFormat::Ico => ico::IcoDecoder::new(fin)?.dimensions(),
        #[cfg(feature = "hdr")]
        image::ImageFormat::Hdr => hdr::HdrAdapter::new(fin)?.dimensions(),
        #[cfg(feature = "openexr")]
        image::ImageFormat::Exr => openexr::ExrDecoder::read(fin)?.dimensions(),
        #[cfg(feature = "pnm")]
        image::ImageFormat::Pnm => {
            pnm::PnmDecoder::new(fin)?.dimensions()
        }
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
    let fout = &mut BufWriter::new(File::create(path)?);
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
    let fout = &mut BufWriter::new(File::create(path)?);

    let format = match format {
        #[cfg(feature = "pnm")]
        image::ImageFormat::Pnm => {
            let ext = path.extension()
            .and_then(|s| s.to_str())
            .map_or("".to_string(), |s| s.to_ascii_lowercase());
            ImageOutputFormat::Pnm(match &*ext {
                "pbm" => pnm::PNMSubtype::Bitmap(pnm::SampleEncoding::Binary),
                "pgm" => pnm::PNMSubtype::Graymap(pnm::SampleEncoding::Binary),
                "ppm" => pnm::PNMSubtype::Pixmap(pnm::SampleEncoding::Binary),
                _ => { return Err(ImageError::Unsupported(ImageFormatHint::Exact(format).into())) }, // Unsupported Pnm subtype.
            })
        },
        // #[cfg(feature = "hdr")]
        // image::ImageFormat::Hdr => hdr::HdrEncoder::new(fout).encode(&[Rgb<f32>], width, height), // usize
        #[cfg(feature = "tiff")]
        image::ImageFormat::Tiff => {
            return tiff::TiffEncoder::new(fout).write_image(buf, width, height, color);
        },
        format => format.into(),
    };

    write_buffer_impl(fout, buf, width, height, color, format)
}

#[allow(unused_variables)]
// Most variables when no features are supported
pub(crate) fn write_buffer_impl<W: std::io::Write>(
    fout: &mut W,
    buf: &[u8],
    width: u32,
    height: u32,
    color: color::ColorType,
    format: ImageOutputFormat,
) -> ImageResult<()> {
    match format {
        #[cfg(feature = "png")]
        ImageOutputFormat::Png => png::PngEncoder::new(fout).write_image(buf, width, height, color),
        #[cfg(feature = "jpeg")]
        ImageOutputFormat::Jpeg(quality) => jpeg::JpegEncoder::new_with_quality(fout, quality)
            .write_image(buf, width, height, color),
        #[cfg(feature = "pnm")]
        ImageOutputFormat::Pnm(subtype) => pnm::PnmEncoder::new(fout)
            .with_subtype(subtype)
            .write_image(buf, width, height, color),
        #[cfg(feature = "gif")]
        ImageOutputFormat::Gif => gif::GifEncoder::new(fout).encode(buf, width, height, color),
        #[cfg(feature = "ico")]
        ImageOutputFormat::Ico => ico::IcoEncoder::new(fout).write_image(buf, width, height, color),
        #[cfg(feature = "bmp")]
        ImageOutputFormat::Bmp => bmp::BmpEncoder::new(fout).write_image(buf, width, height, color),
        #[cfg(feature = "farbfeld")]
        ImageOutputFormat::Farbfeld => farbfeld::FarbfeldEncoder::new(fout).write_image(buf, width, height, color),
        #[cfg(feature = "tga")]
        ImageOutputFormat::Tga => tga::TgaEncoder::new(fout).write_image(buf, width, height, color),
        #[cfg(feature = "openexr")]
        ImageOutputFormat::Exr => openexr::write_image(fout, buf, width, height, color),
        #[cfg(feature = "tiff")]
        ImageOutputFormat::Tiff => {
            let mut cursor = std::io::Cursor::new(Vec::new());
            tiff::TiffEncoder::new(&mut cursor).write_image(buf, width, height, color)?;
            fout.write(&cursor.into_inner()[..])
                .map(|_| ())
                .map_err(ImageError::IoError)
        }
        #[cfg(feature = "avif-encoder")]
        ImageOutputFormat::Avif => avif::AvifEncoder::new(fout).write_image(buf, width, height, color),

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
    (&[0x76, 0x2f, 0x31, 0x01], ImageFormat::Exr), // = &exr::meta::magic_number::BYTES
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
