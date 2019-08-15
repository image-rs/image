use std::fs::File;
use std::io;
use std::io::{BufRead, BufReader, BufWriter, Seek};
use std::path::Path;
use std::u32;

#[cfg(feature = "bmp")]
use bmp;
#[cfg(feature = "gif_codec")]
use gif;
#[cfg(feature = "hdr")]
use hdr;
#[cfg(feature = "ico")]
use ico;
#[cfg(feature = "jpeg")]
use jpeg;
#[cfg(feature = "png_codec")]
use png;
#[cfg(feature = "pnm")]
use pnm;
#[cfg(feature = "tga")]
use tga;
#[cfg(feature = "tiff")]
use tiff;
#[cfg(feature = "webp")]
use webp;

// use buffer::{ConvertBuffer, GrayAlphaImage, GrayImage, ImageBuffer, Pixel, RgbImage, RgbaImage, BgrImage, BgraImage};
use color;
use image;
use dynimage::DynamicImage;
use image::{ImageDecoder, ImageFormat, ImageResult};

pub(crate) fn open_impl(path: &Path) -> ImageResult<DynamicImage> {
    let fin = match File::open(path) {
        Ok(f) => f,
        Err(err) => return Err(image::ImageError::IoError(err)),
    };
    let fin = BufReader::new(fin);

    load(fin, ImageFormat::from_path(path)?)
}

/// Create a new image from a Reader
pub fn load<R: BufRead + Seek>(r: R, format: ImageFormat) -> ImageResult<DynamicImage> {
    #[allow(deprecated, unreachable_patterns)]
    // Default is unreachable if all features are supported.
    match format {
        #[cfg(feature = "png_codec")]
        image::ImageFormat::PNG => DynamicImage::from_decoder(png::PNGDecoder::new(r)?),
        #[cfg(feature = "gif_codec")]
        image::ImageFormat::GIF => DynamicImage::from_decoder(gif::Decoder::new(r)?),
        #[cfg(feature = "jpeg")]
        image::ImageFormat::JPEG => DynamicImage::from_decoder(jpeg::JPEGDecoder::new(r)?),
        #[cfg(feature = "webp")]
        image::ImageFormat::WEBP => DynamicImage::from_decoder(webp::WebpDecoder::new(r)?),
        #[cfg(feature = "tiff")]
        image::ImageFormat::TIFF => DynamicImage::from_decoder(tiff::TIFFDecoder::new(r)?),
        #[cfg(feature = "tga")]
        image::ImageFormat::TGA => DynamicImage::from_decoder(tga::TGADecoder::new(r)?),
        #[cfg(feature = "bmp")]
        image::ImageFormat::BMP => DynamicImage::from_decoder(bmp::BMPDecoder::new(r)?),
        #[cfg(feature = "ico")]
        image::ImageFormat::ICO => DynamicImage::from_decoder(ico::ICODecoder::new(r)?),
        #[cfg(feature = "hdr")]
        image::ImageFormat::HDR => DynamicImage::from_decoder(hdr::HDRAdapter::new(BufReader::new(r))?),
        #[cfg(feature = "pnm")]
        image::ImageFormat::PNM => DynamicImage::from_decoder(pnm::PNMDecoder::new(BufReader::new(r))?),
        _ => Err(image::ImageError::UnsupportedError(format!(
            "A decoder for {:?} is not available.",
            format
        ))),
    }
}

pub(crate) fn image_dimensions_impl(path: &Path) -> ImageResult<(u32, u32)> {
    let fin = File::open(path)?;
    let fin = BufReader::new(fin);

    #[allow(unreachable_patterns)]
    // Default is unreachable if all features are supported.
    let (w, h): (u64, u64) = match image::ImageFormat::from_path(path)? {
        #[cfg(feature = "jpeg")]
        image::ImageFormat::JPEG => jpeg::JPEGDecoder::new(fin)?.dimensions(),
        #[cfg(feature = "png_codec")]
        image::ImageFormat::PNG => png::PNGDecoder::new(fin)?.dimensions(),
        #[cfg(feature = "gif_codec")]
        image::ImageFormat::GIF => gif::Decoder::new(fin)?.dimensions(),
        #[cfg(feature = "webp")]
        image::ImageFormat::WEBP => webp::WebpDecoder::new(fin)?.dimensions(),
        #[cfg(feature = "tiff")]
        image::ImageFormat::TIFF => tiff::TIFFDecoder::new(fin)?.dimensions(),
        #[cfg(feature = "tga")]
        image::ImageFormat::TGA => tga::TGADecoder::new(fin)?.dimensions(),
        #[cfg(feature = "bmp")]
        image::ImageFormat::BMP => bmp::BMPDecoder::new(fin)?.dimensions(),
        #[cfg(feature = "ico")]
        image::ImageFormat::ICO => ico::ICODecoder::new(fin)?.dimensions(),
        #[cfg(feature = "hdr")]
        image::ImageFormat::HDR => hdr::HDRAdapter::new(fin)?.dimensions(),
        #[cfg(feature = "pnm")]
        image::ImageFormat::PNM => {
            pnm::PNMDecoder::new(fin)?.dimensions()
        }
        format => return Err(image::ImageError::UnsupportedError(format!(
            "Image format image/{:?} is not supported.",
            format
        ))),
    };
    if w >= u64::from(u32::MAX) || h >= u64::from(u32::MAX) {
        return Err(image::ImageError::DimensionError);
    }
    Ok((w as u32, h as u32))
}

pub(crate) fn save_buffer_impl(
    path: &Path,
    buf: &[u8],
    width: u32,
    height: u32,
    color: color::ColorType,
) -> io::Result<()> {
    let fout = &mut BufWriter::new(File::create(path)?);
    let ext = path.extension()
        .and_then(|s| s.to_str())
        .map_or("".to_string(), |s| s.to_ascii_lowercase());

    match &*ext {
        #[cfg(feature = "ico")]
        "ico" => ico::ICOEncoder::new(fout).encode(buf, width, height, color),
        #[cfg(feature = "jpeg")]
        "jpg" | "jpeg" => jpeg::JPEGEncoder::new(fout).encode(buf, width, height, color),
        #[cfg(feature = "png_codec")]
        "png" => png::PNGEncoder::new(fout).encode(buf, width, height, color),
        #[cfg(feature = "pnm")]
        "pbm" => pnm::PNMEncoder::new(fout)
            .with_subtype(pnm::PNMSubtype::Bitmap(pnm::SampleEncoding::Binary))
            .encode(buf, width, height, color),
        #[cfg(feature = "pnm")]
        "pgm" => pnm::PNMEncoder::new(fout)
            .with_subtype(pnm::PNMSubtype::Graymap(pnm::SampleEncoding::Binary))
            .encode(buf, width, height, color),
        #[cfg(feature = "pnm")]
        "ppm" => pnm::PNMEncoder::new(fout)
            .with_subtype(pnm::PNMSubtype::Pixmap(pnm::SampleEncoding::Binary))
            .encode(buf, width, height, color),
        #[cfg(feature = "pnm")]
        "pam" => pnm::PNMEncoder::new(fout).encode(buf, width, height, color),
        #[cfg(feature = "bmp")]
        "bmp" => bmp::BMPEncoder::new(fout).encode(buf, width, height, color),
        #[cfg(feature = "tiff")]
        "tif" | "tiff" => tiff::TiffEncoder::new(fout).encode(buf, width, height, color)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, Box::new(e))), // FIXME: see https://github.com/image-rs/image/issues/921
        format => Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            &format!("Unsupported image format image/{:?}", format)[..],
        )),
    }
}

pub(crate) fn save_buffer_with_format_impl(
    path: &Path,
    buf: &[u8],
    width: u32,
    height: u32,
    color: color::ColorType,
    format: ImageFormat,
) -> io::Result<()> {
    let fout = &mut BufWriter::new(File::create(path)?);

    match format {
        #[cfg(feature = "ico")]
        image::ImageFormat::ICO => ico::ICOEncoder::new(fout).encode(buf, width, height, color),
        #[cfg(feature = "jpeg")]
        image::ImageFormat::JPEG => jpeg::JPEGEncoder::new(fout).encode(buf, width, height, color),
        #[cfg(feature = "png_codec")]
        image::ImageFormat::PNG => png::PNGEncoder::new(fout).encode(buf, width, height, color),
        #[cfg(feature = "bmp")]
        image::ImageFormat::BMP => bmp::BMPEncoder::new(fout).encode(buf, width, height, color),
        #[cfg(feature = "tiff")]
        image::ImageFormat::TIFF => tiff::TiffEncoder::new(fout)
            .encode(buf, width, height, color)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, Box::new(e))),
        _ => Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            &format!("Unsupported image format image/{:?}", format)[..],
        )),
    }
}

static MAGIC_BYTES: [(&'static [u8], ImageFormat); 17] = [
    (b"\x89PNG\r\n\x1a\n", ImageFormat::PNG),
    (&[0xff, 0xd8, 0xff], ImageFormat::JPEG),
    (b"GIF89a", ImageFormat::GIF),
    (b"GIF87a", ImageFormat::GIF),
    (b"RIFF", ImageFormat::WEBP), // TODO: better magic byte detection, see https://github.com/image-rs/image/issues/660
    (b"MM\x00*", ImageFormat::TIFF),
    (b"II*\x00", ImageFormat::TIFF),
    (b"BM", ImageFormat::BMP),
    (&[0, 0, 1, 0], ImageFormat::ICO),
    (b"#?RADIANCE", ImageFormat::HDR),
    (b"P1", ImageFormat::PNM),
    (b"P2", ImageFormat::PNM),
    (b"P3", ImageFormat::PNM),
    (b"P4", ImageFormat::PNM),
    (b"P5", ImageFormat::PNM),
    (b"P6", ImageFormat::PNM),
    (b"P7", ImageFormat::PNM),
];

/// Guess image format from memory block
///
/// Makes an educated guess about the image format based on the Magic Bytes at the beginning.
/// TGA is not supported by this function.
/// This is not to be trusted on the validity of the whole memory block
pub fn guess_format(buffer: &[u8]) -> ImageResult<ImageFormat> {
    for &(signature, format) in &MAGIC_BYTES {
        if buffer.starts_with(signature) {
            return Ok(format);
        }
    }
    Err(image::ImageError::UnsupportedError(
        "Unsupported image format".to_string(),
    ))
}
