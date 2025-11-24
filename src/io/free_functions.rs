use std::fs::File;
use std::io::{BufRead, BufWriter, Seek, Write};
use std::path::Path;
use std::{iter, mem::size_of};

use crate::io::encoder::ImageEncoderBoxed;
use crate::{codecs::*, ExtendedColorType, ImageReader};

use crate::error::{
    ImageError, ImageFormatHint, ImageResult, LimitError, LimitErrorKind, UnsupportedError,
    UnsupportedErrorKind,
};
use crate::{DynamicImage, ImageDecoder, ImageFormat};

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

/// Saves the supplied buffer to a file at the path specified.
///
/// The image format is derived from the file extension. The buffer is assumed to have the correct
/// format according to the specified color type. This will lead to corrupted files if the buffer
/// contains malformed data.
pub fn save_buffer(
    path: impl AsRef<Path>,
    buf: &[u8],
    width: u32,
    height: u32,
    color: impl Into<ExtendedColorType>,
) -> ImageResult<()> {
    let format = ImageFormat::from_path(path.as_ref())?;
    save_buffer_with_format(path, buf, width, height, color, format)
}

/// Saves the supplied buffer to a file given the path and desired format.
///
/// The buffer is assumed to have the correct format according to the specified color type. This
/// will lead to corrupted files if the buffer contains malformed data.
pub fn save_buffer_with_format(
    path: impl AsRef<Path>,
    buf: &[u8],
    width: u32,
    height: u32,
    color: impl Into<ExtendedColorType>,
    format: ImageFormat,
) -> ImageResult<()> {
    let buffered_file_write = &mut BufWriter::new(File::create(path)?); // always seekable
    let encoder = encoder_for_format(format, buffered_file_write)?;
    encoder.write_image(buf, width, height, color.into())
}

pub(crate) fn encoder_for_format<'a, W: Write + Seek>(
    format: ImageFormat,
    buffered_write: &'a mut W,
) -> ImageResult<Box<dyn ImageEncoderBoxed + 'a>> {
    Ok(match format {
        #[cfg(feature = "png")]
        ImageFormat::Png => Box::new(png::PngEncoder::new(buffered_write)),
        #[cfg(feature = "jpeg")]
        ImageFormat::Jpeg => Box::new(jpeg::JpegEncoder::new(buffered_write)),
        #[cfg(feature = "pnm")]
        ImageFormat::Pnm => Box::new(pnm::PnmEncoder::new(buffered_write)),
        #[cfg(feature = "gif")]
        ImageFormat::Gif => Box::new(gif::GifEncoder::new(buffered_write)),
        #[cfg(feature = "ico")]
        ImageFormat::Ico => Box::new(ico::IcoEncoder::new(buffered_write)),
        #[cfg(feature = "bmp")]
        ImageFormat::Bmp => Box::new(bmp::BmpEncoder::new(buffered_write)),
        #[cfg(feature = "ff")]
        ImageFormat::Farbfeld => Box::new(farbfeld::FarbfeldEncoder::new(buffered_write)),
        #[cfg(feature = "tga")]
        ImageFormat::Tga => Box::new(tga::TgaEncoder::new(buffered_write)),
        #[cfg(feature = "exr")]
        ImageFormat::OpenExr => Box::new(openexr::OpenExrEncoder::new(buffered_write)),
        #[cfg(feature = "tiff")]
        ImageFormat::Tiff => Box::new(tiff::TiffEncoder::new(buffered_write)),
        #[cfg(feature = "avif")]
        ImageFormat::Avif => Box::new(avif::AvifEncoder::new(buffered_write)),
        #[cfg(feature = "qoi")]
        ImageFormat::Qoi => Box::new(qoi::QoiEncoder::new(buffered_write)),
        #[cfg(feature = "webp")]
        ImageFormat::WebP => Box::new(webp::WebPEncoder::new_lossless(buffered_write)),
        #[cfg(feature = "hdr")]
        ImageFormat::Hdr => Box::new(hdr::HdrEncoder::new(buffered_write)),
        _ => {
            return Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    ImageFormatHint::Unknown,
                    UnsupportedErrorKind::Format(ImageFormatHint::Name(format!("{format:?}"))),
                ),
            ));
        }
    })
}

static MAGIC_BYTES: [(&[u8], &[u8], ImageFormat); 22] = [
    (b"\x89PNG\r\n\x1a\n", b"", ImageFormat::Png),
    (&[0xff, 0xd8, 0xff], b"", ImageFormat::Jpeg),
    (b"GIF89a", b"", ImageFormat::Gif),
    (b"GIF87a", b"", ImageFormat::Gif),
    (
        b"RIFF\0\0\0\0WEBP",
        b"\xFF\xFF\xFF\xFF\0\0\0\0",
        ImageFormat::WebP,
    ),
    (b"MM\x00*", b"", ImageFormat::Tiff),
    (b"II*\x00", b"", ImageFormat::Tiff),
    (b"DDS ", b"", ImageFormat::Dds),
    (b"BM", b"", ImageFormat::Bmp),
    (&[0, 0, 1, 0], b"", ImageFormat::Ico),
    (b"#?RADIANCE", b"", ImageFormat::Hdr),
    (b"\0\0\0\0ftypavif", b"\xFF\xFF\0\0", ImageFormat::Avif),
    (&[0x76, 0x2f, 0x31, 0x01], b"", ImageFormat::OpenExr), // = &exr::meta::magic_number::BYTES
    (b"qoif", b"", ImageFormat::Qoi),
    (b"P1", b"", ImageFormat::Pnm),
    (b"P2", b"", ImageFormat::Pnm),
    (b"P3", b"", ImageFormat::Pnm),
    (b"P4", b"", ImageFormat::Pnm),
    (b"P5", b"", ImageFormat::Pnm),
    (b"P6", b"", ImageFormat::Pnm),
    (b"P7", b"", ImageFormat::Pnm),
    (b"farbfeld", b"", ImageFormat::Farbfeld),
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
    for &(signature, mask, format) in &MAGIC_BYTES {
        if mask.is_empty() {
            if buffer.starts_with(signature) {
                return Some(format);
            }
        } else if buffer.len() >= signature.len()
            && buffer
                .iter()
                .zip(signature.iter())
                .zip(mask.iter().chain(iter::repeat(&0xFF)))
                .all(|((&byte, &sig), &mask)| byte & mask == sig)
        {
            return Some(format);
        }
    }

    None
}

/// Reads all of the bytes of a decoder into a Vec<T>. No particular alignment
/// of the output buffer is guaranteed.
///
/// Panics if there isn't enough memory to decode the image.
pub(crate) fn decoder_to_vec<T>(decoder: &mut (impl ImageDecoder + ?Sized)) -> ImageResult<Vec<T>>
where
    T: crate::traits::Primitive + bytemuck::Pod,
{
    let total_bytes = usize::try_from(decoder.peek_layout()?.total_bytes());
    if total_bytes.is_err() || total_bytes.unwrap() > isize::MAX as usize {
        return Err(ImageError::Limits(LimitError::from_kind(
            LimitErrorKind::InsufficientMemory,
        )));
    }

    let mut buf = vec![num_traits::Zero::zero(); total_bytes.unwrap() / size_of::<T>()];
    decoder.read_image(bytemuck::cast_slice_mut(buf.as_mut_slice()))?;
    Ok(buf)
}

#[test]
fn test_guess_format_agrees_with_extension() {
    let found = glob::glob("tests/images/**/*.*")
        .unwrap()
        .filter_map(|path| test_file(path.unwrap()))
        .collect::<std::collections::HashSet<_>>();

    // Check we’ve actually tested anything.
    for fmt in ImageFormat::all() {
        use ImageFormat::*;
        let found = found.contains(&fmt);
        if matches!(
            fmt,
            Bmp | Farbfeld | Gif | Ico | Hdr | Jpeg | OpenExr | Png | Pnm | Qoi | Tiff | WebP
        ) {
            assert!(found, "No {fmt:?} test files found");
        } else {
            assert!(!found, "Add `{fmt:?}` to test_guess_format");
        }
    }

    fn test_file(path: std::path::PathBuf) -> Option<ImageFormat> {
        let is_supported = |fmt| MAGIC_BYTES.iter().any(|item| fmt == item.2);

        let disppath = path.display();
        match ImageFormat::from_path(path.as_path()) {
            Ok(fmt) if is_supported(fmt) => {
                let buf = std::fs::read(path.as_path()).unwrap();
                let got = guess_format_impl(&buf);
                assert_eq!(Some(fmt), got, "{disppath}");
                Some(fmt)
            }
            Ok(fmt) => {
                eprintln!("{disppath}: unguessable format {fmt:?}, ignoring");
                None
            }
            Err(_) => {
                eprintln!("{disppath}: unrecognised extension, ignoring");
                None
            }
        }
    }
}
