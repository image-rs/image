use std::fs::File;
use std::io::{BufRead, BufWriter, Seek, Write};
use std::mem::size_of;
use std::path::Path;

use crate::io::encoder::ImageEncoderBoxed;
use crate::io::signatures::guess_format_from_signature;
use crate::{ExtendedColorType, ImageReader};

use crate::error::{ImageError, ImageFormatHint, ImageResult, LimitError, LimitErrorKind};
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
    format.create_encoder(buffered_write)
}

/// Guess image format from memory block
///
/// Makes an educated guess about the image format based on the Magic Bytes at the beginning.
/// TGA is not supported by this function.
/// This is not to be trusted on the validity of the whole memory block
pub fn guess_format(buffer: &[u8]) -> ImageResult<ImageFormat> {
    match guess_format_from_signature(buffer) {
        Some(format) => Ok(format),
        None => Err(ImageError::Unsupported(ImageFormatHint::Unknown.into())),
    }
}

/// Reads all of the bytes of a decoder into a Vec<T>. No particular alignment
/// of the output buffer is guaranteed.
///
/// Panics if there isn't enough memory to decode the image.
pub(crate) fn decoder_to_vec<T>(decoder: impl ImageDecoder) -> ImageResult<Vec<T>>
where
    T: crate::traits::Primitive + bytemuck::Pod,
{
    let total_bytes = usize::try_from(decoder.total_bytes());
    if total_bytes.is_err() || total_bytes.unwrap() > isize::MAX as usize {
        return Err(ImageError::Limits(LimitError::from_kind(
            LimitErrorKind::InsufficientMemory,
        )));
    }

    let mut buf = vec![num_traits::Zero::zero(); total_bytes.unwrap() / size_of::<T>()];
    decoder.read_image(bytemuck::cast_slice_mut(buf.as_mut_slice()))?;
    Ok(buf)
}
