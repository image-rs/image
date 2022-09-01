#![no_main]
#[macro_use]
extern crate libfuzzer_sys;
extern crate image;

use image::codecs::openexr::*;
use image::ColorType;
use image::ImageDecoder;
use image::ImageEncoder;
use image::ImageResult;
use std::convert::TryFrom;
use std::io::Cursor;
use std::io::Read;
use std::io::Seek;
use std::io::SeekFrom;
use std::io::Write;

// "just dont panic"
fn roundtrip(bytes: &[u8]) -> ImageResult<()> {
    /// Read the file from the specified path into an `Rgba32FImage`.
    // TODO this method should probably already exist in the main image crate
    fn read_as_rgba_byte_image(read: &mut (impl Read + Seek)) -> ImageResult<(u32, u32, Vec<u8>)> {
        let current_pos = read.stream_position()?;
        let total_size = read.seek(SeekFrom::End(0))?;
        let _ = read.seek(SeekFrom::Start(current_pos))?;

        let decoder = OpenExrDecoder::with_alpha_preference(read, Some(true))?;
        let (width, height) = decoder.dimensions();

        let decoded_size = usize::try_from(decoder.total_bytes()).unwrap();
        if total_size - current_pos < decoded_size as u64 {
            return Err(image::ImageError::Limits(
                image::error::LimitError::from_kind(
                    image::error::LimitErrorKind::InsufficientMemory,
                ),
            ));
        }
        let mut buffer = vec![0; decoded_size];
        decoder.read_image(buffer.as_mut_slice())?;

        Ok((width, height, buffer))
    }

    /// Write an `Rgba32FImage`.
    /// Assumes the writer is buffered. In most cases,
    /// you should wrap your writer in a `BufWriter` for best performance.
    // TODO this method should probably already exist in the main image crate
    fn write_rgba_image(
        write: impl Write + Seek,
        (width, height, data): &(u32, u32, Vec<u8>),
    ) -> ImageResult<()> {
        OpenExrEncoder::new(write).write_image(data.as_slice(), *width, *height, ColorType::Rgba32F)
    }

    let decoded_image = read_as_rgba_byte_image(&mut Cursor::new(bytes))?;

    let mut bytes = Vec::with_capacity(bytes.len() + 20);
    write_rgba_image(Cursor::new(&mut bytes), &decoded_image)?;

    let redecoded_image = read_as_rgba_byte_image(&mut Cursor::new(bytes))?;

    // if both images are valid, assert read/write consistency
    assert_eq!(
        decoded_image, redecoded_image,
        "image was valid but was not reproducible"
    );
    Ok(())
}

fuzz_target!(|data: &[u8]| {
    let _img = roundtrip(data); // fixme not optimized away?
});
