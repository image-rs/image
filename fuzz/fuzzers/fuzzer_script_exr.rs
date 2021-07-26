#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate image;

use std::io::Cursor;
use image::ImageResult;
use image::codecs::openexr::*;
use std::io::Seek;
use std::io::BufRead;
use std::convert::TryFrom;
use image::ImageDecoder;
use image::ImageEncoder;
use image::ColorType;

// "just dont panic"
fn roundtrip(bytes: &[u8]) -> ImageResult<()> {
    use std::io::Write;

    /// Read the file from the specified path into an `Rgba32FImage`.
    // TODO this method should probably already exist in the main image crate
    fn read_as_rgba_byte_image(read: impl BufRead + Seek) -> ImageResult<(u32,u32,Vec<u8>)> {
        let decoder = OpenExrDecoder::with_alpha_preference(read, Some(true))?;
        let (width, height) = decoder.dimensions();

        let mut buffer = vec![0; usize::try_from(decoder.total_bytes()).unwrap()];
        decoder.read_image(buffer.as_mut_slice())?;

        Ok((width, height, buffer))
    }

    /// Write an `Rgba32FImage`.
    /// Assumes the writer is buffered. In most cases,
    /// you should wrap your writer in a `BufWriter` for best performance.
    // TODO this method should probably already exist in the main image crate
    fn write_rgba_image(write: impl Write/* + Seek*/, (width, height, data): &(u32,u32,Vec<u8>)) -> ImageResult<()> {
        OpenExrEncoder::new(write).write_image(
            data.as_slice(), *width, *height,
            ColorType::Rgba32F
        )
    }


    let decoded_image = read_as_rgba_byte_image(Cursor::new(bytes))?;

    let mut bytes = Vec::with_capacity(bytes.len() + 20);
    write_rgba_image(Cursor::new(&mut bytes), &decoded_image)?;

    let redecoded_image = read_as_rgba_byte_image(Cursor::new(bytes))?;

    // if both images are valid, assert read/write consistency
    assert_eq!(decoded_image, redecoded_image, "image was valid but was not reproducible");
    Ok(())
}

fuzz_target!(|data: &[u8]| {
    let _img = roundtrip(data); // fixme not optimized away?
});
