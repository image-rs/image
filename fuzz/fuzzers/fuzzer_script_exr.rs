#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate image;

use std::io::Cursor;
use image::ImageResult;
use image::codecs::openexr::*;

// "just dont panic"
fn roundtrip(bytes: &[u8]) -> ImageResult<()> {

    /// Read the file from the specified path into an `Rgba32FImage`.
    fn read_as_rgba_image(read: impl BufRead + Seek) -> ImageResult<Rgb32FImage> {
        let decoder = OpenExrDecoder::with_alpha_preference(read, Some(true))?;
        let (width, height) = decoder.dimensions();
        let buffer: Vec<f32> = decoder_to_vec(decoder)?;

        ImageBuffer::from_raw(width, height, buffer)

            // this should be the only reason for the "from raw" call to fail,
            // even though such a large allocation would probably cause an error much earlier
            .ok_or_else(|| ImageError::Limits(LimitError::from_kind(LimitErrorKind::InsufficientMemory)))
    }

    let decoded_image = read_as_rgba_image(Cursor::new(bytes))?;

    let mut bytes = Vec::with_capacity(bytes.len() + 20);
    write_rgba_image(Cursor::new(&mut bytes), &decoded_image)?;
    let redecoded_image = read_as_rgba_image(Cursor::new(bytes))?;

    // if both images are valid, assert read/write consistency
    assert_eq!(decoded_image, redecoded_image, "image was valid but was not reproducible");
    Ok(())
}

fuzz_target!(|data: &[u8]| {
    let _img = roundtrip(data); // fixme not optimized away?
});
