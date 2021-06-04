#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate image;

use crate::ImageResult;
use std::io::Cursor;
use crate::codecs::openexr::*;

// "just dont panic"
fn roundtrip(bytes: &[u8]) -> ImageResult<()> {
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
