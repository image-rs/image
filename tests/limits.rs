//! Test enforcement of size and memory limits for various decoding APIs.
//!
//! We create an image, encode it with a given format, and then decode it with various APIs.
//! We expect each decoding API to return an error because the limits are too low.//!
//! The ones that do not return an error are buggy.
//!
//! There are several such buggy APIs in the crate. The tests for them are written but commented out.
//! Pull requests fixing these APIs are very welcome.
//!
//! These tests do not yet cover animated images.

use std::io::Cursor;

use image::{
    io::Limits, load_from_memory_with_format, ImageDecoder, ImageFormat, ImageOutputFormat,
    RgbImage,
};

const WIDTH: u32 = 256;
const HEIGHT: u32 = 256;

fn test_image(format: ImageOutputFormat) -> Vec<u8> {
    let image = RgbImage::new(WIDTH, HEIGHT);
    let mut bytes: Vec<u8> = Vec::new();
    image
        .write_to(&mut Cursor::new(&mut bytes), format)
        .unwrap();
    bytes
}

// Returns `Limits` with width/height smaller than the test image
fn width_height_limits() -> Limits {
    let mut limits = Limits::no_limits();
    limits.max_image_width = Some(128);
    limits.max_image_height = Some(128);
    limits
}

fn allocation_limits() -> Limits {
    let mut limits = Limits::no_limits();
    limits.max_alloc = Some(128 * 128 * 3); // matches dimension limits for RGB images
    limits
}

fn load_through_reader(
    input: &[u8],
    format: ImageFormat,
    limits: Limits,
) -> Result<image::DynamicImage, image::ImageError> {
    let mut reader = image::io::Reader::new(Cursor::new(input));
    reader.set_format(format);
    reader.limits(limits);
    reader.decode()
}

#[test]
#[cfg(feature = "gif")]
fn gif() {
    use image::codecs::gif::GifDecoder;

    let image = test_image(ImageOutputFormat::Gif);
    // sanity check that our image loads successfully without limits
    assert!(load_from_memory_with_format(&image, ImageFormat::Gif).is_ok());
    // image::io::Reader
    assert!(load_through_reader(&image, ImageFormat::Gif, width_height_limits()).is_err());
    assert!(load_through_reader(&image, ImageFormat::Gif, allocation_limits()).is_err());
    // GifDecoder
    assert!(GifDecoder::with_limits(Cursor::new(&image), width_height_limits()).is_err());
    assert!(GifDecoder::with_limits(Cursor::new(&image), allocation_limits()).is_err());
    let mut decoder = GifDecoder::new(Cursor::new(&image)).unwrap();
    assert!(decoder.set_limits(width_height_limits()).is_err());
    let mut decoder = GifDecoder::new(Cursor::new(&image)).unwrap();
    assert!(decoder.set_limits(allocation_limits()).is_err());
}

#[test]
#[cfg(feature = "png")]
fn png() {
    use image::codecs::png::PngDecoder;

    let image = test_image(ImageOutputFormat::Png);
    // sanity check that our image loads successfully without limits
    assert!(load_from_memory_with_format(&image, ImageFormat::Png).is_ok());
    // image::io::Reader
    assert!(load_through_reader(&image, ImageFormat::Png, width_height_limits()).is_err());
    assert!(load_through_reader(&image, ImageFormat::Png, allocation_limits()).is_err());
    // PngDecoder
    assert!(PngDecoder::with_limits(Cursor::new(&image), width_height_limits()).is_err());
    //assert!(PngDecoder::with_limits(Cursor::new(&image), allocation_limits()).is_err()); // BROKEN!
    let mut decoder = PngDecoder::new(Cursor::new(&image)).unwrap();
    assert!(decoder.set_limits(width_height_limits()).is_err());
    let mut decoder = PngDecoder::new(Cursor::new(&image)).unwrap();
    assert!(decoder.set_limits(allocation_limits()).is_err());
}
