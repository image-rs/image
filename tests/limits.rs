//! Test enforcement of size and memory limits for various decoding APIs.
//!
//! We create an image, encode it with a given format, and then decode it with various APIs.
//! We expect each decoding API to return an error because the limits are too low.//!
//! The ones that do not return an error are buggy.
//!
//! There are several such buggy APIs in the crate. The tests for them are written but commented out.
//! Pull requests fixing these APIs are very welcome.
//!
//! It is possible that a maliciously crafted file could bypass these checks
//! and cause a large allocation inside the decoder despite these limits.
//! These tests cannot catch that, but fuzzing can.
//!
//! These tests also don't cover animation (yet). Adding tests for that would be very welcome too.

use std::io::Cursor;

use image::{load_from_memory_with_format, ImageFormat, ImageReader, Limits, RgbImage};

const WIDTH: u32 = 256;
const HEIGHT: u32 = 256;

fn test_image(format: ImageFormat) -> Vec<u8> {
    let image = RgbImage::new(WIDTH, HEIGHT);
    let mut bytes: Vec<u8> = Vec::new();
    image
        .write_to(&mut Cursor::new(&mut bytes), format)
        .unwrap();
    bytes
}

/// Returns `Limits` with width/height smaller than the test image
fn width_height_limits() -> Limits {
    let mut limits = Limits::no_limits();
    limits.max_image_width = Some(WIDTH / 2);
    limits.max_image_height = Some(HEIGHT / 2);
    limits
}

/// Returns `Limits` with allocation limit smaller than the test image
fn allocation_limits() -> Limits {
    let mut limits = Limits::no_limits();
    limits.max_alloc = Some(((WIDTH / 2) * (HEIGHT / 2) * 3).into()); // matches dimension limits for RGB images
    limits
}

/// Returns `Limits` that allow decoding this image without issues
fn permissive_limits() -> Limits {
    let mut limits = Limits::no_limits();
    limits.max_image_width = Some(WIDTH);
    limits.max_image_height = Some(HEIGHT);
    limits.max_alloc = Some((WIDTH * HEIGHT * 5).into()); // `* 3`` would be an exact fit for RGB; `* 5`` allows some slack space
    limits
}

fn load_through_reader(
    input: &[u8],
    format: ImageFormat,
    limits: Limits,
) -> Result<image::DynamicImage, image::ImageError> {
    let mut reader = ImageReader::new(Cursor::new(input));
    reader.set_format(format);
    reader.limits(limits);
    reader.decode()
}

#[test]
#[cfg(feature = "gif")]
fn gif() {
    let image = test_image(ImageFormat::Gif);
    // sanity check that our image loads successfully without limits
    assert!(load_from_memory_with_format(&image, ImageFormat::Gif).is_ok());
    // check that the limits implementation is not overly restrictive
    assert!(load_through_reader(&image, ImageFormat::Gif, permissive_limits()).is_ok());
    // image::ImageReader
    assert!(load_through_reader(&image, ImageFormat::Gif, width_height_limits()).is_err());
    assert!(load_through_reader(&image, ImageFormat::Gif, allocation_limits()).is_err());
    // BROKEN!
}

#[test]
#[cfg(feature = "png")]
fn png() {
    let image = test_image(ImageFormat::Png);
    // sanity check that our image loads successfully without limits
    assert!(load_from_memory_with_format(&image, ImageFormat::Png).is_ok());
    // check that the limits implementation is not overly restrictive
    assert!(load_through_reader(&image, ImageFormat::Png, permissive_limits()).is_ok());
    // image::ImageReader
    assert!(load_through_reader(&image, ImageFormat::Png, width_height_limits()).is_err());
    assert!(load_through_reader(&image, ImageFormat::Png, allocation_limits()).is_err());
}

#[test]
#[cfg(feature = "jpeg")]
fn jpeg() {
    let image = test_image(ImageFormat::Jpeg);
    // sanity check that our image loads successfully without limits
    assert!(load_from_memory_with_format(&image, ImageFormat::Jpeg).is_ok());
    // check that the limits implementation is not overly restrictive
    assert!(load_through_reader(&image, ImageFormat::Jpeg, permissive_limits()).is_ok());
    // image::ImageReader
    assert!(load_through_reader(&image, ImageFormat::Jpeg, width_height_limits()).is_err());
    assert!(load_through_reader(&image, ImageFormat::Jpeg, allocation_limits()).is_err());
}

#[test]
#[cfg(feature = "webp")]
fn webp() {
    let image = test_image(ImageFormat::WebP);
    // sanity check that our image loads successfully without limits
    assert!(load_from_memory_with_format(&image, ImageFormat::WebP).is_ok());
    // check that the limits implementation is not overly restrictive
    assert!(load_through_reader(&image, ImageFormat::WebP, permissive_limits()).is_ok());
    // image::ImageReader
    assert!(load_through_reader(&image, ImageFormat::WebP, width_height_limits()).is_err());
    assert!(load_through_reader(&image, ImageFormat::WebP, allocation_limits()).is_err());
}

#[test]
#[cfg(feature = "tiff")]
fn tiff() {
    let image = test_image(ImageFormat::Tiff);
    // sanity check that our image loads successfully without limits
    assert!(load_from_memory_with_format(&image, ImageFormat::Tiff).is_ok());

    // check that the limits implementation is not overly restrictive
    // The TIFF decoder is special - the `image` crate API does not play well with `tiff` crate API,
    // so there is a copy from the buffer allocated by `tiff` to a buffer allocated by `image`.
    // This results in memory usage overhead the size of the output buffer.
    let mut tiff_permissive_limits = permissive_limits();
    tiff_permissive_limits.max_alloc = Some((WIDTH * HEIGHT * 10).into()); // `* 9` would be exactly three output buffers, `* 10`` has some slack space
    load_through_reader(&image, ImageFormat::Tiff, tiff_permissive_limits).unwrap();

    // image::ImageReader
    assert!(load_through_reader(&image, ImageFormat::Tiff, width_height_limits()).is_err());
    assert!(load_through_reader(&image, ImageFormat::Tiff, allocation_limits()).is_err());
}

#[test]
#[cfg(all(feature = "avif", feature = "avif-native"))]
fn avif() {
    use image::codecs::avif::AvifDecoder;

    let image = test_image(ImageFormat::Avif);
    // sanity check that our image loads successfully without limits
    assert!(load_from_memory_with_format(&image, ImageFormat::Avif).is_ok());
    // check that the limits implementation is not overly restrictive
    assert!(load_through_reader(&image, ImageFormat::Avif, permissive_limits()).is_ok());
    // image::ImageReader
    assert!(load_through_reader(&image, ImageFormat::Avif, width_height_limits()).is_err());
    assert!(load_through_reader(&image, ImageFormat::Avif, allocation_limits()).is_err());
}

#[test]
#[cfg(feature = "bmp")]
fn bmp() {
    let image = test_image(ImageFormat::Bmp);
    // sanity check that our image loads successfully without limits
    assert!(load_from_memory_with_format(&image, ImageFormat::Bmp).is_ok());
    // check that the limits implementation is not overly restrictive
    assert!(load_through_reader(&image, ImageFormat::Bmp, permissive_limits()).is_ok());
    // image::ImageReader
    assert!(load_through_reader(&image, ImageFormat::Bmp, width_height_limits()).is_err());
    assert!(load_through_reader(&image, ImageFormat::Bmp, allocation_limits()).is_err());
}
