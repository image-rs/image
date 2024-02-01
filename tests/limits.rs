//! Test enforcement of size and memory limits for various decoding APIs.
//!
//! We create an image, encode it with a given format, and then decode it with various APIs.
//! We expect each decoding API to return an error because the limits are too low.//!
//! The ones that do not return an error are buggy.
//!
//! There are several such buggy APIs in the crate. The tests for them are written but commented out.
//! Pull requests fixing these APIs are very welcome.
//!
//! It is possible that a maliciously crafted file coud bypass these checks
//! and cause a large allocation inside the decoder despite these limits.
//! These tests cannot catch that, but fuzzing can.
//!
//! These tests also don't cover animation (yet). Adding tests for that would be very welcome too.

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
    // check that the limits implementation is not overly restrictive
    assert!(load_through_reader(&image, ImageFormat::Gif, permissive_limits()).is_ok());
    // image::io::Reader
    assert!(load_through_reader(&image, ImageFormat::Gif, width_height_limits()).is_err());
    assert!(load_through_reader(&image, ImageFormat::Gif, allocation_limits()).is_err()); // BROKEN!

    // GifDecoder
    let mut decoder = GifDecoder::new(Cursor::new(&image)).unwrap();
    assert!(decoder.set_limits(width_height_limits()).is_err());
    // no tests for allocation limits because the caller is responsible for allocating the buffer in this case

    // Custom constructor on GifDecoder
    #[allow(deprecated)]
    {
        assert!(GifDecoder::with_limits(Cursor::new(&image), width_height_limits()).is_err());
        // no tests for allocation limits because the caller is responsible for allocating the buffer in this case
    }
}

#[test]
#[cfg(feature = "png")]
fn png() {
    use image::codecs::png::PngDecoder;

    let image = test_image(ImageOutputFormat::Png);
    // sanity check that our image loads successfully without limits
    assert!(load_from_memory_with_format(&image, ImageFormat::Png).is_ok());
    // check that the limits implementation is not overly restrictive
    assert!(load_through_reader(&image, ImageFormat::Png, permissive_limits()).is_ok());
    // image::io::Reader
    assert!(load_through_reader(&image, ImageFormat::Png, width_height_limits()).is_err());
    assert!(load_through_reader(&image, ImageFormat::Png, allocation_limits()).is_err());

    // PngDecoder
    let mut decoder = PngDecoder::new(Cursor::new(&image)).unwrap();
    assert!(decoder.set_limits(width_height_limits()).is_err());
    // No tests for allocation limits because the caller is responsible for allocating the buffer in this case.
    // Unlike many others, the `png` crate does natively support memory limits for auxiliary buffers,
    // but they are not passed down from `set_limits` - only from the `with_limits` constructor.
    // The proper fix is known to require an API break: https://github.com/image-rs/image/issues/2084

    // Custom constructor on PngDecoder
    assert!(PngDecoder::with_limits(Cursor::new(&image), width_height_limits()).is_err());
    // No tests for allocation limits because the caller is responsible for allocating the buffer in this case.
}

#[test]
#[cfg(feature = "jpeg")]
fn jpeg() {
    use image::codecs::jpeg::JpegDecoder;

    let image = test_image(ImageOutputFormat::Jpeg(80));
    // sanity check that our image loads successfully without limits
    assert!(load_from_memory_with_format(&image, ImageFormat::Jpeg).is_ok());
    // check that the limits implementation is not overly restrictive
    assert!(load_through_reader(&image, ImageFormat::Jpeg, permissive_limits()).is_ok());
    // image::io::Reader
    assert!(load_through_reader(&image, ImageFormat::Jpeg, width_height_limits()).is_err());
    assert!(load_through_reader(&image, ImageFormat::Jpeg, allocation_limits()).is_err());

    // JpegDecoder
    let mut decoder = JpegDecoder::new(Cursor::new(&image)).unwrap();
    assert!(decoder.set_limits(width_height_limits()).is_err());
    // No tests for allocation limits because the caller is responsible for allocating the buffer in this case.
}

#[test]
#[cfg(feature = "webp")]
fn webp() {
    use image::codecs::webp::WebPDecoder;

    let image = test_image(ImageOutputFormat::WebP);
    // sanity check that our image loads successfully without limits
    assert!(load_from_memory_with_format(&image, ImageFormat::WebP).is_ok());
    // check that the limits implementation is not overly restrictive
    assert!(load_through_reader(&image, ImageFormat::WebP, permissive_limits()).is_ok());
    // image::io::Reader
    assert!(load_through_reader(&image, ImageFormat::WebP, width_height_limits()).is_err());
    assert!(load_through_reader(&image, ImageFormat::WebP, allocation_limits()).is_err());

    // WebPDecoder
    let mut decoder = WebPDecoder::new(Cursor::new(&image)).unwrap();
    assert!(decoder.set_limits(width_height_limits()).is_err());
    // No tests for allocation limits because the caller is responsible for allocating the buffer in this case.
}

#[test]
#[cfg(feature = "tiff")]
fn tiff() {
    use image::codecs::tiff::TiffDecoder;

    let image = test_image(ImageOutputFormat::Tiff);
    // sanity check that our image loads successfully without limits
    assert!(load_from_memory_with_format(&image, ImageFormat::Tiff).is_ok());

    // check that the limits implementation is not overly restrictive
    // The TIFF decoder is special - the `image` crate API does not play well with `tiff` crate API,
    // so there is a copy from the buffer allocated by `tiff` to a buffer allocated by `image`.
    // This results in memory usage overhead the size of the output buffer.
    let mut tiff_permissive_limits = permissive_limits();
    tiff_permissive_limits.max_alloc = Some((WIDTH * HEIGHT * 10).into()); // `* 9` would be exactly three output buffers, `* 10`` has some slack space
    load_through_reader(&image, ImageFormat::Tiff, tiff_permissive_limits).unwrap();

    // image::io::Reader
    assert!(load_through_reader(&image, ImageFormat::Tiff, width_height_limits()).is_err());
    assert!(load_through_reader(&image, ImageFormat::Tiff, allocation_limits()).is_err());

    // TiffDecoder
    let mut decoder = TiffDecoder::new(Cursor::new(&image)).unwrap();
    assert!(decoder.set_limits(width_height_limits()).is_err());
    // No tests for allocation limits because the caller is responsible for allocating the buffer in this case.
}

#[test]
#[cfg(all(feature = "avif", feature = "avif-decoder"))]
fn avif() {
    use image::codecs::avif::AvifDecoder;

    let image = test_image(ImageOutputFormat::Avif);
    // sanity check that our image loads successfully without limits
    assert!(load_from_memory_with_format(&image, ImageFormat::Avif).is_ok());
    // check that the limits implementation is not overly restrictive
    assert!(load_through_reader(&image, ImageFormat::Avif, permissive_limits()).is_ok());
    // image::io::Reader
    assert!(load_through_reader(&image, ImageFormat::Avif, width_height_limits()).is_err());
    assert!(load_through_reader(&image, ImageFormat::Avif, allocation_limits()).is_err());

    // AvifDecoder
    let mut decoder = AvifDecoder::new(Cursor::new(&image)).unwrap();
    assert!(decoder.set_limits(width_height_limits()).is_err());
    // No tests for allocation limits because the caller is responsible for allocating the buffer in this case.
}

#[test]
#[cfg(feature = "bmp")]
fn bmp() {
    use image::codecs::bmp::BmpDecoder;

    let image = test_image(ImageOutputFormat::Bmp);
    // sanity check that our image loads successfully without limits
    assert!(load_from_memory_with_format(&image, ImageFormat::Bmp).is_ok());
    // check that the limits implementation is not overly restrictive
    assert!(load_through_reader(&image, ImageFormat::Bmp, permissive_limits()).is_ok());
    // image::io::Reader
    assert!(load_through_reader(&image, ImageFormat::Bmp, width_height_limits()).is_err());
    assert!(load_through_reader(&image, ImageFormat::Bmp, allocation_limits()).is_err());

    // BmpDecoder
    let mut decoder = BmpDecoder::new(Cursor::new(&image)).unwrap();
    assert!(decoder.set_limits(width_height_limits()).is_err());
    // No tests for allocation limits because the caller is responsible for allocating the buffer in this case.
}
