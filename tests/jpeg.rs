//! Test saving "default" and specific quality jpeg.
#![cfg(all(feature = "jpeg", feature = "tiff"))]
extern crate image;

use image::codecs::jpeg::{JpegDecoder, JpegEncoder};
use image::ImageDecoder;
use std::fs;
use std::io::Cursor;

#[test]
fn save_qualities() {
    let img = image::open("tests/images/tiff/testsuite/mandrill.tiff").unwrap();

    let mut default = vec![];
    let encoder = JpegEncoder::new(&mut default);
    img.write_with_encoder(encoder).unwrap();
    assert_eq!(&[255, 216], &default[..2]);

    let mut small = vec![];
    let encoder = JpegEncoder::new_with_quality(&mut small, 10);
    img.write_with_encoder(encoder).unwrap();
    assert_eq!(&[255, 216], &small[..2]);

    assert!(small.len() < default.len());

    let mut large = vec![];
    let encoder = JpegEncoder::new_with_quality(&mut large, 99);
    img.write_with_encoder(encoder).unwrap();
    assert_eq!(&[255, 216], &large[..2]);

    assert!(large.len() > default.len());
}

#[test]
fn strict_mode() {
    let mut image = fs::read("tests/images/jpg/progressive/cat.jpg").unwrap();
    image.truncate(image.len() - 1000); // simulate a truncated image.

    let mut decoder = JpegDecoder::new(Cursor::new(&image)).unwrap();
    let mut buffer = vec![0; decoder.total_bytes() as usize];
    decoder.set_strict_mode(false);
    assert!(decoder.read_image(&mut buffer).is_ok());

    let mut decoder = JpegDecoder::new(Cursor::new(&image)).unwrap();
    decoder.set_strict_mode(true);
    assert!(decoder.read_image(&mut buffer).is_err());
}
