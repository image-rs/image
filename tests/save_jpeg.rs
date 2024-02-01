//! Test saving "default" and specific quality jpeg.
#![cfg(all(feature = "jpeg", feature = "tiff"))]
extern crate image;

use image::{ImageFormat, ImageOutputFormat};
use std::io::Cursor;

#[test]
fn jqeg_qualitys() {
    let img = image::open("tests/images/tiff/testsuite/mandrill.tiff").unwrap();

    let mut default = vec![];
    img.write_to(&mut Cursor::new(&mut default), ImageFormat::Jpeg)
        .unwrap();
    assert_eq!(&[255, 216], &default[..2]);

    let mut small = vec![];
    img.write_to(&mut Cursor::new(&mut small), ImageOutputFormat::Jpeg(10))
        .unwrap();
    assert_eq!(&[255, 216], &small[..2]);

    assert!(small.len() < default.len());

    let mut large = vec![];
    img.write_to(&mut Cursor::new(&mut large), ImageOutputFormat::Jpeg(99))
        .unwrap();
    assert_eq!(&[255, 216], &large[..2]);

    assert!(large.len() > default.len());
}
