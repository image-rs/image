//! Test saving "default" and specific quality jpeg.
#![cfg(all(feature = "jpeg", feature = "tiff"))]
extern crate image;

use image::{ImageOutputFormat, JPEG};

#[test]
fn jqeg_qualitys() {
    let img = image::open("tests/images/tiff/testsuite/lenna.tiff").unwrap();

    let mut default = vec![];
    img.save(&mut default, JPEG).unwrap();
    assert_eq!(&[255, 216], &default[..2]);

    let mut small = vec![];
    img.save(&mut small, ImageOutputFormat::JPEG(10)).unwrap();
    assert_eq!(&[255, 216], &small[..2]);

    assert!(small.len() < default.len());

    let mut large = vec![];
    img.save(&mut large, ImageOutputFormat::JPEG(99)).unwrap();
    assert_eq!(&[255, 216], &large[..2]);

    assert!(large.len() > default.len());
}
