//! Test saving "default" and specific quality jpeg.
#![cfg(all(feature = "jpeg", feature = "tiff"))]
extern crate image;

use image::ImageOutputFormat;

#[test]
fn jqeg_qualitys() {
    let img = image::open("tests/images/tiff/testsuite/lenna.tiff").unwrap();

    let mut default = vec![];
    img.write_to(&mut default, ImageOutputFormat::JPEG(75)).unwrap();
    assert_eq!(&[255, 216], &default[..2]);

    let mut small = vec![];
    img.write_to(&mut small, ImageOutputFormat::JPEG(10))
        .unwrap();
    assert_eq!(&[255, 216], &small[..2]);

    assert!(small.len() < default.len());

    let mut large = vec![];
    img.write_to(&mut large, ImageOutputFormat::JPEG(99))
        .unwrap();
    assert_eq!(&[255, 216], &large[..2]);

    assert!(large.len() > default.len());
}
