//! Test saving "default" and specific quality jpeg.
#![cfg(all(feature = "jpeg", feature = "tiff"))]
use std::path::PathBuf;
use image::{ImageOutputFormat, ImageFormat};

#[test]
fn jqeg_qualitys() {
    let mut path = PathBuf::from("tests/images/tiff/testsuite/mandrill.tiff");
    xtest_data::setup!().rewrite([&mut path]).build();
    let img = image::open(path).unwrap();

    let mut default = vec![];
    img.write_to(&mut default, ImageFormat::Jpeg).unwrap();
    assert_eq!(&[255, 216], &default[..2]);

    let mut small = vec![];
    img.write_to(&mut small, ImageOutputFormat::Jpeg(10))
        .unwrap();
    assert_eq!(&[255, 216], &small[..2]);

    assert!(small.len() < default.len());

    let mut large = vec![];
    img.write_to(&mut large, ImageOutputFormat::Jpeg(99))
        .unwrap();
    assert_eq!(&[255, 216], &large[..2]);

    assert!(large.len() > default.len());
}
