//! Test saving "default" and specific quality jpeg.
#![cfg(all(feature = "jpeg", feature = "tiff"))]
extern crate image;

use image::codecs::jpeg::JpegEncoder;

#[test]
fn jqeg_qualitys() {
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
