//! Test saving images to PNM.
#![cfg(all(feature = "png", feature = "pnm"))]

extern crate image;
use std::fs;

use image::{GenericImageView as _, Luma, Rgb};

#[test]
fn save_16bit_to_pbm() {
    let output_dir = "tests/output/pbm/images";
    fs::create_dir_all(output_dir).expect("failed to create output directory");
    let output_file = "tests/output/pbm/images/basi0g16.pbm";

    let img = image::open("tests/images/png/16bpc/basi0g16.png").expect("failed to load image");
    img.save(output_file).expect("failed to save image");

    // inspect image written
    let img = image::open(output_file).expect("failed to load saved image");
    assert_eq!(img.color(), image::ColorType::L16);
    assert_eq!(img.dimensions(), (32, 32));

    let img = img.as_luma16().unwrap();

    // inspect a few pixels
    assert_eq!(*img.get_pixel(0, 0), Luma([0]));
    assert_eq!(*img.get_pixel(31, 0), Luma([47871]));
    assert_eq!(*img.get_pixel(22, 29), Luma([65535]));
}

#[test]
fn save_16bit_to_ppm() {
    let output_dir = "tests/output/ppm/images";
    fs::create_dir_all(output_dir).expect("failed to create output directory");
    let output_file = "tests/output/ppm/images/basn2c16.ppm";

    let img = image::open("tests/images/png/16bpc/basn2c16.png").expect("failed to load image");
    img.save(output_file).expect("failed to save image");

    // inspect image written
    let img = image::open(output_file).expect("failed to load saved image");
    assert_eq!(img.color(), image::ColorType::Rgb16);
    assert_eq!(img.dimensions(), (32, 32));

    let img = img.as_rgb16().unwrap();

    // inspect a few pixels
    assert_eq!(*img.get_pixel(0, 0), Rgb([65535, 65535, 0]));
    assert_eq!(*img.get_pixel(31, 0), Rgb([0, 65535, 0]));
    assert_eq!(*img.get_pixel(22, 29), Rgb([19026, 4228, 42281]));
}
