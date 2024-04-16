use std::fs;

use image::buffer::ConvertBuffer;
use image::{DynamicSerialImage, Luma, Rgb, Rgba, SerialGrayImage, SerialImageBuffer};

#[test]
fn test_rgbu8_to_rgbu16() {
    // Create an all white image using Rgb<u16>s for pixel values
    let image_u16 =
        SerialImageBuffer::from_pixel(2, 2, image::Rgb::<u16>([u16::MAX, u16::MAX, u16::MAX]));

    // Create an all white image using Rgb<u8>s for pixel values and convert it
    // to Rgb<u16>s.
    let image_u8 = SerialImageBuffer::from_pixel(2, 2, image::Rgb::<u8>([u8::MAX, u8::MAX, u8::MAX]));
    let image_converted: SerialImageBuffer<Rgb<u16>, _> = image_u8.convert();

    assert_eq!(image_u16, image_converted);
}

#[test]
fn test_rgbau8_to_rgbau16() {
    let image_u16 = SerialImageBuffer::from_pixel(
        2,
        2,
        image::Rgba::<u16>([u16::MAX, u16::MAX, u16::MAX, u16::MAX]),
    );

    let image_u8 = SerialImageBuffer::from_pixel(
        2,
        2,
        image::Rgba::<u8>([u8::MAX, u8::MAX, u8::MAX, u8::MAX]),
    );
    let image_converted: SerialImageBuffer<Rgba<u16>, _> = image_u8.convert();

    assert_eq!(image_u16, image_converted);
}

#[test]
fn test_image_to_json() {
    let mut image = SerialGrayImage::new(128, 128);
    for (x, y, pixel) in image.enumerate_pixels_mut() {
        *pixel = Luma([(x + y) as u8]);
    }
    let image: DynamicSerialImage = image.into();
    let json = serde_json::to_string(&image).unwrap();
    fs::write("image.json", json).expect("Unable to write file");
}

#[test]
fn test_image_from_json() {
    let json = fs::read_to_string("image.json").expect("Unable to read file");
    let image: DynamicSerialImage = serde_json::from_str(&json).unwrap();
    let image: SerialGrayImage = image.into();
    for (x, y, pixel) in image.enumerate_pixels() {
        assert_eq!(pixel[0], (x + y) as u8);
    }
}
